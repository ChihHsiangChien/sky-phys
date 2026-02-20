import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import os
import argparse
from datetime import datetime
import matplotlib.ticker as ticker

# 引入 Dashboard 核心類別以重用物理運算邏輯
from dashboard import SkyPhysDashboard

class TerrainStudy(SkyPhysDashboard):
    def __init__(self, base_path, target_date=None, lookback_hours=24):
        super().__init__(base_path, target_date, lookback_hours)
        
        # 定義研究區域 (Region of Interest) - 湖口台地/新埔河谷
        # [121.0238, 24.8030] to [121.1111, 24.8809]
        # 定義研究區域 (Region of Interest) - 擴大範圍包含關西/龍潭/大溪
        # Expanded to capture the dense flight corridor to the East
        self.roi = {
            'lat_min': 24.7500, # 向南延伸
            'lat_max': 24.9000, # 向北微調
            'lon_min': 121.0200,
            'lon_max': 121.2500 # 向東大幅延伸以捕捉主航道
        }
        
        # 專屬輸出目錄
        self.study_dir = os.path.join(self.report_dir, 'terrain_study')
        if not os.path.exists(self.study_dir):
            os.makedirs(self.study_dir)
            
    def filter_roi(self, df):
        """空間過濾：只保留方框內的數據"""
        print(f"正在鎖定研究區域: {self.roi}")
        mask = (
            (df['lat'] >= self.roi['lat_min']) & 
            (df['lat'] <= self.roi['lat_max']) & 
            (df['lon'] >= self.roi['lon_min']) & 
            (df['lon'] <= self.roi['lon_max'])
        )
        df_roi = df[mask].copy()
        print(f"區域內數據量: {len(df_roi)} 筆 (原始: {len(df)} 筆)")
        return df_roi

    def analyze(self):
        # 1. 載入並計算物理量 (重用父類別邏輯)
        df = self.load_and_process()
        if df is None or len(df) < 10:
            print("數據不足，無法進行地形分析。")
            return

        # 0. 原始分佈驗證 (畫全圖 + ROI 框)
        # 注意：這裡傳入的是尚未濾過的 df (全台灣數據)
        # self.plot_coverage_map(df) # User request: 暫時註解以節省時間 

        # 2. 空間裁切
        df_roi = self.filter_roi(df)
        if len(df_roi) < 10:
            print("警告：目標區域內數據過少，分析可能不準確。")
            return

        # 3. 執行三大分析
        self.plot_vertical_profile(df_roi)
        self.plot_d_value_anomaly(df_roi)
        self.plot_turbulence_analysis(df_roi)
        self.plot_spatial_heatmap(df_roi)
        
        print(f"地形研究報告已產出至: {self.study_dir}")

    def plot_vertical_profile(self, df):
        """1. 垂直溫度剖面 (Vertical Temperature Profiling)"""
        plt.figure(figsize=(10, 8))
        
        # 過濾異常溫度的點 (簡單清洗)
        data = df[(df['temp'] > -80) & (df['temp'] < 40)]
        
        # 分類: 進場機 (<10k ft) vs 巡航機 (>20k ft)
        # 這裡我們用顏色漸層代表高度，觀察連續變化
        sc = plt.scatter(data['temp'], data['alt'], c=data['alt'], cmap='coolwarm', alpha=0.6, s=15, edgecolors='none')
        plt.colorbar(sc, label='Altitude (ft)')
        
        # 繪製標準大氣溫度線 (Standard Atmosphere) 供參考
        # T = 15 - 1.98 * (h/1000)
        alt_range = np.linspace(0, 40000, 100)
        std_temp = 15 - 1.98 * (alt_range / 1000)
        plt.plot(std_temp, alt_range, 'k--', alpha=0.5, label='ISA Model (-1.98°C/1000ft)')
        
        # 標註逆溫層 (如果有數據的話)
        # 用簡單的移動平均來畫趨勢線
        if len(data) > 20:
            # 依高度排序
            data_sorted = data.sort_values('alt')
            # 每個高度區間(例如500ft)取平均溫度
            data_sorted['alt_bin'] = (data_sorted['alt'] // 500) * 500
            profile = data_sorted.groupby('alt_bin')['temp'].mean().reset_index()
            plt.plot(profile['temp'], profile['alt_bin'], 'r-', linewidth=2, label='Observed Profile')

        plt.title(f'Vertical Temperature Profile ({self.roi["lat_min"]:.2f}N - {self.roi["lat_max"]:.2f}N)', fontsize=14)
        plt.xlabel('Static Air Temperature (°C)')
        plt.ylabel('Altitude (ft)')
        plt.grid(True, alpha=0.3)
        plt.legend()
        
        fname = os.path.join(self.study_dir, '1_temp_profile.png')
        plt.savefig(fname, dpi=150)
        plt.close()
        print(f"[產出] 垂直溫度剖面圖: {fname}")

    def calculate_flight_anomalies(self, df):
        """
        核心物理算法：計算航次內的 D-Value 異常 (Residual)
        使用 [線性去趨勢 (Linear Detrending)] 來消除爬升/下降造成的 D-Value 自然變化。
        Residual = Observed - (Slope * Alt + Intercept)
        """
        df = df.copy()
        
        # 預計算 alt_bin 用於之後的 Z-Score
        df['alt_bin'] = (df['alt'] // 1000) * 1000
        
        # 準備接收結果的 Series (預設為 NaN)
        residuals = pd.Series(index=df.index, dtype=float)
        
        # 對每一架飛機單獨處理
        for hex_id, group in df.groupby('hex'):
            if len(group) < 3: continue # 點太少無法擬合
            
            # 判斷是否為 [水平巡航] (高度變化 < 500ft)
            alt_range = group['alt'].max() - group['alt'].min()
            
            if alt_range < 500:
                # 水平飛行：不需要去趨勢，直接減去中位數
                residuals.loc[group.index] = group['d_value'] - group['d_value'].median()
            else:
                # 爬升/下降：使用線性回歸消除高度帶來的 D-Value 變化
                try:
                    z = np.polyfit(group['alt'], group['d_value'], 1)
                    p = np.poly1d(z)
                    expected_d = p(group['alt'])
                    residuals.loc[group.index] = group['d_value'] - expected_d
                except:
                    # 如果擬合失敗，退回到減去中位數
                    residuals.loc[group.index] = group['d_value'] - group['d_value'].median()
                
        df['d_residual'] = residuals
        
        # 計算 Z-Score (Global Scale Normalization)
        # 用於統一不同高度層的顯示尺度
        layer_stds = df.groupby('alt_bin')['d_residual'].transform('std')
        df['z_score'] = df['d_residual'] / layer_stds
        
        return df

    def plot_d_value_anomaly(self, df):
        """2. D-Value 與局部氣壓異常 (線性去趨勢版)"""
        plt.figure(figsize=(18, 6)) # 這裡會被下面的 subplot 覆蓋，但保留結構
        
        # 過濾掉異常值
        df_clean = df.dropna(subset=['alt', 'd_value', 'lat', 'lon']).copy()
        
        # --- 使用新的核心算法計算異常 ---
        df_clean = self.calculate_flight_anomalies(df_clean)
        
        # 放寬高度限制，讓高空對照組也進來
        plot_data = df_clean.dropna(subset=['z_score']) # 移除計算失敗的點
        
        # --- Step 3: 分板繪製 (2x3 Grid) ---
        # Row 1: Low Altitude (< 6k ft) - 更貼近地面，強化地形訊號
        # Row 2: High Altitude (> 20k ft) - 對照組 (自由大氣)
        
        low_df = df_clean[df_clean['alt'] < 6000]
        high_df = df_clean[df_clean['alt'] > 20000]
        
        fig, axes = plt.subplots(2, 3, figsize=(18, 12), sharey='col')
        
        # --- Row 1: Low Altitude ---
        ax = axes[0]
        
        # Col 1: Profile
        if not low_df.empty:
            layer_stats = low_df.groupby('alt_bin')['d_value'].mean().reset_index()
            ax[0].scatter(low_df['alt'], low_df['d_value'], alpha=0.3, s=10, c='purple', label='Raw Data')
            ax[0].plot(layer_stats['alt_bin'], layer_stats['d_value'], 'r-o', linewidth=2, label='Layer Mean')
            ax[0].set_title('Low Alt Profile (<10k ft)')
            ax[0].set_ylabel('D-Value (ft)')
            ax[0].grid(True, alpha=0.3)
            ax[0].legend()
            
        # Col 2: Latitude vs Z-Score
        if not low_df.empty:
            ax[1].axhline(0, color='gray', linestyle='--')
            sns.scatterplot(x='lat', y='z_score', data=low_df, ax=ax[1], hue='alt', palette='viridis', alpha=0.6, legend=False)
            
            # Manual Rolling Mean Trend Line (Robust)
            # Sort by X (lat) to calculate rolling mean
            trend_df = low_df.sort_values('lat')
            # Window size = 5% of data or min 10
            win = max(10, int(len(trend_df) * 0.05))
            trend_df['trend'] = trend_df['z_score'].rolling(window=win, center=True).mean()
            ax[1].plot(trend_df['lat'], trend_df['trend'], 'k-', linewidth=2.5, label='Trend')
            
            ax[1].set_title('Low Alt N-S Anomaly (Signal)')
            ax[1].set_ylabel('Z-Score (σ)')
            # ax[1].axvline(24.86, color='red', linestyle=':', label='River Valley') # Removed
            ax[1].grid(True, alpha=0.3)
            ax[1].set_ylim(-3.5, 3.5)
            
        # Col 3: Longitude vs Z-Score
        if not low_df.empty:
            ax[2].axhline(0, color='gray', linestyle='--')
            sns.scatterplot(x='lon', y='z_score', data=low_df, ax=ax[2], hue='alt', palette='viridis', alpha=0.6, legend=False)
            
            # Manual Rolling Mean Trend Line
            trend_df = low_df.sort_values('lon')
            win = max(10, int(len(trend_df) * 0.05))
            trend_df['trend'] = trend_df['z_score'].rolling(window=win, center=True).mean()
            ax[2].plot(trend_df['lon'], trend_df['trend'], 'k-', linewidth=2.5)
            
            ax[2].set_title('Low Alt E-W Anomaly (Signal)')
            ax[2].grid(True, alpha=0.3)
            ax[2].set_ylim(-3.5, 3.5)

        # --- Row 2: High Altitude ---
        ax = axes[1]
        
        # Col 1: Profile
        if not high_df.empty:
            layer_stats = high_df.groupby('alt_bin')['d_value'].mean().reset_index()
            ax[0].scatter(high_df['alt'], high_df['d_value'], alpha=0.1, s=5, c='gray', label='Raw Data')
            ax[0].plot(layer_stats['alt_bin'], layer_stats['d_value'], 'b-o', linewidth=2, label='Layer Mean')
            ax[0].set_title('High Alt Profile (>20k ft)')
            ax[0].set_ylabel('D-Value (ft)')
            ax[0].set_xlabel('Altitude (ft)')
            ax[0].grid(True, alpha=0.3)
            
        # Col 2: Latitude vs Z-Score
        if not high_df.empty:
            ax[1].axhline(0, color='gray', linestyle='--')
            sns.scatterplot(x='lat', y='z_score', data=high_df, ax=ax[1], hue='alt', palette='coolwarm', alpha=0.3, legend=False)
            
            # Manual Rolling Mean Trend Line (Dashed for Control)
            trend_df = high_df.sort_values('lat')
            win = max(10, int(len(trend_df) * 0.05))
            trend_df['trend'] = trend_df['z_score'].rolling(window=win, center=True).mean()
            ax[1].plot(trend_df['lat'], trend_df['trend'], 'k--', linewidth=2.5)

            ax[1].set_title('High Alt N-S Anomaly (Control)')
            ax[1].set_ylabel('Z-Score (σ)')
            ax[1].set_xlabel('Latitude (N)')
            # ax[1].axvline(24.86, color='red', linestyle=':', alpha=0.3) # Removed
            ax[1].grid(True, alpha=0.3)
            ax[1].set_ylim(-3.5, 3.5) # 固定比例尺以便比較
            
        # Col 3: Longitude vs Z-Score
        if not high_df.empty:
            ax[2].axhline(0, color='gray', linestyle='--')
            sns.scatterplot(x='lon', y='z_score', data=high_df, ax=ax[2], hue='alt', palette='coolwarm', alpha=0.3, legend=False)
            
            # Manual Rolling Mean Trend Line
            trend_df = high_df.sort_values('lon')
            win = max(10, int(len(trend_df) * 0.05))
            trend_df['trend'] = trend_df['z_score'].rolling(window=win, center=True).mean()
            ax[2].plot(trend_df['lon'], trend_df['trend'], 'k--', linewidth=2.5)
            
            ax[2].set_title('High Alt E-W Anomaly (Control)')
            ax[2].set_xlabel('Longitude (E)')
            ax[2].grid(True, alpha=0.3)
            ax[2].set_ylim(-3.5, 3.5)
            
        plt.tight_layout()
        fname = os.path.join(self.study_dir, '2_d_value_anomaly.png')
        plt.savefig(fname, dpi=150)
        plt.close()
        print(f"[產出] 層內標準化 D-Value 分析圖 (2x3 對照版): {fname}")


    def plot_turbulence_analysis(self, df):
        """3. 低空亂流與垂直速度變化 (Low-level Turbulence Analysis)"""
        plt.figure(figsize=(12, 7))
        
        # 定義高度層 (每 2000 ft)
        bins = range(0, 42000, 2000)
        # 製作簡短標籤: 0-2k, 2k-4k, ...
        labels = [f"{int(b/1000)}k-{int((b+2000)/1000)}k" for b in bins[:-1]]
        
        df = df.copy()
        df['alt_layer'] = pd.cut(df['alt'], bins=bins, labels=labels)
        
        # 過濾掉數據過少的高度層，避免出現誤導性的極端標準差
        # 計算每層的數據點數量
        counts = df['alt_layer'].value_counts()
        valid_layers = counts[counts > 10].index # 只保留樣本數 > 10 的層
        df_filtered = df[df['alt_layer'].isin(valid_layers)]
        
        if df_filtered.empty:
            print("數據不足，無法繪製亂流分析圖")
            plt.close()
            return

        # --- 算法升級 (v2) ---
        # 計算「垂直加速度」作為亂流指標，而非單純的垂直速率分佈
        # 1. 先確保排序正確
        df_sorted = df_filtered.sort_values(['hex', 'time'])
        
        # 2. 計算每一架飛機的垂直速率變化量 (Delta Baro Rate)
        # 用 abs() 取絕對值，因為劇烈上升或下降都是亂流
        df_sorted['v_accel'] = df_sorted.groupby('hex')['baro_rate'].diff().abs()
        
        # 3. 修正 Boxplot: 我們改畫 'v_accel' (垂直加速度/不穩定度)
        # 修正排序邏輯
        sorted_layers = [l for l in labels if l in valid_layers]
        
        # 繪製 Boxplot (垂直加速度的分佈)
        sns.boxplot(x='alt_layer', y='v_accel', data=df_sorted, hue='alt_layer', legend=False, palette='coolwarm', showfliers=False, order=sorted_layers)
        
        # 計算每層的平均亂流強度 (Mean Turbulence Intensity)
        stats = df_sorted.groupby('alt_layer', observed=True)['v_accel'].mean()
        
        # 繪製平均線 (Twin Axis)
        ax2 = plt.gca().twinx()
        stats_ordered = stats.reindex(sorted_layers)
        
        ax2.plot(range(len(sorted_layers)), stats_ordered.values, 'r-o', linewidth=2, label='Mean V-Accel (Turbulence)')
        ax2.set_ylabel(r'Turbulence Intensity (Mean $\Delta$ Vertical Rate)', color='red')
        ax2.tick_params(axis='y', labelcolor='red')
        
        plt.title('Turbulence Analysis: Vertical Instability (Acceleration) by Altitude')
        plt.xlabel('Altitude Layer (ft)')
        plt.ylabel(r'Vertical Acceleration (|$\Delta$ ft/min|)')
        
        # 調整 X 軸標籤 (確保不重疊)
        plt.xticks(rotation=45)
        
        fname = os.path.join(self.study_dir, '3_turbulence_analysis.png')
        plt.savefig(fname, dpi=150)
        plt.close()
        print(f"[產出] 亂流分析圖: {fname}")

    def plot_spatial_heatmap(self, df):
        """4. 空間壓力特徵圖 (Spatial Pressure Anomaly Map)"""
        import folium
        import matplotlib.colors as mcolors
        
        # 使用與 plot_d_value_anomaly 相同的去層化邏輯 (現在是 線性去趨勢)
        df_clean = df.dropna(subset=['alt', 'd_value', 'lat', 'lon']).copy()
        
        # --- 使用統一的異常計算 ---
        df_clean = self.calculate_flight_anomalies(df_clean)
        plot_data = df_clean.dropna(subset=['z_score'])
        
        if plot_data.empty:
            print("數據不足，無法繪製空間熱圖")
            return
        
        print(f"正在繪製空間壓力特徵圖 (基於 {len(plot_data)} 個數據點)...")
        
        # 初始化地圖
        center_lat = (self.roi['lat_min'] + self.roi['lat_max']) / 2
        center_lon = (self.roi['lon_min'] + self.roi['lon_max']) / 2
        m = folium.Map(location=[center_lat, center_lon], zoom_start=12, tiles='CartoDB dark_matter')
        
        # 建立兩個圖層群組
        layer_low = folium.FeatureGroup(name='Low Altitude (< 8k ft)', show=True)
        layer_high = folium.FeatureGroup(name='High Altitude (> 8k ft)', show=True)
        
        # 定義顏色映射 (Z-Score)
        # 通常 Z > 2 或 Z < -2 視為顯著異常
        norm = mcolors.TwoSlopeNorm(vmin=-2.5, vcenter=0, vmax=2.5)
        cmap = plt.get_cmap('coolwarm')
        
        # 繪製點
        for _, row in plot_data.iterrows():
            # 改用 Z-Score 作為顏色依據
            val = row['z_score']
            abs_res = row['d_residual'] # 保留原始數值給 Popup 看
            
            if pd.isna(val): continue
            
            rgba = cmap(norm(val))
            color_hex = mcolors.to_hex(rgba)
            
            # 根據高度決定圖層
            if row['alt'] < 8000:
                target_layer = layer_low
            else:
                target_layer = layer_high
                
            folium.CircleMarker(
                location=[row['lat'], row['lon']],
                radius=4,
                color=color_hex, 
                weight=0,
                fill=True,
                fill_color=color_hex,
                fill_opacity=0.7,
                popup=f"Alt: {row['alt']}ft<br>Residual: {abs_res:.1f}ft<br>Z-Score: {val:.2f}σ",
                tooltip=f"{val:.1f}σ"
            ).add_to(target_layer)
            
        # 將圖層加入地圖
        layer_low.add_to(m)
        layer_high.add_to(m)
        folium.LayerControl(collapsed=False).add_to(m)
            
        # 加入圖例說明 (用简单的 HTML)
        legend_html = '''
             <div style="position: fixed; 
                         bottom: 50px; left: 50px; width: 150px; height: 90px; 
                         border:2px solid grey; z-index:9999; font-size:14px;
                         background-color:white; opacity:0.8; padding: 10px;">
             <b>Pressure Anomaly</b><br>
             <i style="background:red; width:10px; height:10px; display:inline-block;"></i> High (+)<br>
             <i style="background:blue; width:10px; height:10px; display:inline-block;"></i> Low (Valley)<br>
             </div>
             '''
        m.get_root().html.add_child(folium.Element(legend_html))
        
        out_path = os.path.join(self.study_dir, '4_spatial_anomaly_map.html')
        m.save(out_path)
        print(f"[產出] 空間壓力特徵圖: {out_path}")

    def plot_coverage_map(self, df):
        """0. 原始數據分佈驗證圖 (Raw Data Coverage Map)"""
        import folium
        import matplotlib.colors as mcolors
        import matplotlib.pyplot as plt
        
        print(f"正在繪製原始數據分佈圖 (含所有高度)...")
        
        # 初始化地圖
        center_lat = (self.roi['lat_min'] + self.roi['lat_max']) / 2
        center_lon = (self.roi['lon_min'] + self.roi['lon_max']) / 2
        m = folium.Map(location=[center_lat, center_lon], zoom_start=10, tiles='CartoDB dark_matter')
        
        # 繪製 ROI 方框
        roi_points = [
            [self.roi['lat_min'], self.roi['lon_min']],
            [self.roi['lat_min'], self.roi['lon_max']],
            [self.roi['lat_max'], self.roi['lon_max']],
            [self.roi['lat_max'], self.roi['lon_min']],
            [self.roi['lat_min'], self.roi['lon_min']]
        ]
        folium.PolyLine(roi_points, color='red', weight=2, opacity=0.8, tooltip="ROI Boundary").add_to(m)
        
        # 為了效能，如果點太多則進行抽樣
        plot_df = df
        if len(df) > 50000: # 提高上限到 5萬點，避免過度稀疏
            plot_df = df.sample(50000)
            print(f"  注意: 點數過多，隨機抽樣 50000 點進行繪製")
            
        # 顏色映射 (Altitude)
        norm = mcolors.Normalize(vmin=0, vmax=20000) # 0-20k ft
        cmap = plt.get_cmap('viridis')
        
        for _, row in plot_df.iterrows():
            # 取得顏色
            rgba = cmap(norm(row['alt']))
            color_hex = mcolors.to_hex(rgba)
            
            folium.CircleMarker(
                location=[row['lat'], row['lon']],
                radius=2,
                color=color_hex,
                fill=True,
                fill_color=color_hex,
                fill_opacity=0.6,
                popup=f"Alt: {row['alt']}ft<br>Flt: {row.get('flight', 'N/A')}",
            ).add_to(m)

        out_path = os.path.join(self.study_dir, '0_coverage_map.html')
        m.save(out_path)
        print(f"[產出] 原始數據分佈圖: {out_path}")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Sky-Phys Terrain Analysis')
    parser.add_argument('--hours', type=int, default=24, help='Lookback hours')
    parser.add_argument('--date', type=str, default=None, help='Target date (YYYY-MM-DD)')
    args = parser.parse_args()
    
    base_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
    
    analyzer = TerrainStudy(base_dir, target_date=args.date, lookback_hours=args.hours)
    analyzer.analyze()
