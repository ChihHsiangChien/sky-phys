import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import os
import glob
import matplotlib
from scipy import stats
from datetime import datetime, timedelta

matplotlib.use('Agg')

class SkyPhysDashboard:
    def __init__(self, base_path, target_date=None, lookback_hours=24):
        self.base_path = base_path
        self.target_date = target_date # 如果有指定日期 (YYYY-MM-DD)
        self.lookback_hours = lookback_hours # 預設回推 24 小時
        
        # 定義輸出路徑
        self.report_dir = os.path.join(self.base_path, 'reports')
        self.txt_path = os.path.join(self.report_dir, 'observation_summary.txt')
        
        if not os.path.exists(self.report_dir):
            os.makedirs(self.report_dir)

    def load_and_process(self):
        # 1. 決定要讀取的檔案清單與時間範圍
        current_time = datetime.now()
        files_to_read = set()
        start_dt, end_dt = None, None

        if self.target_date:
            # 模式 A: 指定特定日期 (Calendar Day)
            target_dt = datetime.strptime(self.target_date, "%Y-%m-%d")
            start_dt = target_dt.replace(hour=0, minute=0, second=0, microsecond=0)
            end_dt = start_dt + timedelta(hours=24)
            files_to_read.add(f"adsb_{self.target_date}.csv")
            print(f"分析模式: 指定日期 {self.target_date} (00:00 - 24:00)")
        else:
            # 模式 B: 滾動視窗 (Rolling Window), 預設過去 24 小時
            end_dt = current_time
            start_dt = end_dt - timedelta(hours=self.lookback_hours)
            
            # 找出這段時間會跨越哪些天 (例如昨天+今天)
            d = start_dt
            while d <= end_dt:
                date_str = d.strftime("%Y-%m-%d")
                files_to_read.add(f"adsb_{date_str}.csv")
                d += timedelta(days=1)
            # 補上 end_dt 當天的檔案 (以防萬一迴圈沒跑道)
            files_to_read.add(f"adsb_{end_dt.strftime('%Y-%m-%d')}.csv")
            
            print(f"分析模式: 滾動視窗 (過去 {self.lookback_hours} 小時)")
            print(f"時間範圍: {start_dt.strftime('%Y-%m-%d %H:%M')} 到 {end_dt.strftime('%Y-%m-%d %H:%M')}")

        # 2. 讀取與合併
        df_list = []
        cols_new = ['time', 'hex', 'flight', 'lat', 'lon', 'alt', 'alt_geom', 'gs', 'ias', 'tas', 'mach', 
                   'track', 'track_rate', 'roll', 'mag_heading', 'true_heading', 'baro_rate', 'geom_rate', 
                   'temp', 'wd', 'ws', 'nav_qnh', 'nav_altitude_mcp', 'selected_heading', 'squawk', 
                   'rssi', 'messages', 'rc', 'nic_baro', 'nac_p', 'nac_v', 'sil', 'gva', 'sda',
                   'category', 'nav_modes', 'version']
                   


        logs_dir = os.path.join(self.base_path, 'logs')
        found_files = False
        
        for fname in files_to_read:
            fpath = os.path.join(logs_dir, fname)
            if os.path.exists(fpath):
                try:
                    # Unified format reading
                    temp_df = pd.read_csv(fpath, names=cols_new, engine='python')
                    df_list.append(temp_df)
                    found_files = True
                except Exception as e:
                    print(f"讀取 {fname} 失敗: {e}")
        
        if not found_files:
            print(f"錯誤: 在 {logs_dir} 找不到指定的數據檔案: {files_to_read}")
            return None

        # 合併所有候選檔案
        df = pd.concat(df_list, ignore_index=True)
        
        if df.empty:
            print("警告: 讀取的數據為空。")
            return None

        # 強制轉換時間欄位為數值 (Unix Timestamp)，避免字串比較錯誤
        df['time'] = pd.to_numeric(df['time'], errors='coerce')
        df = df.dropna(subset=['time'])

        # 3. 精確時間過濾 (因為檔案可能包含多餘的頭尾時間)
        # 原始 timestamp 是 Unix Epoch
        # 轉換:
        start_ts = start_dt.timestamp()
        end_ts = end_dt.timestamp()
        
        # 過濾
        df = df[(df['time'] >= start_ts) & (df['time'] <= end_ts)]
        
        if df.empty:
            print("警告: 指定的時間範圍內沒有數據。")
            return None

        print(f"數據載入成功: {len(df)} 筆記錄")

        # 時間維度處理 (維持原本台北時間邏輯)
        df['dt'] = pd.to_datetime(df['time'], unit='s').dt.tz_localize('UTC').dt.tz_convert('Asia/Taipei')
        df['hour'] = df['dt'].dt.hour
        
        # 定義分析時段
        df['session'] = 'Night'
        df.loc[(df['hour'] >= 6) & (df['hour'] < 12), 'session'] = 'Morning'
        df.loc[(df['hour'] >= 12) & (df['hour'] < 18), 'session'] = 'Afternoon'

        # 強制轉換數值
        numeric_cols = ['tas', 'mach', 'gs', 'alt', 'track', 'baro_rate', 'geom_rate', 'alt_geom', 'd_value', 'wd', 'ws']
        for col in numeric_cols:
            if col in df.columns:
                df[col] = pd.to_numeric(df[col], errors='coerce')

        # 基礎過濾
        df = df.dropna(subset=['tas', 'mach', 'alt'])
        df = df[(df['tas'] > 100) & (df['mach'] > 0.3) & (df['alt'] > 5000)]

        # --- 物理量計算核心 ---
        # 1. 計算音速與溫度
        df['speed_of_sound'] = df['tas'] / df['mach']
        df['temp'] = (df['speed_of_sound'] / 38.945)**2 - 273.15
        
        # 過濾異常溫度 (-90 ~ 50)
        df = df[(df['temp'] > -90) & (df['temp'] < 50)]
        
        # 2. 風力分量
        df['wind_comp'] = df['gs'] - df['tas']
        
        # 3. 飛行方向
        df['direction'] = 'Unknown'
        if 'track' in df.columns:
            df.loc[(df['track'] >= 0) & (df['track'] < 180), 'direction'] = 'Eastbound'
            df.loc[(df['track'] >= 180) & (df['track'] <= 360), 'direction'] = 'Westbound'

        # 4. D-Value (幾何高度 - 氣壓高度)
        if 'alt_geom' in df.columns:
            df['d_value'] = df['alt_geom'] - df['alt']
            # 過濾極端異常值
            df.loc[abs(df['d_value']) > 3000, 'd_value'] = np.nan

        # 5. 比能量高度 (Specific Energy Height)
        # Es = h + V^2 / 2g (h in meters, V in m/s)
        g = 9.80665
        v_ms = df['tas'] * 0.514444
        h_m = df['alt'] * 0.3048
        df['specific_energy'] = h_m + (v_ms**2) / (2 * g)
        
        # 6. 空氣密度 (Air Density)
        # P = P0 * (1 - L*h/T0)^(gM/RL) approx for standard pressure
        # rho = P / (R * T_real)
        temp_k = df['temp'] + 273.15
        pressure_pa = 101325 * (1 - 2.25577e-5 * h_m).clip(lower=0.001) ** 5.25588
        df['air_density'] = pressure_pa / (287.058 * temp_k)
            
        return df
        
        df_phys['speed_of_sound'] = df_phys['tas'] / df_phys['mach']
        df_phys['wind_comp'] = df_phys['gs'] - df_phys['tas']
        
        return df_phys

    def analyze_tropopause(self, df):
        # 尋找對流層頂 (Tropopause)：溫度停止下降的高度
        # 我們將數據按高度排序，並檢查最高溫與最低溫的斜率
        top_alt = df['alt'].max()
        min_temp = df['temp'].min()
        
        # 簡易邏輯：如果最高觀測點仍是最低溫，代表對流層頂還在更高處
        # 在台灣冬天，對流層頂通常在 45,000 - 55,000 ft 之間
        return f"對流層頂預測: 尚未偵測到 (目前觀測最高 {top_alt:.0f} ft 溫度仍持續下降至 {min_temp:.1f}°C，預計位於更高空)"

    def calculate_lapse_rate(self, df_session):
        # 過濾包含 NaN 的數據，確保回歸運算正常
        df_clean = df_session[['alt', 'temp']].dropna()
        
        # 額外過濾: 排除極低空 (<2000ft) 或資料異常點，提升回歸品質 (選用)
        # df_clean = df_clean[df_clean['alt'] > 2000]

        if len(df_clean) < 10: return "數據不足"
        
        # 利用線性迴歸計算斜率 (溫度隨高度的變化)
        slope, _, r_value, _, _ = stats.linregress(df_clean['alt'], df_clean['temp'])
        
        # 轉換為每 1000 呎下降幾度 (C/1000ft)
        return f"{abs(slope * 1000):.2f} °C / 1000ft (R²={r_value**2:.2f})"

    def generate_text_report(self, df):
        # 依高度層級統計
        df['alt_bin'] = (df['alt'] // 5000) * 5000
        summary = df.groupby('alt_bin').agg({
            'temp': ['mean', 'min'],
            'speed_of_sound': 'mean',
            'wind_comp': 'mean',
            'hex': 'count'
        }).round(2)

        report = []
        report.append("=== Sky-Phys 觀測數據摘要報告 ===")
        report.append(f"產出時間: {pd.Timestamp.now(tz='Asia/Taipei').strftime('%Y-%m-%d %H:%M:%S')}")
        report.append(f"總分析樣本數: {len(df)}")
        report.append(f"最高觀測高度: {df['alt'].max():.0f} ft")
        
        # 1. 科學指標：對流層頂
        report.append(f"\n[科學指標] {self.analyze_tropopause(df)}")
        
        # 2. 極端風速警報 (Jet Stream Warning)
        max_wind = df['wind_comp'].max()
        if max_wind > 100:
            report.append(f"\n⚠️ [警報] 發現強烈噴射氣流！最大順風分量達 {max_wind:.1f} knots")
        elif max_wind > 60:
            report.append(f"\nℹ️ [資訊] 偵測到高空強風。最大順風分量: {max_wind:.1f} knots")

        # 3. 時間維度分析
        report.append("\n--- 時間維度分析 (氣溫直減率對比) ---")
        for s in ['Morning', 'Afternoon', 'Night']:
            res = self.calculate_lapse_rate(df[df['session'] == s])
            report.append(f"* {s} (時段內遞減率): {res}")

        if 'track' in df.columns:
            # 5. 航向與風力分析 (Directional Analysis)
            # 統計往東 (Eastbound) 與往西 (Westbound) 的平均風力分量
            df_east = df[df['direction'] == 'Eastbound']
            df_west = df[df['direction'] == 'Westbound']
            
            avg_wind_east = df_east['wind_comp'].mean() if not df_east.empty else 0
            avg_wind_west = df_west['wind_comp'].mean() if not df_west.empty else 0
            
            report.append("\n--- 航向與風場分析 (Directional Wind Analysis) ---")
            report.append(f"* 往東航班 (Eastbound): 平均風分量 {avg_wind_east:.1f} kts [{'順風' if avg_wind_east > 0 else '逆風'}] (樣本數: {len(df_east)})")
            report.append(f"* 往西航班 (Westbound): 平均風分量 {avg_wind_west:.1f} kts [{'順風' if avg_wind_west > 0 else '逆風'}] (樣本數: {len(df_west)})")
            
            wind_diff = avg_wind_east - avg_wind_west
            report.append(f"* 高空西風帶強度指標 (Westerly Index): {wind_diff:.1f} kts")

        # 6. 飛行性能統計
        max_gs = df['gs'].max()
        max_mach = df['mach'].max()
        max_tas = df['tas'].max()
        report.append("\n--- 飛行性能極值 (Flight Performance) ---")
        report.append(f"* 最高地速 (Max GS): {max_gs:.0f} kts")
        report.append(f"* 最高真速 (Max TAS): {max_tas:.0f} kts")
        report.append(f"* 最大馬赫數 (Max Mach): M{max_mach:.2f}")

        # 7. 訊號覆蓋統計 (Signal Coverage)
        max_dist_approx = df['lat'].count() # 這裡僅作示意，實際距離需經緯度計算
        # 簡單統計 RSSI 分佈
        avg_rssi = df['rssi'].mean()
        min_rssi = df['rssi'].min()
        max_rssi = df['rssi'].max()
        report.append("\n--- 接收站訊號統計 (Signal Stats) ---")
        report.append(f"* 平均訊號強度: {avg_rssi:.1f} dBFS")
        report.append(f"* 訊號範圍: {min_rssi:.1f} ~ {max_rssi:.1f} dBFS")

        # 8. Markdown 表格自動生成
        report.append("\n--- 高度層級統計 (Markdown Table for Blog) ---")
        report.append("| 高度 (ft) | 均溫 (°C) | 聲速 (kt) | 風分量 (kt) | 樣本 |")
        report.append("| :--- | :--- | :--- | :--- | :--- |")
        for idx, row in summary.iterrows():
            report.append(f"| {idx:.0f} | {row[('temp', 'mean')]} | {row[('speed_of_sound', 'mean')]} | {row[('wind_comp', 'mean')]} | {int(row[('hex', 'count')])} |")
        
        with open(self.txt_path, 'w', encoding='utf-8') as f:
            f.write("\n".join(report))
        print(f"數字報告已產出：{self.txt_path}")

    def generate_plots(self, df):
        # 修正: dropna 時只檢查繪圖相關的欄位，避免因為 'track' 是 NaN (舊資料) 而導致整行被刪除
        df_clean = df.groupby(['hex', pd.cut(df['alt'], bins=50)], observed=True).mean(numeric_only=True).dropna(subset=['speed_of_sound', 'alt', 'temp']).reset_index(drop=True)
        if df_clean is None: return

        # 決定檔名日期：如果有指定日期，就用指定日期；否則用今天
        if self.target_date:
            date_str = self.target_date
        else:
            date_str = pd.Timestamp.now().strftime('%Y-%m-%d')
            
        report_dir = self.report_dir

        # 全局字型大小設定
        plt.rcParams.update({'font.size': 14})

        # 1. 溫度 vs 高度 (加入時段對比)
        plt.figure(figsize=(10, 12))
        sns.scatterplot(x='temp', y='alt', data=df, hue='session', alpha=0.15, palette='Set2', s=20)
        for s, color in zip(['Morning', 'Afternoon'], ['teal', 'orange']):
            data_s = df[df['session'] == s]
            if len(data_s) > 10:
                sns.regplot(x='temp', y='alt', data=data_s, scatter=False, color=color, label=f'{s} Trend')
        plt.title('Temperature Profile (Time Comparison)', fontsize=18)
        plt.xlabel('Temperature (°C)', fontsize=16)
        plt.ylabel('Altitude (ft)', fontsize=16)
        plt.tick_params(axis='both', which='major', labelsize=14)
        plt.legend(fontsize=14)
        plt.ylim(bottom=0)
        plt.grid(True, alpha=0.3)
        plt.tight_layout()
        plt.savefig(os.path.join(report_dir, f'plot_temp_profile_{date_str}.png'), dpi=300)
        plt.close()

        # 2. 聲速 vs 高度
        plt.figure(figsize=(10, 12))
        sc = plt.scatter(df_clean['speed_of_sound'], df_clean['alt'], c=df_clean['temp'], cmap='winter', s=40)
        cbar = plt.colorbar(sc, label='Temperature (°C)')
        cbar.ax.tick_params(labelsize=14)
        cbar.set_label('Temperature (°C)', fontsize=16)
        plt.title('Speed of Sound vs Altitude', fontsize=18)
        plt.xlabel('Speed of Sound (knots)', fontsize=16)
        plt.ylabel('Altitude (ft)', fontsize=16)
        plt.tick_params(axis='both', which='major', labelsize=14)
        plt.grid(True, alpha=0.3)
        plt.tight_layout()
        plt.savefig(os.path.join(report_dir, f'plot_speed_sound_{date_str}.png'), dpi=300)
        plt.close()

        # 3. 風力分量 (噴射氣流分析) - 加入航向分類
        plt.figure(figsize=(10, 12))
        plt.axvline(0, color='gray', linestyle='--')
        plt.axvline(100, color='red', linestyle=':', label='Extreme Wind Warning (100kt)')
        
        # 為了避免 groupby mean 造成 string/categorical 資料遺失，我們需要重新處理 df_clean 的 direction
        # 最簡單的方法是直接用原始 df 畫圖，或者將 direction 加入 groupby
        # 這裡我們選擇直接畫原始點，但為了效能和清晰度，我們還是用 df_clean (如果有 direction 欄位)
        
        # 由於 df_clean 是 numeric mean，字串 direction 會丟失。
        # 重新生成 df_clean 並包含 direction (取 mode 或者 first)
        df['alt_bin'] = pd.cut(df['alt'], bins=50)
        # 用 lambda x: x.mode()[0] if not x.mode().empty else 'Unknown' 比較慢
        # 我們直接對每個 hex 取第一個 direction 即可 (假設飛機不會在一次觀測中大幅掉頭)
        group_cols = ['hex', 'alt_bin']
        df_clean = df.groupby(group_cols, observed=True).agg({
            'wind_comp': 'mean',
            'alt': 'mean',
            'direction': 'first' # 取該航段的主要方向
        }).dropna().reset_index()

        sns.scatterplot(x='wind_comp', y='alt', data=df_clean, hue='direction', 
                        hue_order=['Eastbound', 'Westbound', 'Unknown'],
                        palette={'Eastbound': 'blue', 'Westbound': 'red', 'Unknown': 'gray'}, 
                        s=50, alpha=0.7)
        
        plt.title('Tailwind/Headwind by Direction', fontsize=18)
        plt.xlabel('Wind Component (knots) [Positive=Tailwind]', fontsize=16)
        plt.ylabel('Altitude (ft)', fontsize=16)
        plt.tick_params(axis='both', which='major', labelsize=14)
        plt.legend(fontsize=14)
        plt.grid(True, alpha=0.3)
        plt.tight_layout()
        plt.savefig(os.path.join(report_dir, f'plot_wind_comp_{date_str}.png'), dpi=300)
        plt.close()
        
        # --- 新增四張進階分析圖表 ---
        
        # 4. TAS vs GS (風力影響視覺化)
        plt.figure(figsize=(10, 10))
        # 繪製 y=x 參考線 (無風狀態)
        max_speed = max(df['tas'].max(), df['gs'].max())
        plt.plot([0, max_speed], [0, max_speed], 'k--', alpha=0.5, label='Zero Wind (TAS=GS)')
        
        sns.scatterplot(x='tas', y='gs', data=df, hue='direction', 
                        hue_order=['Eastbound', 'Westbound', 'Unknown'],
                        palette={'Eastbound': 'blue', 'Westbound': 'red', 'Unknown': 'gray'}, 
                        s=20, alpha=0.6)
        
        plt.title('TAS vs Ground Speed (Wind Effect)', fontsize=18)
        plt.xlabel('True Airspeed (TAS) [knots]', fontsize=16)
        plt.ylabel('Ground Speed (GS) [knots]', fontsize=16)
        plt.tick_params(axis='both', which='major', labelsize=14)
        plt.legend(fontsize=14)
        plt.grid(True, alpha=0.3)
        plt.tight_layout()
        plt.savefig(os.path.join(report_dir, f'plot_tas_vs_gs_{date_str}.png'), dpi=300)
        plt.close()

        # 5. Mach vs Altitude (飛行包絡線)
        plt.figure(figsize=(10, 12))
        sc = plt.scatter(df['mach'], df['alt'], c=df['temp'], cmap='plasma', s=20, alpha=0.5)
        cbar = plt.colorbar(sc, label='Temperature (°C)')
        cbar.ax.tick_params(labelsize=14)
        cbar.set_label('Temperature (°C)', fontsize=16)
        
        plt.title('Mach Number vs Altitude', fontsize=18)
        plt.xlabel('Mach Number', fontsize=16)
        plt.ylabel('Altitude (ft)', fontsize=16)
        plt.tick_params(axis='both', which='major', labelsize=14)
        plt.grid(True, alpha=0.3)
        plt.tight_layout()
        plt.savefig(os.path.join(report_dir, f'plot_mach_vs_alt_{date_str}.png'), dpi=300)
        plt.close()
        
        # 6. 航向雷達圖 (Traffic Direction Rose)
        if 'track' in df.columns:
            plt.figure(figsize=(10, 10))
            ax = plt.subplot(111, projection='polar')
            
            # 將 360 度切分為 36 個區間 (每 10 度)
            bins = np.linspace(0, 360, 37)
            # 統計每個區間的飛機數量 (使用 histogram)
            counts, _ = np.histogram(df['track'].dropna(), bins=bins)
            
            # 轉換為極座標需要的格式 (寬度與角度)
            width = np.deg2rad(10) # 每個 bin 寬 10 度
            theta = np.deg2rad(bins[:-1]) + width/2 # 中心點
            
            bars = ax.bar(theta, counts, width=width, bottom=0.0, alpha=0.7, color='skyblue', edgecolor='navy')
            
            ax.set_theta_zero_location("N") # 0度在正北方
            ax.set_theta_direction(-1)      # 順時針方向增加
            ax.set_title('Traffic Density by Heading', fontsize=18, y=1.05)
            plt.tick_params(axis='both', which='major', labelsize=12)
            plt.tight_layout()
            plt.savefig(os.path.join(report_dir, f'plot_heading_rose_{date_str}.png'), dpi=300)
            plt.close()

        # 7. RSSI vs Altitude (訊號強度分析)
        plt.figure(figsize=(10, 12))
        # 這裡只取有 RSSI 的數據，並確保為數值
        df_rssi = df.dropna(subset=['rssi', 'alt']).copy()
        df_rssi['rssi'] = pd.to_numeric(df_rssi['rssi'], errors='coerce')
        df_rssi = df_rssi.dropna(subset=['rssi'])
        
        sns.regplot(x='rssi', y='alt', data=df_rssi, scatter_kws={'s': 10, 'alpha': 0.3}, line_kws={'color': 'red'})
        
        plt.title('Signal Strength (RSSI) vs Altitude', fontsize=18)
        plt.xlabel('RSSI (dBFS)', fontsize=16)
        plt.ylabel('Altitude (ft)', fontsize=16)
        plt.tick_params(axis='both', which='major', labelsize=14)
        plt.grid(True, alpha=0.3)
        plt.tight_layout()
        plt.savefig(os.path.join(report_dir, f'plot_rssi_vs_alt_{date_str}.png'), dpi=300)
        plt.close()
        
        # --- 新增兩張進階遙測圖表 (利用 recorder.sh 的新欄位) ---
        
        # 8. 垂直速率剖面圖 (Vertical Rate Profile)
        if 'baro_rate' in df.columns:
            plt.figure(figsize=(10, 12))
            # 過濾異常值 (爬升率不可能超過 +/- 8000 fpm)
            df_vr = df.dropna(subset=['baro_rate', 'alt'])
            df_vr = df_vr[(df_vr['baro_rate'] > -8000) & (df_vr['baro_rate'] < 8000)]
            
            # 建立明確的分類欄位，避免 Boolean 造成圖例混淆
            df_vr['activity'] = 'Level'
            df_vr.loc[df_vr['baro_rate'] > 100, 'activity'] = 'Climb'      # 爬升 (>100 fpm)
            df_vr.loc[df_vr['baro_rate'] < -100, 'activity'] = 'Descent'   # 下降 (<-100 fpm)
            # Level 就不畫了，或者用灰色，這裡專注於爬升下降
            df_vr = df_vr[df_vr['activity'] != 'Level']

            sns.scatterplot(x='baro_rate', y='alt', data=df_vr, hue='activity', 
                            hue_order=['Climb', 'Descent'],
                            palette={'Climb': 'green', 'Descent': 'red'}, 
                            alpha=0.3, s=20)
            
            plt.axvline(0, color='black', linestyle='-', linewidth=1)
            plt.title('Vertical Rate Profile (Climb/Descent)', fontsize=18)
            plt.xlabel('Barometric Vertical Rate (ft/min)', fontsize=16)
            plt.ylabel('Altitude (ft)', fontsize=16)
            plt.legend(title='Activity', fontsize=12) # 自動使用 hue 的正確標籤
            plt.tick_params(axis='both', which='major', labelsize=14)
            plt.grid(True, alpha=0.3)
            plt.tight_layout()
            plt.savefig(os.path.join(report_dir, f'plot_vertical_rate_{date_str}.png'), dpi=300)
            plt.close()

        # 9. 機型性能分析 (Category Analysis)
        if 'category' in df.columns:
            plt.figure(figsize=(12, 10))
            # 過濾掉不明機型與異常值
            df_cat = df.dropna(subset=['category', 'tas', 'alt'])
            df_cat = df_cat[df_cat['category'].isin(['A1', 'A2', 'A3', 'A4', 'A5'])]
            
            # 定義機型標籤
            cat_map = {'A1': 'Light (<7t)', 'A2': 'Small (<34t)', 'A3': 'Large (<136t)', 'A4': 'Heavy (<300t)', 'A5': 'Super Heavy'}
            df_cat['cat_desc'] = df_cat['category'].map(cat_map)
            
            sns.scatterplot(x='tas', y='alt', data=df_cat, hue='cat_desc', style='cat_desc', 
                            palette='Set1', s=30, alpha=0.6)
            
            plt.title('Performance by Aircraft Category', fontsize=18)
            plt.xlabel('True Airspeed (TAS) [knots]', fontsize=16)
            plt.ylabel('Altitude (ft)', fontsize=16)
            plt.legend(title='Wake Turbulence Category', fontsize=12)
            plt.tick_params(axis='both', which='major', labelsize=14)
            plt.grid(True, alpha=0.3)
            plt.tight_layout()
            plt.savefig(os.path.join(report_dir, f'plot_category_perf_{date_str}.png'), dpi=300)
            plt.close()
        
        # 10. 高空風場剖面 (Wind Profile from DAPs)
        if 'wd' in df.columns and 'ws' in df.columns:
            # 只有當飛機實際回報風向風速時才畫
            df_wind = df.dropna(subset=['wd', 'ws', 'alt'])
            # 過濾異常值 (WS > 300 kts 可能是錯誤, WD 0-360)
            df_wind = df_wind[(df_wind['ws'] >= 0) & (df_wind['ws'] < 300) & (df_wind['wd'] >= 0) & (df_wind['wd'] <= 360)]
            
            if len(df_wind) > 10:
                fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(14, 10), sharey=True)
                
                # 左圖: 風速 (Wind Speed)
                sns.scatterplot(x='ws', y='alt', data=df_wind, ax=ax1, color='purple', alpha=0.3, s=20)
                sns.regplot(x='ws', y='alt', data=df_wind, ax=ax1, scatter=False, color='darkviolet', label='Trend')
                ax1.set_title('Wind Speed Profile', fontsize=16)
                ax1.set_xlabel('Wind Speed (knots)', fontsize=14)
                ax1.set_ylabel('Altitude (ft)', fontsize=14)
                ax1.grid(True, alpha=0.3)
                
                # 右圖: 風向 (Wind Direction)
                sns.scatterplot(x='wd', y='alt', data=df_wind, ax=ax2, hue='ws', palette='viridis', alpha=0.5, s=25)
                # 在 30000 ft 畫一條西風帶參考線 (270度)
                ax2.axvline(270, color='gray', linestyle='--', label='Westerly (270°)')
                ax2.set_title('Wind Direction Profile', fontsize=16)
                ax2.set_xlabel('Wind Direction (0-360°)', fontsize=14)
                ax2.set_xlim(0, 360)
                ax2.set_xticks([0, 90, 180, 270, 360])
                ax2.set_xticklabels(['N', 'E', 'S', 'W', 'N'])
                ax2.legend(title='Wind Speed', fontsize=10)
                ax2.grid(True, alpha=0.3)
                
                plt.suptitle(f'Observed Wind Profile (DAPs) - {date_str}', fontsize=20)
                plt.tight_layout()
                plt.savefig(os.path.join(report_dir, f'plot_wind_profile_{date_str}.png'), dpi=300)
                plt.close()

        # 11. D-Value (Geopotential Height Anomaly) - High/Low Pressure Indicator
        if 'd_value' in df.columns:
            df_d = df.dropna(subset=['d_value', 'alt'])
            if len(df_d) > 10:
                plt.figure(figsize=(10, 12))
                # 使用 RdBu 色票: 紅色=高壓脊(暖), 藍色=低壓槽(冷)
                sc = plt.scatter(df_d['d_value'], df_d['alt'], c=df_d['d_value'], cmap='RdBu_r', s=30, alpha=0.6, vmin=-1000, vmax=1000)
                plt.colorbar(sc, label='D-Value (ft)')
                plt.axvline(0, color='black', linestyle='--', linewidth=1)
                plt.title(f'Geopotential Height Anomaly (D-Value) - {date_str}', fontsize=18)
                plt.xlabel('D-Value (True Alt - Pressure Alt) [ft]', fontsize=16)
                plt.ylabel('Altitude (ft)', fontsize=16)
                
                # 使用相對座標 (transAxes) 來標示文字，避免因 X 軸範圍變動而跑位
                ax = plt.gca()
                plt.text(0.85, 0.95, 'High Pressure\n(Warm/Ridge)', color='red', fontsize=12, 
                         ha='center', va='top', transform=ax.transAxes, fontweight='bold')
                plt.text(0.15, 0.95, 'Low Pressure\n(Cold/Trough)', color='blue', fontsize=12, 
                         ha='center', va='top', transform=ax.transAxes, fontweight='bold')
                
                plt.grid(True, alpha=0.3)
                plt.tight_layout()
                plt.savefig(os.path.join(report_dir, f'plot_d_value_{date_str}.png'), dpi=300)
                plt.close()

        # 12. 比能量高度 (Specific Energy Height)
        if 'specific_energy' in df.columns:
            df_e = df.dropna(subset=['specific_energy', 'alt'])
            plt.figure(figsize=(12, 10))
            sns.scatterplot(x='specific_energy', y='alt', data=df_e, hue='mach', palette='plasma', s=20, alpha=0.5)
            plt.title('Specific Energy Height (TE = PE + KE)', fontsize=18)
            plt.xlabel('Specific Energy Height (m)', fontsize=16)
            plt.ylabel('Altitude (ft)', fontsize=16)
            plt.grid(True, alpha=0.3)
            plt.tight_layout()
            plt.savefig(os.path.join(report_dir, f'plot_energy_{date_str}.png'), dpi=300)
            plt.close()

        # 13. 空氣密度剖面 (Air Density Profile)
        if 'air_density' in df.columns:
            df_rho = df.dropna(subset=['air_density', 'alt'])
            plt.figure(figsize=(10, 12))
            
            # 實測密度
            plt.scatter(df_rho['air_density'], df_rho['alt'], c=df_rho['temp'], cmap='viridis', s=15, alpha=0.4, label='Observed')
            plt.colorbar(label='Temperature (°C)')
            
            # 理論標準大氣密度曲線 (簡化)
            # rho = 1.225 * exp(-h / 9144) approx (h in meters)
            # h_ft to meters
            h_range = np.linspace(0, 45000, 100)
            rho_std = 1.225 * np.exp(-(h_range * 0.3048) / 8500) # Scale height approx 8.5km
            plt.plot(rho_std, h_range, 'r--', linewidth=2, label='Standard Atmosphere Model')
            
            plt.title('Air Density Profile', fontsize=18)
            plt.xlabel('Air Density (kg/m³)', fontsize=16)
            plt.ylabel('Altitude (ft)', fontsize=16)
            plt.legend()
            plt.grid(True, alpha=0.3)
            plt.tight_layout()
            plt.savefig(os.path.join(report_dir, f'plot_density_{date_str}.png'), dpi=300)
            plt.close()
            
        # 14. 亂流強度光譜 (Turbulence Spectrum - Baro Rate Variance)
        if 'baro_rate' in df.columns:
            plt.figure(figsize=(12, 8))
            # 取絕對值並過濾平飛微小波動 (< 64 fpm)
            turbulence_proxy = df['baro_rate'].abs()
            turbulence_proxy = turbulence_proxy[turbulence_proxy > 64] 
            
            sns.histplot(turbulence_proxy, bins=100, log_scale=(False, True), color='purple', kde=False)
            plt.title('Vertical Turbulence Spectrum (Vertical Rate Distribution)', fontsize=18)
            plt.xlabel('Absolute Vertical Rate (|fpm|)', fontsize=16)
            plt.ylabel('Count (Log Scale)', fontsize=16)
            plt.axvline(2000, color='orange', linestyle='--', label='Moderate Turbulence Threshold')
            plt.axvline(4000, color='red', linestyle='--', label='Severe Turbulence Threshold')
            plt.legend()
            plt.grid(True, alpha=0.3, which='both')
            plt.tight_layout()
            plt.savefig(os.path.join(report_dir, f'plot_turbulence_{date_str}.png'), dpi=300)
            plt.close()

        # 15. 飛機雲預測圖 (Contrail Formation Probability) - Appleman Criterion
        # 根據經驗法則，溫度低於 -40C 開始有機會形成，低於 -50C 機率極高且持久
        if 'temp' in df.columns:
            print(f"正在繪製飛機雲預測圖... (資料筆數: {len(df[df['alt'] > 20000])})")
            plt.figure(figsize=(10, 12))
            
            # --- 繪製預測區域背景 (Appleman Zones) ---
            # 必須先畫背景，再畫數據點
            # 獲取 Y 軸範圍 (高度)
            y_min, y_max = 0, 45000
            
            # Zone 3: No Contrails (> -40C) - 暖色背景
            plt.axvspan(-40, 50, color='red', alpha=0.1, label='No Contrails (> -40°C)')
            # Zone 2: Possible Contrails (-40C to -50C) - 黃色背景
            plt.axvspan(-50, -40, color='yellow', alpha=0.1, label='Possible Contrails (-40°C ~ -50°C)')
            # Zone 1: Probable / Persistent Contrails (< -50C) - 藍色背景
            plt.axvspan(-90, -50, color='blue', alpha=0.1, label='Probable / Persistent (< -50°C)')
            
            # --- 繪製實際飛行數據點 ---
            # 篩選高於 20000 ft 的巡航航班 (低空通常太暖不會有)
            df_contrail = df[df['alt'] > 20000]
            if not df_contrail.empty:
                sns.scatterplot(x='temp', y='alt', data=df_contrail, hue='hour', palette='coolwarm', s=20, alpha=0.6)
                plt.legend(title='Hour of Day')
            
            # 標示關鍵溫度線
            plt.axvline(-40, color='red', linestyle='--', linewidth=1.5)
            plt.axvline(-50, color='blue', linestyle='--', linewidth=1.5)
            
            plt.title('Contrail Formation Probability (Appleman Criterion)', fontsize=18)
            plt.xlabel('Static Temperature (°C)', fontsize=16)
            plt.ylabel('Altitude (ft)', fontsize=16)
            plt.xlim(-80, 20) # 限制 X 軸範圍專注於高空低溫區
            plt.ylim(20000, 45000) # 只看高空層
            plt.grid(True, alpha=0.3)
            plt.tight_layout()
            plt.savefig(os.path.join(report_dir, f'plot_contrail_{date_str}.png'), dpi=300)
            plt.close()



        # 16. RSSI 天線場型圖 (Polar Coverage Map)
        # 使用使用者提供的接收站座標: 24.7470081, 121.0823754 (竹北)
        if 'lat' in df.columns and 'lon' in df.columns and 'rssi' in df.columns:
            ref_lat = 24.7470
            ref_lon = 121.0824
            
            # --- 計算方位角 (Bearing) 與 距離 (Distance) ---
            # 將經緯度轉換為弧度
            lat1 = np.radians(ref_lat)
            lon1 = np.radians(ref_lon)
            lat2 = np.radians(df['lat'])
            lon2 = np.radians(df['lon'])
            dlon = lon2 - lon1
            
            # Bearing Calculation (Azimuth)
            y = np.sin(dlon) * np.cos(lat2)
            x = np.cos(lat1) * np.sin(lat2) - np.sin(lat1) * np.cos(lat2) * np.cos(dlon)
            bearing_rad = np.arctan2(y, x)
            bearing_deg = np.degrees(bearing_rad)
            # Normalize to 0-360 (North=0, East=90)
            bearing_deg = (bearing_deg + 360) % 360
            
            # Distance Calculation (Haversine Formula approximation)
            # R = 6371 km
            a = np.sin((lat2-lat1)/2)**2 + np.cos(lat1) * np.cos(lat2) * np.sin(dlon/2)**2
            c = 2 * np.arctan2(np.sqrt(a), np.sqrt(1-a))
            dist_km = 6371 * c
            
            # 準備繪圖數據
            df['bearing'] = bearing_deg
            df['dist_km'] = dist_km
            
            # 濾除過遠的異常點 (> 500km)
            df_polar = df[df['dist_km'] < 500].dropna(subset=['bearing', 'dist_km', 'rssi'])
            
            if not df_polar.empty:
                plt.figure(figsize=(10, 10))
                ax = plt.subplot(111, projection='polar')
                
                # Polar plot 的角度通常是逆時針算 (數學定義)，且 0 度在右邊
                # 我們需要調整讓 0 度在正上方 (北)，且順時針增加
                ax.set_theta_zero_location("N")
                ax.set_theta_direction(-1)
                
                # 繪製散佈圖 (Theta in radians)
                sc = ax.scatter(np.radians(df_polar['bearing']), df_polar['dist_km'], 
                                c=df_polar['rssi'], cmap='plasma', s=5, alpha=0.5, vmin=-30, vmax=-0.1)
                
                # --- 計算並繪製覆蓋邊界 (Coverage Envelope) ---
                # 每 2 度為一個 bin，找出該方位的最大距離 (提高解析度以貼合邊界)
                bins = np.arange(0, 362, 2)
                max_dists = []
                angles = []
                
                for i in range(len(bins)-1):
                    bin_start, bin_end = bins[i], bins[i+1]
                    subset = df_polar[(df_polar['bearing'] >= bin_start) & (df_polar['bearing'] < bin_end)]
                    if not subset.empty:
                        # 取前 95% 分位數作為穩定的最大距離 (排除極端離群值)
                        max_d = subset['dist_km'].max()
                        max_dists.append(max_d)
                        angles.append(np.radians((bin_start + bin_end)/2)) # 取 bin 中心角度

                # 閉合曲線 (首尾相接)
                if angles:
                    angles.append(angles[0])
                    max_dists.append(max_dists[0])
                    # 繪製邊界線
                    ax.plot(angles, max_dists, color='cyan', linewidth=2, linestyle='--', label='Coverage Envelope')

                # 標示最大距離
                global_max_dist = df_polar['dist_km'].max()
                plt.text(np.radians(135), global_max_dist * 1.1, f"Max Range: {global_max_dist:.0f} km", 
                         color='cyan', fontsize=14, weight='bold', bbox=dict(facecolor='black', alpha=0.7))

                plt.colorbar(sc, label='RSSI (dBFS)', pad=0.1)
                plt.title(f'Antenna Coverage Pattern (Range vs Azimuth) - {date_str}', fontsize=16, y=1.08)
                
                # 標示方位
                ax.set_rlabel_position(45)  # Move radial labels away from plotted line
                plt.text(np.radians(0), ax.get_rmax() + 20, 'N', ha='center', va='bottom', fontsize=12, weight='bold')
                plt.text(np.radians(90), ax.get_rmax() + 20, 'E', ha='center', va='bottom', fontsize=12, weight='bold')
                plt.text(np.radians(180), ax.get_rmax() + 20, 'S', ha='center', va='top', fontsize=12, weight='bold')
                plt.text(np.radians(270), ax.get_rmax() + 20, 'W', ha='center', va='bottom', fontsize=12, weight='bold')

                plt.tight_layout()
                plt.savefig(os.path.join(report_dir, f'plot_rssi_polar_{date_str}.png'), dpi=300)
                plt.close()

                # 17. RSSI vs 距離 (Path Loss Analysis)
                # 使用與 Polar Plot 相同的距離數據
                plt.figure(figsize=(12, 8))
                sns.scatterplot(x='dist_km', y='rssi', data=df_polar, hue='alt', palette='viridis', s=10, alpha=0.5)
                
                # 繪製理論自由空間路徑損失 (Free Space Path Loss) 參考線
                # 策略：以近場 (10-50km) 最強訊號為基準，畫出 -20log10(d) 衰減曲線
                # 1. 找出近場參考點 (Reference Point)
                near_field = df_polar[(df_polar['dist_km'] >= 10) & (df_polar['dist_km'] <= 50)]
                if not near_field.empty:
                    # 取前 95% 強度的點作為基準 (避免雜訊干擾)
                    ref_rssi = near_field['rssi'].max()
                    # ref_dist 取該最大 RSSI 對應的距離，或是簡單取 30km 中間值?
                    # 更好做法：取 10km 處的 "虛擬" 強度。
                    # 假設 ref_rssi 是在 10km 處量到的 (我們用 max 來逼近)
                    # 實際上，我們畫一條通過 (ref_dist, ref_rssi) 的 -20log10 線
                    
                    ref_row = near_field.loc[near_field['rssi'].idxmax()]
                    ref_d = ref_row['dist_km']
                    ref_p = ref_row['rssi']
                    
                    # 2. 生成理論曲線點
                    d_curve = np.linspace(10, df_polar['dist_km'].max(), 100)
                    # FSPL: P2 = P1 - 20*log10(d2/d1)
                    rssi_curve = ref_p - 20 * np.log10(d_curve / ref_d)
                    
                    plt.plot(d_curve, rssi_curve, 'r--', linewidth=2, label='Theoretical FSPL (-20dB/dec)')
                
                plt.title(f'Signal Strength vs Distance (Path Loss Analysis) - {date_str}', fontsize=18)
                plt.xlabel('Distance to Station (km)', fontsize=16)
                plt.ylabel('RSSI (dBFS)', fontsize=16)
                plt.legend(title='Altitude/Ref')
                plt.grid(True, alpha=0.3)
                plt.tight_layout()
                plt.savefig(os.path.join(report_dir, f'plot_rssi_vs_dist_{date_str}.png'), dpi=300)
                plt.close()

        print(f"所有圖表 (含 D-Value/Energy/Density/Turbulence/Contrail/RSSI-Polar/RSSI-Dist) 已產出至: {report_dir}")

    def generate(self):
        df = self.load_and_process()
        if df is None or len(df) < 5:
            print("數據不足。")
            return
        self.generate_text_report(df)
        self.generate_plots(df)

if __name__ == "__main__":
    import argparse
    
    parser = argparse.ArgumentParser(description='Sky-Phys Dashboard Generator')
    parser.add_argument('--date', type=str, help='指定分析日期 (YYYY-MM-DD)', default=None)
    parser.add_argument('--hours', type=int, help='回推分析小時數 (預設: 24)', default=24)
    
    args = parser.parse_args()
    
    base_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
    # 傳入參數: base_path 是必要的 positional argument
    dashboard = SkyPhysDashboard(base_dir, target_date=args.date, lookback_hours=args.hours)
    dashboard.generate()