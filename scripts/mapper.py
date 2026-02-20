import pandas as pd
import folium
from folium.plugins import HeatMap, PolyLineTextPath
import hashlib
import os
import glob

class SkyPhysMapper:
    def __init__(self, lookback_hours=24, target_date=None, target_callsign=None, target_hex=None):
        self.base_path = os.path.expanduser('~/Documents/sky-phys')
        self.log_file = os.path.join(self.base_path, 'logs/adsb_physics.csv')
        
        self.lookback_hours = lookback_hours
        self.target_date = target_date
        self.target_callsign = target_callsign
        self.target_hex = target_hex
        
        # 決定日期字串 (用於檔名)
        if self.target_date:
            self.date_str = self.target_date
        else:
            import datetime
            self.date_str = datetime.datetime.now().strftime("%Y-%m-%d")

        # 決定輸出檔名
        if target_callsign:
            self.output_path = os.path.join(self.base_path, f'reports/track_{target_callsign}_{self.date_str}.html')
        elif target_hex:
            self.output_path = os.path.join(self.base_path, f'reports/track_{target_hex}_{self.date_str}.html')
        else:
            self.output_path = os.path.join(self.base_path, f'reports/sky_phys_map_{self.date_str}.html')
            
        self.cols_v5 = ['time', 'hex', 'flight', 'lat', 'lon', 'alt', 'alt_geom', 'gs', 'ias', 'tas', 'mach', 
                   'track', 'track_rate', 'roll', 'mag_heading', 'true_heading', 'baro_rate', 'geom_rate', 
                   'temp', 'wd', 'ws', 'nav_qnh', 'nav_altitude_mcp', 'selected_heading', 'squawk', 
                   'rssi', 'messages', 'rc', 'nic_baro', 'nac_p', 'nac_v', 'sil', 'gva', 'sda',
                   'category', 'nav_modes', 'version']

    def get_color(self, hex_str):
        h = hashlib.md5(str(hex_str).encode()).hexdigest()
        return f"#{h[:6]}"

    def load_data(self):
        df = None
        
        # 模式 A: 指定特定日期
        if self.target_date:
            import datetime
            target_dt = datetime.datetime.strptime(self.target_date, "%Y-%m-%d")
            start_ts = target_dt.replace(hour=0, minute=0, second=0).timestamp()
            end_ts = (target_dt + datetime.timedelta(hours=24)).timestamp()
            
            fname = f"adsb_{self.target_date}.csv"
            fpath = os.path.join(self.base_path, 'logs', fname)
            
            print(f"Loading data for specific date: {self.target_date}")
            if not os.path.exists(fpath):
                print(f"File not found: {fpath}")
                return None
                
            try:
                df = pd.read_csv(fpath, names=self.cols_v5, engine='python')
                
                # 強制轉換時間
                df['time'] = pd.to_numeric(df['time'], errors='coerce')
                df = df.dropna(subset=['time'])

                # 精確過濾
                df = df[(df['time'] >= start_ts) & (df['time'] <= end_ts)]
            except Exception as e:
                print(f"Error reading {fname}: {e}")
                return None
        else:
            # 模式 B: 滾動視窗 (Rolling Window)
            df = self.load_recent_data()
            
        if df is None or df.empty: return None

        # 2. 針對單一飛機進行過濾
        if self.target_callsign:
            t_callsign = self.target_callsign.strip().upper()
            # 確保欄位是字串且轉大寫後比對
            df['flight'] = df['flight'].astype(str).str.strip().str.upper()
            df = df[df['flight'] == t_callsign]
            print(f"Tracking Flight: {t_callsign} -> {len(df)} points")
            
        if self.target_hex:
            t_hex = self.target_hex.strip().lower()
            # 確保欄位字串比對一致 (轉小寫)
            df = df[df['hex'].astype(str).str.lower() == t_hex]
            print(f"Tracking Hex: {t_hex} -> {len(df)} points")
            
        return df.sort_values(['hex', 'time'])

    def load_recent_data(self):
        # 智慧載入：只讀取時間窗口內的檔案 (Rolling Window)
        import datetime
        current_time = datetime.datetime.now()
        end_dt = current_time
        start_dt = end_dt - datetime.timedelta(hours=self.lookback_hours)
        
        files_to_read = set()
        d = start_dt
        while d <= end_dt:
            date_str = d.strftime("%Y-%m-%d")
            files_to_read.add(f"adsb_{date_str}.csv")
            d += datetime.timedelta(days=1)
        files_to_read.add(f"adsb_{end_dt.strftime('%Y-%m-%d')}.csv")
        
        log_files = glob.glob(os.path.join(self.base_path, 'logs/adsb_*.csv'))
        # Filter files that match our target dates
        target_files = [f for f in log_files if os.path.basename(f) in files_to_read]
        
        if not target_files: return None
        
        df_list = []
        for f in target_files:
            try:
                temp_df = pd.read_csv(f, names=self.cols_v5, engine='python')
                df_list.append(temp_df)
            except Exception as e:
                print(f"Error reading {f}: {e}")
        
        if not df_list: return None
        df = pd.concat(df_list, ignore_index=True)
        
        
        # 強制轉換時間欄位為數值 (Unix Timestamp)
        df['time'] = pd.to_numeric(df['time'], errors='coerce')
        df = df.dropna(subset=['time'])

        # 精確時間過濾
        start_ts = start_dt.timestamp()
        end_ts = end_dt.timestamp()
        df = df[(df['time'] >= start_ts) & (df['time'] <= end_ts)]
        
        return df.sort_values(['hex', 'time'])

    def generate(self):
        import datetime
        df = self.load_data()
        if df is None or df.empty:
            print("No data found for the specified time window.")
            return

        print(f"Generating Map with {len(df)} points...")
        
        # 判斷是否為「單機追蹤模式」
        single_mode = (self.target_callsign is not None) or (self.target_hex is not None)
        
        # 初始化地圖
        center_lat = df['lat'].mean()
        center_lon = df['lon'].mean()
        m = folium.Map(location=[center_lat, center_lon], zoom_start=10 if single_mode else 9, tiles='CartoDB dark_matter')

        # 1. 繪製熱圖 (僅在非單機模式下)
        if not single_mode:
            heat_data = df[['lat', 'lon']].dropna().values.tolist()
            HeatMap(heat_data, radius=10, blur=15).add_to(m)

        # 2. 繪製航跡
        for hex_id, group in df.groupby('hex'):
            if len(group) < 5: continue
            
            # 單機模式下，我們不隨機取色，而是依高度變化著色 (或是統一醒目顏色)
            # 這裡維持隨機 Hex Color，但線條加粗
            color = self.get_color(hex_id) if not single_mode else '#00FF00' # 單機用亮綠色
            avg_alt = group['alt'].mean()
            flight_callsign = group['flight'].iloc[0] if 'flight' in group.columns else 'N/A'
            flight_display = str(flight_callsign).strip() if pd.notna(flight_callsign) else 'N/A'
            
            # --- 分段處理 (處理断線) ---
            segments = []
            curr_seg = []
            prev_time = None
            
            for _, row in group.iterrows():
                if prev_time and (row['time'] - prev_time > 300):
                    if len(curr_seg) > 1: segments.append(curr_seg)
                    curr_seg = []
                curr_seg.append(row)
                prev_time = row['time']
            if len(curr_seg) > 1: segments.append(curr_seg)
            
            # --- 繪製每一段 ---
            for seg in segments:
                # 動態決定該航段的 Callsign (掃描整段找出非空值)
                seg_flight = 'N/A'
                for pt in seg:
                    val = str(pt['flight']).strip()
                    if val and val != 'nan':
                        seg_flight = val
                        break # 找到任意一個就當作這段的名字
                
                display_name = seg_flight if seg_flight != 'N/A' else hex_id.upper()
                
                # 把 series list 轉回 dataframe 以便取值 (或直接用 list comprehension)
                latlons = [[r['lat'], r['lon']] for r in seg]
                
                # Popup 內容
                popup_html = f"""
                <b>{display_name}</b> ({hex_id})<br>
                Avg Alt: {avg_alt:.0f} ft<br>
                Len: {len(seg)} pts
                """
                
                line = folium.PolyLine(
                    latlons, 
                    color=color, 
                    weight=4 if single_mode else 2, 
                    opacity=0.8,
                    popup=folium.Popup(popup_html, max_width=200),
                    tooltip=f"{display_name}"
                ).add_to(m)

                # 單機模式：增加方向箭頭與詳細數據點
                if single_mode:
                    # 箭頭
                    PolyLineTextPath(line, '      ➤      ', repeat=True, offset=0, attributes={'fill': color, 'font-size': '16'}).add_to(m)
                    
                    # 取樣畫點 (每隔 N 點)
                    step = max(1, len(seg) // 20) # 每個 segment 最多 20 個點
                    for r in seg[::step]:
                        dt_str = datetime.datetime.fromtimestamp(r['time']).strftime('%H:%M:%S')
                        spd = r['gs'] if pd.notna(r['gs']) else 0
                        trk = r['track'] if pd.notna(r['track']) else 0
                        
                        detail_popup = f"""
                        <b>{dt_str}</b><br>
                        Alt: {r['alt']:.0f} ft<br>
                        GS: {spd:.0f} kts<br>
                        Trk: {trk:.0f}°
                        """
                        
                        folium.CircleMarker(
                            [r['lat'], r['lon']],
                            radius=3,
                            color='white',
                            fill=True,
                            fill_color=color,
                            fill_opacity=1.0,
                            popup=folium.Popup(detail_popup, max_width=200)
                        ).add_to(m)
                else:
                    # 一般模式：簡單箭頭
                    PolyLineTextPath(line, '          ➤          ', repeat=True, offset=0, attributes={'fill': color, 'font-size': '12'}).add_to(m)

        m.save(self.output_path)
        print(f"地圖已產出：{self.output_path}")

    def generate_altitude_map(self):
        # 產出區分高低空的航線圖 (Altitude Map)
        # 使用 helper 讀取時間窗口內的資料
        df = self.load_data()
        if df is None or df.empty: return
        
        # 額外的資料清理 (針對地圖需求)
        df = df.dropna(subset=['lat', 'lon', 'alt'])
        
        print(f"Generating Altitude Map with {len(df)} points (Past {self.lookback_hours}h)...")
        
        # 定義地圖
        m = folium.Map(location=[24.733, 121.083], zoom_start=9, tiles='CartoDB dark_matter')
        
        # 定義圖層: 高空巡航 (High Altitude) 用南北向區分; 低空 (Low Altitude) 維持單一
        layer_high_n = folium.FeatureGroup(name='High Alt (>20k) Northbound', show=True)
        layer_high_s = folium.FeatureGroup(name='High Alt (>20k) Southbound', show=True)
        layer_low = folium.FeatureGroup(name='Low Altitude (<20k ft)', show=False)
        
        for hex_id, group in df.groupby('hex'):
            if len(group) < 5: continue
            
            # --- 分段處理 ---
            segments = []
            curr_seg = []
            prev_time = None
            
            for _, row in group.iterrows():
                if prev_time and (row['time'] - prev_time > 300):
                    if len(curr_seg) > 1: segments.append(curr_seg)
                    curr_seg = []
                curr_seg.append(row)
                prev_time = row['time']
            if len(curr_seg) > 1: segments.append(curr_seg)
            
            # --- 針對每一段獨立繪圖 ---
            for seg in segments:
                # 1. 掃描 Callsign
                seg_flight = 'N/A'
                for pt in seg:
                    val = str(pt.get('flight', '')).strip()
                    if val and val != 'nan':
                        seg_flight = val
                        break
                display_name = seg_flight if seg_flight != 'N/A' else hex_id.upper()
                
                # 2. 判斷該段高度屬性 & 方向
                seg_df = pd.DataFrame(seg)
                avg_alt = seg_df['alt'].mean()
                
                if avg_alt >= 20000:
                    # 判斷南北向 (利用緯度差)
                    lat_start = seg[0]['lat']
                    lat_end = seg[-1]['lat']
                    
                    if lat_end >= lat_start:
                        # 往北 (Northbound)
                        target_layer = layer_high_n
                        color = '#00FFFF' # Cyan
                    else:
                        # 往南 (Southbound)
                        target_layer = layer_high_s
                        color = '#FF1493' # DeepPink
                        
                    opacity = 0.3
                    weight = 1
                else:
                    # 低空
                    target_layer = layer_low
                    color = '#FFFF00' # Yellow
                    opacity = 0.8
                    weight = 2
                
                latlons = [[r['lat'], r['lon']] for r in seg]

                # 3. Popup 內容 (包含 ADS-B Link)
                adsb_link = f"https://globe.adsbexchange.com/?icao={hex_id}"
                cat = group['category'].mode()[0] if 'category' in group and not group['category'].mode().empty else 'N/A'
                
                popup_html = f"""
                <b>{display_name}</b> ({hex_id})<br>
                Type: {cat}<br>
                Alt: {avg_alt:.0f} ft<br>
                <a href="{adsb_link}" target="_blank">Track ➤</a>
                """
                
                tooltip_txt = f"{display_name} ({avg_alt:.0f}ft)"
                
                # 4. 畫線
                line = folium.PolyLine(
                    latlons, 
                    color=color, 
                    weight=weight, 
                    opacity=opacity, 
                    popup=folium.Popup(popup_html, max_width=300),
                    tooltip=tooltip_txt
                ).add_to(target_layer)
                
                # 5. 箭頭
                PolyLineTextPath(
                    line, 
                    '          ➤          ', 
                    repeat=True, 
                    offset=0, 
                    attributes={'fill': color, 'font-size': '12', 'fill-opacity': opacity}
                ).add_to(target_layer)

        layer_high_n.add_to(m)
        layer_high_s.add_to(m)
        layer_low.add_to(m)
        folium.LayerControl().add_to(m)
        
        alt_map_path = os.path.join(self.base_path, f'reports/sky_phys_map_altitude_{self.date_str}.html')
        m.save(alt_map_path)
        print(f"分層航線圖已產出：{alt_map_path}")

    def generate_advanced_maps(self):
        # 產出進階分析地圖: 機型 (Category) & 垂直動態 (Vertical Rate)
        df = self.load_data()
        if df is None or df.empty: return

        # 額外的資料清理
        df = df.dropna(subset=['lat', 'lon', 'alt'])
        
        print(f"Generating Advanced Maps with {len(df)} points (Past {self.lookback_hours}h)...")
        
        # --- Map 1: 機型分類地圖 (Heavy/Large/Small) ---
        m_cat = folium.Map(location=[24.733, 121.083], zoom_start=9, tiles='CartoDB dark_matter')
        
        # 定義不同機型的 FeatureGroup
        layer_heavy = folium.FeatureGroup(name='Heavy/Super (A4/A5)', show=True)
        layer_large = folium.FeatureGroup(name='Large (A3)', show=True)
        layer_small = folium.FeatureGroup(name='Small/Light (A1/A2)', show=True)
        layer_other = folium.FeatureGroup(name='Other/Unknown', show=False)
        
        for hex_id, group in df.groupby('hex'):
            if len(group) < 5: continue
            
            # 取得該航班的主要機型 (取 mode)
            cat = group['category'].mode()
            cat = cat[0] if not cat.empty else 'Unknown'
            
            # 決定顏色與圖層
            if cat in ['A4', 'A5']:
                target_layer = layer_heavy
                color = '#FF4500' # OrangeRed
                weight = 2
            elif cat == 'A3':
                target_layer = layer_large
                color = '#1E90FF' # DodgerBlue
                weight = 1
            elif cat in ['A1', 'A2']:
                target_layer = layer_small
                color = '#32CD32' # LimeGreen
                weight = 1
            else:
                target_layer = layer_other
                color = '#808080' # Gray
                weight = 1

            points = group[['lat', 'lon']].values.tolist()
            flight_info = str(group['flight'].iloc[0]).strip()
            popup_txt = f"Flight: {flight_info}<br>Type: {cat}"
            
            folium.PolyLine(points, color=color, weight=weight, opacity=0.6, popup=popup_txt).add_to(target_layer)
            
        layer_heavy.add_to(m_cat)
        layer_large.add_to(m_cat)
        layer_small.add_to(m_cat)
        layer_other.add_to(m_cat)
        folium.LayerControl().add_to(m_cat)
        
        cat_map_path = os.path.join(self.base_path, f'reports/sky_phys_map_category_{self.date_str}.html')
        m_cat.save(cat_map_path)
        print(f"機型分類地圖已產出：{cat_map_path}")

        # --- Map 2: 垂直動態地圖 (Climb/Descent) ---
        m_vert = folium.Map(location=[24.733, 121.083], zoom_start=9, tiles='CartoDB dark_matter')
        
        # 這裡我們不依照 Flight 分組畫線，而是依照「點」的狀態畫彩色路徑 (Segmented Line)
        # 但為了效能，我們還是以 Flight 為單位，但在 Flight 內切分不同狀態的線段
        
        layer_climb = folium.FeatureGroup(name='Climbing (>500fpm)', show=True)
        layer_descent = folium.FeatureGroup(name='Descending (<-500fpm)', show=True)
        layer_level = folium.FeatureGroup(name='Level Flight', show=False)
        
        for hex_id, group in df.groupby('hex'):
            if len(group) < 5: continue
            if 'baro_rate' not in group.columns: continue
            
            # 將路徑切分為不同狀態的線段
            # 簡單做法：遍歷點，如果狀態改變就斷開重畫 (效能較差但準確)
            # 優化做法：過濾出 Climb/Descent 的子集畫點 (Scatter) 或短線
            # 這裡採用折衷：畫點 (CircleMarker) 對於觀察熱點最直觀
            
            # 過濾出顯著爬升/下降的點
            # 優化：每 10 個點取樣一次 (iloc[::10])，大幅減少 Marker 數量與檔案大小 (4MB -> 400KB)
            climbs = group[group['baro_rate'] > 500].iloc[::10]
            descents = group[group['baro_rate'] < -500].iloc[::10]
            
            for _, row in climbs.iterrows():
                if pd.isna(row['lat']) or pd.isna(row['lon']): continue
                rotation = row['track'] if not pd.isna(row['track']) else 0
                final_rot = rotation - 90
                
                folium.Marker(
                    location=[row['lat'], row['lon']],
                    icon=folium.DivIcon(html=f"""<div style="transform: rotate({final_rot}deg); color: #0f0; font-size: 14px;">➤</div>"""),
                    popup=f"Climb: {row['baro_rate']} fpm<br>Alt: {row['alt']} ft"
                ).add_to(layer_climb)
                
            for _, row in descents.iterrows():
                if pd.isna(row['lat']) or pd.isna(row['lon']): continue
                rotation = row['track'] if not pd.isna(row['track']) else 0
                final_rot = rotation - 90
                
                folium.Marker(
                    location=[row['lat'], row['lon']],
                    icon=folium.DivIcon(html=f"""<div style="transform: rotate({final_rot}deg); color: #f00; font-size: 14px;">➤</div>"""),
                    popup=f"Descent: {row['baro_rate']} fpm<br>Alt: {row['alt']} ft"
                ).add_to(layer_descent)
                
            # 平飛就不畫了，不然太亂
            
        layer_climb.add_to(m_vert)
        layer_descent.add_to(m_vert)
        folium.LayerControl().add_to(m_vert)
        
        vert_map_path = os.path.join(self.base_path, f'reports/sky_phys_map_vertical_{self.date_str}.html')
        m_vert.save(vert_map_path)
        print(f"垂直動態地圖已產出：{vert_map_path}")

if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser(description='Sky-Phys Map Generator')
    parser.add_argument('--hours', type=int, help='Lookback hours (default: 24)', default=24)
    parser.add_argument('--date', type=str, help='Specific date (YYYY-MM-DD)', default=None)
    parser.add_argument('--callsign', type=str, help='Filter by Flight ID (e.g., CI51)', default=None)
    parser.add_argument('--hex', type=str, help='Filter by Hex Code (e.g., A81234)', default=None)
    args = parser.parse_args()

    mapper = SkyPhysMapper(
        lookback_hours=args.hours, 
        target_date=args.date,
        target_callsign=args.callsign,
        target_hex=args.hex
    )
    mapper.generate()
    
    # 只有在沒有指定單機追蹤時，才執行全域地圖
    if not (args.callsign or args.hex):
        mapper.generate_altitude_map()
        mapper.generate_advanced_maps()