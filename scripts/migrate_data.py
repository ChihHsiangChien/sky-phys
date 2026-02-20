
import os
import shutil
import csv
import glob

# ---------------------------------------------------------
# 1. 定義新舊欄位結構 (順序至關重要)
# ---------------------------------------------------------

# 舊格式 (23欄)
COLS_LEGACY = [
    'time', 'hex', 'flight', 'lat', 'lon', 'alt', 
    'gs', 'tas', 'mach', 'rssi', 'track', 
    'baro_rate', 'geom_rate', 'squawk', 'category', 'version', 
    'nav_qnh', 'wd', 'ws', 'alt_geom', 'nic_baro', 'nac_p', 'sil'
]

# 新格式 (37欄)
COLS_NEW = [
    'time', 'hex', 'flight', 'lat', 'lon', 'alt', 'alt_geom', 
    'gs', 'ias', 'tas', 'mach', 
    'track', 'track_rate', 'roll', 'mag_heading', 'true_heading', 
    'baro_rate', 'geom_rate', 
    'temp', 'wd', 'ws', 
    'nav_qnh', 'nav_altitude_mcp', 'selected_heading', 'squawk', 
    'rssi', 'messages', 'rc', 'nic_baro', 'nac_p', 'nac_v', 'sil', 'gva', 'sda',
    'category', 'nav_modes', 'version'
]

def migrate_file(filepath):
    """讀取單一 CSV，將每一行轉換為新格式後寫回"""
    temp_filepath = filepath + ".tmp"
    
    migrated_count = 0
    skipped_count = 0 # 已經是新格式的行數
    
    with open(filepath, 'r', encoding='utf-8', errors='ignore') as f_in, \
         open(temp_filepath, 'w', encoding='utf-8', newline='') as f_out:
        
        reader = csv.reader(f_in)
        writer = csv.writer(f_out)
        
        for row in reader:
            if not row: continue
            
            # 判斷該行是新格式還是舊格式
            if len(row) >= 35:
                # 已經是新格式 (或更長)，直接寫入
                # 為了保險，截斷到 37 欄 (雖然 recorder 不會多寫，但以防萬一)
                writer.writerow(row[:37])
                skipped_count += 1
            else:
                # 舊格式 -> 轉換
                # 建立一個 mapping 字典
                # 因為欄位可能少於 23 (例如最後幾個欄位是空值的 CSV 縮寫?)
                # 為了安全，補齊到 23
                row_padded = row + [''] * (23 - len(row))
                
                # 將舊數據放入字典方便提取
                data_map = {k: v for k, v in zip(COLS_LEGACY, row_padded)}
                
                # 構建新行
                new_row = []
                for col in COLS_NEW:
                    # 如果該欄位在舊數據中有，就填入；沒有則留空
                    val = data_map.get(col, '')
                    new_row.append(val)
                
                writer.writerow(new_row)
                migrated_count += 1
                
    # 取代原檔案
    shutil.move(temp_filepath, filepath)
    return migrated_count, skipped_count

def main():
    base_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
    logs_dir = os.path.join(base_dir, "logs")
    backup_dir = os.path.join(base_dir, "logs_backup_v1")
    
    if not os.path.exists(logs_dir):
        print(f"錯誤: 找不到 logs 目錄 ({logs_dir})")
        return

    # 1. 備份
    if not os.path.exists(backup_dir):
        print(f"正在備份 logs 到 {backup_dir} ...")
        shutil.copytree(logs_dir, backup_dir)
    else:
        print(f"備份目錄已存在 ({backup_dir})，跳過備份步驟。")

    # 2. 遍歷並遷移
    csv_files = glob.glob(os.path.join(logs_dir, "adsb_*.csv"))
    print(f"找到 {len(csv_files)} 個日誌檔案，開始遷移...")
    
    total_migrated = 0
    
    for fw in csv_files:
        filename = os.path.basename(fw)
        try:
            m, s = migrate_file(fw)
            print(f"  - {filename}: {m} 行已升級, {s} 行維持新格式")
            total_migrated += m
        except Exception as e:
            print(f"  [錯誤] 處理 {filename} 時失敗: {e}")
            
    print("-" * 40)
    print(f"遷移完成！共升級了 {total_migrated} 筆歷史數據。")
    print("現在所有 CSV 都是統一的 37 欄格式。")

if __name__ == "__main__":
    main()
