#!/bin/bash
# Sky-Phys 按日數據紀錄器

# 0. 檢查是否已有舊的 recorder.sh 在執行 (避免重複啟動)
#if ps aux | grep "recorder.sh" | grep -v grep | grep -v $$ > /dev/null; then
#    echo "[錯誤] 偵測到另一個 recorder.sh 正在執行。請先停止舊程序。"
#    exit 1
#fi

# 自動偵測腳本位置，將 log 放在專案目錄下的 logs 資料夾
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LOGS_DIR="$SCRIPT_DIR/../logs"

# 確保 log 目錄存在
mkdir -p "$LOGS_DIR"


while true; do
  # 動態產出當天的檔案名稱
  CURRENT_DATE=$(date +%Y-%m-%d)
  OUTPUT_FILE="$LOGS_DIR/adsb_$CURRENT_DATE.csv"

  if [ -f /run/readsb/aircraft.json ]; then
    jq -r '.now as $t | .aircraft[] | select(.lat != null) | [
      $t, .hex, (.flight // "N/A"), .lat, .lon, .alt_baro, .alt_geom, .gs, .ias, .tas, .mach,
      .track, .track_rate, .roll, .mag_heading, .true_heading, .baro_rate, .geom_rate,
      .temp, .wd, .ws, .nav_qnh, .nav_altitude_mcp, .selected_heading, .squawk,
      .rssi, .messages, .rc, .nic_baro, .nac_p, .nac_v, .sil, .gva, .sda,
      .category, .nav_modes, .version
    ] | @csv' /run/readsb/aircraft.json >> "$OUTPUT_FILE"
  fi
  sleep 5
done
