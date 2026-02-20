#!/bin/bash

# 定義路徑
BASE_DIR="$HOME/Documents/sky-phys"
SCRIPTS_DIR="$BASE_DIR/scripts"
REPORTS_DIR="$BASE_DIR/reports"
ARCHIVE_BASE="$REPORTS_DIR/daily_archive"

# 1. 計算昨天的日期 (Linux/Mac date command differences handled)
if date -v -1d > /dev/null 2>&1; then
    # BSD/MacOS date
    YESTERDAY=$(date -v -1d +%Y-%m-%d)
else
    # GNU/Linux date
    YESTERDAY=$(date -d "yesterday" +%Y-%m-%d)
fi

TARGET_DIR="$ARCHIVE_BASE/$YESTERDAY"
mkdir -p "$TARGET_DIR"

echo "--- Sky-Phys 昨日歸檔程序啟動 ---"
echo "目標日期: $YESTERDAY"
echo "歸檔路徑: $TARGET_DIR"

# 2. 執行 Dashboard 分析 (指定日期模式)
echo "[1/2] 正在分析 $YESTERDAY 全日數據..."
python3 "$SCRIPTS_DIR/dashboard.py" --date "$YESTERDAY"

if [ $? -ne 0 ]; then
    echo "[錯誤] Dashboard 分析失敗，請檢查日誌。"
    exit 1
fi

# 3. 移動圖表到歸檔目錄
# 注意：dashboard.py 產生的圖表檔名會帶有日期 (例如 plot_rssi_2026-02-17.png)
echo "[2/2] 正在歸檔圖表與報告..."

# 移動 PNG 圖表 (這次用 mv，因為這些圖表屬於歷史記錄，不應留在即時報告區混淆視聽)
find "$REPORTS_DIR" -maxdepth 1 -name "*$YESTERDAY*.png" -exec mv {} "$TARGET_DIR/" \;

# 複製文字報告 (如果有產生的話)
if [ -f "$REPORTS_DIR/observation_summary.txt" ]; then
    mv "$REPORTS_DIR/observation_summary.txt" "$TARGET_DIR/observation_summary_$YESTERDAY.txt"
fi

echo "--- 歸檔完成 ---"
echo "請檢查: $TARGET_DIR"
