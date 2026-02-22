#!/bin/bash

# =============================================================================
# Sky-Phys 自動更新與分析腳本 (Update & Analysis Trigger)
# =============================================================================
#
# 此腳本用於自動執行 Sky-Phys 專案的數據分析與視覺化流程。
# 它會依序執行以下步驟：
# 1. 呼叫 dashboard.R 進行統計分析與圖表繪製。
# 2. 呼叫 mapper.py 產出互動式地圖 (HTML)。
# 3. 將產出的報告與圖表歸檔至 reports/daily_archive/YYYY-MM-DD 目錄。
#
# 使用方法 (Usage):
#   ./scripts/update_all.sh [OPTIONS]
#
# 選項 (Options):
#   --date YYYY-MM-DD   指定分析特定日期的數據 (例如: 2026-02-18)。
#                       此模式下會讀取 logs/adsb_YYYY-MM-DD.csv 並產出對應日期的報告。
#
#   --hours H           指定回推小時數 (Rolling Window 模式)。
#                       預設為 24 小時。腳本會讀取系統時間前 H 小時內的數據進行分析。
#                       注意: 只有 H=24 時才會執行自動歸檔，避免覆蓋完整日報。
#
# 範例 (Examples):
#   1. 分析今日(最近24小時)數據並與歸檔 (預設行為):
#      ./scripts/update_all.sh
#
#   2. 分析指定日期的數據 (補跑舊資料):
#      ./scripts/update_all.sh --date 2026-02-18
#
#   3. 快速預覽最近 1 小時的狀況 (不歸檔):
#      ./scripts/update_all.sh --hours 1
#
# =============================================================================
# 定義路徑
BASE_DIR="$HOME/Documents/sky-phys"
SCRIPTS_DIR="$BASE_DIR/scripts"
LOGS_DIR="$BASE_DIR/logs"
REPORTS_DIR="$BASE_DIR/reports"
TODAY=$(date +%Y-%m-%d)
TODAY_DIR="$REPORTS_DIR/daily_archive/$TODAY"

echo "--- Sky-Phys 專案自動更新啟動 ---"
echo "時間: $(date)"

# 1. 檢查 logs 目錄下是否有任何 CSV 檔案
count=$(ls "$LOGS_DIR"/adsb_*.csv 2>/dev/null | wc -l)
if [ "$count" -eq 0 ]; then
    echo "[錯誤] 找不到數據檔案 ( $LOGS_DIR/adsb_*.csv )"
    echo "請先確認 recorder.sh 是否有在執行。"
    exit 1
fi

# 建立今日的歸檔目錄 reports/daily_archive/YYYY-MM-DD
mkdir -p "$TODAY_DIR"

# 2. 執行數據分析 (Dashboard)
# 支援傳入參數指定回推小時數 (預設 24)
# 預設參數
HOURS=24
TARGET_DATE=""

# 解析參數
while [[ "$#" -gt 0 ]]; do
    case $1 in
        --hours) HOURS="$2"; shift ;;
        --date) TARGET_DATE="$2"; shift ;;
        *) HOURS="$1" ;; # 兼容舊版直接傳數字
    esac
    shift
done

# 設定日期變數
if [ -n "$TARGET_DATE" ]; then
    TODAY="$TARGET_DATE"
    echo "--- Sky-Phys 分析模式: 指定日期 $TARGET_DATE ---"
else
    TODAY=$(date +%Y-%m-%d)
    echo "--- Sky-Phys 分析模式: 最近 $HOURS 小時 (Rolling Window) ---"
fi

TODAY_DIR="$REPORTS_DIR/daily_archive/$TODAY"

# 1. 檢查 logs 目錄下是否有任何 CSV 檔案
count=$(ls "$LOGS_DIR"/adsb_*.csv 2>/dev/null | wc -l)
if [ "$count" -eq 0 ]; then
    echo "[錯誤] 找不到數據檔案 ( $LOGS_DIR/adsb_*.csv )"
    echo "請先確認 recorder.sh 是否有在執行。"
    exit 1
fi

# 建立今日的歸檔目錄
mkdir -p "$TODAY_DIR"

# 建構傳遞給 Python/R 的參數
CMD_ARGS="--hours $HOURS"
if [ -n "$TARGET_DATE" ]; then
    CMD_ARGS="$CMD_ARGS --date $TARGET_DATE"
fi

# 2. 執行數據分析 (Dashboard R)
echo "[1/2] 正在分析數據 (R)..."
Rscript "$SCRIPTS_DIR/dashboard.R" $CMD_ARGS

# 3. 執行地圖與航跡更新 (Mapper Python)
echo "[2/3] 正在產出互動式地圖 (Python)..."
python3 "$SCRIPTS_DIR/mapper.py" $CMD_ARGS

# 4. 執行進階物理地圖繪製 (Mapper R)
echo "[3/3] 正在產出進階物理地圖 (R)..."
Rscript "$SCRIPTS_DIR/mapper.R" $CMD_ARGS

# 4. 自動歸檔整理 (Organize Daily Reports)
# 邏輯：如果有指定日期，或者在 Rolling Window 模式下指定了 24 小時，則執行歸檔
if [ -n "$TARGET_DATE" ] || [ "$HOURS" -eq 24 ]; then
    echo "[3/3] 正在歸檔報告至 $TODAY_DIR ..."

    # (A) 複製 PNG 圖表
    find "$REPORTS_DIR" -maxdepth 1 -name "*$TODAY*.png" -exec cp {} "$TODAY_DIR/" \;

    # (B) 複製文字報告 (Dashboard R 產出的 report_YYYY-MM-DD.txt)
    find "$REPORTS_DIR" -maxdepth 1 -name "report_$TODAY.txt" -exec cp {} "$TODAY_DIR/" \;
    
    # (C) 複製 HTML 地圖 (Mapper 產出的 *_YYYY-MM-DD.html)
    find "$REPORTS_DIR" -maxdepth 1 -name "*$TODAY.html" -exec cp {} "$TODAY_DIR/" \;

    # (D) 複製進階物理地圖 (Mapper R 產出的 static_maps)
    if [ -d "$REPORTS_DIR/static_maps" ]; then
        mkdir -p "$TODAY_DIR/static_maps"
        cp -r "$REPORTS_DIR/static_maps/"* "$TODAY_DIR/static_maps/"
    fi
    
    # 兼容舊版與備份 Summary (如果存在)
    if [ -f "$REPORTS_DIR/observation_summary.txt" ]; then
        cp "$REPORTS_DIR/observation_summary.txt" "$TODAY_DIR/observation_summary_$TODAY.txt"
    fi
else
    echo "[!] 僅更新即時報告 (reports/)，跳過歸檔以避免覆蓋完整日報。"
fi

echo "--- 更新完成 ---"
echo "最新報告 (Latest): $REPORTS_DIR/"
echo "今日歸檔 (Archive): $TODAY_DIR/"
ls -lh "$TODAY_DIR"