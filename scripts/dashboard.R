#!/usr/bin/env Rscript

# Load necessary libraries
suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(scales)
  library(grid)
  library(gridExtra)
  library(ggplot2)
  library(dplyr)
  library(patchwork) # 使用 patchwork 處理組圖對齊
})

# -----------------------------------------------------------------------------
# Global Constants & Theme Configuration
# -----------------------------------------------------------------------------

# Image Dimensions (LaTeX compatible)
BASE_WIDTH <- 6.3 # inches
BASE_HEIGHT <- 6.3 * 0.618 # Golden ratio
DPI <- 300

# Text Sizes
FONT_BASE_SIZE <- 12 # Matches LaTeX 12pt
ANNOTATION_SIZE <- 3.5 # For geom_text (mm), approx 10pt

# Global Theme
# Consistent font sizing across all plots
theme_skyphys <- function() {
  theme_light(base_family = "sans", base_size = FONT_BASE_SIZE) +
    theme(
      # 標題與副標題
      plot.title = element_text(face = "bold", size = rel(1.2), hjust = 0.5), # 居中更有正式感
      plot.subtitle = element_text(size = rel(1.0), color = "gray30", hjust = 0.5),

      # 註解文字 (解決換行擠壓問題)
      plot.caption = element_text(
        size = rel(0.8),
        color = "gray40",
        hjust = 0, # 左對齊適合長文本
        lineheight = 1.1 # 增加行距，防止換行時字母重疊
      ),

      # 軸標籤與格線
      axis.title = element_text(face = "bold", size = rel(1.0)),
      axis.text = element_text(size = rel(0.9)),
      panel.grid.minor = element_blank(), # 關閉細格線，讓數據點更清晰

      # 圖例設定
      legend.title = element_text(face = "bold", size = rel(0.9)),
      legend.text = element_text(size = rel(0.8)),
      legend.background = element_blank(),

      # 邊距微調：確保圖表內容不會太貼邊
      plot.margin = margin(10, 10, 10, 10, unit = "pt")
    )
}
theme_set(theme_skyphys())

# -----------------------------------------------------------------------------
# Argument Parsing & Data Loading
# -----------------------------------------------------------------------------

parse_args <- function() {
  args <- commandArgs(trailingOnly = TRUE)

  target_date <- NULL
  lookback_hours <- 24
  base_path <- getwd()
  start_hour <- 0
  end_hour <- 24

  if (length(args) > 0) {
    for (i in seq_along(args)) {
      if (args[i] == "--date" && i < length(args)) {
        target_date <- args[i + 1]
      } else if (args[i] == "--hours" && i < length(args)) {
        lookback_hours <- as.integer(args[i + 1])
      } else if (args[i] == "--base_path" && i < length(args)) {
        base_path <- args[i + 1]
      } else if (args[i] == "--start_hour" && i < length(args)) {
        start_hour <- as.numeric(args[i + 1])
      } else if (args[i] == "--end_hour" && i < length(args)) {
        end_hour <- as.numeric(args[i + 1])
      }
    }
  }

  list(
    target_date = target_date,
    lookback_hours = lookback_hours,
    base_path = base_path,
    start_hour = start_hour,
    end_hour = end_hour
  )
}

load_and_process <- function(base_path, target_date = NULL, lookback_hours = 24, start_hour = 0, end_hour = 24) {
  current_time <- Sys.time()
  files_to_read <- c()

  logs_dir <- file.path(base_path, "logs")

  if (!is.null(target_date)) {
    # Mode A: Specific Date with Time Window
    # Parse date (Assuming YYYY-MM-DD)
    base_dt <- as_datetime(paste0(target_date, " 00:00:00"), tz = "Asia/Taipei")
    start_dt <- base_dt + hours(start_hour)
    end_dt <- base_dt + hours(end_hour)

    files_to_read <- paste0("adsb_", target_date, ".csv")
    message(sprintf("分析模式: 指定日期 %s (%02d:00 - %02d:00)", target_date, start_hour, end_hour))
  } else {
    # Mode B: Rolling Window
    end_dt <- current_time
    start_dt <- end_dt - hours(lookback_hours)

    d <- as_date(start_dt, tz = "Asia/Taipei")
    end_date_val <- as_date(end_dt, tz = "Asia/Taipei")

    curr_d <- d
    while (curr_d <= end_date_val) {
      files_to_read <- c(files_to_read, paste0("adsb_", format(curr_d, "%Y-%m-%d"), ".csv"))
      curr_d <- curr_d + days(1)
    }
    files_to_read <- unique(files_to_read)

    message(sprintf("分析模式: 滾動視窗 (過去 %d 小時)", lookback_hours))
    message(sprintf("時間範圍: %s 到 %s", format(start_dt, "%Y-%m-%d %H:%M"), format(end_dt, "%Y-%m-%d %H:%M")))
  }

  cols_new <- c(
    "time", "hex", "flight", "lat", "lon", "alt", "alt_geom", "gs", "ias", "tas", "mach",
    "track", "track_rate", "roll", "mag_heading", "true_heading", "baro_rate", "geom_rate",
    "temp", "wd", "ws", "nav_qnh", "nav_altitude_mcp", "selected_heading", "squawk",
    "rssi", "messages", "rc", "nic_baro", "nac_p", "nac_v", "sil", "gva", "sda",
    "category", "nav_modes", "version"
  )


  df_list <- list()
  found_files <- FALSE

  for (fname in files_to_read) {
    fpath <- file.path(logs_dir, fname)
    if (file.exists(fpath)) {
      tryCatch(
        {
          # Since all historical data is now migrated to the 37-column format,
          # we can directly read using the full column list.
          # Still reading as character first for safety (e.g. mixed types in 'sil')
          temp_df <- read_csv(fpath, col_names = cols_new, col_types = cols(.default = "c"), progress = FALSE)
          df_list[[fname]] <- temp_df
          found_files <- TRUE
        },
        error = function(e) {
          message(sprintf("讀取 %s 失敗: %s", fname, e$message))
        }
      )
    }
  }

  if (!found_files) {
    message(sprintf("錯誤: 在 %s 找不到指定的數據檔案", logs_dir))
    return(NULL)
  }

  df <- bind_rows(df_list)

  start_ts <- as.numeric(start_dt)
  end_ts <- as.numeric(end_dt)
  df <- df %>% filter(time >= start_ts & time <= end_ts)

  if (nrow(df) == 0) {
    message("警告: 指定的時間範圍內沒有數據。")
    return(NULL)
  }

  message(sprintf("數據載入成功: %d 筆記錄", nrow(df)))

  # Data Processing
  # Convert time to numeric explicitly before parsing date
  if ("time" %in% names(df)) df$time <- as.numeric(df$time)

  # Preserve raw ADS-B temperature if available (before it gets overwritten by calculated temp)
  if ("temp" %in% names(df)) {
    df <- df %>% rename(raw_temp = temp)
    df$raw_temp <- as.numeric(df$raw_temp)
  }

  df <- df %>%
    mutate(
      dt = as_datetime(time, tz = "Asia/Taipei"),
      hour = hour(dt),
      session = case_when(
        hour >= 6 & hour < 12 ~ "Morning",
        hour >= 12 & hour < 18 ~ "Afternoon",
        TRUE ~ "Night"
      )
    )

  numeric_cols <- c("time", "lat", "lon", "tas", "mach", "gs", "alt", "track", "baro_rate", "geom_rate", "alt_geom", "d_value", "wd", "ws", "rssi")
  for (col in numeric_cols) {
    if (col %in% names(df)) df[[col]] <- as.numeric(df[[col]])
  }

  df <- df %>%
    filter(!is.na(tas), !is.na(mach), !is.na(alt)) %>%
    filter(tas > 100, mach > 0.3, alt > 5000)

  # Physics Calculations
  g <- 9.80665
  df <- df %>%
    mutate(
      speed_of_sound = tas / mach,
      temp = (speed_of_sound / 38.945)^2 - 273.15,
      wind_comp = gs - tas,
      v_ms = tas * 0.514444,
      h_m = alt * 0.3048,
      specific_energy = h_m + (v_ms^2) / (2 * g),
      temp_k = temp + 273.15,
      pressure_pa = 101325 * (pmax(0.001, 1 - 2.25577e-5 * h_m)^5.25588),
      air_density = pressure_pa / (287.058 * temp_k)
    ) %>%
    filter(temp > -90 & temp < 50)

  if ("track" %in% names(df)) {
    df <- df %>% mutate(direction = case_when(
      track >= 0 & track < 180 ~ "Eastbound",
      track >= 180 & track <= 360 ~ "Westbound",
      TRUE ~ "Unknown"
    ))
  } else {
    df$direction <- "Unknown"
  }

  if ("alt_geom" %in% names(df)) {
    df <- df %>% mutate(d_value = ifelse(abs(alt_geom - alt) > 3000, NA, alt_geom - alt))
  }

  return(df)
}

# -----------------------------------------------------------------------------
# Reporting Functions (Text Report)
# -----------------------------------------------------------------------------

analyze_tropopause <- function(df) {
  top_alt <- max(df$alt, na.rm = TRUE)
  min_temp <- min(df$temp, na.rm = TRUE)

  # Simple logic: if lowest temp is at highest altitude, tropopause is likely higher
  return(sprintf("對流層頂預測: 尚未偵測到 (目前觀測最高 %.0f ft 溫度仍持續下降至 %.1f°C，預計位於更高空)", top_alt, min_temp))
}

calculate_lapse_rate <- function(df_session) {
  df_clean <- df_session %>% filter(!is.na(alt), !is.na(temp))

  if (nrow(df_clean) < 10) {
    return("數據不足")
  }

  # Linear regression temp ~ alt
  model <- lm(temp ~ alt, data = df_clean)
  slope <- coef(model)["alt"]
  r_squared <- summary(model)$r.squared

  # Slope is deg C per ft. Convert to deg C per 1000 ft.
  # Normal lapse rate is negative (temp decreases with height). We want magnitude or signed value.
  # The python script uses abs(slope * 1000). Let's follow that but clarify if it's cooling.

  lapse_1000 <- abs(slope * 1000)
  return(sprintf("%.2f °C / 1000ft (R²=%.2f)", lapse_1000, r_squared))
}

generate_text_report <- function(df, output_dir, date_str) {
  file_path <- file.path(output_dir, sprintf("report_%s.txt", date_str))

  lines <- c()
  add_line <- function(l) lines <<- c(lines, l)

  add_line("=== Sky-Phys 觀測數據摘要報告 ===")
  add_line(sprintf("產出時間: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
  add_line(sprintf("總分析樣本數: %d", nrow(df)))
  add_line(sprintf("最高觀測高度: %.0f ft", max(df$alt, na.rm = TRUE)))

  # 1. Tropopause
  add_line(sprintf("\n[科學指標] %s", analyze_tropopause(df)))

  # 2. Jet Stream Warning
  max_wind <- max(df$wind_comp, na.rm = TRUE)
  if (max_wind > 100) {
    add_line(sprintf("\n⚠️ [警報] 發現強烈噴射氣流！最大順風分量達 %.1f knots", max_wind))
  } else if (max_wind > 60) {
    add_line(sprintf("\nℹ️ [資訊] 偵測到高空強風。最大順風分量: %.1f knots", max_wind))
  }

  # 3. Time Analysis (Lapse Rate)
  add_line("\n--- 時間維度分析 (氣溫直減率對比) ---")
  for (s in c("Morning", "Afternoon", "Night")) {
    sub_df <- df %>% filter(session == s)
    res <- calculate_lapse_rate(sub_df)
    add_line(sprintf("* %s (時段內遞減率): %s", s, res))
  }

  # 5. Directional Wind Analysis
  if ("direction" %in% names(df)) {
    df_east <- df %>% filter(direction == "Eastbound")
    df_west <- df %>% filter(direction == "Westbound")

    avg_wind_east <- if (nrow(df_east) > 0) mean(df_east$wind_comp, na.rm = TRUE) else 0
    avg_wind_west <- if (nrow(df_west) > 0) mean(df_west$wind_comp, na.rm = TRUE) else 0

    add_line("\n--- 航向與風場分析 (Directional Wind Analysis) ---")
    add_line(sprintf(
      "* 往東航班 (Eastbound): 平均風分量 %.1f kts [%s] (樣本數: %d)",
      avg_wind_east, ifelse(avg_wind_east > 0, "順風", "逆風"), nrow(df_east)
    ))
    add_line(sprintf(
      "* 往西航班 (Westbound): 平均風分量 %.1f kts [%s] (樣本數: %d)",
      avg_wind_west, ifelse(avg_wind_west > 0, "順風", "逆風"), nrow(df_west)
    ))

    wind_diff <- avg_wind_east - avg_wind_west
    add_line(sprintf("* 高空西風帶強度指標 (Westerly Index): %.1f kts", wind_diff))
  }

  # 6. Flight Performance
  max_gs <- max(df$gs, na.rm = TRUE)
  max_mach <- max(df$mach, na.rm = TRUE)
  max_tas <- max(df$tas, na.rm = TRUE)

  add_line("\n--- 飛行性能極值 (Flight Performance) ---")
  add_line(sprintf("* 最高地速 (Max GS): %.0f kts", max_gs))
  add_line(sprintf("* 最高真速 (Max TAS): %.0f kts", max_tas))
  add_line(sprintf("* 最大馬赫數 (Max Mach): M%.2f", max_mach))

  # 7. Signal Stats
  if ("rssi" %in% names(df)) {
    avg_rssi <- mean(df$rssi, na.rm = TRUE)
    min_rssi <- min(df$rssi, na.rm = TRUE)
    max_rssi <- max(df$rssi, na.rm = TRUE)

    add_line("\n--- 接收站訊號統計 (Signal Stats) ---")
    add_line(sprintf("* 平均訊號強度: %.1f dBFS", avg_rssi))
    add_line(sprintf("* 訊號範圍: %.1f ~ %.1f dBFS", min_rssi, max_rssi))
  }

  # 8. Markdown Table
  add_line("\n--- 高度層級統計 (Markdown Table for Blog) ---")
  add_line("| 高度 (ft) | 均溫 (°C) | 聲速 (kt) | 風分量 (kt) | 樣本 |")
  add_line("| :--- | :--- | :--- | :--- | :--- |")

  df_summary <- df %>%
    mutate(alt_bin = floor(alt / 5000) * 5000) %>%
    group_by(alt_bin) %>%
    summarise(
      temp_mean = mean(temp, na.rm = TRUE),
      speed_sound_mean = mean(speed_of_sound, na.rm = TRUE),
      wind_comp_mean = mean(wind_comp, na.rm = TRUE),
      count = n_distinct(hex), # Using distinct hex as sample count similar to Python hex count
      .groups = "drop"
    ) %>%
    arrange(alt_bin)

  for (i in 1:nrow(df_summary)) {
    row <- df_summary[i, ]
    add_line(sprintf(
      "| %.0f | %.2f | %.2f | %.2f | %d |",
      row$alt_bin, row$temp_mean, row$speed_sound_mean, row$wind_comp_mean, row$count
    ))
  }

  writeLines(lines, file_path)
  message(sprintf("數字報告已產出：%s", file_path))
}

# -----------------------------------------------------------------------------
# Plotting Helper Functions
# -----------------------------------------------------------------------------

save_plot <- function(plot_obj, filename, output_dir) {
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  ggsave(
    filename = file.path(output_dir, filename),
    plot = plot_obj,
    width = BASE_WIDTH,
    height = BASE_HEIGHT,
    dpi = DPI,
    units = "in"
  )
  message(sprintf("Saved: %s", filename))
}

# -----------------------------------------------------------------------------
# Individual Plot Functions
# -----------------------------------------------------------------------------
plot_temp_profile <- function(df, output_dir, date_str) {
  p <- ggplot(df, aes(x = temp, y = alt)) +
    # 觀測點
    geom_point(aes(color = session), alpha = 0.5, size = 0.5) +

    # 實際觀測的線性迴歸 (LM)
    geom_smooth(
      data = filter(df, session %in% c("Morning", "Afternoon")),
      aes(color = session, fill = session),
      method = "lm", se = TRUE, linewidth = 0.8, alpha = 0.1
    ) +
    scale_color_brewer(palette = "Set2") +
    scale_fill_brewer(palette = "Set2") +
    theme_skyphys() + # 套用你之前的全域主題
    labs(
      title = "Vertical Temperature Profile",
      subtitle = paste("Observed Lapse Rate"),
      x = "Temperature (°C)",
      y = "Altitude (ft)",
      caption = "Colored Lines: Linear regression of observed DAPs data."
    ) +
    ylim(0, NA)

  save_plot(p, sprintf("plot_temp_profile_%s.png", date_str), output_dir)
}

plot_raw_temp_profile <- function(df, output_dir, date_str) {
  if (!("raw_temp" %in% names(df))) {
    return()
  }

  df_clean <- df %>% filter(!is.na(raw_temp), !is.na(alt))
  if (nrow(df_clean) < 10) {
    return()
  }

  p <- ggplot(df_clean, aes(x = raw_temp, y = alt)) +
    geom_point(aes(color = session), alpha = 0.5, size = 0.5) +
    # Add calculated temp as reference if available
    geom_smooth(aes(x = temp, y = alt, color = "Calculated"),
      method = "lm", formula = y ~ x, se = FALSE,
      linetype = "dashed", linewidth = 0.8, alpha = 0.5
    ) +
    scale_color_brewer(palette = "Set2") +
    theme_skyphys() +
    labs(
      title = "Raw ADS-B Temperature Profile",
      subtitle = "Observed .temp Data Validation",
      x = "Raw Temperature (.temp) [°C]",
      y = "Altitude (ft)",
      caption = "Points: Raw ADS-B temperature data.\nDashed Line: Theoretical temperature derived from Mach/TAS."
    )

  save_plot(p, sprintf("plot_raw_temp_profile_%s.png", date_str), output_dir)
}

plot_speed_sound <- function(df, output_dir, date_str) {
  df_clean <- df %>%
    mutate(alt_bin = cut(alt, breaks = 50)) %>%
    group_by(hex, alt_bin) %>%
    summarise(across(c(speed_of_sound, alt, temp), \(x) mean(x, na.rm = TRUE)), .groups = "drop") %>%
    na.omit()

  p <- ggplot(df_clean, aes(x = speed_of_sound, y = alt, color = temp)) +
    geom_point(size = 0.5, alpha = 0.5) +
    scale_color_viridis_c(option = "D", name = "Temp (°C)") +
    labs(
      title = "Speed of Sound vs Altitude",
      x = "Speed of Sound (knots)",
      y = "Altitude (ft)",
      caption = "Speed of sound decreases with altitude primarily due to temperature drop.\nData derived from ADS-B DAPs observations."
    )

  save_plot(p, sprintf("plot_speed_sound_%s.png", date_str), output_dir)
}

plot_wind_comp <- function(df, output_dir, date_str) {
  df_clean <- df %>%
    mutate(alt_bin = cut(alt, breaks = 50)) %>%
    group_by(hex, alt_bin) %>%
    summarise(
      wind_comp = mean(wind_comp, na.rm = TRUE),
      alt = mean(alt, na.rm = TRUE),
      direction = first(direction),
      .groups = "drop"
    ) %>%
    na.omit()

  # Ensure limits accommodate the Jet Stream annotation at x=105
  # Check data max to avoid squashing data if max is very high
  x_max <- max(120, max(df_clean$wind_comp, na.rm = TRUE) + 10)

  p <- ggplot(df_clean, aes(x = wind_comp, y = alt, color = direction)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray", linewidth = 0.5) +
    geom_vline(xintercept = 100, linetype = "dotted", color = "red", linewidth = 0.5) +
    geom_point(alpha = 0.5, size = 0.5) +
    scale_color_manual(values = c("Eastbound" = "blue", "Westbound" = "red", "Unknown" = "gray")) +
    labs(
      title = "Wind Component Analysis",
      subtitle = "Headwind (-) vs Tailwind (+) by Flight Direction",
      x = "Wind Component (knots)",
      y = "Altitude (ft)",
      caption = "Wind Component = Ground Speed - True Airspeed.\nPositive values indicate tailwinds."
    )

  save_plot(p, sprintf("plot_wind_comp_%s.png", date_str), output_dir)
}

plot_tas_vs_gs <- function(df, output_dir, date_str) {
  max_speed <- max(c(max(df$tas, na.rm = TRUE), max(df$gs, na.rm = TRUE)))

  p <- ggplot(df, aes(x = tas, y = gs, color = direction)) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black", alpha = 0.5, linewidth = 0.5) +
    geom_point(size = 0.2, alpha = 0.5) +
    scale_color_manual(values = c("Eastbound" = "blue", "Westbound" = "red", "Unknown" = "gray")) +
    coord_fixed(ratio = 1, xlim = c(0, max_speed), ylim = c(0, max_speed)) +
    labs(
      title = "TAS vs Ground Speed",
      subtitle = "Impact of Wind on Ground Speed",
      x = "True Airspeed (TAS) [knots]",
      y = "Ground Speed (GS) [knots]",
      caption = "Points above diagonal indicate tailwind (GS > TAS).\nPoints below indicate headwind (GS < TAS)."
    )

  save_plot(p, sprintf("plot_tas_vs_gs_%s.png", date_str), output_dir)
}

plot_mach_vs_alt <- function(df, output_dir, date_str) {
  p <- ggplot(df, aes(x = mach, y = alt, color = temp)) +
    geom_point(size = 0.5, alpha = 0.5) +
    scale_color_viridis_c(option = "C", name = "Temp (°C)") +
    labs(
      title = "Mach Number vs Altitude",
      subtitle = "Flight Envelope Overview",
      x = "Mach Number",
      y = "Altitude (ft)",
      caption = expression(paste("", M == TAS / a, ". Higher altitude checks typically fly at higher Mach numbers."))
    )

  save_plot(p, sprintf("plot_mach_vs_alt_%s.png", date_str), output_dir)
}

plot_heading_rose <- function(df, output_dir, date_str) {
  if (!("track" %in% names(df))) {
    return()
  }

  df_rose <- df %>%
    filter(!is.na(track)) %>%
    mutate(bin_start = floor(track / 10) * 10) %>%
    group_by(bin_start) %>%
    summarise(count = n(), .groups = "drop") %>%
    complete(bin_start = seq(0, 350, 10), fill = list(count = 0))

  p <- ggplot(df_rose, aes(x = bin_start, y = count)) +
    geom_bar(stat = "identity", width = 10, fill = "skyblue", color = "navy", alpha = 0.7) +
    coord_polar(start = 0, direction = 1) +
    scale_x_continuous(breaks = c(0, 90, 180, 270), labels = c("N", "E", "S", "W"), limits = c(0, 360)) +
    theme(
      panel.border = element_blank(),
      axis.ticks = element_blank(),
      axis.text.y = element_blank(), # Hide radial count labels for cleaner look
      panel.grid.major = element_line(color = "grey90")
    ) +
    labs(
      title = "Traffic Density by Heading",
      subtitle = "Distribution of Flight Tracks",
      x = NULL, y = NULL,
      caption = "Shows the primary traffic flow directions recorded."
    )

  save_plot(p, sprintf("plot_heading_rose_%s.png", date_str), output_dir)
}

plot_rssi_vs_alt <- function(df, output_dir, date_str) {
  df_rssi <- df %>% filter(!is.na(rssi))

  p <- ggplot(df_rssi, aes(x = rssi, y = alt)) +
    geom_point(size = 0.2, alpha = 0.5, color = "steelblue") +
    # geom_smooth(method = "lm", color = "red", se = FALSE, linewidth = 0.5) +
    labs(
      title = "Signal Strength (RSSI) vs Altitude",
      subtitle = "Reception Quality Analysis",
      x = "RSSI (dBFS)",
      y = "Altitude (ft)",
      caption = "Free space path loss increases with distance.\nHigher altitude aircraft often have better Line-of-Sight."
    )

  save_plot(p, sprintf("plot_rssi_vs_alt_%s.png", date_str), output_dir)
}

plot_vertical_rate <- function(df, output_dir, date_str) {
  if (!("baro_rate" %in% names(df))) {
    return()
  }

  df_vr <- df %>%
    filter(!is.na(baro_rate), abs(baro_rate) < 8000) %>%
    mutate(activity = case_when(
      baro_rate > 100 ~ "Climb",
      baro_rate < -100 ~ "Descent",
      TRUE ~ NA_character_
    )) %>%
    na.omit()

  p <- ggplot(df_vr, aes(x = baro_rate, y = alt, color = activity)) +
    geom_vline(xintercept = 0, color = "black", linewidth = 0.5) +
    geom_point(size = 0.5, alpha = 0.5) +
    scale_color_manual(values = c("Climb" = "green", "Descent" = "red")) +
    labs(
      title = "Vertical Rate Profile",
      subtitle = "Climb vs Descent Performance",
      x = "Vertical Rate (ft/min)",
      y = "Altitude (ft)",
      caption = "Rate of climb/descent typically decreases with altitude due to engine performance limits."
    )

  save_plot(p, sprintf("plot_vertical_rate_%s.png", date_str), output_dir)
}

plot_category_perf <- function(df, output_dir, date_str) {
  if (!("category" %in% names(df))) {
    return()
  }

  cat_map <- c(
    "A1" = "Light (<7t)", "A2" = "Small (<34t)", "A3" = "Large (<136t)",
    "A4" = "Heavy (<300t)", "A5" = "Super Heavy"
  )

  df_cat <- df %>%
    filter(category %in% names(cat_map)) %>%
    mutate(cat_desc = cat_map[category])

  if (nrow(df_cat) == 0) {
    return()
  }

  p <- ggplot(df_cat, aes(x = tas, y = alt, color = cat_desc, shape = cat_desc)) +
    geom_point(size = 0.5, alpha = 0.5) +
    scale_color_brewer(palette = "Set1") +
    scale_shape_manual(values = c(16, 17, 15, 3, 7)) +
    labs(
      title = "Aircraft Performance by Category",
      subtitle = "Wake Turbulence Category vs Flight Envelope",
      x = "True Airspeed (TAS) [knots]",
      y = "Altitude (ft)",
      color = "Category", shape = "Category",
      caption = "heavier aircraft typically operate at higher altitudes and speeds."
    )

  save_plot(p, sprintf("plot_category_perf_%s.png", date_str), output_dir)
}

plot_wind_profile <- function(df, output_dir, date_str) {
  if (!("wd" %in% names(df)) || !("ws" %in% names(df))) {
    return()
  }

  # 1. 資料清理 (Tidyverse 風格)
  df_wind <- df %>%
    filter(!is.na(wd), !is.na(ws), ws >= 0, ws < 300, wd >= 0, wd <= 360)

  if (nrow(df_wind) <= 10) {
    return()
  }

  # 2. 設定 Y 軸統一範圍
  max_alt <- max(df_wind$alt, na.rm = TRUE)
  y_limits <- c(0, max_alt * 1.05)

  # 3. 子圖 1：風速 (保留 Y 軸文字)
  p1 <- ggplot(df_wind, aes(x = ws, y = alt)) +
    geom_point(color = "purple", alpha = 0.2, size = 0.5) +
    geom_smooth(method = "gam", color = "darkviolet", se = FALSE, linewidth = 1) +
    scale_y_continuous(limits = y_limits) +
    labs(title = "Wind Speed", x = "Speed (kt)", y = "Altitude (ft)") +
    theme(
      plot.margin = margin(5, 2, 5, 5) # 縮小右邊距，準備與 p2 接合
    )

  # 4. 子圖 2：風向 (隱藏 Y 軸文字)
  p2 <- ggplot(df_wind, aes(x = wd, y = alt, color = ws)) +
    geom_vline(xintercept = 270, linetype = "dashed", color = "gray", linewidth = 0.5) +
    geom_point(alpha = 0.5, size = 0.2) +
    scale_color_viridis_c(name = "Speed") +
    scale_x_continuous(breaks = c(0, 90, 180, 270, 360), labels = c("N", "E", "S", "W", "N"), limits = c(0, 360)) +
    scale_y_continuous(limits = y_limits) +
    theme(
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      plot.margin = margin(5, 5, 5, 2) # 縮小左邊距，與 p1 對接
    ) +
    labs(title = "Wind Direction", x = "Direction")

  # 5. 使用 patchwork 組合 (確保比例一致)
  # 使用 + 排列，plot_layout 確保寬度權重考慮到左圖的 Y 軸標籤
  p_combined <- (p1 + p2) +
    plot_layout(widths = c(1.15, 1), guides = "collect") +
    plot_annotation(
      title = sprintf("Observed Wind Profile (DAPs)"),
      caption = "Wind shear and direction changes with altitude.",
      theme = theme(
        plot.title = element_text(size = FONT_BASE_SIZE * 1.2, face = "bold", hjust = 0.5),
        plot.caption = element_text(size = FONT_BASE_SIZE * 0.8, color = "gray40", hjust = 0),
        plot.background = element_rect(fill = "white", color = NA) # 確保白色背景
      )
    )

  # 6. 儲存：關鍵在於維持 BASE 尺寸，不進行額外倍增
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  ggsave(
    filename = file.path(output_dir, sprintf("plot_wind_profile_%s.png", date_str)),
    plot = p_combined,
    width = BASE_WIDTH, # 維持基礎寬度
    height = BASE_HEIGHT, # 維持基礎高度
    dpi = DPI,
    units = "in",
    bg = "white" # 雙重保險確保背景不透明
  )

  message(sprintf("Saved: %s", "plot_wind_profile"))
}

plot_wind_profile2 <- function(df, output_dir, date_str) {
  if (!("wd" %in% names(df)) || !("ws" %in% names(df))) {
    return()
  }

  # Filter wind data
  df_wind <- df %>%
    filter(!is.na(wd), !is.na(ws), ws >= 0, ws < 300, wd >= 0, wd <= 360)

  if (nrow(df_wind) <= 10) {
    return()
  }

  # Standardize Y limits for both plots to ensure alignment
  # Add a little buffer so points aren't cut off at the very top
  max_alt <- max(df_wind$alt, na.rm = TRUE)
  y_limits <- c(0, max_alt * 1.05)

  # Plot 1: Wind Speed
  p1 <- ggplot(df_wind, aes(x = ws, y = alt)) +
    geom_point(color = "purple", alpha = 0.5, size = 0.5) +
    geom_smooth(method = "gam", color = "darkviolet", se = FALSE, linewidth = 1) +
    scale_y_continuous(limits = y_limits) +
    # Ensure light theme
    theme_light(base_family = "sans", base_size = FONT_BASE_SIZE) +
    labs(title = "Wind Speed", x = "Speed (kt)", y = "Altitude (ft)")

  # Plot 2: Wind Direction
  p2 <- ggplot(df_wind, aes(x = wd, y = alt, color = ws)) +
    geom_vline(xintercept = 270, linetype = "dashed", color = "gray", linewidth = 0.5) +
    geom_point(alpha = 0.5, size = 0.5) +
    scale_color_viridis_c(name = "Speed") +
    scale_x_continuous(breaks = c(0, 90, 180, 270, 360), labels = c("N", "E", "S", "W", "N"), limits = c(0, 360)) +
    scale_y_continuous(limits = y_limits) +
    theme_light(base_family = "sans", base_size = FONT_BASE_SIZE) +
    theme(
      # Remove Y axis labels and title from the second plot to cleaner layout
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    ) +
    labs(title = "Wind Direction", x = "Direction", y = NULL)

  # Combine plots
  # Use arrangeGrob with specified widths if desired, but equal is usually fine for these.
  p_combined <- arrangeGrob(p1, p2,
    ncol = 2,
    top = textGrob(sprintf("Observed Wind Profile (DAPs)"),
      gp = gpar(fontsize = FONT_BASE_SIZE * 1.2, fontface = "bold")
    ),
    bottom = textGrob("Wind shear and direction changes with altitude (Ekman spiral effects aloft).",
      x = 0, hjust = 0, gp = gpar(fontsize = FONT_BASE_SIZE * 0.8, col = "gray40")
    )
  )

  # Manual save for grid object
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  ggsave(
    filename = file.path(output_dir, sprintf("plot_wind_profile_%s.png", date_str)),
    plot = p_combined,
    width = BASE_WIDTH * 1.6,
    height = BASE_HEIGHT * 1.2,
    dpi = DPI, units = "in",
    bg = "white"
  )
  message(sprintf("Saved: %s", "plot_wind_profile"))
}

plot_d_value <- function(df, output_dir, date_str) {
  if (!("d_value" %in% names(df))) {
    return()
  }
  df_d <- df %>% filter(!is.na(d_value))
  if (nrow(df_d) == 0) {
    return()
  }

  p <- ggplot(df_d, aes(x = d_value, y = alt, color = d_value)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5) +
    geom_point(size = 0.5, alpha = 0.5) +
    scale_color_distiller(palette = "RdBu", direction = -1, limits = c(-1000, 1000), oob = scales::squish) +
    annotate("text", x = 800, y = 40000, label = "High Pressure\n(Warm/Ridge)", color = "red", fontface = "bold", hjust = 1, size = ANNOTATION_SIZE) +
    annotate("text", x = -800, y = 40000, label = "Low Pressure\n(Cold/Trough)", color = "blue", fontface = "bold", hjust = 0, size = ANNOTATION_SIZE) +
    labs(
      title = "Geopotential Height Anomaly (D-Value)",
      subtitle = "Pressure Pattern Analysis",
      x = "D-Value (Geometric - Pressure Alt) [ft]",
      y = "Altitude (ft)",
      color = "D-Value",
      caption = "D = Z_true - Z_pressure.\nPositive D-values indicate higher-than-standard pressure (Ridges).\nNegative values indicate lower pressure (Troughs)."
    )

  save_plot(p, sprintf("plot_d_value_%s.png", date_str), output_dir)
}

plot_specific_energy <- function(df, output_dir, date_str) {
  if (!("specific_energy" %in% names(df))) {
    return()
  }
  df_e <- df %>% filter(!is.na(specific_energy))

  p <- ggplot(df_e, aes(x = specific_energy, y = alt, color = mach)) +
    geom_point(alpha = 0.5, size = 0.5) +
    scale_color_viridis_c(option = "C") +
    labs(
      title = "Specific Energy Height",
      subtitle = "Total Energy State",
      x = "Specific Energy Height (m)",
      y = "Altitude (ft)",
      caption = expression(paste("", E[s] == h + V^2 / (2 * g), ". Represents the total mechanical energy per unit weight."))
    )

  save_plot(p, sprintf("plot_energy_%s.png", date_str), output_dir)
}

plot_air_density <- function(df, output_dir, date_str) {
  if (!("air_density" %in% names(df))) {
    return()
  }

  # Standard Atmosphere Model
  h_range <- seq(0, 45000, length.out = 100)
  rho_std <- 1.225 * exp(-(h_range * 0.3048) / 8500)
  df_std <- data.frame(alt = h_range, air_density = rho_std)

  p <- ggplot() +
    geom_point(data = df, aes(x = air_density, y = alt, color = temp), alpha = 0.5, size = 0.5) +
    geom_line(data = df_std, aes(x = air_density, y = alt), color = "red", linetype = "dashed", linewidth = 0.5) +
    scale_color_viridis_c() +
    labs(
      title = "Air Density Profile",
      subtitle = "Observed vs Standard Atmosphere",
      x = "Air Density (kg/m^3)",
      y = "Altitude (ft)",
      color = "Temp (°C)",
      caption = "Dashed Line: Standard Atmosphere Model (ISA).\nDots: Observed air density derived from P / (R * T)."
    )

  save_plot(p, sprintf("plot_density_%s.png", date_str), output_dir)
}

plot_turbulence <- function(df, output_dir, date_str) {
  if (!("baro_rate" %in% names(df))) {
    return()
  }

  df_turb <- df %>%
    mutate(turbulence_proxy = abs(baro_rate)) %>%
    filter(turbulence_proxy > 64)

  if (nrow(df_turb) == 0) {
    return()
  }

  p <- ggplot(df_turb, aes(x = turbulence_proxy)) +
    geom_histogram(bins = 100, fill = "purple", alpha = 0.7) +
    scale_y_continuous(trans = "log1p") +
    geom_vline(xintercept = 2000, color = "orange", linetype = "dashed", linewidth = 0.5) +
    geom_vline(xintercept = 4000, color = "red", linetype = "dashed", linewidth = 0.5) +
    annotate("text", x = 2050, y = 10, label = "Moderate", color = "orange", angle = 90, hjust = 0, size = ANNOTATION_SIZE) +
    annotate("text", x = 4050, y = 10, label = "Severe", color = "red", angle = 90, hjust = 0, size = ANNOTATION_SIZE) +
    labs(
      title = "Vertical Turbulence Spectrum",
      subtitle = "Vertical Rate Variance",
      x = "Absolute Vertical Rate (|fpm|)",
      y = "Count (Log Scale)",
      caption = "Rapid changes in vertical rate indicate atmospheric instability and turbulence."
    )

  save_plot(p, sprintf("plot_turbulence_%s.png", date_str), output_dir)
}

plot_contrail <- function(df, output_dir, date_str) {
  if (!("temp" %in% names(df))) {
    return()
  }

  df_contrail <- df %>% filter(alt > 20000)
  if (nrow(df_contrail) == 0) {
    return()
  }

  p <- ggplot() +
    annotate("rect", xmin = -40, xmax = 50, ymin = 20000, ymax = 45000, fill = "red", alpha = 0.1) +
    annotate("rect", xmin = -50, xmax = -40, ymin = 20000, ymax = 45000, fill = "yellow", alpha = 0.1) +
    annotate("rect", xmin = -90, xmax = -50, ymin = 20000, ymax = 45000, fill = "blue", alpha = 0.1) +
    geom_point(data = df_contrail, aes(x = temp, y = alt, color = hour), size = 0.2, alpha = 0.6) +
    scale_color_distiller(palette = "RdBu") +
    geom_vline(xintercept = -40, color = "red", linetype = "dashed", linewidth = 0.5) +
    geom_vline(xintercept = -50, color = "blue", linetype = "dashed", linewidth = 0.5) +
    xlim(-80, 20) +
    ylim(20000, 45000) +
    labs(
      title = "Contrail Formation Probability",
      subtitle = "Appleman Criterion",
      x = "Static Temperature (°C)",
      y = "Altitude (ft)",
      color = "Hour",
      caption = "Contrails form when hot engine exhaust mixes with cold air.\nLikely below -40°C, Persistent below -50°C."
    )

  save_plot(p, sprintf("plot_contrail_%s.png", date_str), output_dir)
}

plot_rssi_polar <- function(df, output_dir, date_str) {
  if (!all(c("lat", "lon", "rssi") %in% names(df))) {
    return()
  }

  # Filter valid coordinates first to avoid calculation errors
  df_geo <- df %>% filter(!is.na(lat), !is.na(lon), abs(lat) > 0.1, abs(lon) > 0.1)
  if (nrow(df_geo) < 10) {
    return()
  }

  # Reference point (e.g., station location)
  ref_lat <- 24.7470
  ref_lon <- 121.0824

  to_rad <- function(d) d * pi / 180
  to_deg <- function(r) r * 180 / pi

  lat1 <- to_rad(ref_lat)
  lon1 <- to_rad(ref_lon)

  # Calculate for the filtered dataframe
  lat2 <- to_rad(df_geo$lat)
  lon2 <- to_rad(df_geo$lon)
  dlon <- lon2 - lon1

  y <- sin(dlon) * cos(lat2)
  x <- cos(lat1) * sin(lat2) - sin(lat1) * cos(lat2) * cos(dlon)
  bearing_deg <- (to_deg(atan2(y, x)) + 360) %% 360

  a <- sin((lat2 - lat1) / 2)^2 + cos(lat1) * cos(lat2) * sin(dlon / 2)^2
  dist_km <- 6371 * 2 * atan2(sqrt(a), sqrt(1 - a))

  df_polar <- df_geo %>%
    mutate(bearing = bearing_deg, dist_km = dist_km) %>%
    filter(dist_km < 500, !is.na(bearing), !is.na(dist_km), !is.na(rssi))

  if (nrow(df_polar) == 0) {
    return()
  }

  coverage_env <- df_polar %>%
    mutate(bearing_bin = floor(bearing / 2) * 2 + 1) %>%
    group_by(bearing_bin) %>%
    summarise(max_dist = max(dist_km), .groups = "drop")

  p <- ggplot() +
    geom_point(data = df_polar, aes(x = bearing, y = dist_km, color = rssi), size = 0.2, alpha = 0.5) +
    geom_line(data = coverage_env, aes(x = bearing_bin, y = max_dist), color = "cyan", linetype = "dashed", linewidth = 0.5) +
    coord_polar(start = 0) +
    scale_x_continuous(limits = c(0, 360), breaks = c(0, 90, 180, 270), labels = c("N", "E", "S", "W")) +
    scale_color_viridis_c(option = "C", direction = -1) +
    theme(
      panel.grid.minor = element_line(color = "gray95"), # 極座標圖建議保留一點淺色格線
      axis.text.y = element_text(size = rel(0.7)), # 縮小距離標籤以免干擾
      plot.caption = element_text(lineheight = 1.2) # 確保換行後的行距舒適
    ) +
    labs(
      title = "Antenna Coverage Pattern",
      subtitle = sprintf("Range vs Azimuth - %s", date_str),
      x = NULL, y = NULL, color = "RSSI",
      # 使用 \n 進行物理意義的手動換行
      caption = "Cyan dashed line: Maximum observed signal range."
    )
  save_plot(p, sprintf("plot_rssi_polar_%s.png", date_str), output_dir)

  # Secondary Plot: Path Loss
  plot_path_loss(df_polar, output_dir, date_str)
}

plot_path_loss <- function(df_polar, output_dir, date_str) {
  near_field <- df_polar %>% filter(dist_km >= 10, dist_km <= 50)

  p <- ggplot(df_polar, aes(x = dist_km, y = rssi)) +
    geom_point(aes(color = alt), size = 0.5, alpha = 0.5) +
    scale_color_viridis_c() +
    labs(
      title = "Path Loss Analysis",
      subtitle = "Signal Strength vs Distance",
      x = "Distance (km)",
      y = "RSSI (dBFS)",
      color = "Alt",
      caption = expression(paste("Free Space Path Loss ", L[bf] %~% 20 * log10(d), "."))
    )

  # if (nrow(near_field) > 0) {
  #   ref_row <- near_field %>% slice_max(rssi, n = 1)
  #   ref_d <- ref_row$dist_km; ref_p <- ref_row$rssi
  #   fspl_fun <- function(x) { ref_p - 20 * log10(x / ref_d) }
  #   p <- p + stat_function(fun = fspl_fun, color = "red", linetype = "dashed", xlim = c(10, max(df_polar$dist_km)), linewidth = 0.5)
  # }

  save_plot(p, sprintf("plot_rssi_vs_dist_%s.png", date_str), output_dir)
}

plot_time_vs_alt <- function(df, output_dir, date_str) {
  # Time vs Altitude
  if (!("dt" %in% names(df))) {
    return()
  }

  df_time <- df %>% filter(!is.na(dt), !is.na(alt))
  if (nrow(df_time) < 10) {
    return()
  }

  # Check if time range is valid (finite)
  time_range <- range(df_time$dt, na.rm = TRUE)
  if (any(!is.finite(time_range))) {
    return()
  }

  p <- ggplot(df_time, aes(x = dt, y = alt, group = hex, color = hex)) +
    geom_line(alpha = 0.5, linewidth = 0.5, show.legend = FALSE) +
    scale_y_continuous(labels = scales::comma) +
    # Automatically format time axis
    scale_x_datetime(date_labels = "%H:%M", limits = time_range) +
    labs(
      title = "Traffic Distribution Over Time",
      subtitle = "Altitude vs Time of Day",
      x = "Time (LST)",
      y = "Altitude (ft)",
      caption = "Observed flight altitudes throughout the selected time window.\nEach colored line represents a unique aircraft."
    )

  save_plot(p, sprintf("plot_time_vs_alt_%s.png", date_str), output_dir)
}

# -----------------------------------------------------------------------------
# Main Execution Flow
# -----------------------------------------------------------------------------

main <- function() {
  args <- parse_args()
  df <- load_and_process(args$base_path, args$target_date, args$lookback_hours, args$start_hour, args$end_hour)

  if (!is.null(df)) {
    report_dir <- file.path(args$base_path, "reports")
    date_str <- if (!is.null(args$target_date)) args$target_date else format(Sys.Date(), "%Y-%m-%d")

    message(sprintf("開始繪製圖表至: %s", report_dir))

    # 1. Generate Text Report (New)
    generate_text_report(df, report_dir, date_str)

    plot_functions <- list(
      "plot_temp_profile" = plot_temp_profile,
      "plot_raw_temp_profile" = plot_raw_temp_profile,
      "plot_speed_sound" = plot_speed_sound,
      "plot_wind_comp" = plot_wind_comp,
      "plot_tas_vs_gs" = plot_tas_vs_gs,
      "plot_mach_vs_alt" = plot_mach_vs_alt,
      "plot_heading_rose" = plot_heading_rose,
      "plot_rssi_vs_alt" = plot_rssi_vs_alt,
      "plot_vertical_rate" = plot_vertical_rate,
      "plot_category_perf" = plot_category_perf,
      "plot_wind_profile" = plot_wind_profile,
      "plot_d_value" = plot_d_value,
      "plot_specific_energy" = plot_specific_energy,
      "plot_air_density" = plot_air_density,
      "plot_turbulence" = plot_turbulence,
      "plot_contrail" = plot_contrail,
      "plot_rssi_polar" = plot_rssi_polar,
      "plot_time_vs_alt" = plot_time_vs_alt
    )

    for (name in names(plot_functions)) {
      func <- plot_functions[[name]]
      tryCatch(
        {
          func(df, report_dir, date_str)
        },
        error = function(e) {
          message(sprintf("Error in %s: %s", name, e$message))
        }
      )
    }

    message("完成。")
  }
}

main()
