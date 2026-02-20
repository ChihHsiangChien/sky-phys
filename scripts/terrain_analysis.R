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
  library(patchwork)
  library(zoo) # For rolling means
})

# -----------------------------------------------------------------------------
# Global Constants & Theme Configuration
# -----------------------------------------------------------------------------

# Image Dimensions (LaTeX compatible)
BASE_WIDTH  <- 6.3              # inches
BASE_HEIGHT <- 6.3 * 0.618      # Golden ratio
DPI         <- 300

# Text Sizes
FONT_BASE_SIZE <- 12            # Matches LaTeX 12pt
ANNOTATION_SIZE <- 3.5          # For geom_text (mm), approx 10pt

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
        hjust = 0,      # 左對齊適合長文本
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
# Configuration & ROI
# -----------------------------------------------------------------------------

ROI <- list(
  lat_min = 24.7500,
  lat_max = 24.9000,
  lon_min = 121.0200,
  lon_max = 121.2500
)

# -----------------------------------------------------------------------------
# Data Loading (Adapted from dashboard.R)
# -----------------------------------------------------------------------------

load_and_process <- function(base_path, target_date = NULL, lookback_hours = 24) {
  
  sys_time <- Sys.time() # Capture current time for rolling window
  
  files_to_read <- c()
  logs_dir <- file.path(base_path, "logs")
  
  if (!is.null(target_date)) {
    # Mode A: Specific Date
    target_dt <- as_datetime(paste0(target_date, " 00:00:00"), tz = "Asia/Taipei")
    # For a specific date, we usually want the whole day or related files
    files_to_read <- paste0("adsb_", target_date, ".csv")
    message(sprintf("分析模式: 指定日期 %s", target_date))
    start_ts <- as.numeric(target_dt)
    end_ts <- as.numeric(target_dt + days(1))
  } else {
    # Mode B: Rolling Window
    end_dt <- sys_time
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
    start_ts <- as.numeric(start_dt)
    end_ts <- as.numeric(end_dt)
  }
  
  cols_new <- c('time', 'hex', 'flight', 'lat', 'lon', 'alt', 'alt_geom', 'gs', 'ias', 'tas', 'mach', 
               'track', 'track_rate', 'roll', 'mag_heading', 'true_heading', 'baro_rate', 'geom_rate', 
               'temp', 'wd', 'ws', 'nav_qnh', 'nav_altitude_mcp', 'selected_heading', 'squawk', 
               'rssi', 'messages', 'rc', 'nic_baro', 'nac_p', 'nac_v', 'sil', 'gva', 'sda',
               'category', 'nav_modes', 'version')
               

  
  df_list <- list()
  found_files <- FALSE
    fpath <- file.path(logs_dir, fname)
    if (file.exists(fpath)) {
      tryCatch({
            # Unified format reading
            temp_df <- read_csv(fpath, col_names = cols_new, col_types = cols(.default = "c"), progress = FALSE)
            df_list[[fname]] <- temp_df
            found_files <- TRUE
      }, error = function(e) {
        message(sprintf("讀取 %s 失敗: %s", fname, e$message))
      })
    }
  }
  
  if (!found_files) {
    message(sprintf("錯誤: 在 %s 找不到指定的數據檔案", logs_dir))
    return(NULL)
  }
  
  # Bind all character dataframes first
  df <- bind_rows(df_list)
  
  # Convert types *after* binding
  numeric_cols <- c('time', 'lat', 'lon', 'alt', 'gs', 'tas', 'mach', 'rssi', 'track', 
                    'baro_rate', 'geom_rate', 'nav_qnh', 'wd', 'ws', 'alt_geom', 'nic_baro', 'nac_p', 'sil')
  
  for (col in numeric_cols) {
    if (col %in% names(df)) df[[col]] <- as.numeric(df[[col]])
  }
  
  df <- df %>% filter(time >= start_ts & time <= end_ts)
  
  df <- df %>%
    filter(!is.na(tas), !is.na(mach), !is.na(alt)) %>%
    filter(tas > 100, mach > 0.3, alt > 1000) # Slightly relaxed altitude for terrain analysis
  
  # Physics Calculations
  g <- 9.80665
  df <- df %>%
    mutate(
      dt = as_datetime(time, tz = "Asia/Taipei"),
      speed_of_sound = tas / mach,
      temp = (speed_of_sound / 38.945)^2 - 273.15,
      wind_comp = gs - tas,
      v_ms = tas * 0.514444,
      h_m = alt * 0.3048,
      specific_energy = h_m + (v_ms^2) / (2 * g),
      temp_k = temp + 273.15,
      # Simple pressure model if needed, but we focus on D-value
      pressure_pa = 101325 * (pmax(0.001, 1 - 2.25577e-5 * h_m)^5.25588)
    ) %>%
    filter(temp > -90 & temp < 50)
  
  # Calculate D-Value if avaiable
  if ("alt_geom" %in% names(df)) {
    df <- df %>% mutate(d_value = ifelse(abs(alt_geom - alt) > 3000, NA, alt_geom - alt))
  } else {
    df$d_value <- NA
  }
  
  return(df)
}

filter_roi <- function(df, roi) {
  message(sprintf("正在鎖定研究區域: Lat[%.4f, %.4f], Lon[%.4f, %.4f]", 
                  roi$lat_min, roi$lat_max, roi$lon_min, roi$lon_max))
  
  df_roi <- df %>%
    filter(lat >= roi$lat_min & lat <= roi$lat_max &
             lon >= roi$lon_min & lon <= roi$lon_max)
  
  message(sprintf("區域內數據量: %d 筆 (原始: %d 筆)", nrow(df_roi), nrow(df)))
  return(df_roi)
}

# -----------------------------------------------------------------------------
# Core Algorithm: Flight Anomalies (Detrending)
# -----------------------------------------------------------------------------

calculate_flight_anomalies <- function(df) {
  # 1. Pre-calculate altitude bins for later Z-score normalization
  df <- df %>%
    mutate(alt_bin = floor(alt / 1000) * 1000)
  
  # 2. Calculate Residuals by Flight (Hex)
  # We define a helper function to apply to each group
  calc_residual <- function(sub_df) {
    # If not enough data points for complex detrending, we default to simple median subtraction
    # This ensures we at least visualize the data points (z-score ~ 0), rather than dropping them.
    if (nrow(sub_df) < 3) {
       sub_df$d_residual <-0 # For n<3, self-referenced residual is meaningless (0 for n=1). 
       # Better: sub_df$d_value - median(sub_df$d_value) -> 0. 
       # Just let it fall through to the "simple median" logic below?
       # No, n=1 median is itself. Result 0. Correct.
    }
    
    alt_range <- max(sub_df$alt, na.rm = TRUE) - min(sub_df$alt, na.rm = TRUE)
    
    if (nrow(sub_df) < 10 || alt_range < 500) {
      # Level flight OR insufficient data for regression: simple median subtraction
      med_val <- median(sub_df$d_value, na.rm = TRUE)
      if (is.na(med_val)) {
         sub_df$d_residual <- NA
      } else {
         sub_df$d_residual <- sub_df$d_value - med_val
      }
    } else {
      # Climbing/Descending with enough data: Linear detrending
      # Fit model: d_value ~ alt
      tryCatch({
        m <- lm(d_value ~ alt, data = sub_df)
        sub_df$d_residual <- residues(m)
      }, error = function(e) {
        sub_df$d_residual <- sub_df$d_value - median(sub_df$d_value, na.rm = TRUE)
      })
    }
    return(sub_df)
  }
  
  # Apply grouping and calculation
  # Note: group_modify or split/map/bind might be slower but clearer. 
  # For speed with large groups, data.table is better, but stick to dplyr for consistency
  df <- df %>%
    group_by(hex) %>%
    group_modify(~ calc_residual(.x)) %>%
    ungroup()
  
  # 3. Calculate Z-Score (Global Scale Normalization per layer)
  df <- df %>%
    group_by(alt_bin) %>%
    mutate(
      layer_std = sd(d_residual, na.rm = TRUE),
      z_score = if_else(is.na(layer_std) | layer_std == 0, NA_real_, d_residual / layer_std)
    ) %>%
    ungroup()
  
  return(df)
}

# -----------------------------------------------------------------------------
# Plotting Functions
# -----------------------------------------------------------------------------

plot_vertical_profile <- function(df, out_dir) {
  # Filter outliers
  data <- df %>% filter(temp > -80, temp < 40)
  
  # Create simple profile
  # Color by Altitude to match python style
  p <- ggplot(data, aes(x = temp, y = alt, color = alt)) +
    geom_point(alpha = 0.6, size = 1.5, shape = 16) +
    scale_color_gradientn(colors = c("blue", "white", "red")) +
    scale_y_continuous(labels = scales::comma) +
    
    # ISA Model Line
    stat_function(fun = function(t_c) (15 - t_c) / 1.98 * 1000, # Inverse of T = 15 - 1.98(h/1k) => h = (15-T)/1.98 * 1000
                 geom = "path", color = "black", linetype = "dashed", alpha = 0.5) +
                 
    # We can plot ISA efficiently by generating data instead of inverse function which is hard to map x->y in ggplot
    geom_line(data = data.frame(alt = seq(0, 40000, 1000), temp = 15 - 1.98 * seq(0, 40, 1)),
              aes(x = temp, y = alt), color = "black", linetype = "dashed", alpha = 0.5, inherit.aes = FALSE) +
    
    labs(
      title = sprintf("Vertical Temperature Profile (%.2fN - %.2fN)", ROI$lat_min, ROI$lat_max),
      subtitle = "Observed vs ISA Model",
      x = "Static Air Temperature (°C)",
      y = "Altitude (ft)",
      color = "Altitude"
    ) 
    
    # Add Observed Profile (Mean by bins)
    if (nrow(data) > 20) {
      profile <- data %>%
        mutate(alt_bin = floor(alt / 500) * 500) %>%
        group_by(alt_bin) %>%
        summarise(temp = mean(temp, na.rm=TRUE), .groups = "drop")
      
      p <- p + geom_path(data = profile, aes(x = temp, y = alt_bin), color = "red", linewidth = 1, inherit.aes = FALSE)
    }

  ggsave(file.path(out_dir, "1_temp_profile.png"), p, width = BASE_WIDTH, height = BASE_HEIGHT, dpi = DPI)
}

plot_d_value_anomaly <- function(df, out_dir) {
  # Clean and Calculate
  df_clean <- df %>% filter(!is.na(alt), !is.na(d_value), !is.na(lat), !is.na(lon))
  df_clean <- calculate_flight_anomalies(df_clean)
  df_clean <- df_clean %>% filter(!is.na(z_score))
  
  if (nrow(df_clean) == 0) return()
  
  # Helper for trend lines
  get_trend <- function(d, x_var, y_var, win_frac=0.05) {
    if (nrow(d) < 10) return(d) # Not enough for trend
    d <- d %>% arrange(.data[[x_var]])
    win <- max(10, ceiling(nrow(d) * win_frac))
    d$trend <- rollmean(d[[y_var]], k = win, fill = NA, align = "center")
    d
  }

  process_layer <- function(sub_df, title_prefix, is_control=FALSE) {
    # If no data, return a list of empty plots (Placeholders)
    if (nrow(sub_df) < 5) {
      empty_p <- ggplot() + theme_void() + 
        annotate("text", x=0.5, y=0.5, label=paste(title_prefix, "\nInsufficient Data"))
      return(list(empty_p, empty_p, empty_p))
    }
    
    # 1. Profile
    layer_stats <- sub_df %>%
      group_by(alt_bin) %>%
      summarise(d_value = mean(d_value, na.rm=TRUE), .groups = "drop")
    
    p1 <- ggplot(sub_df, aes(x = d_value, y = alt)) +
      geom_point(alpha = 0.3, size = 0.5, color = if(is_control) "gray" else "purple") +
      geom_path(data = layer_stats, aes(x = d_value, y = alt_bin), color = if(is_control) "blue" else "red", linewidth = 1) +
      labs(title = paste(title_prefix, "Profile"), y = "Altitude (ft)", x = "D-Value (ft)")
    
    # 2. Lat vs Z
    sub_df_lat <- get_trend(sub_df, "lat", "z_score")
    p2 <- ggplot(sub_df_lat, aes(x = lat, y = z_score)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
      geom_point(aes(color = alt), alpha = 0.4, size = 1, show.legend = FALSE) +
      scale_color_viridis_c() +
      coord_cartesian(ylim = c(-3.5, 3.5)) +
      labs(title = paste(title_prefix, "N-S Anomaly"), y = "Z-Score", x = "Latitude")
      
    if("trend" %in% names(sub_df_lat)) {
       p2 <- p2 + geom_line(aes(y = trend), color = "black", linewidth = 1, linetype = if(is_control) "dashed" else "solid", na.rm = TRUE)
    }
    
    # 3. Lon vs Z
    sub_df_lon <- get_trend(sub_df, "lon", "z_score")
    p3 <- ggplot(sub_df_lon, aes(x = lon, y = z_score)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
      geom_point(aes(color = alt), alpha = 0.4, size = 1, show.legend = FALSE) +
      scale_color_viridis_c() +
      coord_cartesian(ylim = c(-3.5, 3.5)) +
      labs(title = paste(title_prefix, "E-W Anomaly"), y = NULL, x = "Longitude")

    if("trend" %in% names(sub_df_lon)) {
       p3 <- p3 + geom_line(aes(y = trend), color = "black", linewidth = 1, linetype = if(is_control) "dashed" else "solid", na.rm = TRUE)
    }
      
    list(p1, p2, p3)
  }
  
  low_plots <- process_layer(df_clean %>% filter(alt < 6000), "Low Alt", FALSE)
  high_plots <- process_layer(df_clean %>% filter(alt > 20000), "High Alt", TRUE)
  
  # Removed the check for NULL because process_layer always returns a list of 3 plots now
  
  # Combine using patchwork
  # Layout:
  # L1 L2 L3
  # H1 H2 H3
  layout <- (low_plots[[1]] | low_plots[[2]] | low_plots[[3]]) /
            (high_plots[[1]] | high_plots[[2]] | high_plots[[3]])
            
  final_plot <- layout + plot_annotation(title = "D-Value Anomaly Analysis (Terrain Effect vs Free Atmosphere)")
  
  ggsave(file.path(out_dir, "2_d_value_anomaly.png"), final_plot, width = 12, height = 8, dpi = 150)
}

plot_turbulence_analysis <- function(df, out_dir) {
  # V2 Algorithm: Vertical Acceleration
  # Sort by Hex, Time
  df_turb <- df %>%
    arrange(hex, time) %>%
    group_by(hex) %>%
    mutate(
      dv = baro_rate - lag(baro_rate),
      # Time difference in minutes approx? baro_rate is ft/min. 
      # Actually Python script just took diff().abs() of baro_rate. 
      # Assuming constant sampling rate (~0.5-1s), diff is proportional to acceleration.
      # Let's stick to Python logic: abs(diff(baro_rate))
      v_accel = abs(dv)
    ) %>%
    filter(!is.na(v_accel)) %>%
    ungroup()
  
  if (nrow(df_turb) == 0) return()
  
  # Binning altitude
  bins <- seq(0, 42000, 2000)
  labels <- paste0(head(bins, -1)/1000, "k-", tail(bins, -1)/1000, "k")
  df_turb$alt_layer <- cut(df_turb$alt, breaks = bins, labels = labels, include.lowest = TRUE)
  
  # Filter layers with enough data
  layer_counts <- df_turb %>% count(alt_layer) %>% filter(n > 10)
  df_turb_filtered <- df_turb %>% filter(alt_layer %in% layer_counts$alt_layer)
  
  # Mean stats for line plot
  layer_stats <- df_turb_filtered %>%
    group_by(alt_layer) %>%
    summarise(mean_vaccel = mean(v_accel, na.rm=TRUE), .groups = "drop")
  
  # In ggplot, dual axis is tricky. Using scaling factor.
  # Boxplot values can be large (e.g. 0-2000). Mean might be small (e.g. 50-200).
  # Let's check ranges. Boxplot is distribution.
  coeff <- 1 # Can adjust if needed
  
  p <- ggplot(df_turb_filtered, aes(x = alt_layer, y = v_accel)) +
    geom_boxplot(aes(fill = alt_layer), outlier.shape = NA, alpha = 0.7, show.legend = FALSE) +
    scale_fill_viridis_d() +
    
    # Add Mean Line
    # Need to make sure x is numeric for line plot
    geom_point(data = layer_stats, aes(y = mean_vaccel, group = 1), color = "red", size = 2) +
    geom_line(data = layer_stats, aes(y = mean_vaccel, group = 1), color = "red", linewidth = 1) +
    
    scale_y_continuous(limits = c(0, quantile(df_turb$v_accel, 0.95, na.rm=TRUE) * 1.5)) + # Crop extreme outliers for view
    
    labs(
      title = "Turbulence Analysis: Vertical Instability",
      subtitle = "Distribution of Vertical Acceleration by Altitude",
      x = "Altitude Layer (ft)",
      y = "Vertical Acceleration (|Delta Baro Rate|)"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
  ggsave(file.path(out_dir, "3_turbulence_analysis.png"), p, width = 10, height = 6, dpi = DPI)
}

plot_spatial_heatmap <- function(df, out_dir) {
  # Clean and Calculate Anomalies
  df_clean <- df %>% filter(!is.na(alt), !is.na(d_value), !is.na(lat), !is.na(lon))
  df_clean <- calculate_flight_anomalies(df_clean)
  df_clean <- df_clean %>% filter(!is.na(z_score))
  
  if (nrow(df_clean) == 0) return()
  
  # Prepare data for faceting
  df_plot <- df_clean %>%
    mutate(layer = if_else(alt < 8000, "Low Altitude (< 8k ft)", "High Altitude (> 8k ft)"))
  
  # Create ggplot Map
  p <- ggplot(df_plot, aes(x = lon, y = lat, color = z_score)) +
    geom_point(alpha = 0.6, size = 1.5) +
    scale_color_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, limits = c(-2.5, 2.5), oob = scales::squish) +
    facet_wrap(~ layer, ncol = 2) +
    coord_quickmap() +
    labs(
      title = "Spatial Pressure Anomaly Map",
      subtitle = "D-Value Z-Score Distribution",
      x = "Longitude",
      y = "Latitude",
      color = "Z-Score",
      caption = "Blue: Low Pressure (Valley/Trough), Red: High Pressure (Ridge)"
    )
    
  ggsave(file.path(out_dir, "4_spatial_anomaly_map.png"), p, width = 12, height = 6, dpi = DPI)
}

# -----------------------------------------------------------------------------
# Main Execution
# -----------------------------------------------------------------------------

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  
  # Parse Args (Minimal)
  target_date <- NULL
  lookback_hours <- 24
  base_path <- getwd()
  
  # Simple parser
  if (length(args) > 0) {
    for (i in seq_along(args)) {
      if (args[i] == "--date" && i < length(args)) target_date <- args[i+1]
      if (args[i] == "--hours" && i < length(args)) lookback_hours <- as.integer(args[i+1])
      if (args[i] == "--base_path" && i < length(args)) base_path <- args[i+1]
    }
  }
  
  # Setup paths
  # Assuming script is in scripts/, so base is one level up if not provided
  if (base_path == getwd() && basename(getwd()) == "scripts") {
    base_path <- dirname(getwd())
  }
  
  report_dir <- file.path(base_path, "reports", "terrain_study")
  if (!dir.exists(report_dir)) dir.create(report_dir, recursive = TRUE)
  
  # Run Analysis
  message("=== Sky-Phys Terrain Analysis (R Version) ===")
  df <- load_and_process(base_path, target_date, lookback_hours)
  
  if (!is.null(df)) {
    # Filter ROI
    df_roi <- filter_roi(df, ROI)
    
    if (nrow(df_roi) > 10) {
      message("Creating plots...")
      
      tryCatch(plot_vertical_profile(df_roi, report_dir), error = function(e) message("Error in profile:", e))
      tryCatch(plot_d_value_anomaly(df_roi, report_dir), error = function(e) message("Error in anomaly:", e))
      tryCatch(plot_turbulence_analysis(df_roi, report_dir), error = function(e) message("Error in turbulence:", e))
      tryCatch(plot_spatial_heatmap(df_roi, report_dir), error = function(e) message("Error in heatmap:", e))
      
      message(sprintf("Analysis complete. Reports saved to: %s", report_dir))
    } else {
      message("Not enough data in ROI for analysis.")
    }
  }
}

if (!interactive()) {
  main()
}
