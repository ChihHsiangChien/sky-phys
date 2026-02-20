#!/usr/bin/env Rscript

# Load necessary libraries
suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(ggplot2)
  library(grid)
  library(scales)
})

# -----------------------------------------------------------------------------
# Constants & Theme
# -----------------------------------------------------------------------------
BASE_WIDTH  <- 10
BASE_HEIGHT <- 10
DPI         <- 300

theme_map_dark <- function() {
  theme_minimal(base_family = "sans", base_size = 12) +
    theme(
      panel.background = element_rect(fill = "#222222", color = NA),
      plot.background = element_rect(fill = "#222222", color = NA),
      panel.grid.major = element_line(color = "#444444", linewidth = 0.3),
      panel.grid.minor = element_line(color = "#333333", linewidth = 0.15),
      text = element_text(color = "white"),
      axis.text = element_text(color = "gray80"),
      plot.title = element_text(face = "bold", size = rel(1.5), hjust = 0.5, color = "white"),
      plot.subtitle = element_text(size = rel(1.1), hjust = 0.5, color = "gray70"),
      plot.caption = element_text(size = rel(0.8), color = "gray50"),
      # Legend Inside (Bottom Right)
      legend.position = c(0.15, 0.85),
      legend.justification = c(0.5, 0.5),
      legend.background = element_rect(fill = alpha("#222222", 0.7), color = NA),
      legend.text = element_text(color = "white"),
      legend.key = element_blank(),
      plot.margin = margin(10, 20, 10, 10) # Add margin to prevent label clipping (Top, Right, Bottom, Left)
    )
}

# -----------------------------------------------------------------------------
# Configuration
# -----------------------------------------------------------------------------

CONFIG <- list(
  # Map Viewport (Northern Taiwan Focus)
  MAP_BOUNDS = list(
    lat_min = 24.7, lat_max = 25.8,
    lon_min = 120.8, lon_max = 122.0
  ),
  
  # Border Data Limits (Safe margins)
  BORDER_LIMITS = list(
    lat_min = 21, lat_max = 27,
    lon_min = 118, lon_max = 124
  ),
  
  # Altitude Layers for Separate Maps
  ALT_LAYERS = list(
    "Low"  = list(min = 0,     max = 10000, color = "#FFFF00", title = "Low Altitude (<10k ft)"),
    "Mid"  = list(min = 10000, max = 25000, color = "#00FF00", title = "Mid Altitude (10k-25k ft)"),
    "High" = list(min = 25000, max = 60000, color = "#00FFFF", title = "High Altitude (>25k ft)")
  )
)

# -----------------------------------------------------------------------------
# Data Loading (Simplified from terrain_analysis.R)
# ... (Calculations) ...
# -----------------------------------------------------------------------------

load_data <- function(base_path, lookback_hours = 24, target_date = NULL) {
  logs_dir <- file.path(base_path, "logs")
  files_to_read <- c()
  
  if (!is.null(target_date)) {
    files_to_read <- paste0("adsb_", target_date, ".csv")
    start_ts <- as.numeric(as_datetime(paste0(target_date, " 00:00:00"), tz="Asia/Taipei"))
    end_ts <- start_ts + 86400
  } else {
    end_dt <- Sys.time()
    start_dt <- end_dt - hours(lookback_hours)
    
    d <- as_date(start_dt, tz="Asia/Taipei")
    end_curr <- as_date(end_dt, tz="Asia/Taipei")
    
    while(d <= end_curr) {
      files_to_read <- c(files_to_read, paste0("adsb_", format(d, "%Y-%m-%d"), ".csv"))
      d <- d + days(1)
    }
    files_to_read <- unique(files_to_read)
    start_ts <- as.numeric(start_dt)
    end_ts <- as.numeric(end_dt)
  }
  
  cols_new <- c('time', 'hex', 'flight', 'lat', 'lon', 'alt', 'alt_geom', 'gs', 'ias', 'tas', 'mach', 
               'track', 'track_rate', 'roll', 'mag_heading', 'true_heading', 'baro_rate', 'geom_rate', 
               'temp', 'wd', 'ws', 'nav_qnh', 'nav_altitude_mcp', 'selected_heading', 'squawk', 
               'rssi', 'messages', 'rc', 'nic_baro', 'nac_p', 'nac_v', 'sil', 'gva', 'sda',
               'category', 'nav_modes', 'version')
               

  
  df_list <- list()
  for (f in files_to_read) {
    fp <- file.path(logs_dir, f)
    if (file.exists(fp)) {
      tryCatch({
         # Read as char to avoid type issues, use standardized cols_new
         tmp <- suppressWarnings(read_csv(fp, col_names = cols_new, col_types = cols(.default = "c"), progress = FALSE))
         df_list[[f]] <- tmp
      }, error = function(e) message(sprintf("Error reading %s: %s", f, e$message)))
    }
  }
  
  if (length(df_list) == 0) return(NULL)
  
  df <- bind_rows(df_list)
  
  # Convert types
  num_cols <- c('time', 'lat', 'lon', 'alt', 'gs', 'track', 'baro_rate', 'rssi', 'alt_geom', 'geom_rate', 'tas', 'mach')
  suppressWarnings({
    for(c in num_cols) if(c %in% names(df)) df[[c]] <- as.numeric(df[[c]])
  })
  
  # Calculate derived physics fields
  if ("alt_geom" %in% names(df)) {
    df <- df %>% mutate(d_value = alt_geom - alt)
  } else {
    df$d_value <- NA
  }
  
  if ("geom_rate" %in% names(df) & "baro_rate" %in% names(df)) {
    df <- df %>% mutate(v_shear = abs(geom_rate - baro_rate))
  } else {
    df$v_shear <- NA
  }
  
  df <- df %>% 
    filter(time >= start_ts, time <= end_ts) %>%
    filter(!is.na(lat), !is.na(lon), !is.na(alt)) %>%
    arrange(hex, time)
    
  return(df)
}

# -----------------------------------------------------------------------------
# Helpers
# -----------------------------------------------------------------------------
# ... (Keep existing helpers) ...

# -----------------------------------------------------------------------------
# Plotters
# -----------------------------------------------------------------------------
# ... (Keep existing plotters) ...

plot_d_value_map <- function(df, out_path, borders, bounds) {
  message("Generating Pressure Anomaly (D-Value) Map...")
  
  # D-Value = True Altitude (GNSS) - Pressure Altitude
  # Positive = High Pressure / Warm Air
  # Negative = Low Pressure / Cold Air
  
  df_d <- df %>%
    filter(!is.na(d_value), abs(d_value) < 3000) %>% # Filter extreme outliers
    group_by(hex) %>%
    mutate(
      time_diff = time - lag(time, default = first(time)),
      is_gap = time_diff > 300,
      segment_id = paste0(hex, "_", cumsum(is_gap))
    ) %>%
    ungroup()
  
  p <- ggplot()
  if (!is.null(borders)) p <- p + geom_polygon(data = borders, aes(x = long, y = lat, group = group), fill = "#333333", color = "#555555", linewidth = 0.2)
  
  p <- p +
    geom_point(data = df_d, aes(x = lon, y = lat, color = d_value), size = 0.5, alpha = 0.4) +
    scale_color_gradient2(
      low = "blue", mid = "white", high = "red", 
      midpoint = 0, limits = c(-1000, 1000), oob = scales::squish,
      name = "D-Value (ft)"
    ) +
    coord_sf(xlim = c(bounds$lon_min, bounds$lon_max), ylim = c(bounds$lat_min, bounds$lat_max), expand = FALSE) +
    theme_map_dark() +
    labs(
      title = "Pressure Anomaly Map (D-Value)", 
      subtitle = "Red = High Pressure/Warm | Blue = Low Pressure/Cold", 
      x=NULL, y=NULL
    )

  ggsave(out_path, p, width = BASE_WIDTH, height = BASE_HEIGHT, dpi = DPI)
  message(sprintf("Saved: %s", out_path))
}

plot_turbulence_map <- function(df, out_path, borders, bounds) {
  message("Generating Turbulence / Vertical Shear Map...")
  
  # Proxy: High Vertical Rate Variance or difference between Baro and Geometric Rate
  # If we use v_shear (abs(geom_rate - baro_rate)), it shows where air mass is moving vertically relative to ground reference?
  # Or simply use abs(baro_rate) to show "Bumpy" areas.
  # Let's use `v_shear` if available (Vertical Wind Shear proxy), otherwise `baro_rate` variance.
  # Actually, simple `abs(baro_rate)` >threshold is a good indicator of activity.
  
  df_turb <- df %>%
    filter(!is.na(baro_rate)) %>%
    mutate(turbulence_intensity = abs(baro_rate)) %>%
    filter(turbulence_intensity > 500) # Only map significant vertical movement
    
  p <- ggplot()
  if (!is.null(borders)) p <- p + geom_polygon(data = borders, aes(x = long, y = lat, group = group), fill = "#333333", color = "#555555", linewidth = 0.2)
  
  p <- p +
    geom_point(data = df_turb, aes(x = lon, y = lat, color = turbulence_intensity), size = 0.5, alpha = 0.4) +
    scale_color_gradientn(
      colors = c("yellow", "orange", "red", "magenta"),
      limits = c(500, 3000), oob = scales::squish,
      name = "|fpm|"
    ) +
    coord_sf(xlim = c(bounds$lon_min, bounds$lon_max), ylim = c(bounds$lat_min, bounds$lat_max), expand = FALSE) +
    theme_map_dark() +
    labs(
      title = "Vertical Activity / Turbulence Map", 
      subtitle = "High Vertical Rates (>500 fpm)", 
      x=NULL, y=NULL
    )

  ggsave(out_path, p, width = BASE_WIDTH, height = BASE_HEIGHT, dpi = DPI)
  message(sprintf("Saved: %s", out_path))
}

plot_energy_map <- function(df, out_path, borders, bounds) {
  message("Generating Specific Energy Map...")
  
  # Specific Energy = h + v^2 / 2g
  g <- 9.80665
  df_e <- df %>%
    filter(!is.na(alt), !is.na(gs)) %>%
    mutate(
      h_m = alt * 0.3048,
      v_ms = gs * 0.514444, # Use Ground Speed for total kinetic energy relative to ground? Or TAS?
      # Physics: Total Energy usually uses TAS. But for ground impact/terrain analysis, maybe GS?
      # Let's use TAS if available, else GS.
      v_true = if("tas" %in% names(.)) tas * 0.514444 else v_ms, 
      specific_energy = h_m + (v_true^2) / (2 * g)
    )
    
  p <- ggplot()
  if (!is.null(borders)) p <- p + geom_polygon(data = borders, aes(x = long, y = lat, group = group), fill = "#333333", color = "#555555", linewidth = 0.2)
  
  p <- p +
    geom_point(data = df_e, aes(x = lon, y = lat, color = specific_energy), size = 0.1, alpha = 0.3) +
    scale_color_viridis_c(option = "inferno", name = "Joules/kg") +
    coord_sf(xlim = c(bounds$lon_min, bounds$lon_max), ylim = c(bounds$lat_min, bounds$lat_max), expand = FALSE) +
    theme_map_dark() +
    labs(
      title = "Specific Energy Distribution", 
      subtitle = "Total Mechanics Energy (Altitude + Speed)", 
      x=NULL, y=NULL
    )

  ggsave(out_path, p, width = BASE_WIDTH, height = BASE_HEIGHT, dpi = DPI)
  message(sprintf("Saved: %s", out_path))
}

# -----------------------------------------------------------------------------
# Main
# -----------------------------------------------------------------------------

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  lookback_hours <- 24
  target_date <- NULL
  
  if (length(args) > 0) {
    for (i in seq_along(args)) {
      if (args[i] == "--hours" && i < length(args)) lookback_hours <- as.integer(args[i+1])
      if (args[i] == "--date" && i < length(args)) target_date <- args[i+1]
    }
  }
  
  base_path <- getwd()
  if (basename(base_path) == "scripts") base_path <- dirname(base_path)
  
  df <- load_data(base_path, lookback_hours, target_date)
  if (is.null(df)) {
    message("No data found.")
    return()
  }
  
  borders <- get_map_borders(CONFIG$BORDER_LIMITS)
  out_dir <- file.path(base_path, "reports", "static_maps")
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  prefix <- if (!is.null(target_date)) target_date else format(Sys.Date(), "%Y-%m-%d")
  
  # Standard Maps
  plot_flow_map(df, file.path(out_dir, paste0("map_flow_combined_", prefix, ".png")), borders, CONFIG$MAP_BOUNDS)
  plot_heatmap(df, file.path(out_dir, paste0("map_heatmap_", prefix, ".png")), borders, CONFIG$MAP_BOUNDS)
  plot_coverage_map(df, file.path(out_dir, paste0("map_coverage_raw_", prefix, ".png")), borders, CONFIG$MAP_BOUNDS)
  
  # Physics Maps
  plot_vertical_map(df, file.path(out_dir, paste0("map_vertical_", prefix, ".png")), borders, CONFIG$MAP_BOUNDS)
  plot_rssi_map(df, file.path(out_dir, paste0("map_rssi_", prefix, ".png")), borders, CONFIG$MAP_BOUNDS)
  plot_speed_map(df, file.path(out_dir, paste0("map_speed_", prefix, ".png")), borders, CONFIG$MAP_BOUNDS)
  plot_category_map(df, file.path(out_dir, paste0("map_category_", prefix, ".png")), borders, CONFIG$MAP_BOUNDS)
  
  # Advanced Physics Maps (NEW)
  plot_d_value_map(df, file.path(out_dir, paste0("map_d_value_", prefix, ".png")), borders, CONFIG$MAP_BOUNDS)
  plot_turbulence_map(df, file.path(out_dir, paste0("map_turbulence_", prefix, ".png")), borders, CONFIG$MAP_BOUNDS)
  plot_energy_map(df, file.path(out_dir, paste0("map_energy_", prefix, ".png")), borders, CONFIG$MAP_BOUNDS)
  
  # Separate Altitude Layers
  for (layer_name in names(CONFIG$ALT_LAYERS)) {
     layer_cfg <- CONFIG$ALT_LAYERS[[layer_name]]
     fname <- paste0("map_layer_", tolower(layer_name), "_", prefix, ".png")
     plot_layer_map(df, file.path(out_dir, fname), borders, CONFIG$MAP_BOUNDS, layer_cfg)
  }
}

# -----------------------------------------------------------------------------
# Helpers
# -----------------------------------------------------------------------------

# Add Map Borders (Dynamic Context)
get_map_borders <- function(limits) {
  tryCatch({
    world <- map_data("world")
    # Filter using user-provided limits to speed up plotting
    world %>% filter(long > limits$lon_min, long < limits$lon_max, 
                     lat > limits$lat_min, lat < limits$lat_max)
  }, error = function(e) {
    return(NULL) # If map_data fails
  })
}

# -----------------------------------------------------------------------------
# Plotters
# -----------------------------------------------------------------------------

plot_flow_map <- function(df, out_path, borders, bounds) {
  message("Generating Combined Altitude Flow Map...")
  
  df_seg <- df %>%
    group_by(hex) %>%
    mutate(
      time_diff = time - lag(time, default = first(time)),
      is_gap = time_diff > 300,
      segment_id = paste0(hex, "_", cumsum(is_gap))
    ) %>%
    ungroup()
  
  # Calculate segment stats
  seg_stats <- df_seg %>%
    group_by(segment_id) %>%
    summarise(
      avg_alt = mean(alt, na.rm=TRUE),
      lat_start = first(lat),
      lat_end = last(lat),
      hex = first(hex),
      count = n(),
      .groups = "drop"
    ) %>%
    filter(count > 2) %>%
    mutate(
      type = case_when(
        avg_alt < 20000 ~ "Low (<20k)",
        lat_end >= lat_start ~ "High Altitude (North)",
        TRUE ~ "High Altitude (South)"
      )
    )
  
  # Join back
  plot_data <- df_seg %>%
    inner_join(seg_stats %>% select(segment_id, type), by = "segment_id") %>%
    arrange(segment_id, time)
  
  # Plot
  p <- ggplot()
  
  if (!is.null(borders)) {
    p <- p + geom_polygon(data = borders, aes(x = long, y = lat, group = group), 
                          fill = "#333333", color = "#555555", linewidth = 0.2)
  }
  
  p <- p +
    geom_path(data = plot_data, 
              aes(x = lon, y = lat, group = segment_id, color = type), 
              alpha = 0.3, linewidth = 0.3) +
    scale_color_manual(values = c(
      "Low (<20k)" = "#FFFF00", 
      "High Altitude (North)" = "#00FFFF", 
      "High Altitude (South)" = "#FF1493"
    )) +
    coord_sf(xlim = c(bounds$lon_min, bounds$lon_max), ylim = c(bounds$lat_min, bounds$lat_max), expand = FALSE) + 
    theme_map_dark() +
    labs(
      title = "Traffic Flow Analysis",
      subtitle = "Combined Altitude",
      x = NULL, y = NULL, color = "Type"
    )
    
  ggsave(out_path, p, width = BASE_WIDTH, height = BASE_HEIGHT, dpi = DPI)
  message(sprintf("Saved: %s", out_path))
  
  # Also generate separate maps for each flow type
  types <- unique(plot_data$type)
  for (t in types) {
    # Sanitize filename
    safe_name <- tolower(gsub("[^a-zA-Z0-9]", "_", t))
    safe_name <- gsub("__+", "_", safe_name) # Remove double underscores
    safe_name <- gsub("_$", "", safe_name)   # Remove trailing underscore
    
    sub_out_path <- gsub("combined", safe_name, out_path)
    
    # Filter data
    sub_data <- plot_data %>% filter(type == t)
    
    p_sub <- ggplot()
    
    if (!is.null(borders)) {
      p_sub <- p_sub + geom_polygon(data = borders, aes(x = long, y = lat, group = group), 
                            fill = "#333333", color = "#555555", linewidth = 0.2)
    }
    
    p_sub <- p_sub +
      geom_path(data = sub_data, 
                aes(x = lon, y = lat, group = segment_id, color = type), 
                alpha = 0.4, linewidth = 0.3) +
      scale_color_manual(values = c(
        "Low (<20k)" = "#FFFF00", 
        "High Altitude (North)" = "#00FFFF", 
        "High Altitude (South)" = "#FF1493"
      )) +
      coord_sf(xlim = c(bounds$lon_min, bounds$lon_max), ylim = c(bounds$lat_min, bounds$lat_max), expand = FALSE) + 
      theme_map_dark() +
      labs(
        title = "Traffic Flow Analysis",
        subtitle = t,
        x = NULL, y = NULL, color = "Type"
      )
      
    ggsave(sub_out_path, p_sub, width = BASE_WIDTH, height = BASE_HEIGHT, dpi = DPI)
    message(sprintf("Saved: %s", sub_out_path))
  }
}

plot_layer_map <- function(df, out_path, borders, bounds, layer_cfg) {
  message(sprintf("Generating Layer Map: %s", layer_cfg$title))
  
  # Filter Data by Altitude
  df_sub <- df %>% filter(alt >= layer_cfg$min, alt < layer_cfg$max)
  
  if (nrow(df_sub) == 0) return()
  
  df_seg <- df_sub %>%
    group_by(hex) %>%
    mutate(
      time_diff = time - lag(time, default = first(time)),
      is_gap = time_diff > 300,
      segment_id = paste0(hex, "_", cumsum(is_gap))
    ) %>%
    ungroup()
  
  # To avoid flying lines connecting filtered points, we must ensure segments are valid
  # Simple grouping by hex/segment is okay as we pre-filtered the whole DF by altitude.
  # But a single flight might dip in and out. 
  # Ideally, we should segment *after* filtering? 
  # No, if we filter first, we might create gaps. 
  # But `geom_path` connects sequential points. If we filter points 2, 3... and keep 1, 4... it connects 1-4.
  # This might draw a line through the excluded altitude. 
  # However, for a static map, this is usually acceptable or we assume the flight is mostly within layer.
  # Better: Split segments if time gap is large (already done).
  
  p <- ggplot()
  
  if (!is.null(borders)) {
    p <- p + geom_polygon(data = borders, aes(x = long, y = lat, group = group), 
                          fill = "#333333", color = "#555555", linewidth = 0.2)
  }
  
  p <- p +
    geom_path(data = df_seg, 
              aes(x = lon, y = lat, group = interaction(hex, segment_id), color = alt), 
              alpha = 0.4, linewidth = 0.3) +
    scale_color_gradient(low = layer_cfg$color, high = "white", guide = "none") + # Simple gradient
    coord_sf(xlim = c(bounds$lon_min, bounds$lon_max), ylim = c(bounds$lat_min, bounds$lat_max), expand = FALSE) + 
    theme_map_dark() +
    labs(
      title = layer_cfg$title,
      subtitle = sprintf("Altitude Range: %.0f - %.0f ft", layer_cfg$min, layer_cfg$max),
      x = NULL, y = NULL
    )
    
  ggsave(out_path, p, width = BASE_WIDTH, height = BASE_HEIGHT, dpi = DPI)
  message(sprintf("Saved: %s", out_path))
}

plot_heatmap <- function(df, out_path, borders, bounds) {
  message("Generating Density Heatmap...")
  
  p <- ggplot()
  
  if (!is.null(borders)) {
    p <- p + geom_polygon(data = borders, aes(x = long, y = lat, group = group), 
                          fill = "#333333", color = "#555555", linewidth = 0.2)
  }
  
  p <- p +
    stat_density_2d(data = df, aes(x = lon, y = lat, fill = after_stat(level), alpha = after_stat(level)), 
                    geom = "polygon", bins = 20) +
    scale_fill_gradientn(
      colors = c("#222222", "blue", "cyan", "green", "yellow", "red"),
      trans = "log10",
      # Use log breaks for the legend to make it readable
      breaks = scales::trans_breaks("log10", function(x) 10^x),
      labels = scales::trans_format("log10", scales::math_format(10^.x))
    ) +
    scale_alpha(range = c(0.1, 0.6), guide = "none") +
    coord_sf(xlim = c(bounds$lon_min, bounds$lon_max), ylim = c(bounds$lat_min, bounds$lat_max), expand = FALSE) +
    theme_map_dark() +
    labs(title = "Traffic Density Heatmap", subtitle = "Concentration of ADS-B points", x=NULL, y=NULL, fill="Density")
  
  ggsave(out_path, p, width = BASE_WIDTH, height = BASE_HEIGHT, dpi = DPI)
  message(sprintf("Saved: %s", out_path))
}

plot_coverage_map <- function(df, out_path, borders, bounds) {
  message("Generating Signal Coverage Map...")
  
  p <- ggplot()
  
  if (!is.null(borders)) {
    p <- p + geom_polygon(data = borders, aes(x = long, y = lat, group = group), 
                          fill = "#333333", color = "#555555", linewidth = 0.2)
  }
  
  # Plot every single point to show coverage
  # Downsample for performance if too large? 
  # But user wants "received signal area", so raw points are best.
  # Use very small points and transparency.
  
  p <- p +
    geom_point(data = df, aes(x = lon, y = lat), 
               color = "#00FF00", size = 0.05, alpha = 0.1) +
    coord_sf(xlim = c(bounds$lon_min, bounds$lon_max), ylim = c(bounds$lat_min, bounds$lat_max), expand = FALSE) +
    theme_map_dark() +
    labs(
      title = "Signal Reception Coverage",
      subtitle = "Raw ADS-B Points Received",
      x = NULL, y = NULL
    )
  
  ggsave(out_path, p, width = BASE_WIDTH, height = BASE_HEIGHT, dpi = DPI)
  message(sprintf("Saved: %s", out_path))
}

plot_vertical_map <- function(df, out_path, borders, bounds) {
  message("Generating Vertical Dynamics Map...")
  
  df_seg <- df %>%
    group_by(hex) %>%
    mutate(
      time_diff = time - lag(time, default = first(time)),
      is_gap = time_diff > 300,
      segment_id = paste0(hex, "_", cumsum(is_gap))
    ) %>%
    ungroup() %>%
    filter(!is.na(baro_rate))
    
  p <- ggplot()
  if (!is.null(borders)) p <- p + geom_polygon(data = borders, aes(x = long, y = lat, group = group), fill = "#333333", color = "#555555", linewidth = 0.2)
  
  p <- p +
    geom_path(data = df_seg, aes(x = lon, y = lat, group = segment_id, color = baro_rate), linewidth = 0.3, alpha = 0.5) +
    scale_color_gradient2(
      low = "#FF0000", mid = "#888888", high = "#00FF00", 
      midpoint = 0, limits = c(-2000, 2000), oob = scales::squish
    ) +
    coord_sf(xlim = c(bounds$lon_min, bounds$lon_max), ylim = c(bounds$lat_min, bounds$lat_max), expand = FALSE) +
    theme_map_dark() +
    labs(title = "Vertical Dynamics", subtitle = "Climb (Green) vs Descent (Red)", x=NULL, y=NULL, color = "fpm")

  ggsave(out_path, p, width = BASE_WIDTH, height = BASE_HEIGHT, dpi = DPI)
  message(sprintf("Saved: %s", out_path))
}

plot_rssi_map <- function(df, out_path, borders, bounds) {
  message("Generating RSSI Signal Strength Map...")
  
  p <- ggplot()
  if (!is.null(borders)) p <- p + geom_polygon(data = borders, aes(x = long, y = lat, group = group), fill = "#333333", color = "#555555", linewidth = 0.2)
  
  p <- p +
    geom_point(data = df, aes(x = lon, y = lat, color = rssi), size = 0.1, alpha = 0.3) +
    scale_color_viridis_c(option = "plasma", direction = 1, name="dBFS") +
    coord_sf(xlim = c(bounds$lon_min, bounds$lon_max), ylim = c(bounds$lat_min, bounds$lat_max), expand = FALSE) +
    theme_map_dark() +
    labs(title = "Signal Strength (RSSI)", subtitle = "Receiver Performance", x=NULL, y=NULL)

  ggsave(out_path, p, width = BASE_WIDTH, height = BASE_HEIGHT, dpi = DPI)
  message(sprintf("Saved: %s", out_path))
}

plot_speed_map <- function(df, out_path, borders, bounds) {
  message("Generating Ground Speed Map...")
  
  df_seg <- df %>%
    group_by(hex) %>%
    mutate(
      time_diff = time - lag(time, default = first(time)),
      is_gap = time_diff > 300,
      segment_id = paste0(hex, "_", cumsum(is_gap))
    ) %>%
    ungroup()
    
  p <- ggplot()
  if (!is.null(borders)) p <- p + geom_polygon(data = borders, aes(x = long, y = lat, group = group), fill = "#333333", color = "#555555", linewidth = 0.2)
  
  p <- p +
    geom_path(data = df_seg, aes(x = lon, y = lat, group = segment_id, color = gs), linewidth = 0.3, alpha = 0.5) +
    scale_color_gradientn(colors = c("blue", "cyan", "yellow", "red", "magenta")) +
    coord_sf(xlim = c(bounds$lon_min, bounds$lon_max), ylim = c(bounds$lat_min, bounds$lat_max), expand = FALSE) +
    theme_map_dark() +
    labs(title = "Ground Speed Analysis", subtitle = "Speed Distribution", x=NULL, y=NULL, color = "Kts")

  ggsave(out_path, p, width = BASE_WIDTH, height = BASE_HEIGHT, dpi = DPI)
  message(sprintf("Saved: %s", out_path))
}

plot_category_map <- function(df, out_path, borders, bounds) {
  message("Generating Aircraft Category Map...")
  
  df_seg <- df %>%
    group_by(hex) %>%
    mutate(
      time_diff = time - lag(time, default = first(time)),
      is_gap = time_diff > 300,
      segment_id = paste0(hex, "_", cumsum(is_gap))
    ) %>%
    ungroup() %>%
    mutate(
      cat_simple = case_when(
        category %in% c("A5", "A4", "B5", "B4", "C5", "C4") ~ "Heavy/Super",
        category %in% c("A3", "B3", "C3") ~ "Large",
        category %in% c("A1", "A2", "B1", "B2", "C1", "C2") ~ "Small/Light",
        category %in% c("A7", "B7") ~ "Rotorcraft",
        TRUE ~ "Other/Unknown"
      )
    )
  
  p <- ggplot()
  if (!is.null(borders)) p <- p + geom_polygon(data = borders, aes(x = long, y = lat, group = group), fill = "#333333", color = "#555555", linewidth = 0.2)
  
  p <- p +
    geom_path(data = df_seg, aes(x = lon, y = lat, group = segment_id, color = cat_simple), linewidth = 0.3, alpha = 0.5) +
    scale_color_manual(values = c(
      "Heavy/Super" = "magenta",
      "Large" = "cyan",
      "Small/Light" = "yellow",
      "Rotorcraft" = "green",
      "Other/Unknown" = "gray30"
    )) +
    coord_sf(xlim = c(bounds$lon_min, bounds$lon_max), ylim = c(bounds$lat_min, bounds$lat_max), expand = FALSE) +
    theme_map_dark() +
    labs(title = "Aircraft Category Distribution", subtitle = "Wake Turbulence Category", x=NULL, y=NULL, color = "Cat")

  ggsave(out_path, p, width = BASE_WIDTH, height = BASE_HEIGHT, dpi = DPI)
  message(sprintf("Saved: %s", out_path))
}


if (!interactive()) main()
