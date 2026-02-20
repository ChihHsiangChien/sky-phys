
source("scripts/terrain_analysis.R")

# Override main to just load and debug
debug_main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  lookback_hours <- 48
  if (length(args) > 0) lookback_hours <- as.integer(args[1])
  
  base_path <- getwd()
  if (basename(base_path) == "scripts") base_path <- dirname(base_path)
  
  message("=== DEBUGGING DATA COUNTS ===")
  df <- load_and_process(base_path, lookback_hours = lookback_hours)
  
  if (is.null(df)) stop("No data loaded")
  
  message(sprintf("Total Loaded: %d", nrow(df)))
  
  # Filter ROI
  ROI <- list(
    lat_min = 24.7500,
    lat_max = 24.9000,
    lon_min = 121.0200,
    lon_max = 121.2500
  )
  df_roi <- filter_roi(df, ROI)
  message(sprintf("ROI Loaded: %d", nrow(df_roi)))
  
  # Check Low Alt
  df_low <- df_roi %>% filter(alt < 6000)
  message(sprintf("Low Alt (<6000) in ROI: %d", nrow(df_low)))
  
  # Check D-Value availability
  df_low_d <- df_low %>% filter(!is.na(d_value))
  message(sprintf("Low Alt with valid D-Value: %d", nrow(df_low_d)))
  
  if (nrow(df_low) > 0) {
      head(df_low %>% select(alt, alt_geom, d_value)) %>% print()
  }
  
  # Check High Alt
  df_high <- df_roi %>% filter(alt > 20000)
  df_high_d <- df_high %>% filter(!is.na(d_value))
  message(sprintf("High Alt (>20000) in ROI: %d", nrow(df_high)))
  message(sprintf("High Alt with valid D-Value: %d", nrow(df_high_d)))

  # Check column types
  str(df_roi %>% select(alt, alt_geom, d_value))
}

debug_main()
