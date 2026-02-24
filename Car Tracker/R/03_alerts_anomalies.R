# R/03_alerts_anomalies.R
telemetry <- readRDS("outputs/telemetry_clean.rds")

# Ensure timestamp is POSIXct
telemetry$timestamp <- as.POSIXct(telemetry$timestamp, tz = "UTC")

# Start with an EMPTY alerts table
alerts <- data.frame(
  timestamp = as.POSIXct(character(), tz = "UTC"),
  alert = character(),
  severity = character(),
  stringsAsFactors = FALSE
)

add_alert <- function(i, msg, sev) {
  alerts <<- rbind(
    alerts,
    data.frame(
      timestamp = telemetry$timestamp[i],
      alert = msg,
      severity = sev,
      stringsAsFactors = FALSE
    )
  )
}

for (i in seq_len(nrow(telemetry))) {
  if (!is.na(telemetry$engine_temp[i]) && telemetry$engine_temp[i] >= 115) {
    add_alert(i, "Engine temperature critical", "CRITICAL")
  }
  if (!is.na(telemetry$fuel_level[i]) && telemetry$fuel_level[i] <= 8) {
    add_alert(i, "Fuel level low", "WARN")
  }
  
  tyres <- c(telemetry$tyre_fl[i], telemetry$tyre_fr[i], telemetry$tyre_rl[i], telemetry$tyre_rr[i])
  if (any(!is.na(tyres)) && any(tyres <= 24, na.rm = TRUE)) {
    add_alert(i, "Tyre pressure low", "WARN")
  }
}

# Simple anomaly score (z-score) for engine_temp and rpm
zscore <- function(x) {
  s <- sd(x, na.rm = TRUE)
  if (is.na(s) || s == 0) return(rep(0, length(x)))
  (x - mean(x, na.rm = TRUE)) / s
}

telemetry$z_engine_temp <- zscore(telemetry$engine_temp)
telemetry$z_rpm <- zscore(telemetry$rpm)
telemetry$anomaly <- abs(telemetry$z_engine_temp) > 3 | abs(telemetry$z_rpm) > 3

write.csv(alerts, "outputs/alerts.csv", row.names = FALSE)
saveRDS(telemetry, "outputs/telemetry_scored.rds")

message("Saved outputs/alerts.csv and outputs/telemetry_scored.rds")