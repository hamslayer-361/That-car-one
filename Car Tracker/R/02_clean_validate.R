# R/02_clean_validate.R
telemetry <- read.csv("data/telemetry.csv")
telemetry$timestamp <- as.POSIXct(telemetry$timestamp, tz = "UTC")

dir.create("outputs", showWarnings = FALSE, recursive = TRUE)

# Basic validation rules
rules <- list(
  speed = c(0, 200),
  rpm = c(500, 8000),
  engine_temp = c(-20, 150),
  fuel_level = c(0, 100),
  tyre = c(15, 45)
)

flag_out_of_range <- function(x, lo, hi) {
  which(is.na(x) | x < lo | x > hi)
}

issues <- data.frame(column = character(), row = integer(), value = numeric(), stringsAsFactors = FALSE)

# Check each key column
for (col in c("speed","rpm","engine_temp","fuel_level")) {
  idx <- flag_out_of_range(telemetry[[col]], rules[[col]][1], rules[[col]][2])
  if (length(idx) > 0) {
    issues <- rbind(issues, data.frame(column = col, row = idx, value = telemetry[[col]][idx]))
  }
}

for (col in c("tyre_fl","tyre_fr","tyre_rl","tyre_rr")) {
  idx <- flag_out_of_range(telemetry[[col]], rules$tyre[1], rules$tyre[2])
  if (length(idx) > 0) {
    issues <- rbind(issues, data.frame(column = col, row = idx, value = telemetry[[col]][idx]))
  }
}

write.csv(issues, "outputs/validation_issues.csv", row.names = FALSE)

# Simple imputation for NAs only (keep out-of-range as-is to catch later)
for (col in names(telemetry)) {
  if (is.numeric(telemetry[[col]])) {
    na_idx <- which(is.na(telemetry[[col]]))
    if (length(na_idx) > 0) telemetry[[col]][na_idx] <- median(telemetry[[col]], na.rm = TRUE)
  }
}

saveRDS(telemetry, "outputs/telemetry_clean.rds")
message("Saved outputs/telemetry_clean.rds and outputs/validation_issues.csv")