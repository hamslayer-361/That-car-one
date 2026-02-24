# R/01_generate_data.R
dir.create("data", showWarnings = FALSE, recursive = TRUE)
set.seed(42)

n <- 2000
start_time <- as.POSIXct("2026-02-24 08:00:00", tz = "UTC")
timestamp <- start_time + seq(0, by = 1, length.out = n)

speed <- pmax(0, round(rnorm(n, mean = 45, sd = 15), 1))
rpm <- pmax(700, round(speed * 60 + rnorm(n, 0, 300)))
engine_temp <- round(rnorm(n, mean = 92, sd = 6), 1)
fuel_level <- round(pmax(0, 100 - cumsum(pmax(0, rnorm(n, 0.02, 0.02)))), 2)

tyre_fl <- round(rnorm(n, mean = 32, sd = 1.2), 1)
tyre_fr <- round(rnorm(n, mean = 32, sd = 1.2), 1)
tyre_rl <- round(rnorm(n, mean = 33, sd = 1.2), 1)
tyre_rr <- round(rnorm(n, mean = 33, sd = 1.2), 1)

# Inject a few faults/anomalies
engine_temp[sample(1:n, 10)] <- engine_temp[sample(1:n, 10)] + 25
tyre_fl[sample(1:n, 8)] <- 22
fuel_level[(n-50):n] <- pmax(0, fuel_level[(n-50):n] - 0.8)

telemetry <- data.frame(
  timestamp = timestamp,
  speed = speed,
  rpm = rpm,
  engine_temp = engine_temp,
  fuel_level = fuel_level,
  tyre_fl = tyre_fl,
  tyre_fr = tyre_fr,
  tyre_rl = tyre_rl,
  tyre_rr = tyre_rr
)

write.csv(telemetry, "data/telemetry.csv", row.names = FALSE)
message("Generated data/telemetry.csv")