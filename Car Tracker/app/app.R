# app/app.R
library(shiny)
library(dplyr)
library(ggplot2)

# Determine project root (parent folder of /app)
ROOT <- normalizePath(file.path(getwd()), winslash = "/", mustWork = TRUE)
# If we started from /app, go up
if (basename(ROOT) == "app") ROOT <- normalizePath(file.path(ROOT, ".."), winslash = "/", mustWork = TRUE)

message("ROOT: ", ROOT)
message("Telemetry exists at ROOT: ", file.exists(file.path(ROOT, "outputs", "telemetry_scored.rds")))

# Robustly set working directory to project root (parent of /app)
ofile <- sys.frame(1)$ofile
this_file <- if (!is.null(ofile)) normalizePath(ofile, winslash = "/", mustWork = FALSE) else ""

if (nzchar(this_file)) {
  app_folder <- dirname(this_file)
  setwd(dirname(app_folder))
} else {
  # Fallback: if launched with wd already in /app, go up one level
  if (basename(getwd()) == "app") setwd("..")
}

message("Working directory: ", getwd())
message("Telemetry exists: ", file.exists("outputs/telemetry_scored.rds"))

load_data <- function() {
  telemetry_path <- file.path(ROOT, "outputs", "telemetry_scored.rds")
  alerts_path <- file.path(ROOT, "outputs", "alerts.csv")
  
  telemetry <- readRDS(telemetry_path)
  alerts <- read.csv(alerts_path, stringsAsFactors = FALSE)
  alerts$timestamp <- as.POSIXct(alerts$timestamp, tz = "UTC")
  list(telemetry = telemetry, alerts = alerts)
}

fmt_num <- function(x, digits = 1) format(round(x, digits), nsmall = digits)

ui <- fluidPage(
  titlePanel("Vehicle Telemetry Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Filter the data and review alerts/anomalies."),
      
      checkboxInput("show_anom", "Show anomaly points", value = TRUE),
      
      uiOutput("time_range_ui"),
      
      actionButton("reload", "Reload data"),
      br(), br(),
      tags$small("Data source: outputs/telemetry_scored.rds")
    ),
    
    mainPanel(
      fluidRow(
        column(4, wellPanel(
          h4("Latest speed"),
          textOutput("kpi_speed")
        )),
        column(4, wellPanel(
          h4("Latest engine temp"),
          textOutput("kpi_temp")
        )),
        column(4, wellPanel(
          h4("Latest fuel level"),
          textOutput("kpi_fuel")
        ))
      ),
      
      tabsetPanel(
        tabPanel("Plots",
                 plotOutput("plot_speed", height = 250),
                 plotOutput("plot_temp", height = 250),
                 plotOutput("plot_rpm", height = 250)
        ),
        tabPanel("Alerts",
                 h4("Alert log (filtered time range)"),
                 tableOutput("alerts_tbl")
        ),
        tabPanel("Data",
                 h4("Telemetry preview"),
                 tableOutput("data_tbl")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  rv <- reactiveValues(data = NULL)
  
  message("Server wd: ", getwd())
  message("Server telemetry path: ", file.path(ROOT, "outputs", "telemetry_scored.rds"))
  message("Server telemetry exists: ", file.exists(file.path(ROOT, "outputs", "telemetry_scored.rds")))
  
  # initial load
  rv$data <- load_data()
  
  observeEvent(input$reload, {
    rv$data <- load_data()
  })
  
  # dynamic time range UI based on data
  output$time_range_ui <- renderUI({
    req(rv$data$telemetry)
    ts <- rv$data$telemetry$timestamp
    sliderInput(
      "time_range",
      "Time range",
      min = min(ts),
      max = max(ts),
      value = c(min(ts), max(ts)),
      timeFormat = "%Y-%m-%d %H:%M:%S",
      width = "100%"
    )
  })
  
  filtered <- reactive({
    req(rv$data$telemetry, input$time_range)
    df <- rv$data$telemetry %>%
      mutate(timestamp = as.POSIXct(timestamp, tz = "UTC")) %>%
      filter(timestamp >= input$time_range[1], timestamp <= input$time_range[2])
    df
  })
  
  filtered_alerts <- reactive({
    req(rv$data$alerts, input$time_range)
    rv$data$alerts %>%
      filter(timestamp >= input$time_range[1], timestamp <= input$time_range[2]) %>%
      arrange(desc(timestamp))
  })
  
  # KPIs
  output$kpi_speed <- renderText({
    df <- filtered()
    req(nrow(df) > 0)
    paste0(fmt_num(tail(df$speed, 1), 1), " mph")
  })
  
  output$kpi_temp <- renderText({
    df <- filtered()
    req(nrow(df) > 0)
    paste0(fmt_num(tail(df$engine_temp, 1), 1), " °C")
  })
  
  output$kpi_fuel <- renderText({
    df <- filtered()
    req(nrow(df) > 0)
    paste0(fmt_num(tail(df$fuel_level, 1), 1), " %")
  })
  
  # Plots
  output$plot_speed <- renderPlot({
    df <- filtered()
    req(nrow(df) > 1)
    
    ggplot(df, aes(x = timestamp, y = speed)) +
      geom_line() +
      labs(x = NULL, y = "Speed (mph)")
  })
  
  output$plot_temp <- renderPlot({
    df <- filtered()
    req(nrow(df) > 1)
    
    p <- ggplot(df, aes(x = timestamp, y = engine_temp)) +
      geom_line() +
      geom_hline(yintercept = 115, linetype = 2) +
      labs(x = NULL, y = "Engine temp (°C)")
    
    if (isTRUE(input$show_anom) && "anomaly" %in% names(df)) {
      p <- p + geom_point(data = df %>% filter(anomaly), aes(x = timestamp, y = engine_temp))
    }
    p
  })
  
  output$plot_rpm <- renderPlot({
    df <- filtered()
    req(nrow(df) > 1)
    
    p <- ggplot(df, aes(x = timestamp, y = rpm)) +
      geom_line() +
      labs(x = NULL, y = "RPM")
    
    if (isTRUE(input$show_anom) && "anomaly" %in% names(df)) {
      p <- p + geom_point(data = df %>% filter(anomaly), aes(x = timestamp, y = rpm))
    }
    p
  })
  
  # Tables
  output$alerts_tbl <- renderTable({
    a <- filtered_alerts()
    if (nrow(a) == 0) return(data.frame(Message = "No alerts in this time range."))
    head(a, 30)
  }, striped = TRUE)
  
  output$data_tbl <- renderTable({
    df <- filtered()
    head(df, 25)
  }, striped = TRUE)
}

shinyApp(ui, server)
