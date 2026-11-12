# ============================================================
# VIRGINIA FIRE & EMS FORECASTING DASHBOARD
# STAT 4996 - THE STAFFING STRATEGISTS
# ============================================================

# Install packages once if needed:
# install.packages(c("shiny", "bslib", "dplyr",
#                    "ggplot2", "plotly", "leaflet",
#                    "readr", "lubridate"))


library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(plotly)
library(leaflet)
library(readr)
library(lubridate)
library(glmmTMB)

# ============================================================
# 1. LOAD DATA
# ============================================================

# Change this path if needed
model_daily <- read_csv(
  "model_daily.csv",
  show_col_types = FALSE
)

# Standardize column names
names(model_daily) <- tolower(names(model_daily))

# Standardize FDIDs
format_fdid <- function(x) {
  x <- trimws(as.character(x))
  valid <- !is.na(x) & grepl("^[0-9]{1,5}$", x)
  x[valid] <- sprintf("%05d", as.integer(x[valid]))
  x
}

model_daily <- model_daily %>%
  mutate(
    fdid = format_fdid(fdid),
    date = as.Date(date)
  )

# Identify cleaned call-volume column
if ("model_calls" %in% names(model_daily)) {
  model_daily$calls_display <- model_daily$model_calls
} else if ("calls_model" %in% names(model_daily)) {
  model_daily$calls_display <- model_daily$calls_model
} else {
  stop("Could not find model_calls or calls_model.")
}

# ============================================================
# LOAD EXISTING NEGATIVE BINOMIAL MODEL
# ============================================================

nb_temporal <- readRDS("nb_temporal.rds")

# Original FDID levels used during model training
model_fd_levels <- levels(nb_temporal$frame$FDID)

# Match formatted dashboard FDIDs to original model IDs
model_fd_lookup <- setNames(
  model_fd_levels,
  format_fdid(model_fd_levels)
)

# ============================================================
# 2. FIRE DEPARTMENT LOOKUP
# ============================================================

# Load coordinate lookup
fd_coords <- read_csv(
  "FDID_coords_lookup.csv",
  col_types = cols(
    FDID = col_character(),
    .default = col_guess()
  )
)

# Load reporting coverage
fd_coverage <- read_csv(
  "all_fd_reporting_coverage.csv",
  col_types = cols(
    FDID = col_character(),
    .default = col_guess()
  )
)

# Standardize FDIDs
fd_coords <- fd_coords %>%
  mutate(
    fdid = format_fdid(FDID),
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude)
  ) %>%
  select(-FDID)

fd_coverage <- fd_coverage %>%
  mutate(fdid = format_fdid(FDID)) %>%
  select(-FDID, -department_name)

# Merge both datasets
fd_lookup <- fd_coords %>%
  left_join(fd_coverage, by = "fdid") %>%
  filter(
    fdid %in% unique(model_daily$fdid)
  ) %>%
  distinct(fdid, .keep_all = TRUE) %>%
  mutate(
    search_label = paste0(
      fdid, " : ", department_name
    )
  ) %>%
  arrange(department_name)



# ============================================================
# 3. DEPARTMENT SUMMARY STATISTICS
# ============================================================

fd_summary <- model_daily %>%
  group_by(fdid) %>%
  summarize(
    mean_calls = if (all(is.na(calls_display))) {
      NA_real_
    } else {
      mean(calls_display, na.rm = TRUE)
    },
    total_calls = sum(calls_display, na.rm = TRUE),
    valid_days = sum(!is.na(calls_display)),
    .groups = "drop"
  )

fd_lookup <- fd_lookup %>%
  left_join(fd_summary, by = "fdid")

# ============================================================
# 4. USER INTERFACE
# ============================================================

ui <- page_fluid(
  
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#245B77"
  ),
  
  tags$head(
    tags$style(HTML("

      body {
        background-color: #F4F7FA;
      }

      .dashboard-title {
        font-size: 30px;
        font-weight: bold;
        color: #17354B;
        margin-top: 20px;
      }

      .dashboard-subtitle {
        color: #667788;
        margin-bottom: 20px;
      }

      .dashboard-card {
        background: white;
        border: 1px solid #DEE5EB;
        border-radius: 12px;
        padding: 20px;
        margin-bottom: 20px;
      }

      .metric-number {
        color: #245B77;
        font-size: 28px;
        font-weight: bold;
      }

    "))
  ),
  
  div(
    class = "dashboard-title",
    "Virginia Fire & EMS Demand Forecasting"
  ),
  
  div(
    class = "dashboard-subtitle",
    "STAT 4996 | The Staffing Strategists"
  ),
  
  layout_sidebar(
    
    sidebar = sidebar(
      
      width = 320,
      
      h4("Fire Department Selection"),
      
      selectizeInput(
        inputId = "selected_fd",
        label = "Search by FDID or Department Name",
        choices = setNames(
          fd_lookup$fdid,
          fd_lookup$search_label
        ),
        selected = fd_lookup$fdid[1],
        options = list(
          placeholder = "Search departments..."
        )
      ),
      
      hr(),
      
      h4("Historical Data Settings"),
      
      selectInput(
        inputId = "history_range",
        label = "Date Range",
        choices = c(
          "Last 30 Days" = 30,
          "Last 90 Days" = 90,
          "Last 365 Days" = 365,
          "All Available Data" = "all"
        ),
        selected = 365
      ),
      
      hr(),
      
      h4("Forecast Period"),
      
      textOutput("forecast_period"),
      
      hr(),
      
      h4("About the Project"),
      
      p("Model: Negative Binomial Regression"),
      
      p("Forecast Horizon: 7 Days"),
      
      p("Geographic Scope: Virginia"),
      
      p("Data Source: NFIRS"),
      
      p("Historical Period: 2020–2025")
    ),
    
    # ========================================================
    # MAIN DASHBOARD
    # ========================================================
    
    div(
      
      # SUMMARY METRICS
      fluidRow(
        
        column(
          width = 4,
          
          div(
            class = "dashboard-card",
            
            h5("Selected Department"),
            
            textOutput("department_name")
          )
        ),
        
        column(
          width = 4,
          
          div(
            class = "dashboard-card",
            
            h5("Average Daily Calls"),
            
            div(
              class = "metric-number",
              textOutput("mean_calls")
            )
          )
        ),
        
        column(
          width = 4,
          
          div(
            class = "dashboard-card",
            
            h5("Valid Reporting Days"),
            
            div(
              class = "metric-number",
              textOutput("valid_days")
            )
          )
        )
      ),
      
      # INTERACTIVE MAP
      div(
        class = "dashboard-card",
        
        h4("Virginia Fire Department Map"),
        
        p(
          "Click a department marker to explore
           historical call volume and forecasts."
        ),
        
        leafletOutput(
          "virginia_map",
          height = 400
        )
      ),
      
      # MAIN TABS
      div(
        class = "dashboard-card",
        
        navset_card_tab(
          
          # -----------------------------------------------
          # HISTORICAL TRENDS
          # -----------------------------------------------
          
          nav_panel(
            "Historical Trends",
            
            br(),
            
            h4("Historical Daily Call Volume"),
            
            plotlyOutput(
              "historical_plot",
              height = 400
            ),
            
            p(
              "Daily call volume from available NFIRS
               reporting data. Suspected missing
               reporting periods are excluded."
            )
          ),
          
          # -----------------------------------------------
          # 7-DAY FORECAST
          # -----------------------------------------------
          
          nav_panel(
            "7-Day Forecast",
            
            br(),
            
            h4("Predicted Daily Call Volume"),
            
            p(
              "This section will display predictions
               generated by the existing negative
               binomial regression model."
            ),
            
            plotlyOutput(
              "forecast_plot",
              height = 400
            ),
            
            h5("Forecast Values"),
            
            tableOutput("forecast_table")
          ),
          
          # -----------------------------------------------
          # MODEL VALIDATION
          # -----------------------------------------------
          
          nav_panel(
            "Model Validation",
            
            br(),
            
            h4("Forecast Accuracy"),
            
            p(
              "This section will display validation
               metrics calculated from the existing
               model."
            ),
            
            tableOutput("validation_table")
          )
        )
      ),
      
      p(
        "Disclaimer: Forecasts are based on historical
         NFIRS reporting and should not independently
         determine operational staffing decisions."
      )
    )
  )
)

# ============================================================
# 5. SERVER
# ============================================================

# Only include departments with valid coordinates
map_data <- fd_lookup %>%
  filter(
    !is.na(latitude),
    !is.na(longitude),
    latitude >= 36,
    latitude <= 40,
    longitude >= -84,
    longitude <= -75
  )

server <- function(input, output, session) {
  
  # ----------------------------------------------------------
  # SELECTED DEPARTMENT
  # ----------------------------------------------------------
  
  model_daily <- model_daily %>%
    arrange(fdid, date)
  
  data_by_fd <- split(model_daily, model_daily$fdid)
  
  selected_data <- reactive({
    req(input$selected_fd)
    
    data_by_fd[[input$selected_fd]]
  })
  
  selected_info <- reactive({
    
    fd_lookup %>%
      filter(fdid == input$selected_fd)
    
  })
  
  # ----------------------------------------------------------
  # FORECAST PERIOD
  # ----------------------------------------------------------
  
  output$forecast_period <- renderText({
    
    forecast <- forecast_data()
    
    if (is.null(forecast) || nrow(forecast) == 0) {
      return("Forecast not yet available")
    }
    
    paste(
      format(min(forecast$date), "%B %d, %Y"),
      "to",
      format(max(forecast$date), "%B %d, %Y")
    )
    
  })
  
  # ----------------------------------------------------------
# FORECAST PLOT
# ----------------------------------------------------------

output$forecast_plot <- renderPlotly({

  df <- forecast_data()

  validate(
    need(nrow(df) > 0, "No forecast data available.")
  )

  p <- ggplot(
    df,
    aes(x = date, y = predicted)
  ) +

    geom_ribbon(
      aes(ymin = lower, ymax = upper),
      fill = "#8CBFD3",
      alpha = 0.35
    ) +

    geom_line(
      color = "#245B77",
      linewidth = 1
    ) +

    geom_point(
      color = "#245B77",
      size = 3
    ) +

    scale_x_date(
      date_breaks = "1 day",
      date_labels = "%b %d"
    ) +

    labs(
      title = "7-Day Call Volume Forecast",
      x = "Date",
      y = "Expected Calls"
    ) +

    expand_limits(y = 0) +

    theme_minimal(base_size = 13) +

    theme(
      plot.title = element_text(
        hjust = 0.5,
        face = "bold"
      )
    )

  ggplotly(p)

})

# ----------------------------------------------------------
# FORECAST TABLE
# ----------------------------------------------------------

output$forecast_table <- renderTable({

  df <- forecast_data()

  validate(
    need(nrow(df) > 0, "No forecast data available.")
  )

  df %>%
    transmute(
      Date = date,
      `Expected Calls` = round(predicted, 2),
      `95% Lower Bound` = lower,
      `95% Upper Bound` = upper
    )

})
  
  
  # ----------------------------------------------------------
  # SUMMARY METRICS
  # ----------------------------------------------------------
  
  output$department_name <- renderText({
    
    info <- selected_info()
    
    paste0(
      info$department_name[1],
      " (",
      info$fdid[1],
      ")"
    )
    
  })
  
  output$mean_calls <- renderText({
    
    x <- selected_data()$calls_display
    
    if (all(is.na(x))) {
      return("N/A")
    }
    
    round(mean(x, na.rm = TRUE), 2)
    
  })
  
  output$valid_days <- renderText({
    
    sum(!is.na(selected_data()$calls_display))
    
  })
  
  # ----------------------------------------------------------
  # INTERACTIVE VIRGINIA MAP
  # ----------------------------------------------------------
  
  output$virginia_map <- renderLeaflet({
    
    # Create Virginia map using OpenStreetMap
    map <- leaflet() %>%
      addTiles(
        urlTemplate = "https://tile.openstreetmap.org/{z}/{x}/{y}.png",
        attribution = '&copy; OpenStreetMap contributors'
      ) %>%
      fitBounds(
        lng1 = -83.8,
        lat1 = 36.4,
        lng2 = -75.1,
        lat2 = 39.6
      )
    
    # Add markers for fire departments
    if (nrow(map_data) > 0) {
      
      map <- map %>%
        addCircleMarkers(
          data = map_data,
          lng = ~longitude,
          lat = ~latitude,
          layerId = ~fdid,
          label = ~search_label,
          popup = ~search_label,
          radius = 6,
          color = "#245B77",
          fillColor = "#245B77",
          fillOpacity = 0.8,
          stroke = TRUE,
          weight = 1
        )
      
    }
    
    map
    
  })
  
  # Clicking a marker updates department selection
  observeEvent(
    input$virginia_map_marker_click,
    {
      
      clicked_id <-
        input$virginia_map_marker_click$id
      
      if (!is.null(clicked_id)) {
        
        updateSelectizeInput(
          session,
          "selected_fd",
          selected = clicked_id
        )
        
      }
      
    }
  )
  
  # Zoom to selected department
  observeEvent(input$selected_fd, {
    
    info <- selected_info()
    
    if (
      nrow(info) > 0 &&
      !is.na(info$latitude[1]) &&
      !is.na(info$longitude[1])
    ) {
      
      leafletProxy("virginia_map") %>%
        setView(
          lng = info$longitude[1],
          lat = info$latitude[1],
          zoom = 9
        )
      
    }
    
  }, ignoreInit = TRUE)
  
  # ----------------------------------------------------------
  # HISTORICAL TREND GRAPH
  # ----------------------------------------------------------
  
  output$historical_plot <- renderPlotly({
    
    df <- selected_data()
    
    if (input$history_range != "all") {
      
      days <- as.numeric(input$history_range)
      
      df <- df %>%
        filter(
          date >= max(date, na.rm = TRUE) - days
        )
    }
    
    plot_ly(
      data = df,
      x = ~date,
      y = ~calls_display,
      type = "scatter",
      mode = "lines",
      connectgaps = FALSE,
      line = list(
        color = "#245B77",
        width = 2
      )
    ) %>%
      layout(
        title = "Historical Daily Call Volume",
        xaxis = list(title = "Date"),
        yaxis = list(title = "Number of Calls")
      )
    
  })
  
  
  
  # ----------------------------------------------------------
  # GENERATE 7-DAY FORECAST USING EXISTING MODEL
  # ----------------------------------------------------------
  
  forecast_data <- reactive({
    
    req(input$selected_fd)
    
    fd <- input$selected_fd
    
    # Convert dashboard FDID to the model's original FDID
    model_fd <- unname(model_fd_lookup[fd])
    
    # Confirm the selected department is in the model
    validate(
      need(
        length(model_fd) == 1 && !is.na(model_fd),
        "This department was not included in model training."
      )
    )
    
    # Get historical data for selected department
    fd_data <- selected_data() %>%
      transmute(
        FDID = fdid,
        Date = date,
        Model_Calls = as.numeric(calls_display)
      ) %>%
      arrange(Date)
    
    # Only use valid observed call counts
    observed_data <- fd_data %>%
      filter(!is.na(Model_Calls))
    
    validate(
      need(
        nrow(observed_data) > 0,
        "No valid historical observations available."
      )
    )
    
    # Last date with valid historical observations
    last_date <- max(observed_data$Date)
    
    # Generate next seven dates
    future_dates <- seq(
      from = last_date + 1,
      by = "day",
      length.out = 7
    )
    
    # Calculate department's training-period average
    avg_calls <- fd_data %>%
      filter(
        Date >= as.Date("2020-01-01"),
        Date <= as.Date("2023-12-31"),
        !is.na(Model_Calls)
      ) %>%
      summarise(
        avg = mean(Model_Calls)
      ) %>%
      pull(avg)
    
    validate(
      need(
        length(avg_calls) == 1 && is.finite(avg_calls),
        "Training-period average unavailable."
      )
    )
    
    # Construct seven-day lag values
    lag_values <- fd_data %>%
      select(Date, Model_Calls) %>%
      mutate(Date = Date + 7)
    
    # Create future predictor data
    future <- data.frame(
      FDID = factor(
        fd,
        levels = model_fd_levels
      ),
      Date = future_dates
    ) %>%
      left_join(
        lag_values,
        by = "Date"
      ) %>%
      mutate(
        Day_of_Week = factor(
          wday(Date, week_start = 1),
          levels = 1:7
        ),
        
        Month = factor(
          month(Date),
          levels = 1:12
        ),
        
        Year_centered = year(Date) - 2023,
        
        avg_daily_calls = avg_calls,
        
        lag7_missing = as.integer(
          is.na(Model_Calls)
        ),
        
        log_lag7 = log1p(
          coalesce(
            Model_Calls,
            avg_daily_calls
          )
        )
      )
    
    # Generate predictions using existing NB model
    future$predicted <- as.numeric(
      predict(
        nb_temporal,
        newdata = future,
        type = "response",
        re.form = NULL,
        allow.new.levels = FALSE
      )
    )
    
    # Retrieve negative binomial dispersion
    future$dispersion <- as.numeric(
      predict(
        nb_temporal,
        newdata = future,
        type = "disp",
        re.form = NULL,
        allow.new.levels = FALSE
      )
    )
    
    # Approximate 95% prediction intervals
    future <- future %>%
      mutate(
        lower = qnbinom(
          0.025,
          mu = predicted,
          size = dispersion
        ),
        
        upper = qnbinom(
          0.975,
          mu = predicted,
          size = dispersion
        )
      )
    
    # Return columns compatible with your existing dashboard
    future %>%
      transmute(
        fdid = as.character(FDID),
        date = Date,
        predicted = predicted,
        lower = lower,
        upper = upper
      )
    
  }) %>%
    bindCache(input$selected_fd)
  
  # ----------------------------------------------------------
  # MODEL VALIDATION PLACEHOLDER
  # ----------------------------------------------------------
  
  output$validation_table <- renderTable({
    
    data.frame(
      Metric = c(
        "Mean Absolute Error",
        "Root Mean Squared Error"
      ),
      Value = c(
        "Not yet connected",
        "Not yet connected"
      )
    )
    
  })
  
}

# ============================================================
# 6. RUN APPLICATION
# ============================================================

shinyApp(
  ui = ui,
  server = server
)