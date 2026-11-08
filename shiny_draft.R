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
# 2. FIRE DEPARTMENT LOOKUP
# ============================================================

# Expected columns:
# fdid, department_name, latitude, longitude

lookup_path <- "data/fd_lookup.csv"

if (file.exists(lookup_path)) {
  
  fd_lookup <- read_csv(
    lookup_path,
    show_col_types = FALSE
  )
  
  names(fd_lookup) <- tolower(names(fd_lookup))
  
  fd_lookup <- fd_lookup %>%
    mutate(fdid = format_fdid(fdid))
  
} else {
  
  fd_lookup <- model_daily %>%
    distinct(fdid) %>%
    mutate(
      department_name = "Unknown Department",
      latitude = NA_real_,
      longitude = NA_real_
    )
  
}

# Only departments in model_daily
fd_lookup <- fd_lookup %>%
  filter(fdid %in% unique(model_daily$fdid)) %>%
  distinct(fdid, .keep_all = TRUE) %>%
  mutate(
    search_label = paste0(
      fdid, " : ", department_name
    )
  ) %>%
  arrange(fdid)

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

server <- function(input, output, session) {
  
  # ----------------------------------------------------------
  # SELECTED DEPARTMENT
  # ----------------------------------------------------------
  
  selected_data <- reactive({
    
    req(input$selected_fd)
    
    model_daily %>%
      filter(fdid == input$selected_fd) %>%
      arrange(date)
    
  })
  
  selected_info <- reactive({
    
    fd_lookup %>%
      filter(fdid == input$selected_fd)
    
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
    
    map <- leaflet() %>%
      addProviderTiles(
        providers$CartoDB.Positron
      ) %>%
      fitBounds(
        lng1 = -83.8,
        lat1 = 36.4,
        lng2 = -75.1,
        lat2 = 39.6
      )
    
    map_data <- fd_lookup %>%
      filter(
        !is.na(latitude),
        !is.na(longitude)
      )
    
    if (nrow(map_data) > 0) {
      
      map <- map %>%
        addCircleMarkers(
          data = map_data,
          lng = ~longitude,
          lat = ~latitude,
          layerId = ~fdid,
          label = ~search_label,
          popup = ~search_label,
          radius = 7,
          color = "#245B77",
          fillOpacity = 0.8
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
    
    p <- ggplot(
      df,
      aes(
        x = date,
        y = calls_display
      )
    ) +
      
      geom_line(
        color = "#245B77",
        linewidth = 0.8,
        na.rm = FALSE
      ) +
      
      labs(
        title = "Historical Daily Call Volume",
        x = "Date",
        y = "Number of Calls"
      ) +
      
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
  # 7-DAY FORECAST PLACEHOLDER
  # ----------------------------------------------------------
  
  # This section does NOT build or run a model.
  #
  # Once your group exports predictions from nb_temporal,
  # they can be loaded here.
  #
  # Suggested CSV structure:
  #
  # fdid,date,predicted,lower,upper
  #
  # 071000,2026-11-08,24.5,17,33
  # 071000,2026-11-09,26.2,19,35
  #
  # Example values above are illustrative ONLY.
  
  forecast_data <- reactive({
    
    path <- "data/forecast_predictions.csv"
    
    if (!file.exists(path)) {
      return(NULL)
    }
    
    df <- read_csv(
      path,
      show_col_types = FALSE
    )
    
    names(df) <- tolower(names(df))
    
    df %>%
      mutate(
        fdid = format_fdid(fdid),
        date = as.Date(date)
      ) %>%
      filter(fdid == input$selected_fd) %>%
      arrange(date)
    
  })
  
  output$forecast_plot <- renderPlotly({
    
    df <- forecast_data()
    
    validate(
      need(
        !is.null(df) && nrow(df) > 0,
        "Forecast predictions have not been connected yet."
      )
    )
    
    p <- ggplot(
      df,
      aes(
        x = date,
        y = predicted
      )
    ) +
      
      geom_ribbon(
        aes(
          ymin = lower,
          ymax = upper
        ),
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
  
  output$forecast_table <- renderTable({
    
    df <- forecast_data()
    
    validate(
      need(
        !is.null(df) && nrow(df) > 0,
        "No forecast data available."
      )
    )
    
    df %>%
      transmute(
        Date = date,
        `Expected Calls` = round(predicted, 2),
        `Lower Bound` = lower,
        `Upper Bound` = upper
      )
    
  })
  
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