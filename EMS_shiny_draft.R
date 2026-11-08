# Load and read the file
library(data.table)
library(readr)
va_nfirs_2020_2025 <- read_csv("va_nfirs_2020_2025_373FDs.csv")
View(va_nfirs_2020_2025)

model_daily <- read_csv(
  "model_daily.csv",
  show_col_types = FALSE
) %>%
  mutate(
    FDID = sprintf("%05d", as.integer(FDID)),
    Date = as.Date(Date)
  )

install.packages(c(
  "shiny",
  "bslib",
  "dplyr",
  "ggplot2",
  "readr",
  "leaflet"
))

library(dplyr)
library(ggplot2)
library(lubridate)

model_daily <- model_daily %>%
  mutate(
    FDID = sprintf("%05d", as.integer(FDID)),
    Date = as.Date(Date)
  )

library(shiny)
library(dplyr)
library(ggplot2)
library(readr)


shinyApp(ui, server)

library(glmmTMB)

nb2 <- readRDS("nb_temporal.rds")

class(nb2)
summary(nb2)

library(dplyr)
library(lubridate)
library(glmmTMB)

# Standardize FDIDs
model_daily <- model_daily %>%
  mutate(
    FDID = sprintf("%05d", as.integer(FDID)),
    Date = as.Date(Date)
  )

# Select one department
selected_fd <- "00100"

# Use its available history
fd_data <- model_daily %>%
  filter(FDID == selected_fd) %>%
  arrange(Date)

# Forecast the seven days after the last date
last_date <- max(fd_data$Date)

future_dates <- seq(
  last_date + 1,
  last_date + 7,
  by = "day"
)

# Calculate average from original training period
avg_calls <- fd_data %>%
  filter(
    Date >= as.Date("2020-01-01"),
    Date <= as.Date("2023-12-31"),
    !is.na(Model_Calls)
  ) %>%
  summarise(avg = mean(Model_Calls)) %>%
  pull(avg)

# Construct lagged predictors
lag_values <- fd_data %>%
  select(Date, Model_Calls) %>%
  mutate(Date = Date + 7)

forecast_data <- data.frame(
  FDID = factor(
    selected_fd,
    levels = levels(nb2$frame$FDID)
  ),
  Date = future_dates
) %>%
  left_join(lag_values, by = "Date") %>%
  mutate(
    Day_of_Week = factor(
      wday(Date, week_start = 1),
      levels = 1:7
    ),
    Month = factor(month(Date), levels = 1:12),
    Year_centered = year(Date) - 2023,
    avg_daily_calls = avg_calls,
    lag7_missing = as.integer(is.na(Model_Calls)),
    log_lag7 = log1p(
      coalesce(Model_Calls, avg_daily_calls)
    )
  )

# Generate predicted daily call counts
forecast_data$Predicted_Calls <- predict(
  nb2,
  newdata = forecast_data,
  type = "response",
  re.form = NULL,
  allow.new.levels = FALSE
)

# View results
forecast_data %>%
  select(Date, Predicted_Calls)

## Updated CODEEEEEEE



# ============================================================
# VIRGINIA FIRE/EMS CALL VOLUME FORECASTING DASHBOARD
# STAT 4996 Capstone
# ============================================================

# ------------------------------------------------------------
# 1. LOAD LIBRARIES
# ------------------------------------------------------------

library(shiny)
library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)
library(glmmTMB)
library(leaflet)

# ------------------------------------------------------------
# 2. LOAD DATA AND TRAINED MODEL
# ------------------------------------------------------------

model_daily <- read_csv(
  "model_daily.csv",
  col_types = cols(
    FDID = col_character(),
    .default = col_guess()
  ),
  show_col_types = FALSE
) %>%
  mutate(
    FDID = trimws(FDID),
    Date = as.Date(Date)
  )

nb2 <- readRDS("nb_temporal.rds")

# Department IDs used in model training
model_fd_levels <- levels(nb2$frame$FDID)

# Department names and locations
department_lookup <- read_csv(
  "department_lookup.csv",
  col_types = cols(
    FDID_key = col_character(),
    .default = col_guess()
  ),
  show_col_types = FALSE
) %>%
  mutate(
    FDID_key = trimws(FDID_key)
  ) %>%
  filter(
    FDID_key %in% model_fd_levels,
    !is.na(latitude),
    !is.na(longitude)
  ) %>%
  arrange(department_name)

# Only include departments available in both datasets
department_lookup <- department_lookup %>%
  filter(FDID_key %in% model_daily$FDID)

# Dropdown labels: FDID : Department Name
fd_choices <- setNames(
  department_lookup$FDID_key,
  paste(
    department_lookup$FDID_key,
    department_lookup$department_name,
    sep = " : "
  )
)

# ------------------------------------------------------------
# 3. USER INTERFACE
# ------------------------------------------------------------

ui <- fluidPage(
  
  titlePanel(
    "Virginia Fire/EMS Call Volume Forecasting"
  ),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h4("Select Fire Department"),
      
      selectizeInput(
        inputId = "selected_fd",
        label = "Search by FDID or Department Name:",
        choices = fd_choices,
        selected = fd_choices[1],
        options = list(
          placeholder = "Search FDID or department name"
        )
      ),
      
      hr(),
      
      h4("Selected Department"),
      
      textOutput("selected_department"),
      
      hr(),
      
      h4("About This Dashboard"),
      
      p(
        "Explore historical daily call volumes,
         seven-day forecasts, and fire department
         locations across Virginia."
      ),
      
      p(
        "Forecasts use a temporal negative
         binomial regression model."
      ),
      
      hr(),
      
      h4("Forecast Period"),
      
      textOutput("forecast_period")
      
    ),
    
    mainPanel(
      
      tabsetPanel(
        
        # ----------------------------------------------------
        # HISTORICAL TRENDS
        # ----------------------------------------------------
        
        tabPanel(
          "Historical Trends",
          
          br(),
          
          h3("Historical Daily Call Volume"),
          
          plotOutput(
            "historical_plot",
            height = "450px"
          )
        ),
        
        # ----------------------------------------------------
        # 7-DAY FORECAST
        # ----------------------------------------------------
        
        tabPanel(
          "7-Day Forecast",
          
          br(),
          
          h3("Predicted Call Volume"),
          
          plotOutput(
            "forecast_plot",
            height = "450px"
          ),
          
          h4("Daily Forecast Values"),
          
          tableOutput("forecast_table")
        ),
        
        # ----------------------------------------------------
        # VIRGINIA MAP
        # ----------------------------------------------------
        
        tabPanel(
          "Virginia Map",
          
          br(),
          
          h3("Virginia Fire Department Locations"),
          
          p(
            "Click a fire department marker to select it.
             The historical and forecast graphs will
             update automatically."
          ),
          
          leafletOutput(
            "department_map",
            height = "550px"
          )
        ),
        
        # ----------------------------------------------------
        # MODEL VALIDATION
        # ----------------------------------------------------
        
        tabPanel(
          "Model Validation",
          
          br(),
          
          h3("Model Accuracy"),
          
          p(
            "This section will display validation
             metrics from the negative binomial models."
          ),
          
          h4("Planned Metrics"),
          
          tags$ul(
            tags$li("Mean Absolute Error (MAE)"),
            tags$li("Root Mean Squared Error (RMSE)"),
            tags$li("Bias"),
            tags$li("Weighted Absolute Percentage Error (WAPE)")
          )
        )
        
      )
    )
  )
)

# ------------------------------------------------------------
# 4. SERVER
# ------------------------------------------------------------

server <- function(input, output, session) {
  
  # ----------------------------------------------------------
  # SELECTED DEPARTMENT
  # ----------------------------------------------------------
  
  selected_data <- reactive({
    
    req(input$selected_fd)
    
    model_daily %>%
      filter(FDID == input$selected_fd) %>%
      arrange(Date)
    
  })
  
  output$selected_department <- renderText({
    
    selected <- department_lookup %>%
      filter(FDID_key == input$selected_fd)
    
    req(nrow(selected) > 0)
    
    paste0(
      selected$FDID_key[1],
      " : ",
      selected$department_name[1]
    )
    
  })
  
  # ----------------------------------------------------------
  # HISTORICAL CALL VOLUME GRAPH
  # ----------------------------------------------------------
  
  output$historical_plot <- renderPlot({
    
    historical_data <- selected_data() %>%
      filter(
        reporting_status == "Valid",
        !is.na(Calls)
      )
    
    validate(
      need(
        nrow(historical_data) > 0,
        "No valid historical data available."
      )
    )
    
    ggplot(
      historical_data,
      aes(x = Date, y = Calls)
    ) +
      geom_line(
        color = "steelblue",
        linewidth = 0.7
      ) +
      labs(
        title = paste(
          "Historical Daily Call Volume - FDID",
          input$selected_fd
        ),
        x = "Date",
        y = "Number of Calls"
      ) +
      theme_minimal(base_size = 13)
    
  })
  
  # ----------------------------------------------------------
  # GENERATE 7-DAY FORECAST
  # ----------------------------------------------------------
  
  forecast_data <- reactive({
    
    fd <- input$selected_fd
    
    fd_data <- selected_data()
    
    validate(
      need(
        nrow(fd_data) > 0,
        "No historical data available."
      ),
      need(
        fd %in% model_fd_levels,
        "This department was not included in model training."
      )
    )
    
    # Most recent observed model call date
    observed_data <- fd_data %>%
      filter(!is.na(Model_Calls))
    
    validate(
      need(
        nrow(observed_data) > 0,
        "No observed model call counts available."
      )
    )
    
    last_date <- max(observed_data$Date)
    
    future_dates <- seq(
      last_date + 1,
      last_date + 7,
      by = "day"
    )
    
    # Training-period department average
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
        length(avg_calls) == 1 &&
          is.finite(avg_calls),
        "Training-period average unavailable."
      )
    )
    
    # Date-based seven-day lag
    lag_values <- fd_data %>%
      select(Date, Model_Calls) %>%
      mutate(
        Date = Date + 7
      )
    
    # Future predictor dataframe
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
    
    # Predict expected daily call counts
    future$Predicted_Calls <- as.numeric(
      predict(
        nb2,
        newdata = future,
        type = "response",
        re.form = NULL,
        allow.new.levels = FALSE
      )
    )
    
    # Negative binomial dispersion
    future$Dispersion <- as.numeric(
      predict(
        nb2,
        newdata = future,
        type = "disp",
        re.form = NULL,
        allow.new.levels = FALSE
      )
    )
    
    # Approximate conditional 95% prediction intervals
    future <- future %>%
      mutate(
        Lower = qnbinom(
          0.025,
          mu = Predicted_Calls,
          size = Dispersion
        ),
        
        Upper = qnbinom(
          0.975,
          mu = Predicted_Calls,
          size = Dispersion
        )
      )
    
    future
    
  })
  
  # ----------------------------------------------------------
  # FORECAST GRAPH
  # ----------------------------------------------------------
  
  output$forecast_plot <- renderPlot({
    
    forecast <- forecast_data()
    
    ggplot(
      forecast,
      aes(x = Date, y = Predicted_Calls)
    ) +
      geom_ribbon(
        aes(
          ymin = Lower,
          ymax = Upper
        ),
        fill = "steelblue",
        alpha = 0.2
      ) +
      geom_line(
        color = "steelblue",
        linewidth = 1
      ) +
      geom_point(
        color = "steelblue",
        size = 3
      ) +
      scale_x_date(
        date_breaks = "1 day",
        date_labels = "%b %d"
      ) +
      labs(
        title = paste(
          "7-Day Forecast - FDID",
          input$selected_fd
        ),
        subtitle = paste(
          "Approximate 95% prediction interval |",
          min(forecast$Date),
          "to",
          max(forecast$Date)
        ),
        x = "Date",
        y = "Expected Number of Calls"
      ) +
      theme_minimal(base_size = 13)
    
  })
  
  # ----------------------------------------------------------
  # FORECAST TABLE
  # ----------------------------------------------------------
  
  output$forecast_table <- renderTable({
    
    forecast_data() %>%
      transmute(
        Date,
        `Predicted Calls` = round(
          Predicted_Calls, 2
        ),
        `95% Lower` = Lower,
        `95% Upper` = Upper
      )
    
  })
  
  # ----------------------------------------------------------
  # FORECAST PERIOD
  # ----------------------------------------------------------
  
  output$forecast_period <- renderText({
    
    forecast <- forecast_data()
    
    paste(
      min(forecast$Date),
      "to",
      max(forecast$Date)
    )
    
  })
  
  # ----------------------------------------------------------
  # VIRGINIA MAP
  # ----------------------------------------------------------
  
  output$department_map <- renderLeaflet({
    
    leaflet(department_lookup) %>%
      addTiles(
        urlTemplate = "https://tile.openstreetmap.org/{z}/{x}/{y}.png",
        attribution = '&copy; OpenStreetMap contributors'
      ) %>%
      setView(
        lng = -78.5,
        lat = 37.5,
        zoom = 7
      ) %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        layerId = ~FDID_key,
        label = ~paste(
          FDID_key,
          department_name,
          sep = " : "
        ),
        radius = 6,
        color = "steelblue",
        fillOpacity = 0.8
      )
    
  })
  
  # ----------------------------------------------------------
  # CLICK MAP MARKER TO SELECT DEPARTMENT
  # ----------------------------------------------------------
  
  observeEvent(
    input$department_map_marker_click,
    {
      
      clicked <- input$department_map_marker_click
      
      updateSelectizeInput(
        session,
        inputId = "selected_fd",
        selected = clicked$id
      )
      
    }
  )
  
}

# ------------------------------------------------------------
# 5. RUN DASHBOARD
# ------------------------------------------------------------

shinyApp(
  ui = ui,
  server = server
)
