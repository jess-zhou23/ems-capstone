# ============================================================
# VIRGINIA FIRE & EMS FORECASTING DASHBOARD
# STAT 4996 - THE STAFFING STRATEGISTS
# ============================================================

# Install packages once if needed:
# install.packages(c("shiny", "bslib", "dplyr",
#                    "ggplot2", "plotly", "leaflet",
#                    "readr", "lubridate"))

# make text changes

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

# Load saved predictions once when the app starts
predictions_2024 <- read_csv(
  "predictions_2024.csv",
  col_types = cols(
    fdid = col_character(),
    date = col_date(),
    forecast_origin = col_date(),
    forecast_start = col_date(),
    .default = col_guess()
  )
) %>%
  mutate(fdid = format_fdid(fdid))

# Split predictions by department for quick lookup
predictions_by_fd <- split(
  predictions_2024,
  predictions_2024$fdid
)

# Prepare forecast week choices
forecast_weeks <- sort(unique(predictions_2024$forecast_start))

forecast_week_ends <- pmin(
  forecast_weeks + 6,
  as.Date("2024-12-31")
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
# PLOT STYLING HELPER
# ============================================================

# Apply Inter typography to Plotly charts:
# chart titles 16px, axis labels and ticks 12px
style_plotly <- function(p) {
  p %>%
    layout(
      font = list(
        family = "Inter, -apple-system, Segoe UI, Roboto, sans-serif",
        size = 12,
        color = "#1F2A37"
      ),
      title = list(
        font = list(size = 16, color = "#1B2A41")
      ),
      xaxis = list(
        titlefont = list(size = 12, color = "#5B6675"),
        tickfont = list(size = 12, color = "#5B6675")
      ),
      yaxis = list(
        titlefont = list(size = 12, color = "#5B6675"),
        tickfont = list(size = 12, color = "#5B6675")
      ),
      hoverlabel = list(
        font = list(
          family = "Inter, -apple-system, Segoe UI, Roboto, sans-serif",
          size = 12
        )
      ),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)"
    ) %>%
    config(displaylogo = FALSE)
}

# ============================================================
# 4. USER INTERFACE
# ============================================================

ui <- page_fluid(
  
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#1B2A41",
    secondary = "#A8483F",
    bg = "#FFFFFF",
    fg = "#1F2A37",
    base_font = font_google("Inter", wght = c(400, 500, 600, 700)),
    heading_font = font_google("Inter", wght = c(400, 500, 600, 700)),
    font_scale = 0.875
  ),
  
  tags$head(
    tags$style(HTML("

      /* ---------- Palette ----------
         Navy:      #1B2A41   Navy light: #33475F
         Muted red: #A8483F   Red tint:   #F6E9E7
         Page bg:   #F5F6F8   Border:     #E3E7EC
         Text:      #1F2A37   Muted text: #5B6675
      */

      /* ---------- Base typography ---------- */
      body,
      button, input, select, textarea,
      .selectize-input, .selectize-dropdown,
      .nav-link, .table, .irs, .leaflet-container {
        font-family: 'Inter', -apple-system, 'Segoe UI', Roboto, sans-serif !important;
      }

      body {
        background-color: #F5F6F8;
        color: #1F2A37;
        font-size: 14px;
        line-height: 1.55;
        -webkit-font-smoothing: antialiased;
      }

      p {
        font-size: 14px;
        font-weight: 400;
        color: #5B6675;
      }

      /* ---------- Header ---------- */
      .dashboard-title {
        font-size: 26px;
        font-weight: 700;
        color: #1B2A41;
        letter-spacing: -0.01em;
        margin-top: 24px;
        padding-left: 14px;
        border-left: 5px solid #A8483F;
        line-height: 1.2;
      }

      .dashboard-subtitle {
        font-size: 14px;
        font-weight: 500;
        color: #5B6675;
        margin: 6px 0 22px 0;
        padding-left: 19px;
        letter-spacing: 0.02em;
      }

      /* ---------- Section headings ---------- */
      h4 {
        font-size: 20px;
        font-weight: 600;
        color: #1B2A41;
        letter-spacing: -0.01em;
        margin-bottom: 10px;
      }

      h5 {
        font-size: 13px;
        font-weight: 600;
        color: #5B6675;
        text-transform: uppercase;
        letter-spacing: 0.06em;
        margin-bottom: 8px;
      }

      /* ---------- Cards ---------- */
      .dashboard-card {
        background: #FFFFFF;
        border: 1px solid #E3E7EC;
        border-radius: 10px;
        padding: 22px 24px;
        margin-bottom: 20px;
        box-shadow: 0 1px 2px rgba(27, 42, 65, 0.05);
      }

      .metric-number {
        color: #1B2A41;
        font-size: 32px;
        font-weight: 700;
        line-height: 1.1;
        letter-spacing: -0.02em;
      }

      /* Selected department name in the first summary box */
      #department_name {
        font-size: 16px;
        font-weight: 600;
        color: #1B2A41;
        line-height: 1.35;
      }

      /* Make the three summary boxes the same height */
      .summary-row > [class*='col'] {
        display: flex;
      }

      .summary-row .dashboard-card {
        flex: 1;
        width: 100%;
        border-top: 3px solid #1B2A41;
      }

      /* ---------- Sidebar ---------- */
      .bslib-sidebar-layout > .sidebar {
        background-color: #FFFFFF;
        border-right: 1px solid #E3E7EC;
      }

      .bslib-sidebar-layout > .sidebar h4 {
        font-size: 16px;
        font-weight: 600;
        margin-bottom: 12px;
      }

      .bslib-sidebar-layout > .sidebar p {
        margin-bottom: 6px;
      }

      .bslib-sidebar-layout > .sidebar hr {
        border-color: #E3E7EC;
        opacity: 1;
        margin: 18px 0;
      }

      #forecast_period {
        font-size: 13px;
        font-weight: 500;
        color: #A8483F;
        margin-top: -6px;
      }

      /* ---------- Inputs and filters ---------- */
      .control-label {
        font-size: 13px;
        font-weight: 500;
        color: #33475F;
        margin-bottom: 6px;
      }

      .selectize-input,
      .form-select,
      .form-control {
        font-size: 14px;
        border: 1px solid #D5DBE2;
        border-radius: 8px;
        box-shadow: none;
      }

      .selectize-input.focus,
      .form-select:focus,
      .form-control:focus {
        border-color: #1B2A41;
        box-shadow: 0 0 0 3px rgba(27, 42, 65, 0.12);
      }

      .selectize-dropdown {
        font-size: 14px;
        border-radius: 8px;
      }

      .selectize-dropdown .active {
        background-color: #F6E9E7;
        color: #1B2A41;
      }

      .btn {
        font-weight: 500;
        border-radius: 8px;
      }

      /* Year range slider */
      .irs--shiny .irs-bar {
        background: #1B2A41;
        border-color: #1B2A41;
      }

      .irs--shiny .irs-handle {
        border-color: #1B2A41;
      }

      .irs--shiny .irs-from,
      .irs--shiny .irs-to,
      .irs--shiny .irs-single {
        background-color: #1B2A41;
        font-size: 12px;
        font-weight: 500;
      }

      .irs--shiny .irs-grid-text,
      .irs--shiny .irs-min,
      .irs--shiny .irs-max {
        font-size: 11px;
        color: #5B6675;
      }

      /* ---------- Navigation tabs ---------- */
      .nav-tabs .nav-link {
        font-size: 14px;
        font-weight: 500;
        color: #5B6675;
        border: none;
        border-bottom: 2px solid transparent;
        padding: 10px 16px;
      }

      .nav-tabs .nav-link:hover {
        color: #1B2A41;
      }

      .nav-tabs .nav-link.active {
        color: #1B2A41;
        font-weight: 600;
        background: transparent;
        border-bottom: 2px solid #A8483F;
      }

      /* ---------- Tables ---------- */
      .shiny-table,
      .dashboard-card table {
        font-size: 14px;
        border: 1px solid #E3E7EC;
        border-radius: 8px;
        border-collapse: separate;
        border-spacing: 0;
        overflow: hidden;
        width: 100%;
      }

      .shiny-table thead th,
      .dashboard-card table thead th {
        background-color: #1B2A41;
        color: #FFFFFF;
        font-weight: 600;
        font-size: 13px;
        padding: 10px 14px;
        border: none;
      }

      .shiny-table td,
      .dashboard-card table td {
        padding: 9px 14px;
        border-top: 1px solid #E3E7EC;
        font-variant-numeric: tabular-nums;
      }

      .shiny-table tbody tr:nth-child(even) {
        background-color: #F8F9FB;
      }

      /* ---------- Plotly charts ---------- */
      .js-plotly-plot .gtitle {
        font-weight: 600 !important;
      }

      /* ---------- Map ---------- */
      .leaflet-container {
        border-radius: 8px;
      }

      /* ---------- Disclaimer ---------- */
      .dashboard-disclaimer {
        font-size: 13px;
        color: #5B6675;
        border-left: 3px solid #A8483F;
        background: #FFFFFF;
        padding: 10px 14px;
        border-radius: 6px;
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
      
      
      h4("Forecast Period"),
      
      selectInput(
        inputId = "forecast_start",
        label = "2024 Forecast Week",
        choices = setNames(
          as.character(forecast_weeks),
          paste(
            format(forecast_weeks, "%m/%d/%Y"),
            "to",
            format(forecast_week_ends, "%m/%d/%Y")
          )
        ),
        selected = as.character(forecast_weeks[1])
      ),
      
      textOutput("forecast_period"),
      
      hr(),
      
      h4("About the Project"),
      
      p("Model: Negative Binomial Regression"),
      
      p("Training Period: 2020–2023"),
      
      p("Validation Period: 2024"),
      
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
        class = "summary-row",
        
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
            
            sliderInput(
              inputId = "history_year_range",
              label = "Select Year Range",
              min = min(year(model_daily$date), na.rm = TRUE),
              max = max(year(model_daily$date), na.rm = TRUE),
              value = c(
                min(year(model_daily$date), na.rm = TRUE),
                max(year(model_daily$date), na.rm = TRUE)
              ),
              step = 1,
              sep = ""
            ),
            
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
        class = "dashboard-disclaimer",
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
  # FORECAST PERIOD
  # ----------------------------------------------------------
  
  output$forecast_period <- renderText({
    df <- forecast_data()
    
    req(nrow(df) > 0)
    
    paste(
      format(min(df$date), "%m/%d/%Y"),
      "to",
      format(max(df$date), "%m/%d/%Y")
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
    
    # Display only: clamp lower bound at 0 (+ 0 turns -0 into 0)
    df <- df %>%
      mutate(
        lower = pmax(lower, 0) + 0,
        hover_text = paste0(
          "Date: ", format(date, "%m/%d/%Y"),
          "<br>Predicted Calls: ", sprintf("%.2f", predicted),
          "<br>95% Lower Bound: ", sprintf("%.2f", lower),
          "<br>95% Upper Bound: ", sprintf("%.2f", upper)
        )
      )
    
    p <- ggplot(
      df,
      aes(x = date, y = predicted)
    ) +
      
      geom_ribbon(
        aes(ymin = lower, ymax = upper),
        fill = "#9AA8BA",
        alpha = 0.30
      ) +
      
      geom_line(
        color = "#1B2A41",
        linewidth = 1
      ) +
      
      geom_point(
        aes(text = hover_text),
        color = "#A8483F",
        size = 3
      ) +
      
      scale_x_date(
        date_breaks = "1 day",
        date_labels = "%m/%d/%Y"
      ) +
      
      labs(
        title = "7-Day Call Volume Forecast",
        x = "Date",
        y = "Expected Calls"
      ) +
      
      expand_limits(y = 0) +
      
      theme_minimal(base_size = 13) +
      
      theme(
        plot.title = element_text(hjust = 0.5),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "#EEF1F4")
      )
    
    style_plotly(ggplotly(p, tooltip = "text"))
    
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
        Date = format(date, "%m/%d/%Y"),
        `Expected Calls` = round(predicted, 2),
        `95% Lower Bound` = round(pmax(lower, 0), 2) + 0,
        `95% Upper Bound` = round(upper, 2)
      )
  }, digits = 2)
  
  
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
    
    # Prepare map data
    map_data <- fd_lookup %>%
      filter(
        !is.na(latitude),
        !is.na(longitude),
        latitude >= 36,
        latitude <= 40,
        longitude >= -84,
        longitude <= -75
      )
    
    # Call-volume thresholds (average daily calls):
    # Small = under 2, Medium = 2 to under 10, Large = 10 or more
    volume_breaks <- c(2, 10)
    
    # Marker color and size for each call-volume category
    volume_styles <- data.frame(
      volume_category = c("Small", "Medium", "Large", "No data"),
      marker_color = c("#8CBFD3", "#245B77", "#B85C5C", "#B8BEC6"),
      marker_radius = c(5, 8, 12, 5)
    )
    
    # Classify departments by average daily calls.
    # Departments without call-volume data get their own category.
    map_data <- map_data %>%
      mutate(
        volume_category = case_when(
          is.na(mean_calls) ~ "No data",
          mean_calls < volume_breaks[1] ~ "Small",
          mean_calls < volume_breaks[2] ~ "Medium",
          TRUE ~ "Large"
        )
      ) %>%
      left_join(volume_styles, by = "volume_category") %>%
      mutate(
        map_popup = paste0(
          "<div style='font-family: Inter, sans-serif; ",
          "font-size: 13px; line-height: 1.5; color: #1F2A37;'>",
          "<strong style='font-size: 14px; color: #1B2A41;'>",
          htmltools::htmlEscape(department_name),
          "</strong><br>",
          "FDID: ", fdid, "<br>",
          "Average daily calls: ",
          if_else(
            is.na(mean_calls),
            "N/A",
            sprintf("%.2f", mean_calls)
          ),
          "<br>",
          "Call volume: ", volume_category,
          "</div>"
        )
      ) %>%
      # Draw larger markers first so smaller ones stay on top
      arrange(desc(marker_radius))
    
    # Build legend rows with the actual call-volume ranges
    legend_row <- function(color, radius, label, range_text) {
      paste0(
        "<div style='display: flex; align-items: center; ",
        "margin-top: 6px;'>",
        "<span style='display: inline-flex; justify-content: center; ",
        "align-items: center; width: 26px; margin-right: 8px;'>",
        "<span style='display: inline-block; ",
        "width: ", 2 * radius, "px; height: ", 2 * radius, "px; ",
        "border-radius: 50%; background: ", color, "; ",
        "opacity: 0.85; border: 1px solid #FFFFFF; ",
        "box-shadow: 0 0 0 1px rgba(27, 42, 65, 0.15);'></span>",
        "</span>",
        "<span style='font-weight: 600; color: #1B2A41; ",
        "width: 62px;'>", label, "</span>",
        "<span style='color: #5B6675; ",
        "font-variant-numeric: tabular-nums;'>", range_text, "</span>",
        "</div>"
      )
    }
    
    legend_html <- paste0(
      "<div style='font-family: Inter, sans-serif; font-size: 12px; ",
      "background: #FFFFFF; border: 1px solid #E3E7EC; ",
      "border-radius: 8px; padding: 10px 12px; ",
      "box-shadow: 0 1px 3px rgba(27, 42, 65, 0.10);'>",
      "<div style='font-size: 12px; font-weight: 600; color: #1B2A41; ",
      "margin-bottom: 2px;'>Average Daily Call Volume</div>",
      legend_row(
        "#8CBFD3", 5, "Small",
        paste0("0 &ndash; ", volume_breaks[1])
      ),
      legend_row(
        "#245B77", 8, "Medium",
        paste0(volume_breaks[1], " &ndash; ", volume_breaks[2])
      ),
      legend_row(
        "#B85C5C", 12, "Large",
        paste0(volume_breaks[2], "+")
      ),
      if (any(map_data$volume_category == "No data")) {
        legend_row("#B8BEC6", 5, "No data", "Not available")
      } else {
        ""
      },
      "<div style='margin-top: 6px; font-size: 11px; color: #5B6675;'>",
      "Number of calls per day</div>",
      "</div>"
    )
    
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
          popup = ~map_popup,
          radius = ~marker_radius,
          color = "#FFFFFF",
          fillColor = ~marker_color,
          fillOpacity = 0.75,
          opacity = 0.9,
          stroke = TRUE,
          weight = 1
        ) %>%
        addControl(
          html = legend_html,
          position = "bottomright",
          className = "fd-volume-legend"
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
    req(input$history_year_range)
    
    # Filter historical data by the selected year range
    df <- selected_data() %>%
      filter(
        year(date) >= input$history_year_range[1],
        year(date) <= input$history_year_range[2]
      )
    
    validate(
      need(
        nrow(df) > 0,
        "No historical data in the selected year range."
      )
    )
    
    p <- ggplot(
      df,
      aes(x = date, y = calls_display)
    ) +
      geom_line(
        color = "#1B2A41",
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
        plot.title = element_text(hjust = 0.5),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "#EEF1F4")
      )
    
    style_plotly(ggplotly(p))
  })
  
  
  
  # ----------------------------------------------------------
  # LOAD SAVED 2024 FORECASTS
  # ----------------------------------------------------------
  # ----------------------------------------------------------
  # LOAD SAVED 2024 FORECASTS
  # ----------------------------------------------------------
  
  forecast_data <- reactive({
    req(input$selected_fd, input$forecast_start)
    
    df <- predictions_by_fd[[input$selected_fd]]
    
    validate(
      need(
        !is.null(df) && nrow(df) > 0,
        "No saved predictions for this department."
      )
    )
    
    df <- df %>%
      filter(
        forecast_start == as.Date(input$forecast_start)
      ) %>%
      arrange(date)
    
    validate(
      need(nrow(df) > 0, "No predictions for the selected week.")
    )
    
    df
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