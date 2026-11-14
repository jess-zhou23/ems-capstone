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

# Call-volume thresholds (average daily calls), shared by the map
# and the Model Validation tab:
# Small = under 2, Medium = 2 to under 10, Large = 10 or more
volume_breaks <- c(2, 10)

# ============================================================
# 3b. MODEL VALIDATION (2025 HELD-OUT TEST SET)
# ============================================================

# Load 2025 test predictions and observed calls
test_2025 <- read_csv(
  "full_test_final_2025.csv",
  col_types = cols(
    FDID = col_character(),
    Date = col_date(),
    .default = col_guess()
  )
) %>%
  transmute(
    fdid = format_fdid(FDID),
    date = Date,
    reporting_status,
    predicted = Predicted_calls,
    observed = Observed_calls
  )

# Forecast accuracy metrics (error = predicted - observed)
calc_metrics <- function(predicted, observed) {
  err <- predicted - observed
  data.frame(
    days = length(err),
    mae = mean(abs(err)),
    rmse = sqrt(mean(err^2)),
    bias = mean(err),
    wape = if (sum(observed) > 0) {
      100 * sum(abs(err)) / sum(observed)
    } else {
      NA_real_
    }
  )
}

# Format metrics as a two-column Metric / Value table
format_metrics_long <- function(m) {
  data.frame(
    Metric = c(
      "Days evaluated",
      "Mean Absolute Error (MAE)",
      "Root Mean Squared Error (RMSE)",
      "Bias (predicted - observed)",
      "Weighted Absolute % Error (WAPE)"
    ),
    Value = c(
      format(m$days, big.mark = ","),
      sprintf("%.2f calls", m$mae),
      sprintf("%.2f calls", m$rmse),
      sprintf("%+.2f calls", m$bias),
      if_else(is.na(m$wape), "N/A", sprintf("%.2f%%", m$wape))
    ),
    check.names = FALSE
  )
}

# Split test data by department for quick lookup
test_by_fd <- split(test_2025, test_2025$fdid)

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
# TEAM HEADSHOTS
# ============================================================

# To add headshots, create a folder "www/headshots" next to this
# file and save each photo with the file name below. Photos appear
# automatically; until then, a placeholder with initials is shown.
team_members <- data.frame(
  name = c("Jake Luo", "Kenzie Alber-Ho", "Jess Zhou", "Claire Arnett"),
  initials = c("JL", "KA", "JZ", "CA"),
  photo = c(
    "jake_luo.jpg",
    "kenzie_alber_ho.jpg",
    "jess_zhou.jpg",
    "claire_arnett.jpg"
  )
)

headshot_dir <- file.path("www", "headshots")

if (dir.exists(headshot_dir)) {
  addResourcePath("headshots", headshot_dir)
}

team_card <- function(name, initials, photo) {
  div(
    class = "team-member",
    if (file.exists(file.path(headshot_dir, photo))) {
      tags$img(
        src = paste0("headshots/", photo),
        alt = name,
        class = "team-photo"
      )
    } else {
      div(class = "team-photo team-placeholder", initials)
    },
    div(class = "team-name", name)
  )
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

      /* Selected department name in the first summary box.
         Long names wrap to at most two lines, then end in an ellipsis */
      #department_name {
        font-size: 18px;
        font-weight: 600;
        color: #1B2A41;
        line-height: 1.3;
        display: -webkit-box;
        -webkit-line-clamp: 2;
        -webkit-box-orient: vertical;
        overflow: hidden;
        word-break: break-word;
      }

      /* Three uniform summary boxes: equal width (column width = 4),
         fixed equal height, content vertically centered */
      .summary-row > [class*='col'] {
        display: flex;
      }

      .summary-row .dashboard-card {
        flex: 1;
        width: 100%;
        height: 128px;
        display: flex;
        flex-direction: column;
        justify-content: center;
        padding: 18px 22px;
        overflow: hidden;
        border-top: 3px solid #1B2A41;
      }

      .summary-row .dashboard-card h5 {
        margin-bottom: 8px;
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

      /* Reset Map View button (top-right of map) */
      .leaflet-bar.easy-button-container {
        margin-right: 16px !important;
        border: 1px solid #D5DBE2;
        border-radius: 6px;
        box-shadow: 0 1px 3px rgba(27, 42, 65, 0.10);
        overflow: hidden;
      }

      .leaflet-bar button.easy-button-button {
        width: auto !important;
        min-width: 120px;
        height: 30px !important;
        padding: 0 12px;
        margin: 0;
        border: none;
        border-radius: 0;
        background-color: #FFFFFF;
        color: #1B2A41;
        font-family: 'Inter', -apple-system, 'Segoe UI', Roboto, sans-serif;
        font-size: 12px;
        font-weight: 600;
        white-space: nowrap;
        cursor: pointer;
      }

      .leaflet-bar button.easy-button-button:hover {
        background-color: #F6E9E7;
        color: #A8483F;
      }

      .leaflet-bar button.easy-button-button .button-state {
        line-height: 30px !important;
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

      /* ---------- About the Project navigation ---------- */
      .about-nav {
        display: flex;
        flex-direction: column;
        gap: 6px;
      }

      .about-nav-btn {
        width: 100%;
        display: flex;
        align-items: center;
        gap: 10px;
        text-align: left;
        background-color: #FFFFFF;
        border: 1px solid #E3E7EC;
        border-radius: 8px;
        color: #33475F;
        font-size: 14px;
        font-weight: 500;
        padding: 9px 12px;
        box-shadow: none;
      }

      .about-nav-btn .fa,
      .about-nav-btn svg {
        width: 16px;
        color: #5B6675;
        fill: #5B6675;
      }

      .about-nav-btn:hover {
        background-color: #F5F6F8;
        border-color: #D5DBE2;
        color: #1B2A41;
      }

      .about-nav-btn.active,
      .about-nav-btn.active:hover {
        background-color: #1B2A41;
        border-color: #1B2A41;
        color: #FFFFFF;
      }

      .about-nav-btn.active .fa,
      .about-nav-btn.active svg {
        color: #FFFFFF;
        fill: #FFFFFF;
      }

      .about-nav-btn:focus {
        box-shadow: 0 0 0 3px rgba(27, 42, 65, 0.15);
      }

      .back-btn {
        background-color: #FFFFFF;
        border: 1px solid #D5DBE2;
        color: #1B2A41;
        font-size: 13px;
        font-weight: 500;
        padding: 6px 12px;
        margin-bottom: 14px;
      }

      .back-btn:hover {
        background-color: #F6E9E7;
        border-color: #A8483F;
        color: #A8483F;
      }

      /* ---------- Information pages ---------- */
      .info-page {
        padding: 28px 32px;
      }

      .info-page-title {
        display: flex;
        align-items: center;
        gap: 10px;
        padding-bottom: 14px;
        margin-bottom: 22px;
        border-bottom: 1px solid #E3E7EC;
      }

      .info-page-title .fa,
      .info-page-title svg {
        color: #A8483F;
        fill: #A8483F;
      }

      .info-section {
        max-width: 860px;
        margin-bottom: 24px;
      }

      .info-heading {
        font-size: 16px;
        font-weight: 600;
        color: #1B2A41;
        margin-bottom: 8px;
      }

      .info-section p {
        color: #33475F;
        line-height: 1.7;
        margin-bottom: 0;
      }

      .limitations-box {
        display: flex;
        gap: 14px;
        max-width: 860px;
        background-color: #F2F5F8;
        border: 1px solid #E3E7EC;
        border-left: 4px solid #1B2A41;
        border-radius: 8px;
        padding: 16px 20px;
        margin-bottom: 24px;
      }

      .limitations-box .fa,
      .limitations-box svg {
        color: #1B2A41;
        fill: #1B2A41;
        margin-top: 3px;
      }

      .limitations-box p {
        color: #33475F;
        line-height: 1.7;
        margin-bottom: 0;
      }

      .model-table {
        max-width: 560px;
      }

      .model-table td:first-child {
        font-weight: 600;
        color: #1B2A41;
        width: 42%;
      }

      /* ---------- Team ---------- */
      .team-grid {
        display: grid;
        grid-template-columns: repeat(4, 1fr);
        gap: 24px;
        max-width: 860px;
        margin: 8px 0 8px 0;
      }

      .team-member {
        text-align: center;
      }

      .team-photo {
        width: 120px;
        height: 120px;
        border-radius: 50%;
        object-fit: cover;
        margin: 0 auto 12px auto;
        display: block;
        border: 3px solid #FFFFFF;
        box-shadow: 0 0 0 1px #E3E7EC;
      }

      .team-placeholder {
        display: flex;
        align-items: center;
        justify-content: center;
        background-color: #E8EDF2;
        color: #1B2A41;
        font-size: 28px;
        font-weight: 600;
      }

      .team-name {
        font-size: 15px;
        font-weight: 600;
        color: #1B2A41;
      }

      .team-footer {
        max-width: 860px;
        text-align: center;
        font-size: 13px;
        color: #5B6675;
        border-top: 1px solid #E3E7EC;
        padding-top: 16px;
        margin-top: 28px;
        line-height: 1.7;
      }

      @media (max-width: 900px) {
        .team-grid {
          grid-template-columns: repeat(2, 1fr);
        }
      }

      @media (max-width: 520px) {
        .team-grid {
          grid-template-columns: 1fr;
        }
      }

      /* ---------- How to Use This Dashboard ---------- */
      details.how-to {
        background: #FFFFFF;
        border: 1px solid #E3E7EC;
        border-radius: 10px;
        padding: 14px 22px;
        margin-bottom: 20px;
        box-shadow: 0 1px 2px rgba(27, 42, 65, 0.05);
      }

      details.how-to summary {
        display: flex;
        align-items: center;
        gap: 8px;
        cursor: pointer;
        list-style: none;
        font-size: 16px;
        font-weight: 600;
        color: #1B2A41;
      }

      details.how-to summary::-webkit-details-marker {
        display: none;
      }

      details.how-to summary .how-to-icon {
        color: #A8483F;
      }

      details.how-to summary .how-to-chevron {
        margin-left: auto;
        color: #5B6675;
        transition: transform 0.2s ease;
      }

      details.how-to[open] summary .how-to-chevron {
        transform: rotate(180deg);
      }

      .how-to-grid {
        display: grid;
        grid-template-columns: 1fr 1fr;
        gap: 12px 32px;
        margin-top: 14px;
      }

      .how-to-item strong {
        display: block;
        font-size: 14px;
        font-weight: 600;
        color: #1B2A41;
        margin-bottom: 2px;
      }

      .how-to-item p {
        font-size: 13px;
        line-height: 1.55;
        margin: 0;
      }

      @media (max-width: 768px) {
        .how-to-grid {
          grid-template-columns: 1fr;
        }
      }

    ")),
    
    # Highlight the active About the Project tab
    tags$script(HTML("
      Shiny.addCustomMessageHandler('setActiveNav', function(view) {
        document.querySelectorAll('.about-nav-btn').forEach(function(btn) {
          btn.classList.toggle('active', btn.getAttribute('data-view') === view);
        });
      });
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
      
      div(
        class = "about-nav",
        
        actionButton(
          inputId = "nav_overview",
          label = "Project Overview",
          icon = icon("circle-info"),
          class = "about-nav-btn",
          `data-view` = "overview"
        ),
        
        actionButton(
          inputId = "nav_methods",
          label = "Data & Methodology",
          icon = icon("database"),
          class = "about-nav-btn",
          `data-view` = "methods"
        ),
        
        actionButton(
          inputId = "nav_team",
          label = "Our Team",
          icon = icon("users"),
          class = "about-nav-btn",
          `data-view` = "team"
        )
      )
    ),
    
    # ========================================================
    # MAIN CONTENT
    # ========================================================
    
    div(
      
      # Back button (only shown on About the Project pages)
      conditionalPanel(
        condition = "input.main_view && input.main_view !== 'dashboard'",
        actionButton(
          inputId = "nav_dashboard",
          label = "Back to Forecast Dashboard",
          icon = icon("arrow-left"),
          class = "back-btn"
        )
      ),
      
      navset_hidden(
        id = "main_view",
        
        # ------------------------------------------------------
        # FORECAST DASHBOARD
        # ------------------------------------------------------
        
        nav_panel_hidden(
          value = "dashboard",
          
          # HOW TO USE THIS DASHBOARD (collapsible)
          tags$details(
            class = "how-to",
            open = NA,
            
            tags$summary(
              icon("circle-info", class = "how-to-icon"),
              span("How to Use This Dashboard"),
              icon("chevron-down", class = "how-to-chevron")
            ),
            
            div(
              class = "how-to-grid",
              
              div(
                class = "how-to-item",
                tags$strong("Selecting a Department"),
                p(
                  "Use the search bar to find a Virginia fire department
                   by its name or FDID, or click a department marker
                   directly on the interactive map."
                )
              ),
              
              div(
                class = "how-to-item",
                tags$strong("Navigating the Map"),
                p(
                  "Explore the geographic distribution of departments
                   using the map's zoom and pan controls. Marker size and
                   color indicate average daily call volume, with larger
                   markers representing departments with higher reported
                   call volumes. Use the map legend to interpret these
                   differences, and the reset button to return to the
                   statewide view."
                )
              ),
              
              div(
                class = "how-to-item",
                tags$strong("Choosing a Forecast Period"),
                p(
                  "Select a forecast week to view the predicted daily call
                   volume for the chosen department over a seven-day
                   period."
                )
              ),
              
              div(
                class = "how-to-item",
                tags$strong("Interpreting the Results"),
                p(
                  "Use the historical trends and seven-day forecast
                   visualizations to compare past call activity with
                   predicted demand. The forecast includes upper and lower
                   prediction bounds to illustrate uncertainty, and the
                   forecast table provides the corresponding numerical
                   values."
                )
              )
            )
          ),
    
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
              "Accuracy of the temporal negative binomial model on
               held-out 2025 test data, which the model never saw
               during training (2020-2023) or validation (2024).
               Only days with valid reporting are evaluated;
               suspected missing reporting periods are excluded."
            ),
            
            h5("Selected Department (2025)"),
            
            tableOutput("validation_fd_table"),
            
            br(),
            
            plotlyOutput(
              "validation_plot",
              height = 380
            ),
            
            br(),
            
            h4("Interpretation"),
            
            uiOutput("validation_interpretation")
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
        ),
        
        # ------------------------------------------------------
        # PROJECT OVERVIEW
        # ------------------------------------------------------
        
        nav_panel_hidden(
          value = "overview",
          
          div(
            class = "dashboard-card info-page",
            
            h4(
              class = "info-page-title",
              icon("circle-info"),
              "Project Overview"
            ),
            
            div(
              class = "info-section",
              tags$h3(class = "info-heading", "Project Background"),
              p(
                "This dashboard uses historical fire and emergency
                 medical services (EMS) incident data to forecast daily
                 call volume for fire departments across Virginia. The
                 goal of the dashboard is to help departments better
                 anticipate call demand and support decisions related to
                 staffing, resource allocation, and overall operational
                 planning."
              )
            ),
            
            div(
              class = "info-section",
              tags$h3(class = "info-heading", "Project Objectives"),
              p(
                "Through this project, we aim to make historical fire and
                 EMS incident data more accessible and useful for Virginia
                 fire departments. By analyzing historical call volume
                 patterns and developing statistical forecasts, the
                 dashboard allows departments to better understand past
                 demand and anticipate future call activity. These
                 insights are intended to provide additional information
                 that can support decisions about staffing, resource
                 allocation, and long-term operational planning."
              )
            ),
            
            div(
              class = "info-section",
              tags$h3(class = "info-heading", "Intended Use"),
              p(
                "The forecasting results are intended to support the
                 development of a free, accessible tool for fire
                 departments to anticipate future demand, allocate
                 appropriate personnel and resources, and make more
                 informed decisions about staffing and long-term
                 department planning. These forecasts should be used
                 alongside departmental knowledge, local conditions, and
                 other available information rather than as the sole
                 basis for operational decisions."
              )
            )
          )
        ),
        
        # ------------------------------------------------------
        # DATA & METHODOLOGY
        # ------------------------------------------------------
        
        nav_panel_hidden(
          value = "methods",
          
          div(
            class = "dashboard-card info-page",
            
            h4(
              class = "info-page-title",
              icon("database"),
              "Data & Methodology"
            ),
            
            div(
              class = "info-section",
              tags$h3(class = "info-heading", "Data Source"),
              p(
                "The data comes from the U.S. Fire Administration's
                 (USFA) National Fire Incident Reporting System (NFIRS),
                 specifically the NFIRS Basic Incident dataset. This
                 dataset contains records of fire, emergency medical
                 services (EMS), hazardous materials, rescues, and other
                 service-related incidents. Our analysis focuses on
                 available incident data reported by Virginia fire
                 departments from 2020 to 2025."
              )
            ),
            
            div(
              class = "limitations-box",
              icon("circle-info"),
              div(
                tags$h3(
                  class = "info-heading",
                  "Important Data Limitations"
                ),
                p(
                  "NFIRS is a voluntary reporting system, meaning U.S.
                   fire departments are not required to submit complete
                   incident records at the national level. As a result of
                   this reporting structure, some departments may have
                   missing reporting periods, incomplete records, or
                   inconsistencies across years. Therefore, the reported
                   call volume used in the forecasting model may not
                   reflect the total number of incidents a department
                   actually responded to. Forecasts should be interpreted
                   with these limitations in mind, particularly for
                   departments with inconsistent reporting histories."
                )
              )
            ),
            
            div(
              class = "info-section",
              tags$h3(class = "info-heading", "Forecasting Methodology"),
              p(
                "The dashboard uses a negative binomial regression model
                 to predict daily call volume across Virginia fire
                 departments. This modeling approach accounts for
                 variation in incident counts and is appropriate for
                 count data that may exhibit overdispersion. The model is
                 trained using historical incident data from 2020 to 2023
                 and evaluated against incidents reported in 2024.
                 Forecasts are presented in seven-day intervals, with
                 upper and lower prediction bounds to communicate
                 uncertainty in the estimated call volume."
              )
            ),
            
            div(
              class = "info-section",
              tags$h3(class = "info-heading", "Model Information"),
              tags$table(
                class = "table model-table",
                tags$thead(
                  tags$tr(
                    tags$th("Category"),
                    tags$th("Details")
                  )
                ),
                tags$tbody(
                  tags$tr(
                    tags$td("Model"),
                    tags$td("Negative Binomial Regression")
                  ),
                  tags$tr(
                    tags$td("Training Period"),
                    tags$td("2020–2023")
                  ),
                  tags$tr(
                    tags$td("Validation Period"),
                    tags$td("2024")
                  ),
                  tags$tr(
                    tags$td("Forecast Horizon"),
                    tags$td("7 Days")
                  ),
                  tags$tr(
                    tags$td("Geographic Scope"),
                    tags$td("Virginia")
                  ),
                  tags$tr(
                    tags$td("Data Source"),
                    tags$td("NFIRS Basic Incident Dataset")
                  ),
                  tags$tr(
                    tags$td("Historical Data"),
                    tags$td("2020–2025")
                  )
                )
              )
            )
          )
        ),
        
        # ------------------------------------------------------
        # OUR TEAM
        # ------------------------------------------------------
        
        nav_panel_hidden(
          value = "team",
          
          div(
            class = "dashboard-card info-page",
            
            h4(
              class = "info-page-title",
              icon("users"),
              "Our Team"
            ),
            
            div(
              class = "info-section",
              tags$h3(class = "info-heading", "The Staffing Strategists"),
              p(
                "Our team, The Staffing Strategists, consists of four
                 University of Virginia Class of 2027 students pursuing
                 Bachelor of Arts degrees in Applied Statistics: Jake
                 Luo, Kenzie Alber-Ho, Jess Zhou, and Claire Arnett."
              ),
              br(),
              p(
                "As part of our STAT 4996 Capstone project, we developed
                 this dashboard to apply the statistical modeling, data
                 analysis, and data visualization skills we have gained
                 throughout our undergraduate studies to a real-world
                 challenge."
              ),
              br(),
              p(
                "Our goal is to make historical fire and EMS call volume
                 data more accessible and actionable for Virginia fire
                 departments. By combining statistical forecasting with
                 interactive visualizations, we hope to help departments
                 better understand historical trends, anticipate future
                 demand, and make more informed staffing and resource
                 allocation decisions."
              )
            ),
            
            div(
              class = "info-section",
              tags$h3(class = "info-heading", "Team Member Profiles"),
              div(
                class = "team-grid",
                lapply(
                  seq_len(nrow(team_members)),
                  function(i) {
                    team_card(
                      team_members$name[i],
                      team_members$initials[i],
                      team_members$photo[i]
                    )
                  }
                )
              )
            ),
            
            div(
              class = "team-footer",
              "University of Virginia | Class of 2027",
              br(),
              "STAT 4996 Capstone Project | B.A. in Applied Statistics"
            )
          )
        )
      )
    )
  )
)

# ============================================================
# 5. SERVER
# ============================================================

server <- function(input, output, session) {
  
  # ----------------------------------------------------------
  # ABOUT THE PROJECT NAVIGATION
  # ----------------------------------------------------------
  
  show_view <- function(view) {
    nav_select("main_view", selected = view)
    session$sendCustomMessage("setActiveNav", view)
  }
  
  observeEvent(input$nav_overview, show_view("overview"))
  observeEvent(input$nav_methods, show_view("methods"))
  observeEvent(input$nav_team, show_view("team"))
  observeEvent(input$nav_dashboard, show_view("dashboard"))
  
  # Changing the department or forecast week returns to the dashboard
  observeEvent(
    list(input$selected_fd, input$forecast_start),
    show_view("dashboard"),
    ignoreInit = TRUE
  )
  
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
    
    # Call-volume thresholds come from the shared volume_breaks
    # defined in the data section (Small < 2, Medium 2-10, Large 10+)
    
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
    # Zoom limits: 6 (all of Virginia) to 15 (street level)
    map <- leaflet(
      options = leafletOptions(
        minZoom = 6,
        maxZoom = 15,
        maxBoundsViscosity = 1
      )
    ) %>%
      addTiles(
        urlTemplate = "https://tile.openstreetmap.org/{z}/{x}/{y}.png",
        attribution = '&copy; OpenStreetMap contributors'
      ) %>%
      fitBounds(
        lng1 = -83.8,
        lat1 = 36.4,
        lng2 = -75.1,
        lat2 = 39.6
      ) %>%
      # Keep panning within a box around Virginia
      setMaxBounds(
        lng1 = -86.5,
        lat1 = 35.0,
        lng2 = -72.5,
        lat2 = 41.0
      ) %>%
      # Reset button returns to the original statewide view.
      # Runs in the browser, so it never changes the selected
      # department and works after any zoom, pan, or selection.
      addEasyButton(
        easyButton(
          icon = "<span>Reset Map View</span>",
          title = "Reset Map View",
          position = "topright",
          id = "reset-map-view",
          onClick = htmlwidgets::JS(
            "function(btn, map) {
               map.fitBounds([[36.4, -83.8], [39.6, -75.1]]);
             }"
          )
        )
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
  # MODEL VALIDATION (2025 TEST SET)
  # ----------------------------------------------------------
  
  # Selected department's 2025 test data
  selected_test <- reactive({
    req(input$selected_fd)
    
    df <- test_by_fd[[input$selected_fd]]
    
    validate(
      need(
        !is.null(df) && nrow(df) > 0,
        "No 2025 test data for this department."
      )
    )
    
    df %>% arrange(date)
  })
  
  # Selected department metrics (shared by table and interpretation)
  selected_metrics <- reactive({
    
    df <- selected_test() %>%
      filter(
        reporting_status == "Valid",
        !is.na(predicted),
        !is.na(observed)
      )
    
    validate(
      need(
        nrow(df) > 0,
        "No valid 2025 reporting days for this department."
      )
    )
    
    list(
      df = df,
      m = calc_metrics(df$predicted, df$observed)
    )
  })
  
  # Selected department metrics table (raw numbers only)
  output$validation_fd_table <- renderTable({
    
    format_metrics_long(selected_metrics()$m)
    
  })
  
  # Interpretation below the graph (raw numbers only)
  output$validation_interpretation <- renderUI({
    
    df <- selected_metrics()$df
    m <- selected_metrics()$m
    
    # Raw quantities used in the interpretations
    avg_obs <- mean(df$observed)
    err <- df$predicted - df$observed
    total_days <- nrow(selected_test())
    excluded_days <- total_days - m$days
    total_abs_err <- sum(abs(err))
    total_obs <- sum(df$observed)
    
    # Bold a calculated metric value
    b <- function(x) paste0("<strong>", x, "</strong>")
    
    days_text <- paste0(
      b(m$days), " of the ", total_days, " days in the 2025 test ",
      "data had valid reporting and were used to calculate these ",
      "metrics. ", excluded_days, " days were excluded as suspected ",
      "missing reporting."
    )
    
    mae_text <- paste0(
      "On an average day, the forecast differed from the observed ",
      "call count by ", b(sprintf("%.2f", m$mae)), " calls."
    )
    
    rmse_text <- paste0(
      "The typical daily forecast error was ",
      b(sprintf("%.2f", m$rmse)), " calls when larger misses are ",
      "given extra weight."
    )
    
    bias_text <- paste0(
      "On average, the forecast predicted ",
      b(sprintf("%.2f", abs(m$bias))), " calls per day ",
      if (m$bias >= 0) "more" else "fewer",
      " than were observed."
    )
    
    wape_text <- if (is.na(m$wape)) {
      "Not available because no calls were observed."
    } else {
      paste0(
        "Across 2025, the daily misses added up to ",
        format(round(total_abs_err), big.mark = ","), " calls, ",
        "equal to ", b(sprintf("%.2f%%", m$wape)), " of the ",
        format(round(total_obs), big.mark = ","),
        " total observed calls.",
        if (avg_obs < 2) {
          paste0(
            " Because this department averaged ",
            sprintf("%.2f", avg_obs), " calls per day, misses of ",
            "less than one call produce a large percentage."
          )
        } else {
          ""
        }
      )
    }
    
    tagList(
      p(tags$strong("Days Evaluated: "), HTML(days_text)),
      p(tags$strong("Mean Absolute Error (MAE): "), HTML(mae_text)),
      p(tags$strong("Root Mean Squared Error (RMSE): "), HTML(rmse_text)),
      p(tags$strong("Bias: "), HTML(bias_text)),
      p(
        tags$strong("Weighted Absolute Percentage Error (WAPE): "),
        HTML(wape_text)
      )
    )
    
  })
  
  # Predicted vs. observed chart for selected department
  output$validation_plot <- renderPlotly({
    
    # Keep all dates so suspected missing periods show as gaps
    df <- selected_test() %>%
      mutate(
        observed = if_else(
          reporting_status == "Valid",
          observed,
          NA_real_
        ),
        predicted = if_else(
          reporting_status == "Valid",
          predicted,
          NA_real_
        )
      )
    
    plot_ly(df, x = ~date) %>%
      add_lines(
        y = ~observed,
        name = "Observed",
        line = list(color = "#1B2A41", width = 1.5),
        connectgaps = FALSE,
        hovertemplate = paste0(
          "%{x|%m/%d/%Y}<br>",
          "Observed: %{y:.2f}<extra></extra>"
        )
      ) %>%
      add_lines(
        y = ~predicted,
        name = "Predicted",
        line = list(color = "#A8483F", width = 1.5),
        connectgaps = FALSE,
        hovertemplate = paste0(
          "%{x|%m/%d/%Y}<br>",
          "Predicted: %{y:.2f}<extra></extra>"
        )
      ) %>%
      layout(
        title = list(
          text = "Predicted vs. Observed Daily Calls (2025)"
        ),
        xaxis = list(title = "Date"),
        yaxis = list(title = "Number of Calls", rangemode = "tozero"),
        legend = list(orientation = "h", x = 0, y = -0.2)
      ) %>%
      style_plotly()
    
  })
  
}

# ============================================================
# 6. RUN APPLICATION
# ============================================================

shinyApp(
  ui = ui,
  server = server
)