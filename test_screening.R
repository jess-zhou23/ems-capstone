################################################################################
# NFIRS TEST SET SCREENING AND MANUAL CLEANING — 2025
# Based on nfirs_training_screening_2020_2023_jess.R
# Requires: dplyr, tidyr, lubridate, ggplot2
################################################################################

# 0. WORKING DIRECTORY — EDIT THIS SECTION
# Uncomment and replace with your folder, or set your working directory in RStudio.
setwd("~/UVA/Year 4/Capstone")

library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)

# Read the same input as the training script (unidentifiable FDs already removed).
nfirs_373 <- read.csv("va_nfirs_2020_2025_373FDs.csv")

OUTPUT_DIR <- file.path(getwd(), "nfirs_test_screening")
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

# Screening thresholds copied from the training script.
ZERO_MONTH_FLAG_THRESHOLD <- 0.40   # strictly greater than 40%
SEVERE_DROP_RATIO <- 0.30          # strictly less than 30% of positive-month median
MIN_BASELINE_CALLS <- 30


# Validation cohort is frozen from cleaned training data.
training_monthly <- read.csv("clean_monthly_full.csv", colClasses = c(FDID = "character"))
TRAINING_FDIDS <- unique(as.character(training_monthly$FDID))
TRAINING_FDIDS <- TRAINING_FDIDS[!is.na(TRAINING_FDIDS) & nzchar(TRAINING_FDIDS)]
stopifnot(length(TRAINING_FDIDS) > 0L)

parse_nfirs_dates <- function(data) {
  stopifnot(all(c("FDID", "INC_DATE") %in% names(data)))
  data |>
    mutate(
      FDID = as.character(FDID),
      incident_date = suppressWarnings(
        lubridate::mdy(sprintf("%08d", as.integer(INC_DATE)))
      )
    )
}

max_zero_run <- function(x) {
  z <- rle(x)
  if (!any(z$values)) return(0L)
  as.integer(max(z$lengths[z$values]))
}

validate_period <- function(start_date, end_date) {
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  if (is.na(start_date) || is.na(end_date) || start_date > end_date) {
    stop("Provide valid start_date and end_date (YYYY-MM-DD), start <= end.")
  }
  if (start_date != floor_date(start_date, "month") ||
      end_date != ceiling_date(end_date, "month") - days(1)) {
    stop("Use COMPLETE calendar months (first day through last day of month).")
  }
  list(start = start_date, end = end_date)
}

# 1. SCREENING FUNCTION ---------------------------------------------------------
# Returns monthly_screening, department_screening, screening_summary,
# review_flag_breakdown, and filtered incident data.
# Also saves tables and plots in nfirs_validation_screening/.
screen_nfirs_validation <- function(nfirs,
                                    start_date = "2025-01-01",
                                    end_date = "2025-12-31",
                                    training_fdids = TRAINING_FDIDS,
                                    output_dir = OUTPUT_DIR) {
  period <- validate_period(start_date, end_date)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  dates <- seq(floor_date(period$start, "month"),
               floor_date(period$end, "month"), by = "month")
  incidents <- parse_nfirs_dates(nfirs) |>
    filter(!is.na(FDID), !is.na(incident_date),
           between(incident_date, period$start, period$end))
  if (!is.null(training_fdids)) {
    incidents <- incidents |>
      filter(FDID %in% as.character(training_fdids))
  }
  # No incidents is permitted: every training FD still gets a complete month panel.
  
  observed <- incidents |>
    mutate(month = floor_date(incident_date, "month")) |>
    count(FDID, month, name = "calls")
  
  # Full cross-product: every original training FD x every requested month.
  monthly_screening <- tidyr::expand_grid(
    FDID = unique(as.character(training_fdids)), month = dates
  ) |>
    left_join(observed, by = c("FDID", "month")) |>
    mutate(calls = replace_na(calls, 0L)) |>
    arrange(FDID, month) |>
    group_by(FDID) |>
    mutate(
      median_positive_monthly_calls = if (any(calls > 0)) median(calls[calls > 0]) else NA_real_,
      zero_month = calls == 0,
      zero_run_group = cumsum(!zero_month),
      zero_run_length = if_else(
        zero_month,
        as.integer(ave(as.integer(zero_month), zero_run_group, FUN = length)),
        0L
      ),
      relative_to_median = if_else(!is.na(median_positive_monthly_calls), calls / median_positive_monthly_calls, NA_real_),
      severe_volume_decline =
        !is.na(median_positive_monthly_calls) &
        median_positive_monthly_calls >= MIN_BASELINE_CALLS &
        relative_to_median < SEVERE_DROP_RATIO
    ) |>
    ungroup()
  
  department_screening <- monthly_screening |>
    group_by(FDID) |>
    summarise(
      first_month = min(month), last_month = max(month),
      span_months = n(), months_with_calls = sum(calls > 0),
      zero_months = sum(zero_month), pct_zero_months = mean(zero_month),
      longest_zero_run = max_zero_run(zero_month),
      median_positive_monthly_calls = first(median_positive_monthly_calls),
      severe_decline_months = sum(severe_volume_decline),
      total_period_calls = sum(calls), .groups = "drop"
    ) |>
    mutate(
      flag_zero_pct = pct_zero_months > ZERO_MONTH_FLAG_THRESHOLD,
      flag_two_consecutive_zeros = longest_zero_run >= 2L,
      flag_three_consecutive_zeros = longest_zero_run >= 3L,
      flag_severe_decline = severe_decline_months > 0L,
      any_quality_flag = flag_zero_pct | flag_two_consecutive_zeros |
        flag_three_consecutive_zeros | flag_severe_decline,
      screening_status = if_else(any_quality_flag,
                                 "Eligible - review flags", "Eligible - no flags")
    ) |>
    arrange(screening_status, FDID)
  
  screening_summary <- department_screening |>
    count(screening_status, name = "n_departments")
  
  # Overlapping reasons are counted independently: their totals can exceed
  # the number of unique departments in the corresponding category.
  review_flag_breakdown <- department_screening |>
    filter(screening_status == "Eligible - review flags") |>
    summarise(
      `More than 40% zero months` = sum(flag_zero_pct),
      `Exactly 2 consecutive zero months` = sum(longest_zero_run == 2L),
      `3+ consecutive zero months` = sum(longest_zero_run >= 3L),
      `Severe nonzero monthly decline` = sum(flag_severe_decline)
    ) |>
    pivot_longer(everything(), names_to = "reason", values_to = "n_departments")
  
  write.csv(department_screening, file.path(output_dir, "department_screening.csv"), row.names = FALSE)
  write.csv(monthly_screening, file.path(output_dir, "monthly_screening.csv"), row.names = FALSE)
  write.csv(screening_summary, file.path(output_dir, "screening_summary.csv"), row.names = FALSE)
  write.csv(review_flag_breakdown, file.path(output_dir, "eligible_review_flag_breakdown.csv"), row.names = FALSE)
  
  # Every training FD gets a plot in at least one folder. Categories can overlap.
  folder_names <- c("eligible_no_flags_plots", "severe_decline_plots",
                    "two_consecutive_zero_plots", "high_zero_percentage_plots",
                    "all_zero_months_plots")
  for (folder in folder_names) {
    dir.create(file.path(output_dir, folder), recursive = TRUE, showWarnings = FALSE)
    # Remove plots from prior runs so each folder reflects the current screen.
    old_plots <- list.files(file.path(output_dir, folder), pattern = "^FD_.*[.]png$", full.names = TRUE)
    if (length(old_plots)) file.remove(old_plots)
  }
  
  plot_fd <- function(fd, folder, highlight) {
    fd_data <- monthly_screening |> filter(FDID == fd)
    marked <- switch(highlight,
                     "severe" = fd_data |> filter(severe_volume_decline),
                     "two" = fd_data |> filter(zero_run_length == 2L),
                     "high_zero" = fd_data |> filter(zero_month),
                     "all_zero" = fd_data |> filter(zero_month),
                     fd_data[0, ]
    )
    plot_obj <- ggplot(fd_data, aes(month, calls)) +
      geom_line(color = "steelblue", linewidth = 0.8) +
      geom_point(color = "steelblue", size = 1.7) +
      geom_point(data = marked, color = "red", size = 3) +
      scale_x_date(date_breaks = "1 month", date_labels = "%b %Y",
                   limits = c(min(dates), max(dates))) +
      labs(title = paste("Monthly call volume — FDID", fd),
           subtitle = paste0(
             "Screening: ", department_screening$screening_status[
               match(fd, department_screening$FDID)],
             " | Zero months: ", sum(fd_data$zero_month), "/", nrow(fd_data),
             " | Red = months matching this folder's flag"),
           x = "Month", y = "Number of reported calls") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggsave(file.path(output_dir, folder, paste0("FD_", fd, ".png")),
           plot_obj, width = 10, height = 5, dpi = 200)
  }
  
  noflag_ids <- department_screening |>
    filter(!any_quality_flag) |> pull(FDID)
  severe_ids <- department_screening |>
    filter(flag_severe_decline) |> pull(FDID)
  two_ids <- department_screening |>
    filter(longest_zero_run == 2L) |> pull(FDID)
  high_zero_ids <- department_screening |>
    filter(flag_zero_pct) |> pull(FDID)
  all_zero_ids <- department_screening |>
    filter(zero_months == span_months) |> pull(FDID)
  # Departments with 3+ consecutive zeros also satisfy high-zero percentage
  # only if the zero percentage exceeds the threshold. Include them in the
  # consecutive-zero folder so no flagged FD is left without a plot.
  consecutive_ids <- department_screening |>
    filter(longest_zero_run >= 2L) |> pull(FDID)
  
  for (fd in noflag_ids) plot_fd(fd, "eligible_no_flags_plots", "none")
  for (fd in severe_ids) plot_fd(fd, "severe_decline_plots", "severe")
  for (fd in consecutive_ids) plot_fd(fd, "two_consecutive_zero_plots", "two")
  for (fd in high_zero_ids) plot_fd(fd, "high_zero_percentage_plots", "high_zero")
  for (fd in all_zero_ids) plot_fd(fd, "all_zero_months_plots", "all_zero")
  
  plotted_ids <- unique(c(noflag_ids, severe_ids, consecutive_ids,
                          high_zero_ids, all_zero_ids))
  missing_plot_ids <- setdiff(department_screening$FDID, plotted_ids)
  if (length(missing_plot_ids)) {
    stop("Some departments received no plot: ", paste(missing_plot_ids, collapse = ", "))
  }
  cat("\n=== PLOT COVERAGE ===\n")
  cat("Unique departments plotted:", length(plotted_ids), "/",
      nrow(department_screening), "\n")
  print(data.frame(folder = folder_names,
                   n_plots = c(length(noflag_ids), length(severe_ids),
                               length(consecutive_ids), length(high_zero_ids),
                               length(all_zero_ids))), row.names = FALSE)
  
  cat("\n=== DEPARTMENT SCREENING SUMMARY ===\n")
  print(screening_summary, n = Inf)
  cat("\n=== ELIGIBLE — REVIEW FLAGS (overlapping reasons) ===\n")
  print(review_flag_breakdown, n = Inf)
  cat("\nPlots and screening CSVs saved to:", output_dir, "\n")
  
  invisible(list(
    incidents = incidents, monthly_screening = monthly_screening,
    department_screening = department_screening,
    screening_summary = screening_summary,
    review_flag_breakdown = review_flag_breakdown
  ))
}

# 2. FINAL CLEANING FUNCTION ----------------------------------------------------
# Accepts manually reviewed department IDs, then writes the two final CSVs.
# - The incident-level CSV preserves ALL original columns and all incident
#   records in retained departments/dates, EVEN in suspected missing months,
#   matching the supplied training script. To score forecasts, join to the
#   monthly reporting_status and exclude Suspected missing months.
# - The monthly CSV matches training: FDID, month, calls_reported,
#   calls_model, reporting_status.
# - The 30% median uses ALL monthly counts (including zeros), as in training.
# - preserve_zeroes takes precedence over manual_zero and thirty_percent
#   ONLY for zero-call months; nonzero low-volume months can still be flagged.
# - No automatic screen flags become 'Suspected missing' without manual input,
#   matching the manual-review nature of the training cleaning script.
clean_nfirs_validation <- function(nfirs,
                                   start_date = "2025-01-01",
                                   end_date = "2025-12-31",
                                   thirty_percent = character(),
                                   manual_exclude = character(),
                                   preserve_zeroes = character(),
                                   manual_zero = character(),
                                   manual_flag = NULL,
                                   screening = NULL,
                                   training_fdids = TRAINING_FDIDS,
                                   output_dir = OUTPUT_DIR) {
  period <- validate_period(start_date, end_date)
  if (is.null(screening)) {
    # Screening will generate the plots and tables if not already supplied.
    screening <- screen_nfirs_validation(
      nfirs, start_date, end_date, training_fdids, output_dir
    )
  }
  all_ids <- as.character(screening$department_screening$FDID)
  normalize_ids <- function(x) unique(as.character(x))
  thirty_percent <- normalize_ids(thirty_percent)
  manual_exclude <- normalize_ids(manual_exclude)
  preserve_zeroes <- normalize_ids(preserve_zeroes)
  manual_zero <- normalize_ids(manual_zero)
  if (is.null(manual_flag)) manual_flag <- tibble::tibble(FDID = character(), month = as.Date(character()))
  # Specific department-month overrides. These always take precedence over
  # preserve_zeroes and other rules, and do not exclude whole departments.
  stopifnot(all(c("FDID", "month") %in% names(manual_flag)))
  manual_flag <- manual_flag |>
    mutate(FDID = as.character(FDID), month = as.Date(month)) |>
    distinct(FDID, month)
  if (anyNA(manual_flag$month)) stop("MANUAL_FLAG contains invalid dates.")
  requested <- unique(c(thirty_percent, manual_exclude, preserve_zeroes,
                        manual_zero, manual_flag$FDID))
  unknown <- setdiff(requested, all_ids)
  if (length(unknown)) warning("Manual FDIDs absent from screening: ", paste(unknown, collapse = ", "))
  if (length(intersect(preserve_zeroes, manual_zero))) {
    warning("FDIDs in both preserve_zeroes and manual_zero: preserve_zeroes wins for zeros.")
  }
  
  keep_ids <- screening$department_screening |>
    filter(!FDID %in% manual_exclude) |> pull(FDID)
  
  nfirs_validation_full <- screening$incidents |>
    filter(FDID %in% keep_ids) |>
    select(-incident_date)
  # Keep the same original column order and column names as input.
  nfirs_validation_full <- nfirs_validation_full[, names(nfirs), drop = FALSE]
  
  flagged_keys <- manual_flag |>
    mutate(flag_specific_month = TRUE)
  missing_keys <- anti_join(manual_flag,
                            screening$monthly_screening |> select(FDID, month),
                            by = c("FDID", "month"))
  if (nrow(missing_keys)) stop("MANUAL_FLAG includes FD-months absent from screening.")
  if (any(manual_flag$FDID %in% manual_exclude)) {
    stop("MANUAL_FLAG includes an FD in manual_exclude; resolve the conflict.")
  }
  
  nfirs_validation_monthly <- screening$monthly_screening |>
    filter(FDID %in% keep_ids) |>
    left_join(flagged_keys, by = c("FDID", "month")) |>
    mutate(flag_specific_month = coalesce(flag_specific_month, FALSE)) |>
    
    group_by(FDID) |>
    mutate(
      median_monthly_calls = median(calls, na.rm = TRUE),
      flag_thirty_percent = FDID %in% thirty_percent &
        calls < 0.30 * median_monthly_calls,
      flag_manual_zero = FDID %in% manual_zero & calls == 0,
      flag_preserve_zero = FDID %in% preserve_zeroes & calls == 0,
      suspected_missing = flag_specific_month |
        ((flag_thirty_percent | flag_manual_zero) & !flag_preserve_zero),
      calls_reported = calls,
      calls_model = if_else(suspected_missing, NA_integer_, as.integer(calls)),
      reporting_status = if_else(suspected_missing, "Suspected missing", "Valid")
    ) |>
    ungroup() |>
    select(FDID, month, calls_reported, calls_model, reporting_status) |>
    arrange(FDID, month)
  
  # Confirm all manually specified department-months are classified correctly.
  manual_flag_check <- nfirs_validation_monthly |>
    inner_join(manual_flag, by = c("FDID", "month"))
  stopifnot(nrow(manual_flag_check) == nrow(manual_flag),
            all(manual_flag_check$reporting_status == "Suspected missing"),
            all(is.na(manual_flag_check$calls_model)))
  
  # Structural checks: monthly includes all training FDs unless manually excluded.
  # Incident-level data can have fewer FDs if some reported zero calls all year.
  stopifnot(
    !anyDuplicated(nfirs_validation_monthly[c("FDID", "month")]),
    all(nfirs_validation_monthly$reporting_status %in% c("Valid", "Suspected missing")),
    all(is.na(nfirs_validation_monthly$calls_model[
      nfirs_validation_monthly$reporting_status == "Suspected missing"])),
    all(nfirs_validation_monthly$calls_model[
      nfirs_validation_monthly$reporting_status == "Valid"] ==
        nfirs_validation_monthly$calls_reported[
          nfirs_validation_monthly$reporting_status == "Valid"]),
    identical(names(nfirs_validation_full), names(nfirs)),
    !any(nfirs_validation_full$FDID %in% manual_exclude),
    setequal(unique(nfirs_validation_monthly$FDID),
             setdiff(as.character(training_fdids), manual_exclude))
  )
  
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  write.csv(nfirs_validation_full,
            file.path(output_dir, "nfirs_test_full.csv"), row.names = FALSE)
  write.csv(nfirs_validation_monthly,
            file.path(output_dir, "nfirs_test_monthly.csv"), row.names = FALSE)
  
  cat("\n=== FINAL VALIDATION DATASETS ===\n")
  cat("Retained departments:", n_distinct(nfirs_validation_monthly$FDID), "\n")
  cat("Incident rows:", nrow(nfirs_validation_full), "\n")
  cat("Department-month rows:", nrow(nfirs_validation_monthly), "\n")
  print(nfirs_validation_monthly |> count(reporting_status), n = Inf)
  cat("Final CSVs saved to:", output_dir, "\n")
  invisible(list(full = nfirs_validation_full, monthly = nfirs_validation_monthly))
}

# 3. RUN SCREENING FIRST --------------------------------------------------------
validation_screen <- screen_nfirs_validation(
  nfirs = nfirs_373,
  start_date = "2025-01-01",
  end_date = "2025-12-31"
)  # this function starts loading in all the plots and puts them in a folder in your current wd

# 4. AFTER REVIEWING PLOTS, ENTER MANUAL FDIDs AND RUN CLEANING -----------------
# Empty vectors are placeholders: do NOT interpret them as reviewed decisions.
# Add your FDIDs after inspecting plots and screening tables.
THIRTY_PERCENT_FDIDS <- c("902","4100","7301","8900","10700","11707","14310","14313","14701",
                          "16544","17100","17304","17900","19110","19301","19706","51000")
# Mark months below 30% of that department's monthly median as suspected missing.

MANUAL_EXCLUDE_FDIDS <- c("2902","7130","7180","14314","17301","66000","92008","12503")
# Only these explicitly listed departments are excluded in final cleaning.

PRESERVE_ZEROES_FDIDS <- c()
# Keep zero-call months valid for these departments, except MANUAL_FLAG overrides.

MANUAL_ZERO_FDIDS <- c("903","2502","14302","14308","14319","14321","16500","16501","16505","16508","16515","16517","16544",
                       "55000","56000","67000","300","6900","7500","8908","9900","16500","16505",
                       "16508","16515","19105","19303","51500")

# Mark all zero-call months as suspected missing for these departments.

# Exact department-month overrides (2024): mark ONLY the specified months.
# These overrides take precedence over preserve_zeroes and apply to nonzero months too.
MANUAL_FLAG <- tibble::tibble(
  FDID = c("10700", "10700"),
  month = as.Date(c(
    "2025-05-01", "2025-11-01"
  ))
)

# Set TRUE only AFTER completing manual review.
RUN_FINAL_CLEANING <- TRUE
if (RUN_FINAL_CLEANING) {
  validation_clean <- clean_nfirs_validation(
    nfirs = nfirs_373,
    start_date = "2025-01-01",
    end_date = "2025-12-31",
    thirty_percent = THIRTY_PERCENT_FDIDS,
    manual_exclude = MANUAL_EXCLUDE_FDIDS,
    preserve_zeroes = PRESERVE_ZEROES_FDIDS,
    manual_zero = MANUAL_ZERO_FDIDS,
    manual_flag = MANUAL_FLAG,
    screening = validation_screen
  )
  nfirs_test_full <- validation_clean$full
  nfirs_test_monthly <- validation_clean$monthly
}
