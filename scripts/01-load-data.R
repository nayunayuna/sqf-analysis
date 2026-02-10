# NYC SQF Analysis

# ---- Load packages ----
library(tidyverse)
library(lubridate)

# ---- Load data for 2006-2012 ----
years <- 2006:2012

for (yr in years) {
  path <- file.path("data", "raw", paste0(yr, ".csv"))

  if (!file.exists(path)) {
    message("Missing file: ", path)
    next
  }

  assign(
    x = paste0("sqf_", yr),
    value = read_csv(path, col_types = cols(.default = col_character()))
  )

  # Print a quick summary for that year
  df <- get(paste0("sqf_", yr))
  message("Loaded ", yr, ": ", nrow(df), " rows, ", ncol(df), " cols")
}

# ---- Yearly summaries ----
# Check variables
names(sqf_2006)

# Convert "Y/N" into True False
to_logical <- function(x) {
  x <- toupper(trimws(x))
  case_when(
    x %in% c("Y") ~ TRUE,
    x %in% c("N") ~ FALSE,
    TRUE ~ NA
  )
}

# Create counts and rates for key variables (frisked, searched, arrested)
summary_list <- list()

for (yr in years) {
  df <- get(paste0("sqf_", yr))

  df <- df %>%
    mutate(
      frisked_l  = to_logical(frisked),
      searched_l = to_logical(searched),
      arrest_l   = to_logical(arstmade)
    )

  summary_list[[as.character(yr)]] <- tibble(
    year = yr,
    stops = nrow(df),

    frisked_n  = sum(df$frisked_l,  na.rm = TRUE),
    searched_n = sum(df$searched_l, na.rm = TRUE),
    arrests_n  = sum(df$arrest_l,   na.rm = TRUE),

    frisked_pct  = round(100 * frisked_n  / stops, 2),
    searched_pct = round(100 * searched_n / stops, 2),
    arrests_pct  = round(100 * arrests_n  / stops, 2)
  )

  message("Summarized ", yr)
}

summary_by_year <- bind_rows(summary_list)
summary_by_year


# Verify
summary_by_year

# Produce table
write_csv(summary_by_year, "output/summary_by_year.csv")

