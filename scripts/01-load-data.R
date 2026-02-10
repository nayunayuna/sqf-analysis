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
names(sqf_2006)
