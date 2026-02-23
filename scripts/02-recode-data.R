# ---- Load packages ----
library(tidyverse)
library(lubridate)

# ---- Source helper functions ----
source("scripts/01-load-data.R")  # For loading raw data
source("R/data_recording.R") # For recoding functions

# ---- Load raw data ----
# Assumes you've already run the data loading script and saved to RDS
# If not, run the loading code from Assignment 1 first
sqf_raw <- read_rds("data/sqf_raw.rds")

message("Loaded raw SQF data: ", nrow(sqf_raw), " rows")

# ---- Recode each year using functional programming ----
# Group by year, then map the recoding function over each group
sqf_clean <- sqf_raw %>%
  group_split(data_year) %>%
  map_dfr(~ recode_sqf_year(.x, unique(.x$data_year)))

# Print summary
message(sprintf("Recoded %s observations", format(nrow(sqf_clean), big.mark = ",")))

# Save
write_rds(sqf_clean, "data/sqf_clean.rds", compress = "gz")
message("Saved to: data/sqf_clean.rds")