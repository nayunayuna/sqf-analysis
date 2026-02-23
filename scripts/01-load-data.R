## --- Create scripts/01-load-data.R ---

# Load libraries
library(tidyverse)

# Source our custom functions
source("R/data_loading.R")

# Load all years of SQF data
sqf_raw <- load_sqf_all(years = 2006:2012, data_dir = "data/raw")

# Print summary statistics
message("\n=== Data Summary ===")
message(sprintf("Total rows: %s", format(nrow(sqf_raw), big.mark = ",")))
message(sprintf("Total columns: %d", ncol(sqf_raw)))

# Rows per year
year_summary <- sqf_raw %>%
  group_by(data_year) %>%
  summarise(n_stops = n(), .groups = "drop") %>%
  arrange(data_year)

print(year_summary)

# --- Save the raw combined data ---
output_path <- "data/sqf_raw.rds"
write_rds(sqf_raw, output_path, compress = "gz")
message(sprintf("\nData saved to: %s", output_path))