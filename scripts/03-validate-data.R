# ---- Load packages ----
library(tidyverse)

# ---- Source validation function ----
source("R/data_validation.R")

# ---- Load cleaned data ----
sqf_clean <- read_rds("data/sqf_clean.rds")

# ---- Run validation ----
validation <- validate_sqf_data(sqf_clean)

# ---- Print validation report ----
message("\n", stringr::str_dup("=", 50))
message("DATA VALIDATION REPORT")
message(stringr::str_dup("=", 50), "\n")

message("Dataset Summary:")
message(sprintf("  • Rows: %s", format(validation$n_rows, big.mark = ",")))
message(sprintf("  • Columns: %d", validation$n_cols))
message(sprintf("  • Years: %s", validation$year_range))
message(sprintf("  • Timestamp: %s\n", validation$timestamp))

if (validation$passed) {
  message("✓ ALL VALIDATION CHECKS PASSED!\n")
} else {
  message(sprintf("✗ FOUND %d ISSUE(S):\n", validation$n_issues))

  for (name in names(validation$issues)) {
    issue <- validation$issues[[name]]
    message(sprintf("  ✗ %s", name))
    message(sprintf("      %s\n", toString(issue)))
  }
}

message(stringr::str_dup("=", 50), "\n")

# ---- Save validation report ----
dir.create("output", showWarnings = FALSE)
write_rds(validation, "output/validation_report.rds")
message("Validation report saved to: output/validation_report.rds")