# ============================================================================
# R/05-ml-analysis.R
# Machine learning analysis of geocoded SQF data
# ============================================================================
# Part 1. Load and explore data
# ===========================================================================

# --- Load libraries ---
library(tidyverse)
library(glmnet)

# --- Load trained data ---
train_data <- readRDS("data/sqf_ml_train.rds")
holdout_data <- readRDS("data/sqf_ml_holdout.rds")

# --- Explore training data variables ---
dim(train_data)
head(train_data)
summary(train_data)

# --- Check arrest variable distribution ---
table(train_data$arrest) 
mean(train_data$arrest)    

# --- Missing data summary ---
missing_summary <- data.frame(
  column = names(train_data),
  missing_count = colSums(is.na(train_data)),
  missing_pct = round(100 * colSums(is.na(train_data)) / nrow(train_data), 2)
) %>%
  filter(missing_count > 0) %>%
  arrange(desc(missing_pct))

print(missing_summary)

# ============== Notes ==============
# Arrests are rare. Only 5.9% of stops result in arrest
# location_type has 30% missing - consider dropping or treating as separate category
# male, age, coordinates have <2% missing - safe to drop rows or impute
# All reason_* and circ_* flags are complete (no missing)

# ============== Decisions ==============
# 1. Drop rows with missing age, male, or coordinates (small %)
# 2. Drop location_type because arrest rates are similar across types and 30% missing
# 3. Keep all reason_* and circ_* flags since they are complete and may be
# ========================================


# --- Find which reason_* and circ_* flags are most predictive of arrest ---
reason_cols <- names(train_data)[grepl("^reason_|^circ_", names(train_data))]

reason_arrest_rates <- tibble(
  feature = reason_cols,
  arrest_rate_when_true = NA_real_,
  arrest_rate_when_false = NA_real_
)

for (col in reason_cols) {
  reason_arrest_rates$arrest_rate_when_true[reason_arrest_rates$feature == col] <- 
    mean(train_data$arrest[train_data[[col]] == TRUE], na.rm = TRUE)
  reason_arrest_rates$arrest_rate_when_false[reason_arrest_rates$feature == col] <- 
    mean(train_data$arrest[train_data[[col]] == FALSE], na.rm = TRUE)
}

reason_arrest_rates %>%
  mutate(diff = arrest_rate_when_true - arrest_rate_when_false) %>%
  arrange(desc(diff))


# --- Check Arrest rate by race ---
train_data %>%
  group_by(race) %>%
  summarise(n = n(), arrest_rate = mean(arrest)) %>%
  arrange(desc(arrest_rate))

# --- Check Arrest rate by sex ---
train_data %>%
  group_by(male) %>%
  summarise(n = n(), arrest_rate = mean(arrest))

# --- Check Arrest rate by precinct ---
train_data %>%
  group_by(precinct) %>%
  summarise(n = n(), arrest_rate = mean(arrest)) %>%
  arrange(desc(arrest_rate))

# --- Check Arrest rate by location type ---
train_data %>%
  group_by(location_type) %>%
  summarise(n = n(), arrest_rate = mean(arrest))

# --- Check Arrest rate by crime suspected ---
train_data %>%
  group_by(crime_suspected) %>%
  summarise(n = n(), arrest_rate = mean(arrest)) %>%
  arrange(desc(arrest_rate))

# --- Check which continuous variables correlate with arrest ---
continuous_vars <- c("age", "height", "weight", "year", "month", "day_of_week", "hour")

cor_with_arrest <- train_data %>%
  select(arrest, all_of(continuous_vars)) %>%
  cor(use = "complete.obs") %>%
  as.data.frame() %>%
  rownames_to_column("var") %>%
  filter(var != "arrest") %>%
  select(var, arrest) %>%
  arrange(desc(abs(arrest)))

print(cor_with_arrest)

# ============== Notes ==============
# Strong positive predictors: reason_object, circ_report, reason_desc, circ_sights, reason_drugs
# Race shows little variation in arrest rates, but Black and Hispanic stops have slightly higher rates than White stops
# Sex shows moderate variation where females have ~2pp higher arrest
# Precincts show strong variation, with precint 9, 25, and 5 has highest and precint 11 and 113 has lowest arrest rates
# Location type shows little variation in arrest rates, but housing projects have slightly higher rates than other 
# Crime suspected shows strong variation, with "Assault" "Other" "Misdemeanor_other" "Drugs" as higher and "Robbery" "Burglary" as lower
# Continuous variables show very weak correlation with arrest
# ============== Decisions ==============
# 1. Include reason_* and circ_* flags
# 2. Include precint, crime_suspected, location_type, male
# 3. Drop race, age, height, weight, year, month, day_of_week, hour due 
# ========================================


# ============================================================================
# Handle missing data and prepare for modeling
# ============================================================================

# Start with original data
train_clean <- train_data

# 1. Drop rows with missing coordinates (3% of data, not predictive anyway)
train_clean <- train_clean %>%
  filter(!is.na(xcoord) & !is.na(ycoord))

cat("After dropping missing coords:", nrow(train_clean), "rows\n")

# 2. Drop rows with missing age (0.1% of data, useful feature)
train_clean <- train_clean %>%
  filter(!is.na(age))

cat("After dropping missing age:", nrow(train_clean), "rows\n")

# 3. Drop rows with missing sex/male (1.9% of data)
train_clean <- train_clean %>%
  filter(!is.na(male))

cat("After dropping missing male:", nrow(train_clean), "rows\n")

# 4. Drop rows with missing hour, month, day_of_week (tiny %, but these cols are mostly not predictive anyway)
train_clean <- train_clean %>%
  filter(!is.na(hour) & !is.na(month) & !is.na(day_of_week))

cat("After dropping missing timing:", nrow(train_clean), "rows\n")

# 5. Handle location_type: recode missing as "unknown" category (don't drop 30% of data)
train_clean <- train_clean %>%
  mutate(location_type = fct_na_value_to_level(location_type, level = "unknown"))

# Verify: check for any remaining NAs in key columns
key_cols <- c("arrest", "age", "male", "precinct", "crime_suspected", 
              "reason_object", "circ_report", "location_type")

remaining_missing <- colSums(is.na(train_clean[key_cols]))
print(remaining_missing)

cat("\nFinal clean dataset:", nrow(train_clean), "rows out of", nrow(train_data), "\n")
cat("Retained:", round(100 * nrow(train_clean) / nrow(train_data), 1), "% of data\n")

# Check arrest rate didn't change much
cat("Original arrest rate:", round(100 * mean(train_data$arrest), 2), "%\n")
cat("Clean arrest rate:   ", round(100 * mean(train_clean$arrest), 2), "%\n")