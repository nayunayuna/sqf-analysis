# ============================================================================
# scripts/05-ml-analysis.R
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





# ============================================================================
# Part 2. Evaluation Infrastructure 
# ============================================================================

source("R/ml_functions.R")

# --- Test functions on simple examples ---

# Test compute_logloss
y_test <- c(0, 0, 1, 1)
p_test <- c(0.1, 0.2, 0.8, 0.9)
cat("Test logloss:", compute_logloss(y_test, p_test), "\n")

# Test compute_accuracy
cat("Test accuracy:", compute_accuracy(y_test, p_test), "\n")

# Test baseline prediction
cat("Test baseline:", baseline_prediction(y_test), "\n")


# ============================================================================
# Compute baseline (majority-class always predicting arrest rate)
# ============================================================================

baseline_rate <- baseline_prediction(train_clean$arrest)

cat("\n=== BASELINE MODEL (predict arrest rate for everyone) ===\n")
cat("Overall arrest rate:", round(100 * baseline_rate, 2), "%\n")

# Create vector of baseline predictions (same probability for all rows)
baseline_predictions <- rep(baseline_rate, nrow(train_clean))

# Evaluate baseline
baseline_logloss <- compute_logloss(train_clean$arrest, baseline_predictions)
baseline_accuracy <- compute_accuracy(train_clean$arrest, baseline_predictions)

cat("Baseline Log-Loss: ", round(baseline_logloss, 4), "\n")
cat("Baseline Accuracy: ", round(100 * baseline_accuracy, 2), "%\n\n")




# ============================================================================
# Part 3: Simple Baseline Model (Fit & Evaluate)
# ============================================================================

# Choose a few key predictors based on exploration
simple_formula <- arrest ~ age + male + race + precinct + crime_suspected

# Prepare data (remove any rows that might have NAs in these columns)
model_data <- train_clean %>%
  select(all_of(c("arrest", "age", "male", "race", "precinct", "crime_suspected"))) %>%
  drop_na()

cat("Model data has", nrow(model_data), "rows\n\n")

# Fit simple logistic regression
model_simple <- glm(simple_formula, data = model_data, family = binomial())

# Evaluate on training data
train_results <- evaluate_model(model_simple, model_data, truth_col = "arrest")

cat("=== SIMPLE MODEL (in-sample performance) ===\n")
cat("Log-Loss: ", round(train_results$logloss, 4), "\n")
cat("Accuracy: ", round(100 * train_results$accuracy, 2), "%\n\n")

cat("Improvement over baseline:\n")
cat("  Log-Loss: ", round(baseline_logloss - train_results$logloss, 4), 
    " (", round(100 * (baseline_logloss - train_results$logloss) / baseline_logloss, 1), "% better)\n")
cat("  Accuracy: ", round(100 * (train_results$accuracy - baseline_accuracy), 2), " pp\n\n")

# Look at model summary
cat("Model coefficients:\n")
print(summary(model_simple))

# ============================================================================
# Part 3B: Train/Validation Split to Detect Overfitting
# ============================================================================

set.seed(2024)
n <- nrow(model_data)
train_idx <- sample(1:n, size = 0.7 * n)

train_set <- model_data[train_idx, ]
val_set <- model_data[-train_idx, ]

cat("Train set:", nrow(train_set), "rows\n")
cat("Val set:  ", nrow(val_set), "rows\n\n")

# Fit model on training set only
model_simple_cv <- glm(simple_formula, data = train_set, family = binomial())

# Evaluate on both
train_eval <- evaluate_model(model_simple_cv, train_set, truth_col = "arrest")
val_eval <- evaluate_model(model_simple_cv, val_set, truth_col = "arrest")

# Compare
cat("=== TRAIN vs VALIDATION PERFORMANCE ===\n")
cat("Training:\n")
cat("  Log-Loss: ", round(train_eval$logloss, 4), "\n")
cat("  Accuracy: ", round(100 * train_eval$accuracy, 2), "%\n\n")

cat("Validation:\n")
cat("  Log-Loss: ", round(val_eval$logloss, 4), "\n")
cat("  Accuracy: ", round(100 * val_eval$accuracy, 2), "%\n\n")

cat("Overfitting Check (validation - training):\n")
cat("  Log-Loss diff: ", round(val_eval$logloss - train_eval$logloss, 4), 
    " (", ifelse(val_eval$logloss > train_eval$logloss, "overfit", "no overfit"), ")\n")
cat("  Accuracy diff: ", round(100 * (val_eval$accuracy - train_eval$accuracy), 2), " pp\n")

# ============================================================================
# Part 3C: Enhanced Model with Reason and Circumstance Flags
# ============================================================================

# New formula with strong predictors
enhanced_formula <- arrest ~ age + male + race + precinct + crime_suspected +
                            reason_object + reason_desc + reason_drugs +
                            reason_furtive + reason_violent + reason_bulge +
                            reason_casing + reason_clothing + reason_lookout + reason_other +
                            circ_report + circ_invest + circ_proximity + circ_evasive +
                            circ_associate + circ_direction + circ_incident + circ_time +
                            circ_sights + circ_other

# Prepare data with all predictors
enhanced_data <- train_clean %>%
  select(all_of(c("arrest", "age", "male", "race", "precinct", "crime_suspected",
                  "reason_object", "reason_desc", "reason_drugs", "reason_furtive",
                  "reason_violent", "reason_bulge", "reason_casing", "reason_clothing",
                  "reason_lookout", "reason_other",
                  "circ_report", "circ_invest", "circ_proximity", "circ_evasive",
                  "circ_associate", "circ_direction", "circ_incident", "circ_time",
                  "circ_sights", "circ_other"))) %>%
  drop_na()

cat("Enhanced model data:", nrow(enhanced_data), "rows\n\n")

# Do train/val split
set.seed(2024)
n_enh <- nrow(enhanced_data)
train_idx_enh <- sample(1:n_enh, size = 0.7 * n_enh)

train_enh <- enhanced_data[train_idx_enh, ]
val_enh <- enhanced_data[-train_idx_enh, ]

# Fit on training set
model_enhanced <- glm(enhanced_formula, data = train_enh, family = binomial())

# Evaluate on both
train_enh_eval <- evaluate_model(model_enhanced, train_enh, truth_col = "arrest")
val_enh_eval <- evaluate_model(model_enhanced, val_enh, truth_col = "arrest")

cat("=== ENHANCED MODEL (with reason & circumstance flags) ===\n")
cat("Training:\n")
cat("  Log-Loss: ", round(train_enh_eval$logloss, 4), "\n")
cat("  Accuracy: ", round(100 * train_enh_eval$accuracy, 2), "%\n\n")

cat("Validation:\n")
cat("  Log-Loss: ", round(val_enh_eval$logloss, 4), "\n")
cat("  Accuracy: ", round(100 * val_enh_eval$accuracy, 2), "%\n\n")

cat("Overfitting Check:\n")
cat("  Log-Loss diff: ", round(val_enh_eval$logloss - train_enh_eval$logloss, 4), "\n\n")

# Compare to simple model and baseline
cat("=== COMPARISON ===\n")
cat("Baseline Log-Loss:        ", round(baseline_logloss, 4), "\n")
cat("Simple Model Val Log-Loss:", round(val_eval$logloss, 4), 
    " (", round(100 * (baseline_logloss - val_eval$logloss) / baseline_logloss, 1), "% better)\n")
cat("Enhanced Model Val Log-Loss:", round(val_enh_eval$logloss, 4),
    " (", round(100 * (baseline_logloss - val_enh_eval$logloss) / baseline_logloss, 1), "% better)\n")

# ============================================================================
# Part 4: K-Fold Cross Validation 
# ============================================================================

cat("\n=== 5-FOLD CROSS-VALIDATION (Simple Model) ===\n")
cv_simple <- cv_logistic(simple_formula, data = model_data, k = 5, seed = 2024)
print(cv_simple)
cat("\nAverage Log-Loss:", round(attr(cv_simple, "mean_logloss"), 4),
    "± ", round(attr(cv_simple, "sd_logloss"), 4), "\n")
cat("Average Accuracy:", round(100 * attr(cv_simple, "mean_accuracy"), 2), "%\n\n")

cat("\n=== 5-FOLD CROSS-VALIDATION (Enhanced Model) ===\n")
cv_enhanced <- cv_logistic(enhanced_formula, data = enhanced_data, k = 5, seed = 2024)
print(cv_enhanced)
cat("\nAverage Log-Loss:", round(attr(cv_enhanced, "mean_logloss"), 4),
    "± ", round(attr(cv_enhanced, "sd_logloss"), 4), "\n")
cat("Average Accuracy:", round(100 * attr(cv_enhanced, "mean_accuracy"), 2), "%\n\n")