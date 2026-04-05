# ============================================================================
# R/ml_functions.R
# Helper functions for machine learning analysis of geocoded SQF data
# ============================================================================
# Part 2. Evaluation Function for Classification Models
# ============================================================================


# ============================================================================
# Compute Log-Loss (Binary Cross-Entropy)
# ============================================================================
# Calculates log-loss for binary classification predictions.
# This is the primary metric for the prediction competition.
# Lower values are better.
#
#' @param y_true Actual binary outcomes (logical, numeric 0/1, or factor)
#' @param y_pred Predicted probabilities (numeric between 0 and 1)
#' @return Numeric scalar: average log-loss across all observations
#' @details
# Formula: -mean(y * log(p) + (1-y) * log(1-p))
# Predictions are clipped to [1e-15, 1-1e-15] to avoid log(0)
#' @examples
# y_true <- c(0, 0, 1, 1)
# y_pred <- c(0.1, 0.2, 0.8, 0.9)
# compute_logloss(y_true, y_pred)
#' @export
compute_logloss <- function(y_true, y_pred) {
  # Convert to numeric (0/1)
  y_true <- as.numeric(y_true)
  
  # Clip predictions to avoid log(0)
  eps <- 1e-15
  y_pred <- pmax(pmin(y_pred, 1 - eps), eps)
  
  # Compute log-loss
  mean(-(y_true * log(y_pred) + (1 - y_true) * log(1 - y_pred)))
}

# ============================================================================
# Compute Classification Accuracy
# ============================================================================
# Calculates accuracy (proportion correct) for binary classification.
# Uses a threshold to convert probabilities to class predictions.
# Note: With imbalanced data, accuracy is misleading. Use log-loss instead.
#
#' @param y_true Actual binary outcomes (logical, numeric 0/1, or factor)
#' @param y_pred Predicted probabilities (numeric between 0 and 1)
#' @param threshold Probability threshold for classifying as positive (default 0.5)
#' @return Numeric scalar: proportion of correct predictions

# @export
compute_accuracy <- function(y_true, y_pred, threshold = 0.5) {
  y_true <- as.numeric(y_true)
  predictions <- ifelse(y_pred > threshold, 1, 0)
  mean(predictions == y_true)
}

# ============================================================================
# Compute Majority-Class Baseline Predictions
# ============================================================================
# Returns the overall proportion of positive cases.
# This is what you get by always predicting the same probability for everyone.
# Any real model should beat this baseline.

#' @param y_true Actual binary outcomes from training data
#' @return Numeric scalar: the baseline probability (arrest rate in this case)
#' @examples
#' y_true <- c(0, 0, 0, 0, 1)
#' baseline_prediction(y_true)  # Returns 0.2
#' @export
baseline_prediction <- function(y_true) {
  mean(as.numeric(y_true))
}

# ============================================================================
# Evaluate Model on Any Dataset
# ============================================================================
# Computes both log-loss and accuracy for a fitted logistic regression model.
# Returns a named list with both metrics for easy reporting.

#' @param model Fitted logistic regression model (from glm)
#' @param data Dataset to evaluate on (must have all predictors from model)
#' @param truth_col Name of the outcome column (default "arrest")
#' @return List with elements:
#   - logloss: log-loss value
#   - accuracy: classification accuracy at 0.5 threshold
#   - predictions: the predicted probabilities


#' @export
evaluate_model <- function(model, data, truth_col = "arrest") {
  # Get predictions
  pred_probs <- predict(model, newdata = data, type = "response")
  
  # Get true values
  y_true <- data[[truth_col]]
  
  # Compute metrics
  list(
    logloss = compute_logloss(y_true, pred_probs),
    accuracy = compute_accuracy(y_true, pred_probs),
    predictions = pred_probs
  )
}

# ============================================================================
# Compare In-Sample vs Out-of-Sample Performance
# ============================================================================
# Fits a model on training data and evaluates on both train and validation sets.
# Returns a comparison showing overfitting (if val performance < train performance).

#' @param formula Model formula (e.g., arrest ~ age + male + precinct)
#' @param train_data Training dataset
#' @param val_data Validation dataset
#' @param truth_col Name of outcome column (default "arrest")

#' @return Data frame with columns:
#   - dataset: "train" or "validation"
#   - logloss: log-loss for that dataset
#   - accuracy: accuracy for that dataset
#   - n: number of observations
#

#' @export
compare_performance <- function(formula, train_data, val_data, truth_col = "arrest") {
  # Fit model on training data
  model <- glm(formula, data = train_data, family = binomial())
  
  # Evaluate on train
  train_results <- evaluate_model(model, train_data, truth_col)
  
  # Evaluate on validation
  val_results <- evaluate_model(model, val_data, truth_col)
  
  # Return as a readable data frame
  data.frame(
    dataset = c("train", "validation"),
    logloss = c(train_results$logloss, val_results$logloss),
    accuracy = c(train_results$accuracy, val_results$accuracy),
    n = c(nrow(train_data), nrow(val_data))
  )
}


# ============================================================================
# K-Fold Cross-Validation for Logistic Regression
# ============================================================================

# Performs k-fold cross-validation on a logistic regression model.
# Splits data into k folds, trains on k-1, evaluates on 1, repeats k times.
# Returns average log-loss and accuracy across all folds.

#' @param formula Model formula (e.g., arrest ~ age + male + race)
#' @param data Full dataset to cross-validate on
#' @param k Number of folds (default 5)
#' @param truth_col Name of outcome column (default "arrest")
#' @param seed Random seed for reproducibility (default 2024)
#'
#' @return Data frame with columns:
#   - fold: fold number (1 to k)
#   - logloss: log-loss for that fold
#   - accuracy: accuracy for that fold
#   - n_train: training set size
#   - n_test: test set size
#   And attributes with mean and sd of metrics

#' @export
cv_logistic <- function(formula, data, k = 5, truth_col = "arrest", seed = 2024) {
  set.seed(seed)
  
  n <- nrow(data)
  fold_ids <- sample(rep(1:k, length.out = n))
  
  results <- data.frame(
    fold = integer(),
    logloss = numeric(),
    accuracy = numeric(),
    n_train = integer(),
    n_test = integer()
  )
  
  for (fold in 1:k) {
    # Split into train and test for this fold
    test_idx <- which(fold_ids == fold)
    train_fold <- data[-test_idx, ]
    test_fold <- data[test_idx, ]
    
    # Fit model on training fold
    model <- glm(formula, data = train_fold, family = binomial())
    
    # Evaluate on test fold
    eval_results <- evaluate_model(model, test_fold, truth_col)
    
    # Store results
    results <- rbind(results, data.frame(
      fold = fold,
      logloss = eval_results$logloss,
      accuracy = eval_results$accuracy,
      n_train = nrow(train_fold),
      n_test = nrow(test_fold)
    ))
  }
  
  # Compute and store mean and sd
  attr(results, "mean_logloss") <- mean(results$logloss)
  attr(results, "sd_logloss") <- sd(results$logloss)
  attr(results, "mean_accuracy") <- mean(results$accuracy)
  attr(results, "sd_accuracy") <- sd(results$accuracy)
  
  results
}


# ============================================================================
# Cross-Validated Lasso for Logistic Regression
#============================================================================

# Uses glmnet with cross-validation to find optimal regularization strength (lambda).
#
#' @param formula Model formula
#' @param data Dataset
#' @param alpha Regularization type: 1 = lasso, 0.5 = elastic net
#' @param k Number of folds
#' @param truth_col Name of outcome column
#
#' @return List with cv_model, best_lambda, best_logloss
#
#' @export
fit_regularized_model <- function(formula, data, alpha = 1, k = 5, truth_col = "arrest") {
  
  # Prepare data
  y <- as.numeric(data[[truth_col]])
  X <- model.matrix(formula, data = data)[, -1]  # Remove intercept
  
  # Fit lasso with CV
  cv_fit <- glmnet::cv.glmnet(
    x = X,
    y = y,
    family = "binomial",
    alpha = alpha,
    nfolds = k,
    type.measure = "deviance"
  )
  
  # Extract best lambda and error
  best_lambda <- cv_fit$lambda.min
  best_logloss <- cv_fit$cvm[cv_fit$lambda == best_lambda] / 2
  
  list(
    cv_model = cv_fit,
    best_lambda = best_lambda,
    best_logloss = best_logloss
  )
}

