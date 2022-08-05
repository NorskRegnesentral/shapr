library(data.table)

options(digits = 5) # To avoid round off errors when printing output on different systems

set.seed(1234)

data <- data.table::as.data.table(airquality)
data[, Month_factor := as.factor(Month)]

data_complete <- data[complete.cases(airquality), ]
data_complete <- data_complete[sample(1:.N)] # Sh

y_var_numeric <- "Ozone"

x_var_numeric <- c("Solar.R", "Wind", "Temp", "Month", "Day")
x_var_mixed <- c("Solar.R", "Wind", "Temp", "Day", "Month_factor")

data_train <- head(data_complete, -2)
data_test <- tail(data_complete, 2)

x_train_numeric <- data_train[, ..x_var_numeric]
x_train_mixed <- data_train[, ..x_var_mixed]

x_test_numeric <- data_test[, ..x_var_numeric]
x_test_mixed <- data_test[, ..x_var_mixed]

lm_formula_numeric <- as.formula(paste0(y_var_numeric, " ~ ", paste0(x_var_numeric, collapse = " + ")))
lm_formula_mixed <- as.formula(paste0(y_var_numeric, " ~ ", paste0(x_var_mixed, collapse = " + ")))

model_lm_numeric <- lm(lm_formula_numeric, data = data_complete)
model_lm_mixed <- lm(lm_formula_mixed, data = data_complete)

p0 <- data_train[, mean(get(y_var_numeric))]

test_that("check_data gives correct messages", {
  set.seed(123)
  custom_predict_model <- function(x, newdata) {
    beta <- coef(x)
    X <- model.matrix(~.,newdata)
    return(as.vector(beta %*% t(X)))
  }

  model_custom_lm_mixed <- model_lm_mixed
  class(model_custom_lm_mixed) <- "whatever"

  # Custom model with no get_model_specs
  expect_snapshot(
    explain(x_train_mixed,
            x_test_mixed,
            model_custom_lm_mixed,
            approach = "independence",
            prediction_zero = p0,
            predict_model = custom_predict_model,
            get_model_specs = NA)
  )

  # Custom model where get_model_specs gives NA-labels
  custom_get_model_specs_no_labels <- function(x){
    feature_specs <- list(labels = NA, classes = NA, factor_levels = NA)
  }

  expect_snapshot(
    explain(x_train_mixed,
            x_test_mixed,
            model_custom_lm_mixed,
            approach = "independence",
            prediction_zero = p0,
            predict_model = custom_predict_model,
            get_model_specs = custom_get_model_specs_no_labels)
  )

  # Custom model where get_model_specs gives NA-classes
  custom_get_model_specs_no_classes <- function(x){
    feature_specs <- list(labels = labels(x$terms), classes = NA, factor_levels = NA)
  }

  expect_snapshot(
    explain(x_train_mixed,
            x_test_mixed,
            model_custom_lm_mixed,
            approach = "independence",
            prediction_zero = p0,
            predict_model = custom_predict_model,
            get_model_specs = custom_get_model_specs_no_classes)
  )

  # Custom model where get_model_specs gives NA-factor levels
  custom_get_model_specs_no_factor_levels <- function(x){
    feature_specs <- list(labels = labels(x$terms),
                          classes = attr(x$terms, "dataClasses")[-1],
                          factor_levels = NA)
    }

  expect_snapshot(
    explain(x_train_mixed,
            x_test_mixed,
            model_custom_lm_mixed,
            approach = "independence",
            prediction_zero = p0,
            predict_model = custom_predict_model,
            get_model_specs = custom_get_model_specs_no_factor_levels)
  )


})
