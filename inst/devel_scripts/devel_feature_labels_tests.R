# Testing feature_labels and input to explain

# To be converted to tests

library(shapr)

rm(list = ls())

data("Boston", package = "MASS")

x_var <- c("lstat", "rm", "dis", "indus")
y_var <- "medv"
x_var_sub <- x_var[1:2]
not_x_var <- "crim"
not_even_var <- "not_a_column_name"


x_train <- as.matrix(tail(Boston[, x_var], -6))
y_train <- tail(Boston[, y_var], -6)
x_test <- as.matrix(head(Boston[, x_var], 6))
x_test_full <- as.matrix(head(Boston[, ], 6))
x_test_reordered <- as.matrix(head(Boston[, rev(x_var)], 6))


xy_train_full_df <- tail(Boston[, ], -6)
xy_train_missing_lstat_df <- xy_train_full_df[,!(colnames(xy_train_full_df) == "lstat")]
# Fitting a basic xgboost model to the training data
model1 <- xgboost::xgboost(
  data = x_train,
  label = y_train,
  nround = 20,
  verbose = FALSE
)
formula <- as.formula(paste0("medv ~ ",paste0(x_var,collapse="+")))

model2 <- lm(formula = formula,
             data = xy_train_full_df)
model3 <- ranger::ranger(formula = formula,
                         data = xy_train_full_df,
                         num.trees = 50)

# Just making up a fictive model class
model4 <- "cumstom_testmodel"
class(model4) = "testclass"

# Create custom function of model_type for caret
model_type.testclass <- function(x) {
  "regression"
  }

# Create custom function of predict_model for caret
predict_model.testclass <- function(x, newdata) {
  newdata[,1] # Always giving the first argument of newdata as the prediction
}



# expect_success
explainer1_success <- shapr(x_train, model1) # OK
explainer2_success <- shapr(xy_train_full_df, model2) #OK
explainer3_success <- shapr(xy_train_full_df, model3) # OK
explainer4_success <- shapr(xy_train_full_df, model4,feature_labels = x_var) # OK


# Expect message that feature_labels is ignored
explainer1_message <- shapr(xy_train_full_df, model1, feature_labels = x_var_sub) # Throws, error, should not do that
explainer1_message2 <- shapr(xy_train_full_df, model1, feature_labels = x_var) # Throws, error, should not do that
explainer2_message <- shapr(xy_train_full_df, model2, feature_labels = x_var_sub) # OK
explainer3_message <- shapr(xy_train_full_df, model3, feature_labels = x_var_sub) # OK

# Expect error, giving error message that throws indicates that the x misses columns used by the model
explainer1_error <- shapr(xy_train_missing_lstat_df, model1) # Should give better error message
explainer2_error <- shapr(xy_train_missing_lstat_df, model2) # Should give better error message

# Expect error that feature_labels is not in training data or used by the model
explainer4_error <- shapr(xy_train_full_df, model4,feature_labels = not_x_var) # Should throw an error, does not do that
explainer4_error2 <- shapr(xy_train_full_df, model4,feature_labels = not_even_var) # Should give better error message

# Expect error, that feature_labels is missing
explainer4_error_2 <- shapr(xy_train_full_df, model4)  # OK



#### Turning to the explain function
p0 <- mean(y_train)

# expect_success
explanation1_success <- explain(x_test, explainer1_success, approach = "empirical", prediction_zero = p0) # OK
explanation2_success <- explain(x_test, explainer2_success, approach = "empirical", prediction_zero = p0) # OK
explanation3_success <- explain(x_test, explainer3_success, approach = "empirical", prediction_zero = p0) # OK
explanation4_success <- explain(x_test, explainer4_success, approach = "empirical", prediction_zero = p0) # OK

# expect_success using reordered test data
explanation1_success2 <- explain(x_test_reordered, explainer1_success, approach = "empirical", prediction_zero = p0) # error using all.equal insted of identical
explanation2_success2 <- explain(x_test_reordered, explainer2_success, approach = "empirical", prediction_zero = p0) # error using all.equal insted of identical
explanation3_success2 <- explain(x_test_reordered, explainer3_success, approach = "empirical", prediction_zero = p0) # error using all.equal insted of identical
explanation4_success2 <- explain(x_test_reordered, explainer4_success, approach = "empirical", prediction_zero = p0) # error using all.equal insted of identical

# expect_success using test data with extra
explanation1_success3 <- explain(x_test_full, explainer1_success, approach = "empirical", prediction_zero = p0) # error using all.equal insted of identical
explanation2_success3 <- explain(x_test_full, explainer2_success, approach = "empirical", prediction_zero = p0) # error using all.equal insted of identical
explanation3_success3 <- explain(x_test_full, explainer3_success, approach = "empirical", prediction_zero = p0) # error using all.equal insted of identical
explanation4_success3 <- explain(x_test_full, explainer4_success, approach = "empirical", prediction_zero = p0) # error using all.equal insted of identical
