library(shapr)

rm(list = ls())

data("Boston", package = "MASS")

x_var <- c("lstat", "rm", "dis", "indus")
y_var <- "medv"

x_train <- as.matrix(tail(Boston[, x_var], -6))
y_train <- tail(Boston[, y_var], -6)
x_test <- as.matrix(head(Boston[, x_var], 6))

# Just looking at the dependence between the features
cor(x_train)

# Fitting a basic xgboost model to the training data
model <- xgboost::xgboost(
  data = x_train,
  label = y_train,
  nround = 20,
  verbose = FALSE
)

df <- tail(Boston, 50)

model2 <- lm(medv ~ lstat + rm + dis + indus + nox, data = df)
model3 <- ranger::ranger(formula = medv ~ lstat + rm + dis + indus, data = df, num.trees = 50)

explainer <- shapr(x_train, model)
explainer2 <- shapr(df, model2, feature_labels = c("lstat", "nox"))
explainer3 <- shapr(df, model3, feature_labels = c("lstat", "rm"))


# Spedifying the phi_0, i.e. the expected prediction without any features
p0 <- mean(y_train)

# Computing the actual Shapley values with kernelSHAP accounting for feature dependence using
# the empirical (conditional) distribution approach with bandwidth parameter sigma = 0.1 (default)
explanation <- explain(x_test, explainer, approach = "empirical", prediction_zero = p0)

explanation <- explain(df, explainer, approach = "empirical", prediction_zero = p0)

explanation <- explain(x_test, explainer2, approach = "empirical", prediction_zero = p0)

x_test_2 = x_test
colnames(x_test_2)[1:2] =colnames(x_test_2)[2:1]
explanation <- explain(x_test_2, explainer, approach = "empirical", prediction_zero = p0)


# DEBUGGING


x
explainer$x_train

# Prepare the data for explanation

x <- df
# Remove variables that were not used for training
x <- data.table::as.data.table(x)
cnms_remove <- setdiff(colnames(x), explainer$feature_labels)
if (length(cnms_remove) > 0) x[, (cnms_remove) := NULL]
data.table::setcolorder(x, explainer$feature_labels)







# Printing the Shapley values for the test data
explanation$dt

# Finally we plot the resulting explanations
plot(explanation)
