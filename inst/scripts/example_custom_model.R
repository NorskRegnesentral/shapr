rm(list = ls())

library(gbm)
library(shapr)

# Load data
data("Boston", package = "MASS")

# Create test- and training data
x_var <- c("lstat", "rm", "dis", "indus")
y_var <- "medv"

xy_train <- tail(Boston, -6)
x_test <- head(Boston,6)

form = as.formula(paste0(y_var,"~",paste0(x_var,collapse="+")))

# Fitting a gbm model
set.seed(825)
model <- gbm::gbm(
  form,
  data = xy_train,
  distribution="gaussian"
)


# Create custom function of model_type for gbm
model_type.gbm <- function(x) {
  ifelse(
    x$distribution$name %in% c("bernoulli","adaboost"),
    "classification",
    "regression"
  )
}

# Create custom function of predict_model for gbm
predict_model.gbm <- function(x, newdata) {

  if (!requireNamespace('gbm', quietly = TRUE)) {
    stop('The gbm package is required for predicting train models')
  }
  model_type <- model_type(x)

  if (model_type == "classification") {

    predict(x, as.data.frame(newdata), type = "response",n.trees = x$n.trees)
  } else {

    predict(x, as.data.frame(newdata),n.trees = x$n.trees)
  }
}

# Prepare the data for explanation
set.seed(123)
explainer <- shapr(xy_train, model,feature_labels = x_var)

# Spedifying the phi_0, i.e. the expected prediction without any features
p0 <- mean(xy_train[,y_var])

# Computing the actual Shapley values with kernelSHAP accounting for feature dependence using
# the empirical (conditional) distribution approach with bandwidth parameter sigma = 0.1 (default)
explanation <- explain(x_test, explainer, approach = "empirical", prediction_zero = p0)

# Printing the Shapley values for the test data
explanation$dt

# Plot results
plot(explanation)
