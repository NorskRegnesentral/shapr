library(gbm)
library(shapr)

# Load data
data("Boston", package = "MASS")

# Create test- and training data
x_var <- c("lstat", "rm", "dis", "indus")
y_var <- "medv"

x_train <- as.matrix(Boston[-1:-6, x_var])
y_train <- Boston[-1:-6, y_var]
x_test <- as.matrix(Boston[1:6, x_var])

form = as.formula(paste0(y_var,"~",paste0(x_var,collapse="+")))

library(gbm)

xy_train <- data.frame(x_train,medv = y_train)


# Fitting a gbm model
set.seed(825)
model <- gbm::gbm(
  form,
  data = xy_train,
  distribution = "gaussian"
)

#### Full feature versions of the three required model functions ####

predict_model.gbm <- function(x, newdata) {

  if (!requireNamespace('gbm', quietly = TRUE)) {
    stop('The gbm package is required for predicting train models')
  }

  model_type <- ifelse(
    x$distribution$name %in% c("bernoulli","adaboost"),
    "classification",
    "regression"
  )
  if (model_type == "classification") {

    predict(x, as.data.frame(newdata), type = "response",n.trees = x$n.trees)
  } else {

    predict(x, as.data.frame(newdata),n.trees = x$n.trees)
  }
}

get_model_specs.gbm <- function(x){
  feature_list = list()
  feature_list$labels <- labels(x$Terms)
  m <- length(feature_list$labels)

  feature_list$classes <- attr(x$Terms,"dataClasses")[-1]
  feature_list$factor_levels <- setNames(vector("list", m), feature_list$labels)
  feature_list$factor_levels[feature_list$classes=="factor"] <- NA # the model object doesn't contain factor levels info

  return(feature_list)
}

# Prepare the data for explanation
set.seed(123)
explainer <- shapr(xy_train, model)
p0 <- mean(xy_train[,y_var])
explanation <- explain(x_test, explainer, approach = "empirical", prediction_zero = p0)
# Plot results
plot(explanation)


# Minimal version of the three required model functions
# Note: Working only for this exact version of the model class
# Avoiding to define get_model_specs skips all feature
# consistency checking between your data and model

# Removing the previously defined functions to simulate a fresh start
rm(predict_model.gbm)
rm(get_model_specs.gbm)


predict_model.gbm <- function(x, newdata) {
  predict(x, as.data.frame(newdata),n.trees = x$n.trees)
}


# Prepare the data for explanation
set.seed(123)
explainer <- shapr(x_train, model)
p0 <- mean(xy_train[,y_var])
explanation <- explain(x_test, explainer, approach = "empirical", prediction_zero = p0)
# Plot results
plot(explanation)
