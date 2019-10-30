library(shapr)

data("Boston", package = "MASS")

x_var <- c("lstat", "rm", "dis", "indus")
y_var <- "medv"

x_train <- as.data.frame(tail(Boston[, x_var], -6))
y_train <- tail(Boston[, y_var], -6)
x_test <- as.data.frame(head(Boston[, x_var], 6))

# Just looking at the dependence between the features
cor(x_train)

# Fitting a basic xgboost model to the training data
model <- xgboost::xgboost(
  data = as.matrix(x_train),
  label = y_train,
  nround = 20,
  verbose = FALSE
)

# Spedifying the phi_0, i.e. the expected prediction without any features
p0 <- mean(y_train)


### RUN WITH NEW CODE #####
# Prepare the data for explanation
explainer <- shapr(x_train, model)
explanation <- explain(x_test, explainer, approach = "empirical", prediction_zero = p0)
explanation$dt


### RUN WITH OLD CODE
explainer.old <- shapr(as.matrix(x_train), model)
explanation.old <- explain(as.matrix(x_test), explainer.old, approach = "empirical", prediction_zero = p0)
explanation.old$dt

