library(xgboost)

# Load data -----------
data("Boston", package = "MASS")
x_var <- c("lstat", "rm", "dis", "indus")
y_var <- "medv"

x_train <- as.matrix(tail(Boston[, x_var], 50))
y_train <- tail(Boston[, y_var], 50)
x_test <- as.matrix(head(Boston[, x_var], 2))


# Fitting a basic xgboost model to the training data
#suppressWarnings(RNGversion("3.5.0"))
set.seed(123)
model <- xgboost::xgboost(
  data = x_train,
  label = y_train,
  nround = 20,
  verbose = FALSE
)

saveRDS(object = model,"tests/testthat/test_objects/xgboost_model_object.rds")
