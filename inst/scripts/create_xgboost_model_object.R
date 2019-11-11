library(xgboost)

data("Boston")

x_var <- c("lstat", "rm", "dis", "indus")
y_var <- "medv"

x_train <- as.matrix(tail(Boston[, x_var], -6))
y_train <- tail(Boston[, y_var], -6)
x_test <- as.matrix(head(Boston[, x_var], 6))

# Creating a larger test data set (300 observations) for more realistic function time calls.
# Modifying x_test to repeat the 6 test observations 50 times
x_test = rep(1,50) %x% x_test
colnames(x_test) <- colnames(x_train)

# Fitting a basic xgboost model to the training data
model <- xgboost(
  data = x_train,
  label = y_train,
  nround = 20
)

saveRDS(model,file = "inst/model_objects/xgboost_model_object.rds") 

xgb.save(model=model,fname = "inst/model_objects/xgboost_model_object_raw") 
