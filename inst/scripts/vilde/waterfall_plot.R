library(xgboost)
library(shapr)
library(ggplot2)
library(data.table)

#test plotting w Boston data
data("Boston", package = "MASS")
x_var <- c("lstat", "rm", "dis", "indus", "crim", "age")
y_var <- "medv"
b <- 150
x_train <- as.matrix(Boston[-1:-b, x_var])
y_train <- Boston[-1:-b, y_var]
x_test <- as.matrix(Boston[1:b, x_var])

model <- xgboost(
  data = x_train,
  label = y_train,
  nround = 20,
  verbose = FALSE
)
p <- mean(y_train)
x <- explain_final(x_train,x_test,model,approach="independence",prediction_zero=p,n_batches = 4)
plot.shapr(x,
           plot_type = "bar",
           digits = 3,
           plot_phi0 = TRUE,
           index_x_explain = NULL,
           top_k_features = NULL,
           col = c("#00BA38","#F8766D"), #first increasing color, then decreasing color
           plot_order = "largest_first",
           features_to_plot = NULL,
           histogram = TRUE,
           )

# data("AdultUCI", package = "arules")
# names(AdultUCI) <- gsub("-","_",names(AdultUCI))
# data <- na.omit(AdultUCI)
# data$income <-ifelse(data$income==2,1,0)
# x_var <- c("age", "workclass", "hours_per_week","native_country")
# y_var <- "income"
# x_train <- as.matrix(data[-1:-b, x_var])
# y_train <- data[-1:-b, y_var]
# x_test <- as.matrix(data[1:b, x_var])

#test plotting with simulated data
test <- data.frame(x1 = rnorm(5000, mean=10, sd=4),
                   x2 = rnorm(5000, mean=-60, sd=2),
                   x3 = rnorm(5000, mean=100, sd=1),
                   x4 = rnorm(5000, mean=0, sd=1),
                   y = rnorm(5000, mean=-5, sd=2))

x_var <- c("x1", "x2", "x3", "x4")
y_var <- "y"
b <- 350
x_train <- as.matrix(test[-1:-b, x_var])
y_train <- test[-1:-b, y_var]
x_test <- as.matrix(test[1:b, x_var])

# Fitting a basic xgboost model to the training data
model <- xgboost(
  data = x_train,
  label = y_train,
  nround = 20,
  verbose = FALSE
)
p <- mean(y_train)

plot.shapr(x,
           plot_type = "bar",
           digits = 3,
           plot_phi0 = TRUE,
           index_x_explain = NULL,
           top_k_features = NULL,
           col = c("#00BA38","#F8766D"), #first increasing color, then decreasing color
           plot_order = "largest_first",
           features_to_plot = NULL,
           histogram = TRUE
           )

