library(xgboost)
library(shapr)

data("airquality")
data <- data.table::as.data.table(airquality)
data <- data[complete.cases(data), ]
data[,new1 :=sqrt(Wind*Ozone)]
data[,new2 :=sqrt(Wind*Temp)]
data[,new3 :=sqrt(Wind*Day)]
data[,new4 :=sqrt(Wind*Solar.R)]
data[,new5 :=rnorm(.N)]
data[,new6 :=rnorm(.N)]
data[,new7 :=rnorm(.N)]


x_var <- c("Solar.R", "Wind", "Temp", "Month","Day","new1","new2","new3","new4","new5","new6","new7")
y_var <- "Ozone"

ind_x_explain <- 1:6
x_train <- data[-ind_x_explain, ..x_var]
y_train <- data[-ind_x_explain, get(y_var)]
x_explain <- data[ind_x_explain, ..x_var]

# Looking at the dependence between the features
cor(x_train)

# Fitting a basic xgboost model to the training data
model <- xgboost(
  data = as.matrix(x_train),
  label = y_train,
  nround = 20,
  verbose = FALSE
)

# Specifying the phi_0, i.e. the expected prediction without any features
p0 <- mean(y_train)

# Computing the actual Shapley values with kernelSHAP accounting for feature dependence using
# the empirical (conditional) distribution approach with bandwidth parameter sigma = 0.1 (default)
explanation_adaptive <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = "gaussian",
  n_combinations = 20,
  prediction_zero = p0,
  adaptive = TRUE
)

explanation_regular <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = "gaussian",
  n_combinations = NULL,
  prediction_zero = p0,
  adaptive = FALSE
)

