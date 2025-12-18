devtools::load_all()

library(xgboost)
#library(shapr)

data("airquality")
data <- data.table::as.data.table(airquality)
data <- data[complete.cases(data), ]

x_var <- c("Solar.R", "Wind", "Temp", "Month")
y_var <- "Ozone"

ind_x_explain <- 1:6
x_train <- data[-ind_x_explain, ..x_var]
y_train <- data[-ind_x_explain, get(y_var)]
x_explain <- data[ind_x_explain, ..x_var]

# Look at the dependence between the features
cor(x_train)

# Fit a basic xgboost model to the training data
model <- xgboost(
  data = as.matrix(x_train),
  label = y_train,
  nround = 20,
  verbose = FALSE
)

model2 <- xgboost(
  data = x_train,
  label = y_train,
  nround = 20,
  verbose = FALSE
)

shapr:::get_model_specs.xgboost(model2)

shapr:::get_model_specs(model2)

shapr:::get_feature_specs(x_train, model2)

# Specify phi_0, i.e., the expected prediction without any features
p0 <- mean(y_train)

# Compute Shapley values with Kernel SHAP, accounting for feature dependence using
# the empirical (conditional) distribution approach with bandwidth parameter sigma = 0.1 (default)
explanation <- shapr::explain(
  model = model2,
  x_explain = x_explain,
  x_train = x_train,
  approach = "empirical",
  phi0 = p0,
  seed = 1
)

# Print the Shapley values for the observations to explain.
print(explanation)
