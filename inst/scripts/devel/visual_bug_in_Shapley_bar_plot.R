# In this file we illustrate a visual bug that appears (before the bugfix)
# when the features values of the x_explain are of different order for the
# different observations.
# The visual bug in `plot.shapr()` gave extra whitespace in the strings "feature = value".
# The bugfix adds trim whitespace to remove redundant whitespace.
# Before the bugfix we got extra whitespace for those features that have different numbers
# of digits. E.g., if a feature is "1000" for one observation and "5" for another, then we
# would previously get the strings "1000" and " 5", i.e., the latter/smaller one got three
# extra white spaces. We illustrate this below.

library(xgboost)
library(data.table)

data("airquality")
data <- data.table::as.data.table(airquality)
data <- data[complete.cases(data), ]

x_var <- c("Solar.R", "Wind", "Temp", "Month")
y_var <- "Ozone"

ind_x_explain <- 1:6
x_train <- data[-ind_x_explain, ..x_var]
y_train <- data[-ind_x_explain, get(y_var)]
x_explain <- data[ind_x_explain, ..x_var]

# Fitting a basic xgboost model to the training data
model <- xgboost::xgboost(
  data = as.matrix(x_train),
  label = y_train,
  nround = 20,
  verbose = FALSE
)

# Specifying the phi_0, i.e. the expected prediction without any features
p0 <- mean(y_train)

# Computing the actual Shapley values with kernelSHAP accounting for feature dependence using
# the gaussian approach
explanation <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = "gaussian",
  prediction_zero = p0,
  n_samples = 10,
  keep_samp_for_vS = TRUE
)

# Overwrite the feature value for one of the explicands (x_explain) with
# many digits to make the visual bug easier to spot
explanation$internal$data$x_explain$Wind[4] = 200000

# Make the plot. Note that `Wind = ...` has a lot of whitespace now.
plot(explanation)
