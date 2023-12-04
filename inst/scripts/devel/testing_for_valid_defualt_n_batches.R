# In this code we demonstrate that (before the bugfix) the `explain()` function
# does not enter the exact mode when n_combinations is larger than or equal to 2^m.
# The mode is only changed if n_combinations is strictly larger than 2^m.
# This means that we end up with using all coalitions when n_combinations is 2^m,
# but use not the exact Shapley kernel weights.
# Bugfix replaces `>` with `=>`in the places where the code tests if
# n_combinations is larger than or equal to 2^m. Then the text/messages printed by
# shapr and the code correspond.

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

# Shapr sets the default number of batches to be 10 for this dataset for the
# "ctree", "gaussian", and "copula" approaches. Thus, setting `n_combinations`
# to any value lower of equal to 10 causes the error.
any_number_equal_or_below_10 = 8

# Before the bugfix, shapr:::check_n_batches() throws the error:
# Error in check_n_batches(internal) :
#   `n_batches` (10) must be smaller than the number feature combinations/`n_combinations` (8)
# Bug only occures for "ctree", "gaussian", and "copula" as they are treated different in
# `get_default_n_batches()`, I am not certain why. Ask Martin about the logic behind that.
explanation <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  n_samples = 2, # Low value for fast computations
  approach = "gaussian",
  prediction_zero = p0,
  n_combinations = any_number_equal_or_below_10
)
