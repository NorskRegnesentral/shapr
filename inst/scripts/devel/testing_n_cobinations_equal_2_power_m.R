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

# Computing the conditional Shapley values using the gaussian approach
explanation_exact <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  n_samples = 2, # Low value for fast computations
  n_batches = 1, # Not related to the bug
  approach = "gaussian",
  prediction_zero = p0,
  n_combinations = NULL
)

# Computing the conditional Shapley values using the gaussian approach
explanation_should_also_be_exact <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  n_samples = 2, # Low value for fast computations
  n_batches = 1, # Not related to the bug
  approach = "gaussian",
  prediction_zero = p0,
  n_combinations = 2^ncol(x_explain)
)

# see that both `explain()` objects have the same number of combinations
explanation_exact$internal$parameters$n_combinations
explanation_should_also_be_exact$internal$parameters$n_combinations

# But the first one of them is exact and the other not.
explanation_exact$internal$parameters$exact
explanation_should_also_be_exact$internal$parameters$exact

# Can also see that the Shapley weights differ, as the first one uses
# the correct values, while the other one uses the sampling frequency.
explanation_exact$internal$objects$X$shapley_weight
explanation_should_also_be_exact$internal$objects$X$shapley_weight
