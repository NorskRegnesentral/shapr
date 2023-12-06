library(xgboost)
library(shapr)

data("airquality")
data <- data.table::as.data.table(airquality)
data <- data[complete.cases(data), ]

set.seed(123)
data[,V1:=rnorm(.N)]
data[,V2:=rnorm(.N)]
data[,V3:=rnorm(.N)]
data[,V4:=rnorm(.N)]
data[,V5:=rnorm(.N)]

x_var <- c("Solar.R", "Wind", "Temp", "Month","Day","V1","V2","V3","V4","V5")
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
n_permutations_vec <- c(4,8,16,32,64,128)


exp_full <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = "gaussian",seed = 123,n_batches=1,
  shap_approach = "kernel",
  n_combinations = NULL,
  prediction_zero = p0
)

exp_full$timing


exp_list <- exp_kern_list <- list()
mse_vec <- mse_kern_vec <- NULL
for (i in seq_along(n_permutations_vec)) {
  exp_list[[i]] <- explain(
    model = model,
    x_explain = x_explain,
    x_train = x_train,
    approach = "gaussian",seed = 123,n_batches=1,
    shap_approach = "permutation",
    n_permutations = n_permutations_vec[i],
    prediction_zero = p0
  )

  exp_kern_list[[i]] <- explain(
    model = model,
    x_explain = x_explain,
    x_train = x_train,
    approach = "gaussian",seed = 123,n_batches=1,
    shap_approach = "kernel",
    n_combinations = exp_list[[i]]$internal$parameters$n_combinations,
    prediction_zero = p0
  )

  print(exp_list[[i]]$timing)
  print(exp_kern_list[[i]]$timing)

  mse_vec[i] <- mean(unlist((exp_list[[i]]$shapley_values[,..x_var]-exp_full$shapley_values[,..x_var])^2))
  mse_kern_vec[i] <- mean(unlist((exp_kern_list[[i]]$shapley_values[,..x_var]-exp_full$shapley_values[,..x_var])^2))

}

mse_vec
mse_kern_vec


explanation_perm <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = "gaussian",seed = 123,n_batches=1,
  prediction_zero = p0,shap_approach = "permutation",n_permutations = 24
)

explanation_kernel <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = "gaussian",seed = 123,n_batches=1,
  n_combinations = explanation_perm$internal$parameters$n_combinations,
  prediction_zero = p0
)

explanation_perm$internal$objects$X
explanation_kernel$internal$objects$X

explanation_perm$internal$output$dt_vS
explanation_kernel$internal$output$dt_vS

explanation_perm$shapley_values
explanation_kernel$shapley_values

explanation_perm$timing
explanation_kernel$timing

#### Some TODO notes:

# Implement antithetic sampling for both permutation and kernel method
# run basic simulations to show improved convergence vs kernel method (based on same n_combinations)
# if the tests shows improved convergence, the setup and computation code needs increased efficiency



