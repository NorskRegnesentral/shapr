

aa = explain(
  model = model_lm_numeric,
  x_explain = x_explain_numeric,
  x_train = x_train_numeric,
  approach = "independence",
  prediction_zero = p0,
  iterative_args = list(
    initial_n_coalitions = 10,
    convergence_tolerance = 0.01,
    reduction_factor_vec = rep(10^(-5), 10),
    max_iter = 30
  ),
  iterative = TRUE,
  print_shapleyres = TRUE,
  print_iter_info = TRUE,kernelSHAP_reweighting = "on_N"
)

bb = explain(
  model = model_lm_numeric,
  x_explain = x_explain_numeric,
  x_train = x_train_numeric,
  approach = "independence",
  prediction_zero = p0,
  iterative_args = list(
    initial_n_coalitions = 10,
    convergence_tolerance = 0.001,
    reduction_factor_vec = rep(10^(-5), 10),
    max_iter = 30
  ),
  iterative = TRUE,
  print_shapleyres = TRUE,
  print_iter_info = TRUE,kernelSHAP_reweighting = "on_N",prev_shapr_object = aa
)




##### Reproducable results setting seed outside, and not setting it inside of explain (+ an seed-independent approach)
# Add something like this


set.seed(123)
full = explain(
  model = model_lm_numeric,
  x_explain = x_explain_numeric,
  x_train = x_train_numeric,
  approach = "independence",
  prediction_zero = p0,
  iterative_args = list(
    initial_n_coalitions = 10,
    convergence_tolerance = 0.001,
    reduction_factor_vec = rep(10^(-5), 10),
    max_iter = 7
  ),
  iterative = TRUE,
  print_shapleyres = TRUE,
  print_iter_info = TRUE,
  kernelSHAP_reweighting = "on_N",
  seed=NULL
)

set.seed(123)
first = explain(
  model = model_lm_numeric,
  x_explain = x_explain_numeric,
  x_train = x_train_numeric,
  approach = "independence",
  prediction_zero = p0,
  iterative_args = list(
    initial_n_coalitions = 10,
    convergence_tolerance = 0.001,
    reduction_factor_vec = rep(10^(-5), 10),
    max_iter = 4
  ),
  iterative = TRUE,
  print_shapleyres = TRUE,
  print_iter_info = TRUE,
  kernelSHAP_reweighting = "on_N",
  seed=NULL
)


second = explain(
  model = model_lm_numeric,
  x_explain = x_explain_numeric,
  x_train = x_train_numeric,
  approach = "independence",
  prediction_zero = p0,
  iterative_args = list(
    initial_n_coalitions = 10,
    convergence_tolerance = 0.001,
    reduction_factor_vec = rep(10^(-5), 10),
    max_iter = 7
  ),
  iterative = TRUE,
  print_shapleyres = TRUE,
  print_iter_info = TRUE,
  kernelSHAP_reweighting = "on_N",
  seed=NULL,
  prev_shapr_object = first
)



# This cannot be tested, I think.
second_path = explain(
  model = model_lm_numeric,
  x_explain = x_explain_numeric,
  x_train = x_train_numeric,
  approach = "independence",
  prediction_zero = p0,
  iterative_args = list(
    initial_n_coalitions = 10,
    convergence_tolerance = 0.001,
    reduction_factor_vec = rep(10^(-5), 10),
    max_iter = 5
  ),
  iterative = TRUE,
  print_shapleyres = TRUE,
  print_iter_info = TRUE,
  kernelSHAP_reweighting = "on_N",
  seed=NULL,
  prev_shapr_object = first$internal$parameters$saving_path
)


# Identical results
all.equal(full$shapley_values,second$shapley_values) # TRUE
all.equal(full$shapley_values,second2$shapley_values) # TRUE
all.equal(full$shapley_values,second_path$shapley_values) # TRUE
