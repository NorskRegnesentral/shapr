

aa = explain(
  model = model_lm_numeric,
  x_explain = x_explain_numeric,
  x_train = x_train_numeric,
  approach = "independence",
  phi0 = p0,
  iterative_args = list(
    initial_n_coalitions = 10,
    convergence_tol = 0.01,
    n_coal_next_iter_factor_vec = rep(10^(-5), 10),
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
  phi0 = p0,
  iterative_args = list(
    initial_n_coalitions = 10,
    convergence_tol = 0.001,
    n_coal_next_iter_factor_vec = rep(10^(-5), 10),
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
  phi0 = p0,
  iterative_args = list(
    initial_n_coalitions = 10,
    convergence_tol = 0.001,
    n_coal_next_iter_factor_vec = rep(10^(-5), 10),
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
  phi0 = p0,
  iterative_args = list(
    initial_n_coalitions = 10,
    convergence_tol = 0.001,
    n_coal_next_iter_factor_vec = rep(10^(-5), 10),
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
  phi0 = p0,
  iterative_args = list(
    initial_n_coalitions = 10,
    convergence_tol = 0.001,
    n_coal_next_iter_factor_vec = rep(10^(-5), 10),
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
  phi0 = p0,
  iterative_args = list(
    initial_n_coalitions = 10,
    convergence_tol = 0.001,
    n_coal_next_iter_factor_vec = rep(10^(-5), 10),
    max_iter = 5
  ),
  iterative = TRUE,
  print_shapleyres = TRUE,
  print_iter_info = TRUE,
  kernelSHAP_reweighting = "on_N",
  seed=NULL,
  prev_shapr_object = first$internal$parameters$output_args$saving_path
)


# Identical results
all.equal(full$shapley_values_est,second$shapley_values_est) # TRUE
all.equal(full$shapley_values_est,second2$shapley_values_est) # TRUE
all.equal(full$shapley_values_est,second_path$shapley_values_est) # TRUE
