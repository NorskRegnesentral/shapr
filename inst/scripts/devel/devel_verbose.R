ex <- explain(
  testing = TRUE,
  model = model_lm_numeric,
  x_explain = x_explain_numeric,
  x_train = x_train_numeric,
  approach = "independence",
  prediction_zero = p0,
  max_n_coalitions = 30,
  adaptive_arguments = list(
    initial_n_coalitions = 6,
    convergence_tolerance = 0.0005,
    reduction_factor_vec = rep(10^(-6), 10),
    max_iter = 8
  ),
  adaptive = TRUE,verbose=c("basic","convergence","shapley")
)

ex <- explain(
  testing = TRUE,
  model = model_lm_numeric,
  x_explain = x_explain_numeric,
  x_train = x_train_numeric,
  approach = "regression_separate",
  prediction_zero = p0,
  max_n_coalitions = 30,
  adaptive = TRUE,verbose=c("basic","convergence","shapley","vS_details")
)

ex <- explain(
  testing = TRUE,
  model = model_lm_numeric,
  x_explain = x_explain_numeric,
  x_train = x_train_numeric,
  approach = "empirical",
  prediction_zero = p0,
  max_n_coalitions = 30,
  adaptive_arguments = list(
    initial_n_coalitions = 6,
    convergence_tolerance = 0.0005,
    reduction_factor_vec = rep(10^(-6), 10),
    max_iter = 8
  ),
  adaptive = TRUE,verbose=c("basic","convergence","shapley")
)


explain(
  testing = TRUE,
  model = model_lm_numeric,
  x_explain = x_explain_numeric,
  x_train = x_train_numeric,
  approach = "independence",
  prediction_zero = p0,
  adaptive = TRUE,
  adaptive_arguments <- list(n_initial_)
  verbose = c("basic"),
  paired_shap_sampling = TRUE
)
