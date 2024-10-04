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
  adaptive = TRUE,verbose=c("basic","progress")
)

ex <- explain(
  testing = TRUE,
  model = model_lm_numeric,
  x_explain = x_explain_numeric,
  x_train = x_train_numeric,
  approach = "regression_separate",
  prediction_zero = p0,
  max_n_coalitions = 30,
  adaptive = TRUE,verbose=c("vS_details")
)
ex <- explain(
  model = model_lm_numeric,
  x_explain = x_explain_numeric,
  x_train = x_train_numeric,
  approach = "regression_separate",
  prediction_zero = p0,
  max_n_coalitions = 30,
  adaptive = TRUE,verbose=c("basic","progress","vS_details"),
  regression.model = parsnip::decision_tree(tree_depth = hardhat::tune(), engine = "rpart", mode = "regression"),
  regression.tune_values = dials::grid_regular(dials::tree_depth(), levels = 4),
  regression.vfold_cv_para = list(v = 5)
)

ex <- explain(
  model = model_lm_numeric,
  x_explain = x_explain_numeric,
  x_train = x_train_numeric,
  approach = "regression_surrogate",
  prediction_zero = p0,
  max_n_coalitions = 30,
  adaptive = FALSE,verbose=c("basic","vS_details"),
  regression.model = parsnip::decision_tree(tree_depth = hardhat::tune(), engine = "rpart", mode = "regression"),
  regression.tune_values = dials::grid_regular(dials::tree_depth(), levels = 4),
  regression.vfold_cv_para = list(v = 5)
)


future::plan("multisession", workers = 4)
progressr::handlers(global = TRUE)


ex <- explain(
  testing = TRUE,
  model = model_lm_numeric,
  x_explain = x_explain_numeric,
  x_train = x_train_numeric,
  approach = "vaeac",
  prediction_zero = p0,
  max_n_coalitions = 30,
  adaptive = FALSE,verbose=c("basic","progress","vS_details"),
  n_MC_samples = 100,
  vaeac.epochs = 3
)

ex2 <- explain(
  testing = TRUE,
  model = model_lm_numeric,
  x_explain = x_explain_numeric,
  x_train = x_train_numeric,
  approach = "vaeac",
  prediction_zero = p0,
  max_n_coalitions = 30,
  adaptive = FALSE,verbose=c("basic","progress","vS_details"),
  n_MC_samples = 100,
  vaeac.extra_parameters = list(
    vaeac.pretrained_vaeac_model = ex$internal$parameters$vaeac
  )
)



vaeac.extra_parameters = list(
  vaeac.pretrained_vaeac_model = explanation$internal$parameters$vaeac
)


ex <- explain(
  testing = TRUE,
  model = model_lm_numeric,
  x_explain = x_explain_numeric,
  x_train = x_train_numeric,
  approach = "regression_separate",
  prediction_zero = p0,
  max_n_coalitions = 30,
  adaptive = FALSE,verbose=c("basic")
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
