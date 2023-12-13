#' Explain the output of a linear model with  Shapley values
#'
#' @inheritParams explain
#'
#' @export
#'
#' @author Martin Jullum
#'
explain_linear <- function(model,
                           x_explain,
                           x_train,
                           paired_shap_sampling = FALSE,
                           prediction_zero,
                           n_combinations = NULL,
                           n_permutations = NULL,
                           group = NULL,
                           n_samples = 1e3,
                           n_batches = NULL,
                           seed = 1,
                           keep_samp_for_vS = FALSE,
                           predict_model = NULL,
                           get_model_specs = NULL,
                           MSEv_uniform_comb_weights = TRUE,
                           timing = TRUE,
                           ...) { # ... is further arguments passed to specific approaches

  timing_list <- list(
    init_time = Sys.time()
  )

  set.seed(seed)

  # Gets and check feature specs from the model
  feature_specs <- get_feature_specs(get_model_specs, model)

  linear_model_coef <- get_linear_coeff(model)

  null_object <- NULL
  # Sets up and organizes input parameters
  # Checks the input parameters and their compatability
  # Checks data/model compatability
  internal <- setup(
    type = "linear_gaussian",
    x_train = x_train,
    x_explain = x_explain,
    approach = "gaussian", # always set to "gaussian" although we never really use this argument for linear_gaussian
    shap_approach = "permutation", # Always use the permute shap_approach
    paired_shap_sampling = paired_shap_sampling,
    prediction_zero = prediction_zero,
    n_combinations = n_combinations, # We always set the n_permutations instead
    n_permutations = n_permutations,
    group = group,
    n_samples = n_samples,
    n_batches = n_batches,
    seed = seed,
    keep_samp_for_vS = keep_samp_for_vS,
    feature_specs = feature_specs,
    MSEv_uniform_comb_weights = MSEv_uniform_comb_weights,
    timing = timing,
    linear_model_coef = linear_model_coef, # TODO: Make this a proper input argument in setup(). For now this is just included through ... so no checking performed
    ...
  )

  timing_list$setup <- Sys.time()

  # Gets predict_model (if not passed to explain)
  predict_model <- get_predict_model(
    predict_model = predict_model,
    model = model
  )

  #TODO Make test for the linear model, checking whether predict() gives the
  # same predictions as using the coefficients

  # Checks that predict_model gives correct format
  test_predict_model(
    x_test = head(internal$data$x_train, 2),
    predict_model = predict_model,
    model = model,
    internal = internal
  )

  timing_list$test_prediction <- Sys.time()


  # Sets up the Shapley (sampling) framework and prepares the
  # conditional expectation computation for the chosen approach
  # Note: model and predict_model are ONLY used by the AICc-methods of approach empirical to find optimal parameters
  internal <- setup_computation_linear_gaussian(internal)

  timing_list$setup_computation <- Sys.time()


  # Compute the v(S):
  # Get the samples for the conditional distributions with the specified approach
  # Predict with these samples
  # Perform MC integration on these to estimate the conditional expectation (v(S))
  vS_list <- compute_vS(internal, model, predict_model)

  timing_list$compute_vS <- Sys.time()


  # Compute Shapley values based on conditional expectations (v(S))
  # Organize function output
  output <- finalize_explanation(
    vS_list = vS_list,
    internal = internal
  )

  timing_list$shapley_computation <- Sys.time()

  if (timing == TRUE) {
    output$timing <- compute_time(timing_list)
  }

  # Temporary to avoid failing tests

  output$internal$objects$id_combination_mapper_dt <- NULL
  output$internal$objects$cols_per_horizon <- NULL
  output$internal$objects$W_list <- NULL

  return(output)
}
