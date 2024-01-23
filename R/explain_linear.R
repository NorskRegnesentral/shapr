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
                           n_permutations = NULL,
                           group = NULL,
                           n_batches = NULL,
                           seed = 1,
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
    paired_shap_sampling = TRUE, # Always use paired sampling since simplified computation of the required Q and U objects requires it
    prediction_zero = 0, # Never used, we extract this from the model object instead.
    n_combinations = NULL, # We always set the n_permutations instead
    n_permutations = n_permutations,
    group = group,
    n_samples = 1, # Not applicable for the linear_gaussian method as no sampling is done
    n_batches = n_batches,
    seed = seed,
    keep_samp_for_vS = FALSE, # Not applicable for the linear_gaussian method as no sampling is done
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

  # Checks that predict_model gives correct format
  test_predict_linear_model(
    x_test = head(internal$data$x_train, 2),
    predict_model = predict_model,
    model = model,
    linear_model_coef = linear_model_coef,
    internal = internal
  )

  timing_list$test_prediction <- Sys.time()

  # Computes the necessary objects for the linear Gaussian approach
  internal <- shapley_setup_linear_gaussian(internal)

  timing_list$setup_computation <- Sys.time()

  internal <- compute_linear_gaussian_Tmu_Tx(internal,...)

  timing_list$compute_Tmu_Tx <- Sys.time()


  # Compute Shapley values with the linear Gaussian method
  output <- compute_shapley_linear_gaussian(internal = internal)

  timing_list$shapley_computation <- Sys.time()

  if (timing == TRUE) {
    output$timing <- compute_time(timing_list)
  }


  return(output)
}
