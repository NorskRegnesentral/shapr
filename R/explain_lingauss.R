#' Explain the output of a linear model with Gaussian distributed features, using Shapley values values
#'
#' @inheritParams explain
#'
#' @param n_permutations Integer. Number of permutations to sample when estimating the Shapley values with
#' the permutation approach. If `NULL`, all permutations are used, which corresponds to exact computation of the
#' Shapley values (under the linear + Gaussian assumption).
#' The maximum number of permutations equals `m!`, where `m` is the number of features.
#'
#' @export
#'
#' @author Martin Jullum
#'
explain_lingauss <- function(model,
                             x_explain,
                             x_train,
                             n_permutations = NULL,
                             group = NULL,
                             seed = 1,
                             predict_model = NULL,
                             get_model_specs = NULL,
                             timing = TRUE,
                             ...) { # ... is further arguments passed to specific approaches

  timing_list <- list(
    init_time = Sys.time()
  )

  set.seed(seed)

  # Gets and check feature specs from the model
  feature_specs <- get_feature_specs(get_model_specs, model)

  lingauss_model_coef <- get_linear_coef(model, feature_specs)

  null_object <- NULL
  # Sets up and organizes input parameters
  # Checks the input parameters and their compatability
  # Checks data/model compatability
  internal <- setup(
    type = "lingauss",
    x_train = x_train,
    x_explain = x_explain,
    approach = "gaussian", # always set to "gaussian" although we never really use this argument for lingauss
    prediction_zero = 0, # Never used, we extract this from the model object instead.
    n_permutations = n_permutations,
    group = group,
    n_samples = 1, # Not applicable for the lingauss method as no sampling is done
    seed = seed,
    keep_samp_for_vS = FALSE, # Not applicable for the lingauss method as no sampling is done
    feature_specs = feature_specs,
    timing = timing,
    lingauss_model_coef = lingauss_model_coef,
    ...
  )

  timing_list$setup <- Sys.time()

  # Gets predict_model (if not passed to explain)
  predict_model <- get_predict_model(
    predict_model = predict_model,
    model = model
  )

  # Checks that predict_model gives correct format
  test_predict_lingauss_model(
    x_test = head(internal$data$x_train, 2),
    predict_model = predict_model,
    model = model,
    lingauss_model_coef = lingauss_model_coef,
    internal = internal
  )

  timing_list$test_prediction <- Sys.time()

  # Computes the necessary objects for the linear Gaussian approach
  internal <- shapley_setup_lingauss(internal)

  timing_list$setup_computation <- Sys.time()

  internal <- compute_lingauss_Tmu_Tx(internal, ...)

  timing_list$compute_Tmu_Tx <- Sys.time()


  # Compute Shapley values with the linear Gaussian method
  output <- compute_shapley_lingauss(internal = internal)

  timing_list$shapley_computation <- Sys.time()

  if (timing == TRUE) {
    output$timing <- compute_time(timing_list)
  }


  return(output)
}


#' Function for Lightning fast explanation of linear model with gaussian data from pre-computed explain_lingauss object
#'
#' @param explain_lingauss_object output from explain_lingauss
#' @inherit explain
explain_lingauss_precomputed <- function(explain_lingauss_object,
                                         x_explain,
                                         timing = TRUE) {
  timing_list <- list(
    init_time = Sys.time()
  )

  # Check that object is of correct class
  if (!inherits(explain_lingauss_object, "lingauss")) {
    stop("'object' must be of the shapr class lingauss, as outputted by explain_ligauss()")
  }

  internal <- explain_lingauss_object$internal
  internal$data <- get_data(
    internal$data$x_train,
    x_explain
  )

  check_data(internal)

  internal <- get_extra_parameters(internal) # Needed to reset the n_explain parameter

  timing_list$setup <- Sys.time()

  output <- compute_shapley_lingauss(internal = internal)

  timing_list$shapley_computation <- Sys.time()

  if (timing == TRUE) {
    output$timing <- compute_time(timing_list)
  }


  return(output)
}
