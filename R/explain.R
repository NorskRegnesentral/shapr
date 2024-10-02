#' Explain the output of machine learning models with more accurately estimated Shapley values
#'
#' @description Computes dependence-aware Shapley values for observations in `x_explain` from the specified
#' `model` by using the method specified in `approach` to estimate the conditional expectation.
#'
#' @param x_train Matrix or data.frame/data.table.
#' Contains the data used to estimate the (conditional) distributions for the features
#' needed to properly estimate the conditional expectations in the Shapley formula.
#'
#' @param x_explain A matrix or data.frame/data.table.
#' Contains the the features, whose predictions ought to be explained.
#'
#' @param model The model whose predictions we want to explain.
#' Run [get_supported_models()]
#' for a table of which models `explain` supports natively. Unsupported models
#' can still be explained by passing `predict_model` and (optionally) `get_model_specs`,
#' see details for more information.
#'
#' @param approach Character vector of length `1` or one less than the number of features.
#' All elements should, either be `"gaussian"`, `"copula"`, `"empirical"`, `"ctree"`, `"vaeac"`,
#' `"categorical"`, `"timeseries"`, `"independence"`, `"regression_separate"`, or `"regression_surrogate"`.
#' The two regression approaches can not be combined with any other approach. See details for more information.
#'
#' @param prediction_zero Numeric.
#' The prediction value for unseen data, i.e. an estimate of the expected prediction without conditioning on any
#' features.
#' Typically we set this value equal to the mean of the response variable in our training data, but other choices
#' such as the mean of the predictions in the training data are also reasonable.
#'
#' @param max_n_coalitions Integer.
#' The upper limit on the number of unique feature/group coalitions to use in the adaptive procedure
#' (if `adaptive = TRUE`).
#' If `adaptive = FALSE` it represents the number of feature/group coalitions to use directly.
#' The quantity refers to the number of unique feature coalitions if `group = NULL`,
#' and group coalitions if `group != NULL`.
#' `max_n_coalitions = NULL` corresponds to `max_n_coalitions=2^n_features`.
#'
#' @param group List.
#' If `NULL` regular feature wise Shapley values are computed.
#' If provided, group wise Shapley values are computed. `group` then has length equal to
#' the number of groups. The list element contains character vectors with the features included
#' in each of the different groups.
#'
#' @param n_MC_samples Positive integer.
#' Indicating the maximum number of samples to use in the  Monte Carlo integration for every conditional expectation.
#' For `approach="ctree"`, `n_MC_samples` corresponds to the number of samples
#' from the leaf node (see an exception related to the `ctree.sample` argument [shapr::setup_approach.ctree()]).
#' For `approach="empirical"`, `n_MC_samples` is  the \eqn{K} parameter in equations (14-15) of
#' Aas et al. (2021), i.e. the maximum number of observations (with largest weights) that is used, see also the
#' `empirical.eta` argument [shapr::setup_approach.empirical()].
#'
#' @param seed Positive integer.
#' Specifies the seed before any randomness based code is being run.
#' If `NULL` no seed is set in the calling environment.
#'
#' @param keep_samp_for_vS Logical.
#' Indicates whether the samples used in the Monte Carlo estimation of v_S should be returned (in `internal$output`).
#' Not used for `approach="regression_separate"` or `approach="regression_surrogate"`.
#'
#' @param predict_model Function.
#' The prediction function used when `model` is not natively supported.
#' (Run [get_supported_models()] for a list of natively supported models.)
#' The function must have two arguments, `model` and `newdata` which specify, respectively, the model
#' and a data.frame/data.table to compute predictions for.
#' The function must give the prediction as a numeric vector.
#' `NULL` (the default) uses functions specified internally.
#' Can also be used to override the default function for natively supported model classes.
#'
#' @param get_model_specs Function.
#' An optional function for checking model/data consistency when `model` is not natively supported.
#' (Run [get_supported_models()] for a list of natively supported models.)
#' The function takes `model` as argument and provides a list with 3 elements:
#' \describe{
#'   \item{labels}{Character vector with the names of each feature.}
#'   \item{classes}{Character vector with the classes of each features.}
#'   \item{factor_levels}{Character vector with the levels for any categorical features.}
#' }
#' If `NULL` (the default) internal functions are used for natively supported model classes, and the checking is
#' disabled for unsupported model classes.
#' Can also be used to override the default function for natively supported model classes.
#'
#' @param MSEv_uniform_comb_weights Logical.
#' If `TRUE` (default), then the function weights the coalitions uniformly when computing the MSEv criterion.
#' If `FALSE`, then the function use the Shapley kernel weights to weight the coalitions when computing the MSEv
#' criterion.
#' Note that the Shapley kernel weights are replaced by the sampling frequency when not all coalitions are considered.
#'
#' @param verbose String vector or NULL.
#' Specifies the verbosity (printout detail level) through one or more of strings `"basic"`, `"convergence"`
#'  `"shapley"`  and `"vS_details"`.
#' `"basic"` (default) displays basic information about the estimation and where in the calculation process the function
#' currently is.
#' #' `"convergence"` displays information on how close to convergence the Shapley value estimates are
#' (only when `adaptive = TRUE`) .
#' `"shapley"` displays intermediate Shapley value estimates and standard deviations (only when `adaptive = TRUE`)
#' + the final estimates.
#' `"vS_details"` displays information about the v_S estimates.
#' This is most relevant for `approach %in% c("regression_separate", "regression_surrogate", "vaeac"`).
#' `NULL` means no printout.
#' Note that any combination of four strings can be used.
#' E.g. `verbose = c("basic", "vS_details")` will display basic information + details about the vS estimation process.
#'
#' @param paired_shap_sampling Logical.
#' If `TRUE` (default), paired versions of all sampled coalitions are also included in the computation.
#' That is, if there are 5 features and e.g. coalitions (1,3,5) are sampled, then also coalition (2,4) is used for
#' computing the Shapley values. This is done to reduce the variance of the Shapley value estimates.
#'
#' @param adaptive Logical or NULL
#' If `NULL` (default), the argument is set to `TRUE` if there are more than 5 features/groups, and `FALSE` otherwise.
#' If eventually `TRUE`, the Shapley values are estimated adaptively in an iterative manner.
#' This provides sufficiently accurate Shapley value estimates faster.
#' First an initial number of coalitions is sampled, then bootsrapping is used to estimate the variance of the Shapley
#' values.
#' A convergence criterion is used to determine if the variances of the Shapley values are sufficently small.
#' If the variances are too high, we estimate the number of required samples to reach convergence, and thereby add more
#' coalitions.
#' The process is repeated until the variances are below the threshold.
#' Specifics related to the adaptive process and convergence criterion are set through `adaptive_arguments`.
#'
#' @param adaptive_arguments Named list.
#' Specifices the arguments for the adaptive procedure.
#' See [shapr::get_adaptive_arguments_default()] for description of the arguments and their default values.

#' @param print_shapleyres TODO: move to verbose
#'
#' @param print_iter_info TODO: move to verbose
#'
#' @param shapley_reweighting String.
#' How to reweight the sampling frequency weights in the kernelSHAP solution after sampling, with the aim of reducing
#' the randomness and thereby the variance of the Shapley value estimates.
#' One of `'none'`, `'on_N'`, `'on_all'`, `'on_all_cond'` (default).
#' `'none'` means no reweighting, i.e. the sampling frequency weights are used as is.
#' `'on_coal_size'` means the sampling frequencies are averaged over all coalitions of the same size.
#' `'on_N'` means the sampling frequencies are averaged over all coalitions with the same original sampling
#' probabilities.
#' `'on_all'` means the original sampling probabilities are used for all coalitions.
#' `'on_all_cond'` means the original sampling probabilities are used for all coalitions, while adjusting for the
#' probability that they are sampled at least once.
#' This method is preferred as it has performed the best in simulation studies.
#'
#' @param prev_shapr_object `shapr` object or string.
#' If an object of class `shapr` is provided or string with a path to where intermediate results are strored,
#' then the function will use the previous object to continue the computation.
#' This is useful if the computation is interrupted or you want higher accuracy than already obtained, and therefore
#' want to continue the adaptive estimation. See the vignette for examples.
#' @param ... Further arguments passed to specific approaches
#'
#' @inheritDotParams setup_approach.empirical
#' @inheritDotParams setup_approach.independence
#' @inheritDotParams setup_approach.gaussian
#' @inheritDotParams setup_approach.copula
#' @inheritDotParams setup_approach.ctree
#' @inheritDotParams setup_approach.vaeac
#' @inheritDotParams setup_approach.categorical
#' @inheritDotParams setup_approach.regression_separate
#' @inheritDotParams setup_approach.regression_surrogate
#' @inheritDotParams setup_approach.timeseries
#'
#' @details The `shapr` package implements kernelSHAP estimation of dependence-aware Shapley values with
#' eight different Monte Carlo-based approaches for estimating the conditional distributions of the data, namely
#' `"empirical"`, `"gaussian"`, `"copula"`, `"ctree"`, `"vaeac"`, `"categorical"`, `"timeseries"`, and `"independence"`.
#' `shapr` has also implemented two regression-based approaches `"regression_separate"` and `"regression_surrogate"`.
#' It is also possible to combine the different approaches, see the vignettes for more information.
#'
#' The package is allows for parallelized computation with progress updates through the tightly connected
#' [future::future] and [progressr::progressr] packages. See the examples below.
#' For adaptive estimation (`adaptive=TRUE`), intermediate results may also be printed to the console
#' (according to the `verbose` argument).
#' Moreover, the intermediate results are written to disk.
#' This combined with adaptive estimation with (optional) intermediate results printed to the console (and temporary
#' written to disk, and batch computing of the v(S) values, enables fast and accurate estimation of the Shapley values
#' in a memory friendly manner.
#'
#' @return Object of class `c("shapr", "list")`. Contains the following items:
#' \describe{
#'   \item{shapley_values}{data.table with the estimated Shapley values with explained observation in the rows and
#'   features along the columns.
#'   The column `none` is the prediction not devoted to any of the features (given by the argument `prediction_zero`)}
#'   \item{shapley_values_sd}{data.table with the standard deviation of the Shapley values reflecting the uncertainty.
#'   Note that this only reflects the coalition sampling part of the kernelSHAP procedure, and is therefore by
#'   definition 0 when all coalitions is used.
#'   Only present when `adaptive = TRUE` and `adaptive_arguments$compute_sd=TRUE`.}
#'   \item{internal}{List with the different parameters, data, functions and other output used internally.}
#'   \item{pred_explain}{Numeric vector with the predictions for the explained observations}
#'   \item{MSEv}{List with the values of the MSEv evaluation criterion for the approach. See the
#'   \href{https://norskregnesentral.github.io/shapr/articles/understanding_shapr.html#msev-evaluation-criterion
#'   }{MSEv evaluation section in the vignette for details}.}
#'   \item{timing}{List containing timing information for the different parts of the computation.
#'   `init_time` and `end_time` gives the time stamps for the start and end of the computation.
#'   `total_time_secs` gives the total time in seconds for the complete execution of `explain()`.
#'   `main_timing_secs` gives the time in seconds for the main computations.
#'   `iter_timing_secs` gives for each iteration of the adaptive estimation, the time spent on the different parts
#'   adaptive estimation routine.}
#' }
#'
#' @examples
#'
#' # Load example data
#' data("airquality")
#' airquality <- airquality[complete.cases(airquality), ]
#' x_var <- c("Solar.R", "Wind", "Temp", "Month")
#' y_var <- "Ozone"
#'
#' # Split data into test- and training data
#' data_train <- head(airquality, -3)
#' data_explain <- tail(airquality, 3)
#'
#' x_train <- data_train[, x_var]
#' x_explain <- data_explain[, x_var]
#'
#' # Fit a linear model
#' lm_formula <- as.formula(paste0(y_var, " ~ ", paste0(x_var, collapse = " + ")))
#' model <- lm(lm_formula, data = data_train)
#'
#' # Explain predictions
#' p <- mean(data_train[, y_var])
#'
#' \dontrun{
#' # (Optionally) enable parallelization via the future package
#' if (requireNamespace("future", quietly = TRUE)) {
#'   future::plan("multisession", workers = 2)
#' }
#' }
#'
#' # (Optionally) enable progress updates within every iteration via the progressr package
#' if (requireNamespace("progressr", quietly = TRUE)) {
#'   progressr::handlers(global = TRUE)
#' }
#'
#' # Empirical approach
#' explain1 <- explain(
#'   model = model,
#'   x_explain = x_explain,
#'   x_train = x_train,
#'   approach = "empirical",
#'   prediction_zero = p,
#'   n_MC_samples = 1e2
#' )
#'
#' # Gaussian approach
#' explain2 <- explain(
#'   model = model,
#'   x_explain = x_explain,
#'   x_train = x_train,
#'   approach = "gaussian",
#'   prediction_zero = p,
#'   n_MC_samples = 1e2
#' )
#'
#' # Gaussian copula approach
#' explain3 <- explain(
#'   model = model,
#'   x_explain = x_explain,
#'   x_train = x_train,
#'   approach = "copula",
#'   prediction_zero = p,
#'   n_MC_samples = 1e2
#' )
#'
#' # ctree approach
#' explain4 <- explain(
#'   model = model,
#'   x_explain = x_explain,
#'   x_train = x_train,
#'   approach = "ctree",
#'   prediction_zero = p,
#'   n_MC_samples = 1e2
#' )
#'
#' # Combined approach
#' approach <- c("gaussian", "gaussian", "empirical")
#' explain5 <- explain(
#'   model = model,
#'   x_explain = x_explain,
#'   x_train = x_train,
#'   approach = approach,
#'   prediction_zero = p,
#'   n_MC_samples = 1e2
#' )
#'
#' # Print the Shapley values
#' print(explain1$shapley_values)
#'
#' # Plot the results
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   plot(explain1)
#'   plot(explain1, plot_type = "waterfall")
#' }
#'
#' # Group-wise explanations
#' group_list <- list(A = c("Temp", "Month"), B = c("Wind", "Solar.R"))
#'
#' explain_groups <- explain(
#'   model = model,
#'   x_explain = x_explain,
#'   x_train = x_train,
#'   group = group_list,
#'   approach = "empirical",
#'   prediction_zero = p,
#'   n_MC_samples = 1e2
#' )
#' print(explain_groups$shapley_values)
#'
#' # Separate and surrogate regression approaches with linear regression models.
#' # More complex regression models can be used, and we can use CV to
#' # tune the hyperparameters of the regression models and preprocess
#' # the data before sending it to the model. See the regression vignette
#' # (Shapley value explanations using the regression paradigm) for more
#' # details about the `regression_separate` and `regression_surrogate` approaches.
#' explain_separate_lm <- explain(
#'   model = model,
#'   x_explain = x_explain,
#'   x_train = x_train,
#'   prediction_zero = p,
#'   approach = "regression_separate",
#'   regression.model = parsnip::linear_reg()
#' )
#'
#' explain_surrogate_lm <- explain(
#'   model = model,
#'   x_explain = x_explain,
#'   x_train = x_train,
#'   prediction_zero = p,
#'   approach = "regression_surrogate",
#'   regression.model = parsnip::linear_reg()
#' )
#'
#' ## Adaptive estimation
#' # For illustration purposes only. By default not used for such small dimensions as here
#'
#' # Gaussian approach
#' explain_adaptive <- explain(
#'   model = model,
#'   x_explain = x_explain,
#'   x_train = x_train,
#'   approach = "gaussian",
#'   prediction_zero = p,
#'   n_MC_samples = 1e2,
#'   adaptive = TRUE,
#'   adaptive_arguments = list(initial_n_coalitions = 10)
#' )

#'
#' @export
#'
#' @author Martin Jullum, Lars Henry Berge Olsen
#'
#' @references
#'   Aas, K., Jullum, M., & L<U+00F8>land, A. (2021). Explaining individual predictions when features are dependent:
#'   More accurate approximations to Shapley values. Artificial Intelligence, 298, 103502.
explain <- function(model,
                    x_explain,
                    x_train,
                    approach,
                    paired_shap_sampling = TRUE,
                    prediction_zero,
                    max_n_coalitions = NULL,
                    adaptive = NULL,
                    group = NULL,
                    n_MC_samples = 1e3,
                    seed = 1,
                    keep_samp_for_vS = FALSE,
                    predict_model = NULL,
                    get_model_specs = NULL,
                    MSEv_uniform_comb_weights = TRUE,
                    verbose = "basic",
                    adaptive_arguments = list(),
                    print_shapleyres = FALSE, # tmp
                    print_iter_info = FALSE, # tmp
                    shapley_reweighting = "on_all_cond",
                    prev_shapr_object = NULL,
                    ...) { # ... is further arguments passed to specific approaches



  init_time <- Sys.time()

  if (!is.null(seed)) {
    set.seed(seed)
  }
  # Gets and check feature specs from the model
  feature_specs <- get_feature_specs(get_model_specs, model)

  # Sets up and organizes input parameters
  # Checks the input parameters and their compatability
  # Checks data/model compatability
  internal <- setup(
    x_train = x_train,
    x_explain = x_explain,
    approach = approach,
    paired_shap_sampling = paired_shap_sampling,
    prediction_zero = prediction_zero,
    max_n_coalitions = max_n_coalitions,
    group = group,
    n_MC_samples = n_MC_samples,
    seed = seed,
    keep_samp_for_vS = keep_samp_for_vS,
    feature_specs = feature_specs,
    MSEv_uniform_comb_weights = MSEv_uniform_comb_weights,
    verbose = verbose,
    adaptive = adaptive,
    adaptive_arguments = adaptive_arguments,
    shapley_reweighting = shapley_reweighting,
    init_time = init_time,
    prev_shapr_object = prev_shapr_object,
    ...
  )



  # Gets predict_model (if not passed to explain)
  predict_model <- get_predict_model(predict_model = predict_model, model = model)

  # Checks that predict_model gives correct format
  test_predict_model(
    x_test = head(internal$data$x_train, 2),
    predict_model = predict_model,
    model = model,
    internal = internal
  )

  internal$timing_list$test_prediction <- Sys.time()


  internal <- additional_regression_setup(internal, model = model, predict_model = predict_model)

  # Not called for approach %in% c("regression_surrogate","vaeac")
  internal <- setup_approach(internal, model = model, predict_model = predict_model)

  internal$main_timing_list <- internal$timing_list

  converged <- FALSE
  iter <- length(internal$iter_list)

  if (!is.null(seed)) {
    set.seed(seed)
  }

  while (converged == FALSE) {
    internal$timing_list <- list(init = Sys.time())

    # setup the Shapley framework
    internal <- shapley_setup(internal)

    # Only actually called for approach = regression_surrogate
    internal <- setup_approach(internal, model = model, predict_model = predict_model)

    # Compute the vS
    vS_list <- compute_vS(internal, model, predict_model)

    # Compute shapley value estimated and bootstrapped standard deviations
    internal <- compute_estimates(internal, vS_list)

    # Check convergence based on estimates and standard deviations (and thresholds)
    internal <- check_convergence(internal)

    # Save intermediate results
    save_results(internal)

    # Preparing parameters for next iteration (does not do anything if already converged)
    internal <- prepare_next_iteration(internal)

    # Printing iteration information
    print_iter(internal, print_iter_info, print_shapleyres)

    ### Setting globals for to simplify the loop
    converged <- internal$iter_list[[iter]]$converged

    internal$timing_list$postprocess_res <- Sys.time()

    internal$iter_timing_list[[iter]] <- internal$timing_list

    iter <- iter + 1
  }

  internal$main_timing_list$adaptive_estimation <- Sys.time()


  # Rerun after convergence to get the same output format as for the non-adaptive approach
  output <- finalize_explanation(internal = internal)

  internal$main_timing_list$finalize_explanation <- Sys.time()

  output$timing <- compute_time(internal)


  # Some cleanup when doing testing
  testing <- internal$parameters$testing
  if (isTRUE(testing)) {
    output <- testing_cleanup(output)
  }

  return(output)
}

#' Cleans out certain output arguments to allow perfect reproducability of the output
#'
#' @inheritParams default_doc_explain
#'
#' @export
#' @keywords internal
#' @author Lars Henry Berge Olsen, Martin Jullum
testing_cleanup <- function(output) {
  # Removing the timing of different function calls
  output$timing <- NULL

  # Clearing out the timing lists as well
  output$internal$main_timing_list <- NULL
  output$internal$iter_timing_list <- NULL
  output$internal$timing_list <- NULL

  # Removing paths to non-reproducable vaeac model objects
  if (isFALSE(output$internal$parameters$vaeac.extra_parameters$vaeac.save_model)) {
    output$internal$parameters[c(
      "vaeac", "vaeac.sampler", "vaeac.model", "vaeac.activation_function", "vaeac.checkpoint"
    )] <- NULL
    output$internal$parameters$vaeac.extra_parameters[c("vaeac.folder_to_save_model", "vaeac.model_description")] <-
      NULL
  }

  # Removing the fit times for regression surrogate models
  if ("regression_surrogate" %in% output$internal$parameters$approach) {
    # Deletes the fit_times for approach = regression_surrogate to make tests pass.
    # In the future we could delete this only when a new argument in explain called testing is TRUE
    output$internal$objects$regression.surrogate_model$pre$mold$blueprint$recipe$fit_times <- NULL
  }

  # Delete the saving_path
  output$internal$parameters$adaptive_arguments$saving_path <- NULL

  return(output)
}
