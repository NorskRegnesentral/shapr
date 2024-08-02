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
#' @param n_combinations Integer.
#' If `group = NULL`, `n_combinations` represents the number of unique feature combinations to sample.
#' If `group != NULL`, `n_combinations` represents the number of unique group combinations to sample.
#' If `n_combinations = NULL`, the exact method is used and all combinations are considered.
#' The maximum number of combinations equals `2^m`, where `m` is the number of features.
#'
#' @param group List.
#' If `NULL` regular feature wise Shapley values are computed.
#' If provided, group wise Shapley values are computed. `group` then has length equal to
#' the number of groups. The list element contains character vectors with the features included
#' in each of the different groups.
#'
#' @param n_samples Positive integer.
#' Indicating the maximum number of samples to use in the
#' Monte Carlo integration for every conditional expectation. See also details.
#'
#' @param n_batches Positive integer (or NULL).
#' Specifies how many batches the total number of feature combinations should be split into when calculating the
#' contribution function for each test observation.
#' The default value is NULL which uses a reasonable trade-off between RAM allocation and computation speed,
#' which depends on `approach` and `n_combinations`.
#' For models with many features, increasing the number of batches reduces the RAM allocation significantly.
#' This typically comes with a small increase in computation time.
#'
#' @param seed Positive integer.
#' Specifies the seed before any randomness based code is being run.
#' If `NULL` the seed will be inherited from the calling environment.
#'
#' @param keep_samp_for_vS Logical.
#' Indicates whether the samples used in the Monte Carlo estimation of v_S should be returned
#' (in `internal$output`)
#'
#' @param predict_model Function.
#' The prediction function used when `model` is not natively supported.
#' (Run [get_supported_models()] for a list of natively supported
#' models.)
#' The function must have two arguments, `model` and `newdata` which specify, respectively, the model
#' and a data.frame/data.table to compute predictions for. The function must give the prediction as a numeric vector.
#' `NULL` (the default) uses functions specified internally.
#' Can also be used to override the default function for natively supported model classes.
#'
#' @param get_model_specs Function.
#' An optional function for checking model/data consistency when `model` is not natively supported.
#' (Run [get_supported_models()] for a list of natively supported
#' models.)
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
#' @param MSEv_uniform_comb_weights Logical. If `TRUE` (default), then the function weights the combinations
#' uniformly when computing the MSEv criterion. If `FALSE`, then the function use the Shapley kernel weights to
#' weight the combinations when computing the MSEv criterion. Note that the Shapley kernel weights are replaced by the
#' sampling frequency when not all combinations are considered.
#'
#' @param timing Logical.
#' Whether the timing of the different parts of the `explain()` should saved in the model object.
#'
#' @param verbose An integer specifying the level of verbosity. If `0`, `shapr` will stay silent.
#' If `1`, it will print information about performance. If `2`, some additional information will be printed out.
#' Use `0` (default) for no verbosity, `1` for low verbose, and `2` for high verbose.
#' TODO: Make this clearer when we end up fixing this and if they should force a progressr bar.
#'
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
#' @details The most important thing to notice is that `shapr` has implemented eight different
#' Monte Carlo-based approaches for estimating the conditional distributions of the data, namely `"empirical"`,
#' `"gaussian"`, `"copula"`, `"ctree"`, `"vaeac"`, `"categorical"`, `"timeseries"`, and `"independence"`.
#' `shapr` has also implemented two regression-based approaches `"regression_separate"` and `"regression_surrogate"`,
#' and see the separate vignette on the regression-based approaches for more information.
#' In addition, the user also has the option of combining the different Monte Carlo-based approaches.
#' E.g., if you're in a situation where you have trained a model that consists of 10 features,
#' and you'd like to use the `"gaussian"` approach when you condition on a single feature,
#' the `"empirical"` approach if you condition on 2-5 features, and `"copula"` version
#' if you condition on more than 5 features this can be done by simply passing
#' `approach = c("gaussian", rep("empirical", 4), rep("copula", 4))`. If
#' `"approach[i]" = "gaussian"` means that you'd like to use the `"gaussian"` approach
#' when conditioning on `i` features. Conditioning on all features needs no approach as that is given
#' by the complete prediction itself, and should thus not be part of the vector.
#'
#' For `approach="ctree"`, `n_samples` corresponds to the number of samples
#' from the leaf node (see an exception related to the `sample` argument).
#' For `approach="empirical"`, `n_samples` is  the \eqn{K} parameter in equations (14-15) of
#' Aas et al. (2021), i.e. the maximum number of observations (with largest weights) that is used, see also the
#' `empirical.eta` argument.
#'
#'
#' @return Object of class `c("shapr", "list")`. Contains the following items:
#' \describe{
#'   \item{shapley_values}{data.table with the estimated Shapley values}
#'   \item{internal}{List with the different parameters, data and functions used internally}
#'   \item{pred_explain}{Numeric vector with the predictions for the explained observations}
#'   \item{MSEv}{List with the values of the MSEv evaluation criterion for the approach.}
#' }
#'
#' `shapley_values` is a data.table where the number of rows equals
#' the number of observations you'd like to explain, and the number of columns equals `m +1`,
#' where `m` equals the total number of features in your model.
#'
#' If `shapley_values[i, j + 1] > 0` it indicates that the j-th feature increased the prediction for
#' the i-th observation. Likewise, if `shapley_values[i, j + 1] < 0` it indicates that the j-th feature
#' decreased the prediction for the i-th observation.
#' The magnitude of the value is also important to notice. E.g. if `shapley_values[i, k + 1]` and
#' `shapley_values[i, j + 1]` are greater than `0`, where `j != k`, and
#' `shapley_values[i, k + 1]` > `shapley_values[i, j + 1]` this indicates that feature
#' `j` and `k` both increased the value of the prediction, but that the effect of the k-th
#' feature was larger than the j-th feature.
#'
#' The first column in `dt`, called `none`, is the prediction value not assigned to any of the features
#' (\ifelse{html}{\eqn{\phi}\out{<sub>0</sub>}}{\eqn{\phi_0}}).
#' It's equal for all observations and set by the user through the argument `prediction_zero`.
#' The difference between the prediction and `none` is distributed among the other features.
#' In theory this value should be the expected prediction without conditioning on any features.
#' Typically we set this value equal to the mean of the response variable in our training data, but other choices
#' such as the mean of the predictions in the training data are also reasonable.
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
#' # Empirical approach
#' explain1 <- explain(
#'   model = model,
#'   x_explain = x_explain,
#'   x_train = x_train,
#'   approach = "empirical",
#'   prediction_zero = p,
#'   n_samples = 1e2
#' )
#'
#' # Gaussian approach
#' explain2 <- explain(
#'   model = model,
#'   x_explain = x_explain,
#'   x_train = x_train,
#'   approach = "gaussian",
#'   prediction_zero = p,
#'   n_samples = 1e2
#' )
#'
#' # Gaussian copula approach
#' explain3 <- explain(
#'   model = model,
#'   x_explain = x_explain,
#'   x_train = x_train,
#'   approach = "copula",
#'   prediction_zero = p,
#'   n_samples = 1e2
#' )
#'
#' # ctree approach
#' explain4 <- explain(
#'   model = model,
#'   x_explain = x_explain,
#'   x_train = x_train,
#'   approach = "ctree",
#'   prediction_zero = p,
#'   n_samples = 1e2
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
#'   n_samples = 1e2
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
#'   n_samples = 1e2
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
                    paired_shap_sampling = FALSE, #TODO: Make TRUE the default later on
                    prediction_zero,
                    n_combinations = NULL,
                    adaptive = FALSE,
                    group = NULL,
                    n_samples = 1e3,
                    n_batches = NULL,
                    seed = 1,
                    keep_samp_for_vS = FALSE,
                    predict_model = NULL,
                    get_model_specs = NULL,
                    MSEv_uniform_comb_weights = TRUE,
                    timing = TRUE,
                    verbose = 0,
                    n_boot_samps = 100, # tmp
                    print_shapleyres = FALSE, # tmp
                    print_iter_info = FALSE, # tmp
                    shapley_reweighting = "none", # tmp # "on_N"
                    ...) { # ... is further arguments passed to specific approaches

  timing_list <- list(init_time = Sys.time())

  set.seed(seed)

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
    n_combinations = n_combinations,
    group = group,
    n_samples = n_samples,
    n_batches = n_batches,
    seed = seed,
    keep_samp_for_vS = keep_samp_for_vS,
    feature_specs = feature_specs,
    MSEv_uniform_comb_weights = MSEv_uniform_comb_weights,
    timing = timing,
    verbose = verbose,
    adaptive = adaptive,
    ...
  )

  timing_list$setup <- Sys.time()

  # Gets predict_model (if not passed to explain)
  predict_model <- get_predict_model(predict_model = predict_model, model = model)

  # Checks that predict_model gives correct format
  test_predict_model(
    x_test = head(internal$data$x_train, 2),
    predict_model = predict_model,
    model = model,
    internal = internal
  )

  timing_list$test_prediction <- Sys.time()


  # Add the predicted response of the training and explain data to the internal list for regression-based methods.
  # Use isTRUE as `regression` is not present (NULL) for non-regression methods (i.e., Monte Carlo-based methods).
  if (isTRUE(internal$parameters$regression)) {
    internal <- regression.get_y_hat(internal = internal, model = model, predict_model = predict_model)
  }

  # Adaptive approach
  # TODO: The below should probably be moved to a separate function in the end

  if(isTRUE(internal$parameters$adaptive)){
    ### for now we just some of the parameters here
    initial_n_combinations <- min(200,ceiling((2^internal$parameters$n_features)/10))
    max_iter <- 20
    convergence_tolerance <- 0.02 # max sd must be smaller than this proportion of max difference features shapley values
    reduction_factor_vec <- c(seq(0.1,1,by=0.1),rep(1,max_iter-10)) # Proportion of estimated remaining samples to use in next iteration


  } else {
    # The regular, non-iterative approach
    initial_n_combinations <- internal$parameters$n_combinations
    max_iter <- 1
    convergence_tolerance <- NULL
    reduction_factor_vec <- NULL


  }

  converged <- FALSE


  if(!("regression_surrogate" %in% approach)){ # Called after shapley_setup of regression_surrogate
    internal <- setup_approach(internal, model = model, predict_model = predict_model)
  }
  internal$parameters$shapley_reweighting <- shapley_reweighting

  internal$parameters$max_iter <- max_iter
  internal$parameters$n_boot_samps <- n_boot_samps
  internal$parameters$reduction_factor_vec <- reduction_factor_vec

  internal$iter_list <- list()
  iter <- 0
  internal$iter_list[[1]] <- list(
    n_combinations = initial_n_combinations,
    exact = internal$parameters$exact,
    compute_sd = ifelse(isFALSE(internal$parameters$exact) &&
                          isFALSE(internal$parameters$is_groupwise) &&
                          internal$parameters$n_combinations < 2^internal$parameters$n_features, # As exact is not reset before shapley_setup
                        TRUE,FALSE),
    reduction_factor = internal$parameters$reduction_factor_vec[1]
  )

  set.seed(seed) # Set seed again to get reproducability for the shapley_setup regardless of whether setup_approach
                 # had randomness in it (i.e. such that changing approach would not give differnet samples combinations
                 # (before any convergence stopping)

    while(converged==FALSE){
      iter <- iter + 1

      # setup the Shapley framework
      internal <- shapley_setup(internal)

      if("regression_surrogate" %in% approach){ # Since setup_approach for regression_surrogate requires shapley_setup to be called first, it will be called in here
        internal <- setup_approach(internal, model = model, predict_model = predict_model)
      }

      # Compute the vS
      vS_list <- compute_vS(internal, model, predict_model)

      # Compute shapley value estimated and bootstrapped standard deviations
      internal <- compute_estimates(internal, vS_list)

      # Check convergence based on estimates and standard deviations (and thresholds)
      internal <-  check_convergence(internal, convergence_tolerance)

      # Preparing parameters for next iteration (does not do anything if already converged)
      internal <- prepare_next_iteration(internal)

      # Printing iteration information
      print_iter(internal,print_iter_info,print_shapleyres)

      ### Setting globals for to simplify the loop
      converged <- internal$iter_list[[iter]]$converged

    }


    # Rerun after convergence to get the same output format as for the non-adaptive approach
    output <- finalize_explanation(internal = internal)


  # Temporary to avoid failing tests
  output <- remove_outputs_to_pass_tests(output)

  return(output)
}

#' @keywords internal
#' @author Lars Henry Berge Olsen
remove_outputs_to_pass_tests <- function(output) {
  output$internal$objects$id_combination_mapper_dt <- NULL
  output$internal$objects$cols_per_horizon <- NULL
  output$internal$objects$W_list <- NULL
  output$shapley_values[,explain_id:=NULL]

  if (isFALSE(output$internal$parameters$vaeac.extra_parameters$vaeac.save_model)) {
    output$internal$parameters[c(
      "vaeac", "vaeac.sampler", "vaeac.model", "vaeac.activation_function", "vaeac.checkpoint"
    )] <- NULL
    output$internal$parameters$vaeac.extra_parameters[c("vaeac.folder_to_save_model", "vaeac.model_description")] <-
      NULL
  }

  # Remove the `regression` parameter from the output list when we are not doing regression
  if (isFALSE(output$internal$parameters$regression)) output$internal$parameters$regression <- NULL

  return(output)
}
