#' Explain a forecast from time series models with dependence-aware (conditional/observational) Shapley values
#'
#' @description Computes dependence-aware Shapley values for observations in `explain_idx` from the specified
#' `model` by using the method specified in `approach` to estimate the conditional expectation.
#' See
# nolint start
#' \href{https://martinjullum.com/publication/aas-2021-explaining/aas-2021-explaining.pdf}{Aas, et. al (2021)}
# nolint end
#' for a thorough introduction to dependence-aware prediction explanation with Shapley values.
#'
#' @inheritParams explain
#' @param y Matrix, data.frame/data.table or a numeric vector.
#' Contains the endogenous variables used to estimate the (conditional) distributions
#' needed to properly estimate the conditional expectations in the Shapley formula
#' including the observations to be explained.
#'
#' @param xreg Matrix, data.frame/data.table or a numeric vector.
#' Contains the exogenous variables used to estimate the (conditional) distributions
#' needed to properly estimate the conditional expectations in the Shapley formula
#' including the observations to be explained.
#' As exogenous variables are used contemporaneously when producing a forecast,
#' this item should contain nrow(y) + horizon rows.
#'
#' @param train_idx Numeric vector.
#' The row indices in data and reg denoting points in time to use when estimating the conditional expectations in
#' the Shapley value formula.
#' If `train_idx = NULL` (default) all indices not selected to be explained will be used.
#'
#' @param explain_idx Numeric vector.
#' The row indices in data and reg denoting points in time to explain.
#'
#' @param explain_y_lags Numeric vector.
#' Denotes the number of lags that should be used for each variable in `y` when making a forecast.
#'
#' @param explain_xreg_lags Numeric vector.
#' If `xreg != NULL`, denotes the number of lags that should be used for each variable in `xreg` when making a forecast.
#'
#' @param horizon Numeric.
#' The forecast horizon to explain. Passed to the `predict_model` function.
#'
#' @param group_lags Logical.
#' If `TRUE` all lags of each variable are grouped together and explained as a group.
#' If `FALSE` all lags of each variable are explained individually.
#'
#' @inherit explain return author references
#' @inheritDotParams setup_approach.categorical
#' @inheritDotParams setup_approach.copula
#' @inheritDotParams setup_approach.ctree
#' @inheritDotParams setup_approach.empirical
#' @inheritDotParams setup_approach.gaussian
#' @inheritDotParams setup_approach.independence
#' @inheritDotParams setup_approach.timeseries
#' @inheritDotParams setup_approach.vaeac
#'
#' @details This function explains a forecast of length `horizon`. The argument `train_idx`
#' is analogous to x_train in `explain()`, however, it just contains the time indices of where
#' in the data the forecast should start for each training sample. In the same way `explain_idx`
#' defines the time index (indices) which will precede a forecast to be explained.
#'
#' As any autoregressive forecast model will require a set of lags to make a forecast at an
#' arbitrary point in time, `explain_y_lags` and `explain_xreg_lags` define how many lags
#' are required to "refit" the model at any given time index. This allows the different
#' approaches to work in the same way they do for time-invariant models.
#'
#' See the \href{https://norskregnesentral.github.io/shapr/articles/general_usage.html#forecasting}{
#' forecasting section of the general usages} for further details.
#'
#' @author Jon Lachmann, Martin Jullum
#' @examples
#' \donttest{
#' # Load example data
#' data("airquality")
#' data <- data.table::as.data.table(airquality)
#'
#' # Fit an AR(2) model.
#' model_ar_temp <- ar(data$Temp, order = 2)
#'
#' # Calculate the zero prediction values for a three step forecast.
#' p0_ar <- rep(mean(data$Temp), 3)
#'
#' # Empirical approach, explaining forecasts starting at T = 152 and T = 153.
#' explain_forecast(
#'   model = model_ar_temp,
#'   y = data[, "Temp"],
#'   train_idx = 2:151,
#'   explain_idx = 152:153,
#'   explain_y_lags = 2,
#'   horizon = 3,
#'   approach = "empirical",
#'   phi0 = p0_ar,
#'   group_lags = FALSE
#' )
#' }
#'
#' @export
explain_forecast <- function(model,
                             y,
                             xreg = NULL,
                             train_idx = NULL,
                             explain_idx,
                             explain_y_lags,
                             explain_xreg_lags = explain_y_lags,
                             horizon,
                             approach,
                             phi0,
                             max_n_coalitions = NULL,
                             iterative = NULL,
                             group_lags = TRUE,
                             group = NULL,
                             n_MC_samples = 1e3,
                             seed = NULL,
                             predict_model = NULL,
                             get_model_specs = NULL,
                             verbose = "basic",
                             extra_computation_args = list(),
                             iterative_args = list(),
                             output_args = list(),
                             ...) {
  init_time <- Sys.time()

  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Gets and check feature specs from the model
  feature_specs <- get_feature_specs(get_model_specs, model)

  # Set up default values for train_idx if it is not explicitly set by the user.
  if (is.null(train_idx)) {
    train_idx <- seq.int(from = max(c(explain_y_lags, explain_xreg_lags)), to = nrow(y))[-explain_idx]
  }

  # Sets up and organizes input parameters
  # Checks the input parameters and their compatability
  # Checks data/model compatability
  internal <- setup(
    approach = approach,
    phi0 = phi0,
    output_size = horizon,
    max_n_coalitions = max_n_coalitions,
    n_MC_samples = n_MC_samples,
    seed = seed,
    feature_specs = feature_specs,
    type = "forecast",
    horizon = horizon,
    iterative = iterative,
    init_time = init_time,
    y = y,
    xreg = xreg,
    train_idx = train_idx,
    explain_idx = explain_idx,
    explain_y_lags = explain_y_lags,
    explain_xreg_lags = explain_xreg_lags,
    group_lags = group_lags,
    group = group,
    verbose = verbose,
    extra_computation_args = extra_computation_args,
    iterative_args = iterative_args,
    output_args = output_args,
    ...
  )


  # Gets predict_model (if not passed to explain)
  predict_model <- get_predict_model(
    predict_model = predict_model,
    model = model
  )

  # Checks that predict_model gives correct format
  test_predict_model(
    x_test = head(internal$data$x_train, 2),
    predict_model = predict_model,
    model = model,
    internal = internal
  )

  internal$timing_list$test_prediction <- Sys.time()

  # Setup for approach
  internal <- setup_approach(internal, model = model, predict_model = predict_model)

  internal$main_timing_list <- internal$timing_list

  converged <- FALSE
  iter <- length(internal$iter_list)

  if (!is.null(seed)) {
    set.seed(seed)
  }

  cli_startup(internal, class(model), verbose)

  while (converged == FALSE) {
    cli_iter(verbose, internal, iter)

    internal$timing_list <- list(init = Sys.time())

    # setup the Shapley framework
    internal <- shapley_setup_forecast(internal)

    # May not need to be called here?
    internal <- setup_approach(internal, model = model, predict_model = predict_model)

    # Compute the vS
    vS_list <- compute_vS(internal, model, predict_model)

    # Compute Shapley values based on conditional expectations (v(S))
    internal <- compute_estimates(
      vS_list = vS_list,
      internal = internal
    )

    # Check convergence based on estimates and standard deviations (and thresholds)
    internal <- check_convergence(internal)

    # Save intermediate results
    save_results(internal)

    # Preparing parameters for next iteration (does not do anything if already converged)
    internal <- prepare_next_iteration(internal)

    # Printing iteration information
    print_iter(internal)

    ### Setting globals for to simplify the loop
    converged <- internal$iter_list[[iter]]$converged

    internal$timing_list$postprocess_res <- Sys.time()

    internal$iter_timing_list[[iter]] <- internal$timing_list

    iter <- iter + 1
  }

  internal$main_timing_list$main_computation <- Sys.time()

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


#' Set up data for explain_forecast
#'
#' @inheritParams explain_forecast
#'
#' @return A list containing
#' - The data.frames x_train and x_explain which holds the lagged data examples.
#' - A numeric, n_endo denoting how many columns are endogenous in x_train and x_explain.
#' - A list, group with groupings of each variable to explain per variable and not per variable and lag.
#' @keywords internal
get_data_forecast <- function(y, xreg, train_idx, explain_idx, explain_y_lags, explain_xreg_lags, horizon) {
  # Check data object type
  stop_message <- NULL
  if (!is.vector(y) &&
    !(is.matrix(y) && ncol(y) >= 1) &&
    !(is.data.frame(y) && ncol(y) >= 1)) {
    stop_message <- c(
      stop_message,
      "y should be a matrix or data.frame/data.table with one or more columns, or a numeric vector."
    )
  }
  if (!is.null(xreg) && !is.matrix(xreg) && !is.data.frame(xreg)) {
    stop_message <- c(stop_message, "xreg should be a matrix or a data.frame/data.table.")
  }
  if (!is.null(stop_message)) {
    names(stop_message) <- rep("!", length(stop_message))
    cli::cli_abort(stop_message)
  }

  if (is.vector(y)) {
    y <- as.matrix(y)
    colnames(y) <- "Y" # Currently we only allow a single endogenous variable.
  } else {
    y <- as.matrix(y)
  }
  if (ncol(y) != length(explain_y_lags)) {
    cli::cli_abort(
      paste0(
        "`y` has ", ncol(y), " columns (", paste0(colnames(y), collapse = ","), ").\n",
        "`explain_y_lags` has length ", length(explain_y_lags), ".\n",
        "These two should match.\n"
      )
    )
  }

  if (!is.null(xreg)) {
    xreg <- as.matrix(xreg)
    # Check column names
    if (all(is.null(colnames(xreg)))) {
      cli::cli_abort("`xreg` misses column names.")
    }

    if (ncol(xreg) != length(explain_xreg_lags)) {
      cli::cli_abort(
        paste0(
          "`xreg` has ", ncol(xreg), " columns (", paste0(colnames(xreg), collapse = ","), "). ",
          "`explain_xreg_lags` has length ", length(explain_xreg_lags), ". ",
          "These two should match. "
        )
      )
    }
    if (nrow(xreg) < max(c(train_idx, explain_idx)) + horizon) {
      cli::cli_abort("`xreg` must have at least as many observations as the data + the forecast horizon.")
    }
  } else {
    xreg <- matrix(NA, max(c(train_idx, explain_idx)) + horizon, 0)
  }

  max_lag <- max(c(explain_y_lags, explain_xreg_lags))

  if (any(c(train_idx, explain_idx) < max_lag) ||
    any(c(train_idx, explain_idx) > nrow(y))) {
    cli::cli_abort(paste0(
      "The train (`train_idx`) and explain (`explain_idx`) indices must fit in the lagged data.\n",
      "The lagged data begins at index ", max_lag, " and ends at index ", nrow(y), ". "
    ))
  }

  # Create a matrix and groups of all lagged data.
  data_reg <- as.matrix(cbind(y, xreg[seq_len(nrow(y)), , drop = FALSE]))
  data_lag <- lag_data(data_reg, c(explain_y_lags, explain_xreg_lags))

  # Create a matrix and groups of the forecasted values of the exogenous data.
  reg_fcast <- reg_forecast_setup(xreg[seq.int(to = max(c(train_idx, explain_idx)) + horizon, from = max_lag + 1), ,
    drop = FALSE
  ], horizon, data_lag$group)

  if (ncol(data_lag$lagged) == 0 && ncol(reg_fcast$fcast) == 0) {
    cli::cli_abort("`explain_y_lags=0` is not allowed for models without exogeneous variables.")
  }

  # Select the train and explain sets from the data and exogenous forecast values.
  train_idx <- train_idx - max_lag + 1
  explain_idx <- explain_idx - max_lag + 1
  return(list(
    y = y,
    xreg = xreg,
    group = reg_fcast$group,
    horizon_group = reg_fcast$horizon_group,
    shap_names = names(data_lag$group),
    n_endo = ncol(data_lag$lagged),
    x_train = cbind(
      data.table::as.data.table(data_lag$lagged[train_idx, , drop = FALSE]),
      data.table::as.data.table(reg_fcast$fcast[train_idx, , drop = FALSE])
    ),
    x_explain = cbind(
      data.table::as.data.table(data_lag$lagged[explain_idx, , drop = FALSE]),
      data.table::as.data.table(reg_fcast$fcast[explain_idx, , drop = FALSE])
    )
  ))
}

#' Lag a matrix of variables a specific number of lags for each variables.
#'
#' @param x The matrix of variables (one variable per column).
#' @param lags A numeric vector denoting how many lags each variable should have.
#'
#' @return A list with two items
#' - A matrix, lagged with the lagged data.
#' - A list, group, with groupings of the lagged data per variable.
#' @keywords internal
lag_data <- function(x, lags) {
  lagged_obs <- nrow(x) - max(lags) + 1
  lagged <- matrix(NA, lagged_obs, 0)
  group <- list()
  names <- character()
  for (i in seq_len(ncol(x))) {
    if (lags[i] != 0) {
      names_i <- paste0(colnames(x)[i], ".", seq_len(lags[i]))
      names <- c(names, names_i)

      lagged_i <- embed(x[, i], lags[i])
      lagged <- cbind(lagged, lagged_i[seq.int(to = nrow(lagged_i), length.out = lagged_obs), , drop = FALSE])

      group[[colnames(x)[i]]] <- names_i
    }
  }
  colnames(lagged) <- names
  return(list(lagged = lagged, group = group))
}

#' Set up exogenous regressors for explanation in a forecast model.
#'
#' @inheritParams explain_forecast
#' @param x A matrix with the exogenous variables.
#' @param group The list of endogenous groups, to append exogenous groups to.
#'
#' @return A list containing
#' - fcast A matrix containing the exogenous observations needed for each observation.
#' - group The list group with the exogenous groups appended.
#' @keywords internal
reg_forecast_setup <- function(x, horizon, group) {
  fcast <- matrix(NA, nrow(x) - horizon + 1, 0)
  names <- character()
  horizon_group <- lapply(seq_len(horizon), function(i) names(group)[!(names(group) %in% colnames(x))])
  for (i in seq_len(ncol(x))) {
    names_i <- paste0(colnames(x)[i], ".F", seq_len(horizon))
    names <- c(names, names_i)

    fcast_i <- embed(x[, i], horizon)[, rev(seq_len(horizon)), drop = FALSE]
    fcast <- cbind(fcast, fcast_i)

    # Append group names if the exogenous regressor also has lagged values.
    for (h in seq_len(horizon)) {
      group[[paste0(colnames(x)[i], ".", h)]] <- c(group[[colnames(x)[i]]], names_i[seq_len(h)])
      horizon_group[[h]] <- c(horizon_group[[h]], paste0(colnames(x)[i], ".", h))
    }
    group[[colnames(x)[i]]] <- NULL
  }
  colnames(fcast) <- names
  return(list(fcast = fcast, group = group, horizon_group = horizon_group))
}

#' Set up user provided groups for explanation in a forecast model.
#'
#' @param group The list of groups to be explained.
#' @param horizon_features A list of features per horizon, to split appropriate groups over.
#'
#' @return A list containing
#' - group The list group with entries that differ per horizon split accordingly.
#' - horizon_group A list of which groups are applicable per horizon.
#' @keywords internal
group_forecast_setup <- function(group, horizon_features) {
  horizon_group <- vector("list", length(horizon_features))
  new_group <- list()

  for (i in seq_along(group)) {
    if (!all(group[[i]] %in% horizon_features[[1]])) {
      for (h in seq_along(horizon_group)) {
        new_name <- paste0(names(group)[i], ".", h)
        new_group[[new_name]] <- group[[i]][group[[i]] %in% horizon_features[[h]]]
        horizon_group[[h]] <- c(horizon_group[[h]], new_name)
      }
    } else {
      name <- names(group)[i]
      new_group[[name]] <- group[[i]]
      for (h in seq_along(horizon_group)) {
        horizon_group[[h]] <- c(horizon_group[[h]], name)
      }
    }
  }
  return(list(group = new_group, horizon_group = horizon_group))
}
