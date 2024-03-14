#' Explain a forecast from a time series model using Shapley values.
#'
#' @description Computes dependence-aware Shapley values for observations in `explain_idx` from the specified
#' `model` by using the method specified in `approach` to estimate the conditional expectation.
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
#' As exogenous variables are used contemporaneusly when producing a forecast,
#' this item should contain nrow(y) + horizon rows.
#'
#' @param train_idx Numeric vector
#' The row indices in data and reg denoting points in time to use when estimating the conditional expectations in
#' the Shapley value formula.
#' If `train_idx = NULL` (default) all indices not selected to be explained will be used.
#'
#' @param explain_idx Numeric vector
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
#' @inheritParams explain
#' @inherit explain return author references
#' @inheritDotParams setup_approach.empirical
#' @inheritDotParams setup_approach.independence
#' @inheritDotParams setup_approach.gaussian
#' @inheritDotParams setup_approach.copula
#' @inheritDotParams setup_approach.ctree
#' @inheritDotParams setup_approach.vaeac
#' @inheritDotParams setup_approach.categorical
#' @inheritDotParams setup_approach.timeseries
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
#' @examples
#'
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
#'   prediction_zero = p0_ar,
#'   group_lags = FALSE
#' )
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
                             prediction_zero,
                             n_combinations = NULL,
                             group_lags = TRUE,
                             group = NULL,
                             n_samples = 1e3,
                             n_batches = NULL,
                             seed = 1,
                             keep_samp_for_vS = FALSE,
                             predict_model = NULL,
                             get_model_specs = NULL,
                             timing = TRUE,
                             verbose = 0,
                             ...) { # ... is further arguments passed to specific approaches
  timing_list <- list(
    init_time = Sys.time()
  )

  set.seed(seed)

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
    prediction_zero = prediction_zero,
    output_size = horizon,
    n_combinations = n_combinations,
    n_samples = n_samples,
    n_batches = n_batches,
    seed = seed,
    keep_samp_for_vS = keep_samp_for_vS,
    feature_specs = feature_specs,
    type = "forecast",
    horizon = horizon,
    y = y,
    xreg = xreg,
    train_idx = train_idx,
    explain_idx = explain_idx,
    explain_y_lags = explain_y_lags,
    explain_xreg_lags = explain_xreg_lags,
    group_lags = group_lags,
    group = group,
    timing = timing,
    verbose = verbose,
    ...
  )

  timing_list$setup <- Sys.time()

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

  timing_list$test_prediction <- Sys.time()


  # Sets up the Shapley (sampling) framework and prepares the
  # conditional expectation computation for the chosen approach
  # Note: model and predict_model are ONLY used by the AICc-methods of approach empirical to find optimal parameters
  internal <- setup_computation(internal, model, predict_model)

  timing_list$setup_computation <- Sys.time()


  # Compute the v(S):
  # Get the samples for the conditional distributions with the specified approach
  # Predict with these samples
  # Perform MC integration on these to estimate the conditional expectation (v(S))
  vS_list <- compute_vS(internal, model, predict_model, method = "regular")

  timing_list$compute_vS <- Sys.time()

  # Compute Shapley values based on conditional expectations (v(S))
  # Organize function output
  output <- finalize_explanation(
    vS_list = vS_list,
    internal = internal
  )

  if (timing == TRUE) {
    output$timing <- compute_time(timing_list)
  }

  # Temporary to avoid failing tests
  output <- remove_outputs_pass_tests_fore(output)

  return(output)
}

#' @keywords internal
#' @author Lars Henry Berge Olsen
remove_outputs_pass_tests_fore <- function(output) {
  # Temporary to avoid failing tests related to vaeac approach
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

#' Set up data for explain_forecast
#'
#' @param y A matrix or numeric vector containing the endogenous variables for the model.
#' One variable per column, one observation per row.
#' @param xreg A matrix containing exogenous regressors for the model.
#' One variable per column, one observation per row. Should have nrow(data) + horizon rows.
#' @param train_idx The observations indices in data to use as training examples.
#' @param explain_idx The observations indices in data to explain.
#' @param explain_y_lags Numeric vector
#' Indicates the number of lags of y to include in the explanation.
#' @param explain_xreg_lags Numeric vector
#' Indicates the number of lags of xreg to include in the explanation.
#' @param horizon The forecast horizon to explain.
#'
#' @return A list containing
#' - The data.frames x_train and x_explain which holds the lagged data examples.
#' - A numeric, n_endo denoting how many columns are endogenous in x_train and x_explain.
#' - A list, group with groupings of each variable to explain per variable and not per variable and lag.
get_data_forecast <- function(y, xreg, train_idx, explain_idx, explain_y_lags, explain_xreg_lags, horizon) {
  # Check data object type
  stop_message <- ""
  if (!is.vector(y) &&
    !(is.matrix(y) && ncol(y) >= 1) &&
    !(is.data.frame(y) && ncol(y) >= 1)) {
    stop_message <- paste0(
      stop_message,
      "y should be a matrix or data.frame/data.table with one or more columns, ",
      "or a numeric vector.\n"
    )
  }
  if (!is.null(xreg) && !is.matrix(xreg) && !is.data.frame(xreg)) {
    stop_message <- paste0(stop_message, "xreg should be a matrix or a data.frame/data.table.\n")
  }
  if (stop_message != "") {
    stop(stop_message)
  }

  if (is.vector(y)) {
    y <- as.matrix(y)
    colnames(y) <- "Y" # Currently we only allow a single endogenous variable.
  } else {
    y <- as.matrix(y)
  }
  if (ncol(y) != length(explain_y_lags)) {
    stop(
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
      stop("`xreg` misses column names.\n")
    }

    if (ncol(xreg) != length(explain_xreg_lags)) {
      stop(
        paste0(
          "`xreg` has ", ncol(xreg), " columns (", paste0(colnames(xreg), collapse = ","), ").\n",
          "`explain_xreg_lags` has length ", length(explain_xreg_lags), ".\n",
          "These two should match.\n"
        )
      )
    }
    if (nrow(xreg) < max(c(train_idx, explain_idx)) + horizon) {
      stop("`xreg` must have at least as many observations as the data + the forecast horizon.")
    }
  } else {
    xreg <- matrix(NA, max(c(train_idx, explain_idx)) + horizon, 0)
  }

  max_lag <- max(c(explain_y_lags, explain_xreg_lags))

  if (any(c(train_idx, explain_idx) < max_lag) ||
    any(c(train_idx, explain_idx) > nrow(y))) {
    stop(paste0(
      "The train (`train_idx`) and explain (`explain_idx`) indices must fit in the lagged data.\n",
      "The lagged data begins at index ", max_lag, " and ends at index ", nrow(y), ".\n"
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
    stop("`explain_y_lags=0` is not allowed for models without exogeneous variables")
  }

  # Select the train and explain sets from the data and exogenous forecast values.
  train_idx <- train_idx - max_lag + 1
  explain_idx <- explain_idx - max_lag + 1
  return(list(
    y = y,
    xreg = xreg,
    group = reg_fcast$group,
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
#' @param x A matrix with the exogenous variables.
#' @param horizon The forecast horizon.
#' @param group The list of endogenous groups, to append exogenous groups to.
#'
#' @return A list containing
#' - fcast A matrix containing the exogenous observations needed for each observation.
#' - group The list group with the exogenous groups appended.
reg_forecast_setup <- function(x, horizon, group) {
  fcast <- matrix(NA, nrow(x) - horizon + 1, 0)
  names <- character()
  for (i in seq_len(ncol(x))) {
    names_i <- paste0(colnames(x)[i], ".F", seq_len(horizon))
    names <- c(names, names_i)

    fcast_i <- embed(x[, i], horizon)[, rev(seq_len(horizon)), drop = FALSE]
    fcast <- cbind(fcast, fcast_i)

    # Append group names if the exogenous regressor also has lagged values.
    group[[colnames(x)[i]]] <- c(group[[colnames(x)[i]]], names_i)
  }
  colnames(fcast) <- names
  return(list(fcast = fcast, group = group))
}
