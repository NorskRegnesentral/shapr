#' Explain a forecast.
#'
#' @inheritParams explain
#' @param data TODO: Write
#' @param reg TODO: Write
#' @param train_idx TODO: Write
#' @param explain_idx TODO: Write
#' @param lags TODO: Write
#' @param horizon TODO: Write
#' @param group_lags TODO: Write
#' @param ... TODO: Write
#' TODO: Write documentation.
explain_forecast <- function(model,
                    data,
                    reg = NULL,
                    train_idx,
                    explain_idx,
                    lags,
                    horizon,
                    approach,
                    prediction_zero,
                    n_combinations = NULL,
                    group_lags = TRUE,
                    n_samples = 1e3,
                    n_batches = 1,
                    seed = 1,
                    keep_samp_for_vS = FALSE,
                    predict_model = NULL,
                    get_model_specs = NULL,
                    ...) { # ... is further arguments passed to specific approaches

  set.seed(seed)

  # Gets and check feature specs from the model
  feature_specs <- get_feature_specs(get_model_specs, model)

  # Sets up and organize input parameters and data
  # Checks the input parameters and their compatability
  # Checks data/model compatability
  internal <- setup_forecast(
    data = data,
    train_idx = train_idx,
    explain_idx = explain_idx,
    reg = reg,
    lags = lags,
    horizon = horizon,
    approach = approach,
    prediction_zero = prediction_zero,
    n_combinations = n_combinations,
    group_lags = group_lags,
    n_samples = n_samples,
    n_batches = n_batches,
    seed = seed,
    keep_samp_for_vS = keep_samp_for_vS,
    feature_specs = feature_specs, ...
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

  # Sets up the Shapley (sampling) framework and prepares the
  # conditional expectation computation for the chosen approach
  # Note: model and predict_model are ONLY used by the AICc-methods of approach empirical to find optimal parameters
  internal <- setup_computation(internal, model, predict_model)

  # Compute the v(S):
  # Get the samples for the conditional distributions with the specified approach
  # Predict with these samples
  # Perform MC integration on these to estimate the conditional expectation (v(S))
  vS_list <- compute_vS(internal, model, predict_model, method = "regular")

  # Compute Shapley values based on conditional expectations (v(S))
  # Organize function output
  output <- finalize_explanation(
    vS_list = vS_list,
    internal = internal
  )


  return(output)
}

setup_forecast <- function(data,
                           reg,
                           train_idx,
                           explain_idx,
                           lags,
                           horizon,
                           approach,
                           prediction_zero,
                           n_combinations,
                           group_lags,
                           n_samples,
                           n_batches,
                           seed,
                           keep_samp_for_vS,
                           feature_specs,
                           is_python = FALSE, ...) {
  internal <- list()

  internal$parameters <- get_parameters(
    approach = approach,
    prediction_zero = prediction_zero,
    output_size = horizon,
    n_combinations = n_combinations,
    group = NULL,
    n_samples = n_samples,
    n_batches = n_batches,
    seed = seed,
    keep_samp_for_vS = keep_samp_for_vS,
    is_python = is_python,
    type = "forecast",
    horizon = horizon,
    ...
  )

  internal$data <- get_data_forecast(
    data,
    reg,
    train_idx,
    explain_idx,
    lags,
    horizon
    )

  if (group_lags) {
    internal$parameters$group <- internal$data$group
  }

  internal$objects <- list(feature_specs=feature_specs)

  check_data(internal)

  internal <- get_extra_parameters(internal) # This includes both extra parameters and other objects

  check_parameters(internal)

  return(internal)
}

#' Set up data for explain_forecast
#'
#' @param data A matrix containing the endogenous variables for the model. One variable per column, one observation per row.
#' @param reg A matrix containing exogenous regressors for the model. One variable per column, one observation per row. Should have nrow(data) + horizon rows.
#' @param train_idx The observations indices in data to use as training examples.
#' @param explain_idx The observations indices in data to explain.
#' @param lags A list containing two numeric vectors, data and reg, denoting the lag order for each variable used in the model.
#' @param horizon The forecast horizon to explain.
#'
#' @return A list containing
#' - The data.frames x_train and x_explain which holds the lagged data examples.
#' - A numeric, n_endo denoting how many columns are endogenous in x_train and x_explain.
#' - A list, group with groupings of each variable to explain per variable and not per variable and lag.
get_data_forecast <- function (data, reg, train_idx, explain_idx, lags, horizon) {
  if (ncol(data) != length(lags$data)) {
    stop("Each data column must have a lag order set in lags$data.")
  }
  data <- as.matrix(data)

  if (!is.null(reg)) {
    if (ncol(reg) != length(lags$reg)) {
      stop("Each reg column must have a lag order set in lags$reg.")
    }

    if (nrow(reg) < nrow(data) + horizon) {
      stop("The exogenous data must have at least as many observations as the data + the forecast horizon.")
    }
    reg <- as.matrix(reg)
  } else {
    reg <- matrix(NA, nrow(data) + horizon, 0)
  }


  max_lag <- max(c(lags$data, lags$reg))

  if (any(c(train_idx, explain_idx) < max_lag) ||
      any(c(train_idx, explain_idx) > nrow(data))) {
    stop(paste0("The train and explain indices must fit in the lagged data. The lagged data begins at index "),
         max_lag, " and ends at index ", nrow(data), ".")
  }

  # Create a matrix and groups of all lagged data.
  data_reg <- as.matrix(cbind(data, reg[seq_len(nrow(data)), , drop = FALSE]))
  data_lag <- lag_data(data_reg, c(lags$data, lags$reg))

  # Create a matrix and groups of the forecasted values of the exogenous data.
  reg_fcast <- reg_forecast_setup(reg[seq.int(to = nrow(reg), from = max_lag), , drop = FALSE], horizon, data_lag$group)

  # Select the train and explain sets from the data and exogenous forecast values.
  train_idx <- train_idx - max_lag
  explain_idx <- explain_idx - max_lag
  return(list(
    group = reg_fcast$group,
    n_endo = ncol(data_lag$lagged),
    x_train = cbind(
      as.data.frame(data_lag$lagged[train_idx, , drop = FALSE]),
      as.data.frame(reg_fcast$fcast[train_idx, , drop = FALSE])
    ),
    x_explain = cbind(
      as.data.frame(data_lag$lagged[explain_idx, , drop = FALSE]),
      as.data.frame(reg_fcast$fcast[explain_idx, , drop = FALSE])
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
lag_data <- function (x, lags) {
  lagged_obs <- nrow(x) - max(lags)
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
  return(list(lagged=lagged, group=group))
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
reg_forecast_setup <- function (x, horizon, group) {
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
  return(list(fcast=fcast, group=group))
}
