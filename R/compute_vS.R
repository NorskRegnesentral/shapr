#' Computes `v(S)` for all features subsets `S`.
#'
#' @inheritParams default_doc
#' @inheritParams explain
#'
#' @param method Character
#' Indicates whether the lappy method (default) or loop method should be used.
#'
#' @export
compute_vS <- function(internal, model, predict_model, method = "future") {
  S_batch <- internal$objects$S_batch

  if (method == "future") {
    ret <- future_compute_vS_batch(S_batch = S_batch, internal = internal, model = model, predict_model = predict_model)
  } else {
    # Doing the same as above without future without progressbar or paralellization
    ret <- list()
    for (i in seq_along(S_batch)) {
      S <- S_batch[[i]]
      ret[[i]] <- batch_compute_vS(
        S = S,
        internal = internal,
        model = model,
        predict_model = predict_model
      )
    }
  }

  return(ret)
}

future_compute_vS_batch <- function(S_batch, internal, model, predict_model) {
  if (requireNamespace("progressr", quietly = TRUE)) {
    p <- progressr::progressor(sum(lengths(S_batch)))
  } else {
    p <- NULL
  }
  ret <- future.apply::future_lapply(
    X = S_batch,
    FUN = batch_compute_vS,
    internal = internal,
    model = model,
    predict_model = predict_model,
    p = p,
    future.seed = internal$parameters$seed
  )
  return(ret)
}

#' @keywords internal
#' @author Martin Jullum, Lars Henry Berge Olsen
batch_compute_vS <- function(S, internal, model, predict_model, p = NULL) {
  regression <- internal$parameters$regression

  # Check if we are to use regression or Monte Carlo integration to compute the contribution function values
  if (regression) {
    dt_vS <- batch_prepare_vS_regression(S = S, internal = internal)
  } else {
    # Here dt_vS is either only dt_vS or a list containing dt_vS and dt if internal$parameters$keep_samp_for_vS = TRUE
    dt_vS <- batch_prepare_vS_MC(S = S, internal = internal, model = model, predict_model = predict_model)
  }

  # Update the progress bar if provided
  # TODO: Add a message to state what batch has been computed
  if (!is.null(p)) p(amount = length(S), message = "Estimating v(S)")

  return(dt_vS)
}

#' @keywords internal
#' @author Lars Henry Berge Olsen
batch_prepare_vS_regression <- function(S, internal) {
  max_id_comb <- internal$parameters$n_combinations
  x_explain_y_hat <- internal$data$x_explain_y_hat

  # Compute the contribution functions different based on if the grand coalition is in S or not
  if (!(max_id_comb %in% S)) {
    dt <- prepare_data(internal, index_features = S)
  } else {
    # Remove the grand coalition. NULL is for the special case for when the batch only includes the grand coalition.
    dt <- if (length(S) > 1) prepare_data(internal, index_features = S[S != max_id_comb]) else NULL

    # Add the results for the grand coalition (Need to add names in case the batch only contains the grand coalition)
    dt <- rbind(dt, data.table(as.integer(max_id_comb), matrix(x_explain_y_hat, nrow = 1)), use.names = FALSE)

    # Need to add column names if batch S only contains the grand coalition
    if (length(S) == 1) setnames(dt, c("id_combination", paste0("p_hat1_", seq_len(internal$parameters$n_explain))))
  }

  # Set id_combination to be the key
  setkey(dt, id_combination)

  return(dt)
}

#' @keywords internal
#' @author Martin Jullum, Lars Henry Berge Olsen
batch_prepare_vS_MC <- function(S, internal, model, predict_model) {
  output_size <- internal$parameters$output_size
  feature_names <- internal$parameters$feature_names
  type <- internal$parameters$type
  horizon <- internal$parameters$horizon
  n_endo <- internal$data$n_endo
  explain_idx <- internal$parameters$explain_idx
  explain_lags <- internal$parameters$explain_lags
  y <- internal$data$y
  xreg <- internal$data$xreg
  keep_samp_for_vS <- internal$parameters$keep_samp_for_vS

  dt <- batch_prepare_vS_MC_auxiliary(S = S, internal = internal) # Make it optional to store and return the dt_list

  pred_cols <- paste0("p_hat", seq_len(output_size))

  compute_preds(
    dt, # Updating dt by reference
    feature_names = feature_names,
    predict_model = predict_model,
    model = model,
    pred_cols = pred_cols,
    type = type,
    horizon = horizon,
    n_endo = n_endo,
    explain_idx = explain_idx,
    explain_lags = explain_lags,
    y = y,
    xreg = xreg
  )
  dt_vS <- compute_MCint(dt, pred_cols)

  # Also return the dt object if keep_samp_for_vS is TRUE
  return(if (keep_samp_for_vS) list(dt_vS = dt_vS, dt_samp_for_vS = dt) else dt_vS)
}

#' @keywords internal
batch_prepare_vS_MC_auxiliary <- function(S, internal) {
  max_id_combination <- internal$parameters$n_combinations
  x_explain <- internal$data$x_explain
  n_explain <- internal$parameters$n_explain

  # TODO: Check what is the fastest approach to deal with the last observation.
  # Not doing this for the largest id combination (should check if this is faster or slower, actually)
  # An alternative would be to delete rows from the dt which is provided by prepare_data.
  if (!(max_id_combination %in% S)) {
    # TODO: Need to handle the need for model for the AIC-versions here (skip for Python)
    dt <- prepare_data(internal, index_features = S)
  } else {
    if (length(S) > 1) {
      S <- S[S != max_id_combination]
      dt <- prepare_data(internal, index_features = S)
    } else {
      dt <- NULL # Special case for when the batch only include the largest id
    }
    dt_max <- data.table(id_combination = max_id_combination, x_explain, w = 1, id = seq_len(n_explain))
    dt <- rbind(dt, dt_max)
    setkey(dt, id, id_combination)
  }
  return(dt)
}

#' @keywords internal
compute_preds <- function(
    dt,
    feature_names,
    predict_model,
    model,
    pred_cols,
    type,
    horizon = NULL,
    n_endo = NULL,
    explain_idx = NULL,
    explain_lags = NULL,
    y = NULL,
    xreg = NULL) {
  # Predictions

  if (type == "forecast") {
    dt[, (pred_cols) := predict_model(
      x = model,
      newdata = .SD[, 1:n_endo],
      newreg = .SD[, -(1:n_endo)],
      horizon = horizon,
      explain_idx = explain_idx[id],
      explain_lags = explain_lags,
      y = y,
      xreg = xreg
    ), .SDcols = feature_names]
  } else {
    dt[, (pred_cols) := predict_model(model, newdata = .SD), .SDcols = feature_names]
  }

  return(dt)
}

compute_MCint <- function(dt, pred_cols = "p_hat") {
  # Calculate contributions
  dt_res <- dt[, lapply(.SD, function(x) sum(((x) * w) / sum(w))), .(id, id_combination), .SDcols = pred_cols]
  data.table::setkeyv(dt_res, c("id", "id_combination"))
  dt_mat <- data.table::dcast(dt_res, id_combination ~ id, value.var = pred_cols)
  if (length(pred_cols) == 1) {
    names(dt_mat)[-1] <- paste0(pred_cols, "_", names(dt_mat)[-1])
  }
  # dt_mat[, id_combination := NULL]

  return(dt_mat)
}
