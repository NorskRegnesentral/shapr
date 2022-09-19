
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
    ret <- future_compute_vS_batch(
      S_batch = S_batch,
      internal = internal,
      model = model,
      predict_model = predict_model
    )
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
  p <- progressr::progressor(sum(lengths(S_batch)))
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
batch_compute_vS <- function(S, internal, model, predict_model, p = NULL) {
  keep_samp_for_vS <- internal$parameters$keep_samp_for_vS
  feature_names <- internal$parameters$feature_names

  dt <- batch_prepare_vS(S = S, internal = internal) # Make it optional to store and return the dt_list

  compute_preds(dt, # Updating dt by reference
    feature_names = feature_names,
    predict_model = predict_model,
    model
  )
  dt_vS <- compute_MCint(dt)
  if (!is.null(p)) {
    p(
      amount = length(S),
      message = "Estimating v(S)"
    ) # TODO: Add a message to state what batch has been computed
  }

  if (keep_samp_for_vS) {
    return(list(dt_vS = dt_vS, dt_samp_for_vS = dt))
  } else {
    return(dt_vS = dt_vS)
  }
}

#' @keywords internal
batch_prepare_vS <- function(S, internal) {

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
    S <- S[S != max_id_combination]
    dt <- prepare_data(internal, index_features = S)
    dt_max <- data.table(x_explain, id_combination = max_id_combination, w = 1, id = seq_len(n_explain))
    dt <- rbind(dt, dt_max)
    setkey(dt, id, id_combination)
  }
  return(dt)
}

#' @keywords internal
compute_preds <- function(dt, feature_names, predict_model, model) {

  # Predictions
  dt[id_combination != 1, p_hat := predict_model(model, newdata = .SD), .SDcols = feature_names]

  return(dt)
}

compute_MCint <- function(dt) {

  # Calculate contributions
  dt_res <- dt[, .(k = sum((p_hat * w) / sum(w))), .(id, id_combination)]
  data.table::setkeyv(dt_res, c("id", "id_combination"))
  dt_mat <- data.table::dcast(dt_res, id_combination ~ id, value.var = "k")
  # dt_mat[, id_combination := NULL]

  dt_mat
}
