#' Compute `v(S)` for All Feature Subsets `S`
#'
#' @inheritParams default_doc_export
#'
#' @return List of `v(S)` for different coalitions `S`, optionally including the samples used to estimate `v(S)`.
#'
#' @export
#' @keywords internal
compute_vS <- function(internal, model, predict_model) {
  vS_batching_method <- internal$parameters$extra_computation_args$vS_batching_method
  iter <- length(internal$iter_list)

  S_batch <- internal$iter_list[[iter]]$S_batch

  # verbose
  cli_compute_vS(internal)

  if (vS_batching_method == "future") {
    vS_list <- future_compute_vS_batch(
      S_batch = S_batch,
      internal = internal,
      model = model,
      predict_model = predict_model
    )
  } else {
    # Same as above, but without future, progress bar, or parallelization

    rnorm(1) # Advance the RNG state by one step. This ensures consistency with
    # future.apply::future_lapply, which does this to guarantee consistent parallelization.
    # See ?future.apply::future_lapply for details.

    vS_list <- list()
    for (i in seq_along(S_batch)) {
      S <- S_batch[[i]]
      vS_list[[i]] <- batch_compute_vS(
        S = S,
        internal = internal,
        model = model,
        predict_model = predict_model
      )
    }
  }

  #### Append the v(S) output above to any vS_list already computed ####
  vS_list <- append_vS_list(vS_list, internal)


  return(vS_list)
}

#' @keywords internal
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
batch_compute_vS <- function(S, internal, model, predict_model, p = NULL) {
  regression <- internal$parameters$regression

  # Check if we are to use regression or Monte Carlo integration to compute the contribution function values
  if (regression) {
    dt_vS <- batch_prepare_vS_regression(S = S, internal = internal)
  } else {
    # Here dt_vS is either only dt_vS or a list containing dt_vS and dt if
    # internal$parameters$output_args$keep_samp_for_vS = TRUE
    dt_vS <- batch_prepare_vS_MC(S = S, internal = internal, model = model, predict_model = predict_model)
  }

  # Update the progress bar if provided
  if (!is.null(p)) p(amount = length(S), message = "Estimating v(S)")

  return(dt_vS)
}

#' @keywords internal
#' @author Lars Henry Berge Olsen
batch_prepare_vS_regression <- function(S, internal) {
  iter <- length(internal$iter_list)

  X <- internal$iter_list[[iter]]$X

  max_id_coal <- X[, .N]
  x_explain_y_hat <- internal$data$x_explain_y_hat

  # Compute the contribution functions different based on if the grand coalition is in S or not
  if (!(max_id_coal %in% S)) {
    dt <- prepare_data(internal, index_features = S)
  } else {
    # Remove the grand coalition. NULL is for the special case for when the batch only includes the grand coalition.
    dt <- if (length(S) > 1) prepare_data(internal, index_features = S[S != max_id_coal]) else NULL

    # Add the results for the grand coalition (Need to add names in case the batch only contains the grand coalition)
    dt <- rbind(dt, data.table(as.integer(max_id_coal), matrix(x_explain_y_hat, nrow = 1)), use.names = FALSE)

    # Need to add column names if batch S only contains the grand coalition
    if (length(S) == 1) setnames(dt, c("id_coalition", paste0("p_hat1_", seq_len(internal$parameters$n_explain))))
  }

  # Set id_coalition to be the key
  setkey(dt, id_coalition)

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
  keep_samp_for_vS <- internal$parameters$output_args$keep_samp_for_vS
  causal_sampling <- internal$parameters$causal_sampling

  # Make it optional to store and return the dt_list
  dt <- batch_prepare_vS_MC_auxiliary(S = S, internal = internal, causal_sampling = causal_sampling)

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
#' @author Lars Henry Berge Olsen and Martin Jullum
batch_prepare_vS_MC_auxiliary <- function(S, internal, causal_sampling) {
  x_explain <- internal$data$x_explain
  n_explain <- internal$parameters$n_explain
  prepare_data_function <- if (causal_sampling) prepare_data_causal else prepare_data

  iter <- length(internal$iter_list)
  X <- internal$iter_list[[iter]]$X
  max_id_coalition <- X[, .N]

  if (max_id_coalition %in% S) {
    dt <- if (length(S) == 1) NULL else prepare_data_function(internal, index_features = S[S != max_id_coalition])
    dt <- rbind(dt, data.table(id_coalition = max_id_coalition, x_explain, w = 1, id = seq_len(n_explain)))
    setkey(dt, id, id_coalition)
  } else {
    dt <- prepare_data_function(internal, index_features = S)
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
  xreg = NULL
) {
  # Predictions

  if (type == "forecast") {
    dt[, (pred_cols) := predict_model(
      x = model,
      newdata = .SD[, .SD, .SDcols = seq_len(n_endo)],
      newreg = .SD[, .SD, .SDcols = seq_len(length(feature_names) - n_endo) + n_endo],
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
  dt_res <- dt[, lapply(.SD, function(x) sum(((x) * w) / sum(w))), .(id, id_coalition), .SDcols = pred_cols]
  data.table::setkeyv(dt_res, c("id", "id_coalition"))
  dt_mat <- data.table::dcast(dt_res, id_coalition ~ id, value.var = pred_cols)
  if (length(pred_cols) == 1) {
    names(dt_mat)[-1] <- paste0(pred_cols, "_", names(dt_mat)[-1])
  }
  # dt_mat[, id_coalition := NULL]

  return(dt_mat)
}

#' Append the New `vS_list` to the Previous `vS_list`
#'
#' @inheritParams compute_estimates
#'
#' @return The vS_list after being merged with previously computed vS_lists (stored in internal)
#'
#' @export
#' @keywords internal
append_vS_list <- function(vS_list, internal) {
  iter <- length(internal$iter_list)
  keep_samp_for_vS <- internal$parameters$output_args$keep_samp_for_vS

  # Adds v(S) output above to any vS_list already computed
  if (iter > 1) {
    prev_coalition_map <- internal$iter_list[[iter - 1]]$coalition_map
    prev_vS_list <- internal$iter_list[[iter - 1]]$vS_list

    # Need to map the old id_coalitions to the new numbers for this merging to work out
    current_coalition_map <- internal$iter_list[[iter]]$coalition_map

    # Creates a mapper from the last id_coalition to the new id_coalition numbering
    id_coalitions_mapper <- merge(prev_coalition_map,
      current_coalition_map,
      by = "coalitions_str",
      suffixes = c("", "_new")
    )
    prev_vS_list_new <- list()

    # Applies the mapper to update the prev_vS_list ot the new id_coalition numbering
    if (isFALSE(keep_samp_for_vS)) {
      for (k in seq_along(prev_vS_list)) {
        this_vS <- prev_vS_list[[k]]

        this_vS_new <- merge(this_vS,
          id_coalitions_mapper[, .(id_coalition, id_coalition_new)],
          by = "id_coalition"
        )

        this_vS_new[, id_coalition := id_coalition_new]
        this_vS_new[, id_coalition_new := NULL]


        prev_vS_list_new[[k]] <- this_vS_new
      }
    } else {
      for (k in seq_along(prev_vS_list)) {
        this_vS <- prev_vS_list[[k]]$dt_vS
        this_samp_for_vS <- prev_vS_list[[k]]$dt_samp_for_vS


        this_vS_new <- merge(this_vS,
          id_coalitions_mapper[, .(id_coalition, id_coalition_new)],
          by = "id_coalition"
        )

        this_vS_new[, id_coalition := id_coalition_new]
        this_vS_new[, id_coalition_new := NULL]

        this_samp_for_vS_new <- merge(this_samp_for_vS,
          id_coalitions_mapper[, .(id_coalition, id_coalition_new)],
          by = "id_coalition"
        )

        this_samp_for_vS_new[, id_coalition := id_coalition_new]
        this_samp_for_vS_new[, id_coalition_new := NULL]


        prev_vS_list_new[[k]] <- list(dt_vS = this_vS_new, dt_samp_for_vS = this_samp_for_vS_new)
      }
    }
    names(prev_vS_list_new) <- names(prev_vS_list)

    # Merge the new vS_list with the old vS_list
    vS_list <- c(prev_vS_list_new, vS_list)
  }


  return(vS_list)
}
