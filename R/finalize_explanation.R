#' Computes the Shapley values given `v(S)`
#'
#' @inherit explain
#' @inheritParams default_doc
#' @param vS_list List
#' Output from [compute_vS()]
#'
#' @export
finalize_explanation <- function(vS_list, internal) {
  keep_samp_for_vS <- internal$parameters$keep_samp_for_vS

  processed_vS_list <- postprocess_vS_list(
    vS_list = vS_list,
    internal = internal
  )

  # Extract the predictions we are explaining
  p <- get_p(processed_vS_list$dt_vS, internal)

  internal$timing$postprocessing <- Sys.time()

  # Compute the Shapley values
  dt_shapley <- compute_shapley_new(internal, processed_vS_list$dt_vS)

  internal$timing$shapley_computation <- Sys.time()


  # Clearnig out the tmp list with model and predict_model (only added for AICc-types of empirical approach)
  internal$tmp <- NULL

  internal$output <- processed_vS_list

  if(internal$parameters$timing){
    timing_secs = mapply(FUN=difftime,
                         internal$timing[-1],
                         internal$timing[-length(internal$timing)],
                         units="secs")

    timing_list <- list(
      init_time= internal$timing$init,
      total_time_secs = sum(timing_secs),
      timing_secs = timing_secs
    )
  } else {
    timing_list <- NULL
  }

  internal$timing <- NULL

  output <- list(
    shapley_values = dt_shapley,
    internal = internal,
    pred_explain = p,
    timing = timing_list
  )
  attr(output, "class") <- c("shapr", "list")

  return(output)
}


#' @keywords internal
postprocess_vS_list <- function(vS_list, internal) {
  id_combination <- NULL # due to NSE

  keep_samp_for_vS <- internal$parameters$keep_samp_for_vS
  prediction_zero <- internal$parameters$prediction_zero
  n_explain <- internal$parameters$n_explain

  # Appending the zero-prediction to the list
  dt_vS0 <- as.data.table(rbind(c(1, rep(prediction_zero, n_explain))))
  names(dt_vS0) <- names(vS_list[[1]])

  # Extracting/merging the data tables from the batch running
  # TODO: Need a memory and speed optimized way to transform the output form dt_vS_list to two different lists,
  # I.e. without copying the data more than once. For now I have modified run_batch such that it
  # if keep_samp_for_vS=FALSE
  # then there is only one copy, but there are two if keep_samp_for_vS=TRUE. This might be OK since the
  # latter is used rarely
  if (keep_samp_for_vS) {
    vS_list[[length(vS_list) + 1]] <- list(dt_vS0, NULL)

    dt_vS <- rbindlist(lapply(vS_list, `[[`, 1))

    dt_samp_for_vS <- rbindlist(lapply(vS_list, `[[`, 2))
    data.table::setorder(dt_samp_for_vS, id_combination)
  } else {
    vS_list[[length(vS_list) + 1]] <- dt_vS0

    dt_vS <- rbindlist(vS_list)
    dt_samp_for_vS <- NULL
  }

  data.table::setorder(dt_vS, id_combination)

  output <- list(
    dt_vS = dt_vS,
    dt_samp_for_vS = dt_samp_for_vS
  )
  return(output)
}

#' @keywords internal
get_p <- function(dt_vS, internal) {
  id_combination <- NULL # due to NSE

  max_id_combination <- internal$parameters$n_combinations
  p <- unlist(dt_vS[id_combination == max_id_combination, ][, id_combination := NULL])

  return(p)
}

#' Compute shapley values
#' @param explainer An `explain` object.
#' @param dt_vS The contribution matrix.
#' @return A `data.table` with shapley values for each test observation.
#' @export
#' @keywords internal
compute_shapley_new <- function(internal, dt_vS) {
  is_groupwise <- internal$parameters$is_groupwise
  labels <- internal$parameters$feature_names
  W <- internal$objects$W

  if (!is_groupwise) {
    shap_names <- labels
  } else {
    shap_names <- names(internal$parameters$group) # TODO: Add group_names (and feature_names) to internal earlier
  }

  kshap <- t(W %*% as.matrix(dt_vS[, -"id_combination"]))
  dt_kshap <- data.table::as.data.table(kshap)
  colnames(dt_kshap) <- c("none", shap_names)

  return(dt_kshap)
}
