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

  if (internal$parameters$type == "forecast") {
    names(p) <- internal$parameters$output_labels
  }

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
  feature_names <- internal$parameters$feature_names
  W <- internal$objects$W
  type <- internal$parameters$type

  if (!is_groupwise) {
    shap_names <- feature_names
  } else {
    shap_names <- names(internal$parameters$group) # TODO: Add group_names (and feature_names) to internal earlier
  }

  # If multiple horizons with explain_forecast are used, we only distribute value to those used at each horizon
  if(type=="forecast"){
    id_combination_mapper_dt <- internal$objects$id_combination_mapper_dt
    horizon <- internal$parameters$horizon
    cols_per_horizon <- internal$objects$cols_per_horizon
    kshap_list <- list()
    for(i in seq_len(horizon)){
      these_W_rows <- c(1,1+which(feature_names %in% cols_per_horizon[[i]]))
      these_W_cols <- id_combination_mapper_dt[horizon==i,id_combination]
      W0 <- W[these_W_rows,these_W_cols]

      dt_vS0 <- merge(dt_vS,id_combination_mapper_dt[horizon==i],by="id_combination",all.y = T)
      these_vS0_cols <- grep(paste0("p_hat",i),names(dt_vS0))

      kshap0 <- t(W0 %*% as.matrix(dt_vS0[,..these_vS0_cols]))
      kshap_list[[i]] <- data.table::as.data.table(kshap0)
      names(kshap_list[[i]]) <- c("none",cols_per_horizon[[i]])
    }
    dt_kshap <- rbindlist(kshap_list,fill=TRUE)
  } else {
    kshap <- t(W %*% as.matrix(dt_vS[, -"id_combination"]))
    dt_kshap <- data.table::as.data.table(kshap)
    colnames(dt_kshap) <- c("none", shap_names)
  }

  if (internal$parameters$type == "forecast") {
    rownames(dt_kshap) <- internal$parameters$output_labels
  }

  return(dt_kshap)
}
