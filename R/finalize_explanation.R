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
  shap_approach <- internal$parameters$shap_approach

  processed_vS_list <- postprocess_vS_list(
    vS_list = vS_list,
    internal = internal
  )

  # Extract the predictions we are explaining
  p <- get_p(processed_vS_list$dt_vS, internal)

  # internal$timing$postprocessing <- Sys.time()

  # Compute the Shapley values
  if(shap_approach == "permutation"){
    dt_shapley <- compute_shapley_permutation(internal, processed_vS_list$dt_vS)
  } else {
    dt_shapley <- compute_shapley_new(internal, processed_vS_list$dt_vS)
  }

  # internal$timing$shapley_computation <- Sys.time()


  # Clearnig out the tmp list with model and predict_model (only added for AICc-types of empirical approach)
  internal$tmp <- NULL

  internal$output <- processed_vS_list


  output <- list(
    shapley_values = dt_shapley,
    internal = internal,
    pred_explain = p
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

  # Extracting/merging the data tables from the batch running
  # TODO: Need a memory and speed optimized way to transform the output form dt_vS_list to two different lists,
  # I.e. without copying the data more than once. For now I have modified run_batch such that it
  # if keep_samp_for_vS=FALSE
  # then there is only one copy, but there are two if keep_samp_for_vS=TRUE. This might be OK since the
  # latter is used rarely
  if (keep_samp_for_vS) {
    names(dt_vS0) <- names(vS_list[[1]][[1]])

    vS_list[[length(vS_list) + 1]] <- list(dt_vS0, NULL)

    dt_vS <- rbindlist(lapply(vS_list, `[[`, 1))

    dt_samp_for_vS <- rbindlist(lapply(vS_list, `[[`, 2), use.names = TRUE)

    data.table::setorder(dt_samp_for_vS, id_combination)
  } else {
    names(dt_vS0) <- names(vS_list[[1]])

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
    names(p) <- apply(internal$parameters$output_labels, 1, function(x) paste0("explain_idx_", x[1], "_horizon_", x[2]))
  }

  return(p)
}

compute_shapley_permutation <- function(internal,dt_vS){
  feature_names <- internal$parameters$feature_names
  X_perm <- internal$objects$X_perm
  n_permutations <- internal$parameters$n_permutations
  n_features <- internal$parameters$n_features
  n_explain <- internal$parameters$n_explain
  max_id_combination <- internal$parameters$n_combinations
  S <- internal$objects$S
  phi0 <- internal$parameters$prediction_zero


  apply_cols <- names(dt_vS)[-1]

  kshap <- matrix(0,ncol=n_explain,nrow=n_features)

  for(i in seq(n_permutations)){
    # Find id combinations that are permuted
    these_id_combs <- c(1,X_perm[permute_id==i,id_combination],max_id_combination)

    # Find the feature to map the contributions to
    mapping_mat <- apply(S[these_id_combs,],FUN=diff,MARGIN=2)
    contributes_to <- apply(mapping_mat,FUN=function(x) which(x==1),MARGIN=1)
    reorder_vec <- order(contributes_to)

    #### LOOK HERE: OK, something is off here, I think it might be the merging that is messing up the order
    # I need the order to be such that the X_perm always have the smallest feature numbers first.

    # Find the corresponding rows in dt_vS and get the contribution
    these_vS <- dt_vS[id_combination %in% these_id_combs]
    these_contribs <- these_vS[,lapply(.SD,diff),.SDcols=apply_cols]

    reordered_contribs <- as.matrix(these_contribs[reorder_vec,])
    kshap <- kshap + reordered_contribs
  }
  kshap <- kshap/n_permutations



  dt_shapley <- data.table::data.table(cbind(none=phi0,t(kshap)))
  names(dt_shapley)[-1] <- feature_names
  return(dt_shapley)
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
  if (type == "forecast") {
    id_combination_mapper_dt <- internal$objects$id_combination_mapper_dt
    horizon <- internal$parameters$horizon
    cols_per_horizon <- internal$objects$cols_per_horizon
    W_list <- internal$objects$W_list

    kshap_list <- list()
    for (i in seq_len(horizon)) {
      W0 <- W_list[[i]]

      dt_vS0 <- merge(dt_vS, id_combination_mapper_dt[horizon == i], by = "id_combination", all.y = TRUE)
      data.table::setorder(dt_vS0, horizon_id_combination)
      these_vS0_cols <- grep(paste0("p_hat", i, "_"), names(dt_vS0))

      kshap0 <- t(W0 %*% as.matrix(dt_vS0[, these_vS0_cols, with = FALSE]))
      kshap_list[[i]] <- data.table::as.data.table(kshap0)

      if (!is_groupwise) {
        names(kshap_list[[i]]) <- c("none", cols_per_horizon[[i]])
      } else {
        names(kshap_list[[i]]) <- c("none", shap_names)
      }
    }

    dt_kshap <- cbind(internal$parameters$output_labels, rbindlist(kshap_list, fill = TRUE))
  } else {
    kshap <- t(W %*% as.matrix(dt_vS[, -"id_combination"]))
    dt_kshap <- data.table::as.data.table(kshap)
    colnames(dt_kshap) <- c("none", shap_names)
  }

  return(dt_kshap)
}
