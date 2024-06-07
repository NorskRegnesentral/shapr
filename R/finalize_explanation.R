print_iter <- function(internal,print_iter_info,print_shapleyres){

  iter <- length(internal$iter_list)-1 # This function is called after the preparation of the next iteration

  if(print_iter_info){

    converged <- internal$iter_list[[iter]]$converged
    estimated_remaining_samples <- internal$iter_list[[iter]]$estimated_remaining_samples
    estimated_required_samples <- internal$iter_list[[iter]]$estimated_required_samples
    n_current_samples <- internal$iter_list[[iter]]$n_current_samples

    next_n_combinations <- internal$iter_list[[iter+1]]$n_combinations

    if(converged==FALSE){

      cat(paste0("\nIteration ", iter, "\n",
                 "Not converged after ", n_current_samples, " samples.\n",
                 "Estimated remaining samples: ", estimated_remaining_samples, "\n",
                 "Estimated required samples: ", estimated_required_samples, "\n",
                 "Using ", next_n_combinations, " new samples in the next iteration.\n"))

    } else {
      cat("\nConvergence reached!\n")
    }
  }

  if(print_shapleyres){
    n_explain <- internal$parameters$n_explain

    dt_shapley_est <- internal$iter_list[[iter]]$dt_shapley_est[,-1]
    dt_shapley_sd <- internal$iter_list[[iter]]$dt_shapley_sd[,-1]

    # Printing the current Shapley values
    matrix1 <- format(round(dt_shapley_est,3),nsmall=2,justify = "right")
    matrix2 <- format(round(dt_shapley_sd,2),nsmall=2,justify = "right")

    if(print_shapleyres){
      cat("Current estimated Shapley values [sd]:\n")
      print_dt <- as.data.table(matrix(paste(matrix1, " (", matrix2,") ", sep = ""), nrow = n_explain))
      names(print_dt) <- names(dt_shapley_est)
      print(print_dt)
    }
  }
}

prepare_next_iteration <- function(internal){

  iter <- length(internal$iter_list)
  converged <-internal$iter_list[[iter]]$converged


  if(converged==FALSE){
    next_iter_list <- list()

    n_features <- internal$parameters$n_features
    reduction_factor_vec <- internal$parameters$reduction_factor_vec

    estimated_remaining_samples <- internal$iter_list[[iter]]$estimated_remaining_samples
    reduction_factor <- internal$iter_list[[iter]]$reduction_factor
    n_current_samples <- internal$iter_list[[iter]]$n_current_samples

    X <- internal$iter_list[[iter]]$X

    proposal_next_n_combinations <- ceiling(estimated_remaining_samples*reduction_factor)

    if((n_current_samples+proposal_next_n_combinations)>=2^n_features){
      # Use all coalitions in the last iteration as the estimated number of samples is more than what remains
      next_iter_list$exact <- TRUE
      next_iter_list$n_combinations <- 2^n_features - n_current_samples
      next_iter_list$compute_sd <- FALSE
    } else {
      # Sample more keeping the current samples
      next_iter_list$exact <- FALSE
      next_iter_list$n_combinations <- proposal_next_n_combinations
      next_iter_list$compute_sd <- TRUE
    }

    next_iter_list$reduction_factor <- reduction_factor_vec[iter]

    # Storing the feature samples I have from before (not sure I need these if I run exact).
    # Could also be moved to shapley_setup as it is computed based on X only, and that is stored in previous iter_list anyway
    repetitions <- X[-c(1,.N),sample_freq]
    unique_feature_samples <- X[-c(1,.N),features]

    next_iter_list$prev_feature_samples <- unlist(lapply(seq_along(unique_feature_samples),
                                                         function(i) rep(list(unique_feature_samples[[i]]),
                                                                         repetitions[i])),
                                                  recursive = FALSE)

  } else {
    next_iter_list <- NULL
  }

  internal$iter_list[[iter+1]] <- next_iter_list

  return(internal)

}



#' Computes the Shapley values given `v(S)`
#'
#' @inherit explain
#' @inheritParams default_doc
#' @param vS_list List
#' Output from [compute_vS()]
#'
#' @export
compute_estimates <- function(internal, vS_list) {

  iter <- length(internal$iter_list)
  compute_sd <- internal$iter_list[[iter]]$compute_sd

  n_boot_samps <- internal$parameters$n_boot_samps

  processed_vS_list <- postprocess_vS_list(
    vS_list = vS_list,
    internal = internal
  )


  # Compute the Shapley values
  dt_shapley_est <- compute_shapley_new(internal, processed_vS_list$dt_vS)

  if(compute_sd){
    dt_shapley_sd <- bootstrap_shapley(internal,n_boot_samps = n_boot_samps,processed_vS_list$dt_vS)
  } else {
    dt_shapley_sd <- dt_shapley_est*0
  }

  # Adding explain_id to the output dt
  dt_shapley_est[,explain_id:=.I]
  setcolorder(dt_shapley_est,"explain_id")
  dt_shapley_sd[,explain_id:=.I]
  setcolorder(dt_shapley_sd,"explain_id")


  internal$iter_list[[iter]]$dt_shapley_est = dt_shapley_est
  internal$iter_list[[iter]]$dt_shapley_sd = dt_shapley_sd
  internal$iter_list[[iter]]$vS_list = vS_list
  internal$iter_list[[iter]]$dt_vS = processed_vS_list$dt_vS

  # internal$timing$shapley_computation <- Sys.time()

  # Clearing out the tmp list with model and predict_model (only added for AICc-types of empirical approach)
  internal$output <- processed_vS_list

  return(internal)
}


#' Finalizes the explanation object
#'
#' @inherit explain
#' @inheritParams default_doc
#'
#' @export
finalize_explanation <- function(internal) {
  MSEv_uniform_comb_weights <- internal$parameters$MSEv_uniform_comb_weights
  output_size <- internal$parameters$output_size
  dt_vS <- internal$output$dt_vS

  iter <- length(internal$iter_list)
  dt_shapley_est <- internal$iter_list[[iter]]$dt_shapley_est
  dt_shapley_sd <- internal$iter_list[[iter]]$dt_shapley_sd

  # Clearing out the tmp list with model and predict_model (only added for AICc-types of empirical approach)
  internal$tmp <- NULL

  # Extract the predictions we are explaining
  p <- get_p(dt_vS, internal)


  # Compute the MSEv evaluation criterion if the output of the predictive model is a scalar.
  # TODO: check if it makes sense for output_size > 1.
  if (output_size == 1) {
    MSEv <- compute_MSEv_eval_crit(
      internal = internal,
      dt_vS = dt_vS,
      MSEv_uniform_comb_weights = MSEv_uniform_comb_weights
    )
  } else {
    MSEv <- NULL
  }

  # Extract iterative results in a simplified format
  internal$iter_results <- get_iter_results(internal$iter_list)

  output <- list(
    shapley_values = dt_shapley_est,
    shapley_values_sd = dt_shapley_sd,
    internal = internal,
    pred_explain = p,
    MSEv = MSEv
  )
  attr(output, "class") <- c("shapr", "list")

  return(output)
}

get_iter_results <- function(iter_list){

  ret <- list()
  ret$dt_iter_shapley_est = rbindlist(lapply(iter_list, `[[`, "dt_shapley_est"),idcol = "iter")
  ret$dt_iter_shapley_sd = rbindlist(lapply(iter_list, `[[`, "dt_shapley_sd"),idcol = "iter")
  ret$iter_info_dt = iter_list_to_dt(iter_list)
  return(ret)
}

iter_list_to_dt <- function(iter_list,what = c("exact","n_combinations","compute_sd","reduction_factor",
                                               "converged","n_current_samples","estimated_required_samples",
                                               "estimated_remaining_samples")){
  extracted=lapply(iter_list,function(x) x[what])
  ret <- do.call(rbind, lapply(extracted, as.data.table))
  return(ret)
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

  dt_vS <- unique(dt_vS, by = id_combination) # To remove duplicated full pred row in the iterative procedure

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


bootstrap_shapley <- function(internal,dt_vS,n_boot_samps = 100,seed = 123){

  set.seed(seed)

  X_org <- copy(internal$objects$X)
  n_explain <- internal$parameters$n_explain
  n_features <- internal$parameters$n_features
  shap_names <- internal$parameters$feature_names
  paired_shap_sampling <- internal$parameters$paired_shap_sampling
  shapley_reweighting <- internal$parameters$shapley_reweighting

  boot_sd_array <- array(NA,dim=c(n_explain,n_features+1,n_boot_samps))

  X_keep <- X_org[c(1,.N),.(id_combination,features,n_features,N,shapley_weight)]
  X_samp <- X_org[-c(1,.N),.(id_combination,features,n_features,N,shapley_weight,sample_freq)]
  X_samp[, features_tmp := sapply(features, paste, collapse = " ")]

  n_combinations_boot <- X_samp[,sum(sample_freq)]

  for (i in seq_len(n_boot_samps)){

    if(paired_shap_sampling){

        # Sample with replacement
      X_boot00 <- X_samp[sample.int(n=.N,
                                   size=ceiling(n_combinations_boot/2),
                                                replace = TRUE,
                                                prob=sample_freq),
                                   .(id_combination,features,n_features,N)]

      X_boot00[, features_tmp := sapply(features, paste, collapse = " ")]
      # Not sure why I have to two the next two lines in two steps, but I don't get it to work otherwise
      boot_features_dup <- lapply(X_boot00$features, function(x) seq(n_features)[-x])
      X_boot00[, features_dup := boot_features_dup]
      X_boot00[, features_dup_tmp := sapply(features_dup, paste, collapse = " ")]

      # Extract the paired coalitions from X_samp
      X_boot00_paired <- merge(X_boot00[,.(features_dup_tmp)],
                              X_samp[,.(id_combination,features,n_features,N,features_tmp)],
                              by.x = "features_dup_tmp",by.y="features_tmp")
      X_boot0 <- rbind(X_boot00[,.(id_combination, features, n_features, N)],
                       X_boot00_paired[,.(id_combination, features, n_features, N)])
    } else {
      X_boot0 <- X_samp[sample.int(n=.N,
                                   size=n_combinations_boot,
                                   replace = TRUE,
                                   prob=sample_freq),
                        .(id_combination,features,n_features,N)]

    }


    X_boot0[,shapley_weight:=.N,by="id_combination"]
    X_boot0 <- unique(X_boot0,by="id_combination")

    X_boot <- rbind(X_keep,X_boot0)
    data.table::setorder(X_boot,id_combination)

    shapley_reweighting(X_boot, reweight=shapley_reweighting) # reweights the shapley weights by reference

    W_boot <- shapr:::weight_matrix(
      X = X_boot,
      normalize_W_weights = TRUE,
      is_groupwise = FALSE
    )

    kshap_boot <- t(W_boot %*% as.matrix(dt_vS[id_combination %in% X_boot[,id_combination], -"id_combination"]))

    boot_sd_array[,,i] <- copy(kshap_boot)

  }

  std_dev_mat <- apply(boot_sd_array, c(1, 2), sd)

  dt_kshap_boot_sd <- data.table::as.data.table(std_dev_mat)
  colnames(dt_kshap_boot_sd) <- c("none", shap_names)

  return(dt_kshap_boot_sd)

}


check_convergence <- function(internal, convergence_tolerance=0.1){

  iter <- length(internal$iter_list)

  dt_shapley_est <- internal$iter_list[[iter]]$dt_shapley_est
  dt_shapley_sd <- internal$iter_list[[iter]]$dt_shapley_sd

  n_current_samples <- internal$parameters$n_combinations-2
  max_iter <- internal$parameters$max_iter

  max_sd <- dt_shapley_sd[,max(.SD),.SDcols=-1,by=.I]$V1 # Max per prediction
  max_sd0 <- max_sd*sqrt(n_current_samples)

  dt_shapley_est0 <- copy(dt_shapley_est)

  if(!is.null(convergence_tolerance)){
    dt_shapley_est0[,maxval:=max(.SD),.SDcols=-1,by=.I]
    dt_shapley_est0[,minval:=min(.SD),.SDcols=-1,by=.I]
    dt_shapley_est0[,req_samples:=(max_sd0/((maxval-minval)*convergence_tolerance))^2]
    estimated_required_samples <- ceiling(dt_shapley_est0[,median(req_samples)]) # TODO: Consider other ways to do this
    estimated_remaining_samples <- estimated_required_samples - n_current_samples

    converged_sd <- (estimated_remaining_samples <= 0)

    estimated_required_samples_per_explain_id <- dt_shapley_est0[,req_samples]
    names(estimated_required_samples_per_explain_id) <- paste0("req_samples_explain_id_",seq_along(estimated_required_samples_per_explain_id))

  } else {
    estimated_required_samples_per_explain_id <- estimated_required_samples <- estimated_remaining_samples <- NULL
    converged_sd <- FALSE
  }
  converged_max_iter <- (iter >= max_iter)

  converged <- converged_sd || converged_max_iter

  internal$iter_list[[iter]]$converged <- converged
  internal$iter_list[[iter]]$n_current_samples <- n_current_samples
  internal$iter_list[[iter]]$estimated_required_samples <- estimated_required_samples
  internal$iter_list[[iter]]$estimated_remaining_samples <- estimated_remaining_samples
  internal$iter_list[[iter]]$estimated_required_samples_per_explain_id <- as.list(estimated_required_samples_per_explain_id)

  return(internal)

}




#' Compute shapley values
#' @param dt_vS The contribution matrix.
#'
#' @inheritParams default_doc
#'
#' @return A `data.table` with Shapley values for each test observation.
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

#' Mean Squared Error of the Contribution Function `v(S)`
#'
#' @inheritParams explain
#' @inheritParams default_doc
#' @param dt_vS Data.table of dimension `n_combinations` times `n_explain + 1` containing the contribution function
#' estimates. The first column is assumed to be named `id_combination` and containing the ids of the combinations.
#' The last row is assumed to be the full combination, i.e., it contains the predicted responses for the observations
#' which are to be explained.
#' @param MSEv_skip_empty_full_comb Logical. If `TRUE` (default), we exclude the empty and grand
#' combinations/coalitions when computing the MSEv evaluation criterion. This is reasonable as they are identical
#' for all methods, i.e., their contribution function is independent of the used method as they are special cases not
#' effected by the used method. If `FALSE`, we include the empty and grand combinations/coalitions. In this situation,
#' we also recommend setting `MSEv_uniform_comb_weights = TRUE`, as otherwise the large weights for the empty and
#' grand combinations/coalitions will outweigh all other combinations and make the MSEv criterion uninformative.
#'
#' @return
#' List containing:
#' \describe{
#'  \item{`MSEv`}{A \code{\link[data.table]{data.table}} with the overall MSEv evaluation criterion averaged
#'  over both the combinations/coalitions and observations/explicands. The \code{\link[data.table]{data.table}}
#'  also contains the standard deviation of the MSEv values for each explicand (only averaged over the combinations)
#'  divided by the square root of the number of explicands.}
#'  \item{`MSEv_explicand`}{A \code{\link[data.table]{data.table}} with the mean squared error for each
#'  explicand, i.e., only averaged over the combinations/coalitions.}
#'  \item{`MSEv_combination`}{A \code{\link[data.table]{data.table}} with the mean squared error for each
#'  combination/coalition, i.e., only averaged over the explicands/observations.
#'  The \code{\link[data.table]{data.table}} also contains the standard deviation of the MSEv values for
#'  each combination divided by the square root of the number of explicands.}
#' }
#'
#' @description Function that computes the Mean Squared Error (MSEv) of the contribution function
#' v(s) as proposed by \href{https://arxiv.org/pdf/2006.01272.pdf}{Frye et al. (2019)} and used by
#' \href{https://www.jmlr.org/papers/volume23/21-1413/21-1413.pdf}{Olsen et al. (2022)}.
#'
#' @details
#' The MSEv evaluation criterion does not rely on access to the true contribution functions nor the
#' true Shapley values to be computed. A lower value indicates better approximations, however, the
#' scale and magnitude of the MSEv criterion is not directly interpretable in regard to the precision
#' of the final estimated Shapley values. \href{https://arxiv.org/pdf/2305.09536.pdf}{Olsen et al. (2022)}
#' illustrates in Figure 11 a fairly strong linear relationship between the MSEv criterion and the
#' MAE between the estimated and true Shapley values in a simulation study. Note that explicands
#' refer to the observations whose predictions we are to explain.
#'
#' @keywords internal
#' @author Lars Henry Berge Olsen
compute_MSEv_eval_crit <- function(internal,
                                   dt_vS,
                                   MSEv_uniform_comb_weights,
                                   MSEv_skip_empty_full_comb = TRUE) {
  n_explain <- internal$parameters$n_explain
  n_combinations <- internal$parameters$n_combinations
  id_combination_indices <- if (MSEv_skip_empty_full_comb) seq(2, n_combinations - 1) else seq(1, n_combinations)
  n_combinations_used <- length(id_combination_indices)
  features <- internal$objects$X$features[id_combination_indices]

  # Extract the predicted responses f(x)
  p <- unlist(dt_vS[id_combination == n_combinations, -"id_combination"])

  # Create contribution matrix
  vS <- as.matrix(dt_vS[id_combination_indices, -"id_combination"])

  # Square the difference between the v(S) and f(x)
  dt_squared_diff_original <- sweep(vS, 2, p)^2

  # Get the weights
  averaging_weights <- if (MSEv_uniform_comb_weights) rep(1, n_combinations) else internal$objects$X$shapley_weight
  averaging_weights <- averaging_weights[id_combination_indices]
  averaging_weights_scaled <- averaging_weights / sum(averaging_weights)

  # Apply the `averaging_weights_scaled` to each column (i.e., each explicand)
  dt_squared_diff <- dt_squared_diff_original * averaging_weights_scaled

  # Compute the mean squared error for each observation, i.e., only averaged over the coalitions.
  # We take the sum as the weights sum to 1, so denominator is 1.
  MSEv_explicand <- colSums(dt_squared_diff)

  # The MSEv criterion for each coalition, i.e., only averaged over the explicands.
  MSEv_combination <- rowMeans(dt_squared_diff * n_combinations_used)
  MSEv_combination_sd <- apply(dt_squared_diff * n_combinations_used, 1, sd) / sqrt(n_explain)

  # The MSEv criterion averaged over both the coalitions and explicands.
  MSEv <- mean(MSEv_explicand)
  MSEv_sd <- sd(MSEv_explicand) / sqrt(n_explain)

  # Set the name entries in the arrays
  names(MSEv_explicand) <- paste0("id_", seq(n_explain))
  names(MSEv_combination) <- paste0("id_combination_", id_combination_indices)
  names(MSEv_combination_sd) <- paste0("id_combination_", id_combination_indices)

  # Convert the results to data.table
  MSEv <- data.table(
    "MSEv" = MSEv,
    "MSEv_sd" = MSEv_sd
  )
  MSEv_explicand <- data.table(
    "id" = seq(n_explain),
    "MSEv" = MSEv_explicand
  )
  MSEv_combination <- data.table(
    "id_combination" = id_combination_indices,
    "features" = features,
    "MSEv" = MSEv_combination,
    "MSEv_sd" = MSEv_combination_sd
  )

  return(list(
    MSEv = MSEv,
    MSEv_explicand = MSEv_explicand,
    MSEv_combination = MSEv_combination
  ))
}
