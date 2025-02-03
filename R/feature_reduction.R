probfunc <- function(est, sd, shapley_threshold_val,addnoise=TRUE){
  prob <- 1 - (pnorm(shapley_threshold_val, mean = est, sd = sd) - pnorm(-shapley_threshold_val, mean = est, sd = sd))
  if(addnoise){
    prob <- prob + runif(length(est),10^(-7),10^(-6))  # Making sure there are no identical probabilities
  }
  return(prob)
}

check_reduction <- function(internal){
    iter <- length(internal$iter_list)

    adaptive <- internal$parameters$adaptive
    if (!adaptive){
      return(internal)
    }
    if (!internal$parameters$adaptive_arguments$allow_feature_reduction){
      return(internal)
    }
    if (internal$iter_list[[iter]]$converged){
      # Update the total number of coalitions computed so far. In the case of feature reduction, this can differ
      # from the number of rows in the X-matrix since we may have discarded some coalitions.
       if (iter > 1){
        n_prev = internal$iter_list[[iter-1]]$n_coalitions
        n = internal$iter_list[[iter]]$n_coalitions
        tot_n = internal$iter_list[[iter-1]]$total_n_coalitions
        tot_n = tot_n + n - n_prev
        internal$iter_list[[iter]]$total_n_coalitions = tot_n
      }

      internal$iter_list[[iter]]$prob_of_red = internal$iter_list[[iter]]$prob_of_red*NA
      return(internal)
    }


    if (nrow(internal$data$x_explain) > 1){
        if (iter == 1){
            print("Feature reduction is only implemented for one observation at a time.")
            print("Proceeding without feature reduction.")
        }
        return(internal)
    }

    # TODO: Need to take group into account
    S_mapper = data.table(
                        feature_numbers = 1:internal$parameters$n_shapley_values,
                        feature_names = internal$parameters$shap_names
                        )
    internal$iter_list[[iter]]$shap_reduction$S_mapper = S_mapper

    shapley_threshold_val <- internal$parameters$adaptive_arguments$shapley_threshold_val
    shapley_threshold_prob <- internal$parameters$adaptive_arguments$shapley_threshold_prob
    dt_shapley_est <- internal$iter_list[[iter]]$dt_shapley_est
    dt_shapley_sd <- internal$iter_list[[iter]]$dt_shapley_sd

    reduced_dt_shapley_est = internal$iter_list[[iter]]$shap_reduction$reduced_dt_shapley_est

    if (!is.null(reduced_dt_shapley_est)){
      reduced_names = colnames(reduced_dt_shapley_est)

      dt_shapley_est = dt_shapley_est[, -..reduced_names]
      dt_shapley_sd = dt_shapley_sd[, -..reduced_names]
    }

    kshap_est_mat = as.matrix(dt_shapley_est[, -"explain_id"])
    kshap_sd_mat = as.matrix(dt_shapley_sd[, -"explain_id"])

    prob_of_red <- probfunc(kshap_est_mat[,-1,drop=FALSE], kshap_sd_mat[,-1,drop=FALSE], shapley_threshold_val)
    internal$iter_list[[iter]]$prob_of_red = as.data.table(prob_of_red)

    if (!any(prob_of_red < shapley_threshold_prob)){
      # Update the total number of coalitions computed so far. In the case of feature reduction, this can differ
      # from the number of rows in the X-matrix since we may have discarded some coalitions.
      if (iter > 1){
        n_prev = internal$iter_list[[iter-1]]$n_coalitions
        n = internal$iter_list[[iter]]$n_coalitions
        tot_n = internal$iter_list[[iter-1]]$total_n_coalitions
        tot_n = tot_n + n - n_prev
        internal$iter_list[[iter]]$total_n_coalitions = tot_n
      }
      return(internal)
    }

    min.ind <- function(row){
        row.ind = as.integer(row == min(row))
        return(row.ind)
    }

    min.names <- function(row){
        row.ind = as.integer(row == min(row))
        row.names = names(row)[row.ind == 1]
        return(row.names)
    }

    exclude_feature = which.min(prob_of_red)
    exclude_feature_names = colnames(prob_of_red)[exclude_feature]

    # Keep the Shapley value estimate of the features that are excluded
    keep = internal$iter_list[[iter]]$shap_reduction$reduced_dt_shapley_est
    keep = as.data.table(c(keep, dt_shapley_est[, ..exclude_feature_names]))
    internal$iter_list[[iter]]$shap_reduction$reduced_dt_shapley_est = keep
    internal$iter_list[[iter]]$shap_reduction$sum_reduced_shapley_est = sum(keep)

    # Also keep the standard deviation
    keep_sd = internal$iter_list[[iter]]$shap_reduction$reduced_dt_shapley_sd
    keep_sd = as.data.table(c(keep_sd, dt_shapley_sd[, ..exclude_feature_names]))
    internal$iter_list[[iter]]$shap_reduction$reduced_dt_shapley_sd = keep_sd

    dropped_features = internal$iter_list[[iter]]$shap_reduction$dropped_features
    new_row = matrix(NA, 1, ncol(dropped_features))
    shap_names = c("none", internal$parameters$shap_names_org)
    ind = which(shap_names %in% colnames(keep))
    new_row[, ind] = 1
    rownames(new_row) = paste0("Iter ", iter)
    internal$iter_list[[iter]]$shap_reduction$dropped_features = rbind(dropped_features, new_row)

    # Since n_shapley_values and n_features are used in 'shapley_setup()', we need to update both.
    internal$parameters$n_shapley_values <- internal$parameters$n_shapley_values - length(exclude_feature)
    internal$parameters$n_features <- internal$parameters$n_features - length(exclude_feature)
    internal$parameters$feature_names <- internal$parameters$feature_names[!internal$parameters$feature_names %in% exclude_feature_names]
    internal$parameters$shap_names <- internal$parameters$shap_names[!internal$parameters$shap_names %in% exclude_feature_names]

    internal$data$x_train <- subset(internal$data$x_train,select=-exclude_feature)
    internal$data$x_explain <- subset(internal$data$x_explain,select=-exclude_feature)

    if("gaussian" %in% approach){
      internal$parameters$gaussian.mu <- internal$parameters$gaussian.mu[-exclude_feature]
      internal$parameters$gaussian.cov_mat <- internal$parameters$gaussian.cov_mat[-exclude_feature,-exclude_feature]
    }

    internal$iter_list[[iter]]$shap_reduction$exclude_feature <- exclude_feature

    internal <- reduce(internal)

    return(internal)
}


reduce <- function(internal){
  reduction_strategy <- internal$parameters$reduction_strategy

    iter <- length(internal$iter_list)

    exclude_feature = internal$iter_list[[iter]]$shap_reduction$exclude_feature

    next_S_mapper <- internal$iter_list[[iter]]$shap_reduction$S_mapper[-exclude_feature, ]

    X_org <- internal$iter_list[[iter]]$X   # Should this be X og X_curr in new code?
    Xtmp = X_org[, .(coalitions, id_coalition, coalition_size, sample_freq)]

    m <- internal$iter_list[[iter]]$X[.N,coalition_size]

    Xtmp[,coalitions_bar:=lapply(coalitions, function(x) seq(m)[-x])]
    Xtmp[1, coalitions_bar := list(1:m)]

    Xtmp[, coalitions_next := lapply(coalitions, function(x) x[!(x %in% exclude_feature)])]
    Xtmp[, coalitions_next := sapply(coalitions_next, match, next_S_mapper$feature_numbers)]
    Xtmp[, coalitions_next_char := sapply(coalitions_next, paste0, collapse = "_")]
    Xtmp[, coalitions_char := sapply(coalitions, paste0, collapse = "_")]

    Xtmp[, coalitions_bar_next := lapply(coalitions_bar, function(x) x[!(x %in% exclude_feature)])]
    Xtmp[, coalitions_bar_next := sapply(coalitions_bar_next, match, next_S_mapper$feature_numbers)]
    Xtmp[, coalitions_bar_next_char := sapply(coalitions_bar_next, paste0, collapse = "_")]
    Xtmp[, coalitions_bar_char := sapply(coalitions_bar, paste0, collapse = "_")]

    if(is.null(reduction_strategy)) reduction_strategy = "by_S"

    if(reduction_strategy == "by_S"){
      # Uses 12|345 as replacement for 12|34, if 5 is removed
      setorder(Xtmp,id_coalition )
      Xtmp[, keep := !duplicated(coalitions_next_char)]
    } else if(reduction_strategy == "by_Sbar"){
      # Uses 125|34 as replacement for 12|34, if 5 is removed
      setorder(Xtmp,-id_coalition )
      Xtmp[, keep := !duplicated(coalitions_next_char)]
      setorder(Xtmp,id_coalition )
    } else {
      # Uses the mean of 125|34 and 12|345 as replacement for 12|34, if 5 is removed
      Xtmp[,keep:=TRUE]
    }


    Xtmp[, id_coalition_next := .GRP, by = coalitions_next_char]

    # If any of the reduced coalitions are equal to the full and zero coalition,
    # we remove these, and keep the original zero and full coalition
    id_coalition_next_keepers <- Xtmp[c(1, .N), id_coalition_next]
    Xtmp[id_coalition_next %in% id_coalition_next_keepers, keep := FALSE]
    Xtmp[c(1, .N), keep := TRUE]

    # TODO: should this be dt_vS or dt_vS_curr?
    dt_vS = internal$iter_list[[iter]]$dt_vS
    cnames_dt_vS = colnames(dt_vS)

    X_dt_vS = merge(dt_vS,
                  Xtmp,
                  # by = c("coalitions_char", "coalitions_bar_char") # How its done in 'iterative_kshap_func(...)', but the above is equivalent (I think)
                  by = "id_coalition"
                  )

    ################### BALANCING WEIGHTS AFTER REDUCTION ###################
    # TODO: When reducing the number of features in the paired_shap_sampling approach,
    # the bootstrap code expects the sum of the sample_freq in X to be an even number
    # (such that half the samples are used for the 'unpaired' and the other half for the 'paired' samples).
    # A solution that works code-wise, is to sum the sample_freq over the rows that are reduced to the same rows.
    # This ensures that the 'unpaired' and 'paired' versions have the same sample_freq. Another solution is to
    # take the mean of the sample_freq over the rows that are reduced to the same rows, and round to the nearest integer.
    # This may be less biased, as in the summations case, the sample_freq will increase for the rows that are reduced to the same rows.
    # This is done in the following lines of code. We omit the full and empty rows since these are required in order
    # for the Shapley values to sum to the prediction.
    weight_balancing = "mean" # "mean" or "sum"
    if (weight_balancing == "mean"){
        X_dt_vS[-c(1, .N), sample_freq := max(round(mean(sample_freq)), 1), by = id_coalition_next]
    } else if (weight_balancing == "sum"){
        X_dt_vS[-c(1, .N), sample_freq := sum(sample_freq), by = id_coalition_next]
    }

    # Update the total number of coalitions computed so far.
    if (iter > 1){
        n_prev = internal$iter_list[[iter-1]]$n_coalitions
        n = X_dt_vS[, .N]
        tot_n = internal$iter_list[[iter-1]]$total_n_coalitions
        tot_n = tot_n + n - n_prev
        internal$iter_list[[iter]]$total_n_coalitions = tot_n
    }

    X_dt_vS = X_dt_vS[keep == TRUE]

    # This was used in 'iterative_kshap_func(...)', but is not used with this reduction strategy
    # might needed if other strategies are revisited
    # X_dt_vS[, p_hat1_1 := mean(p_hat1_1), by = id_coalition_next]

    X_dt_vS[, id_coalition := id_coalition_next]
    X_dt_vS = unique(X_dt_vS, by = c("coalitions_char", "coalitions_bar_char"))

    # Need to update coalition_map to correspond to the new dt_vS
    # This is necessary for the next iteration
    coalition_map = X_dt_vS[, .(coalitions_str = sapply(coalitions_next, paste, collapse = " "), id_coalition)]
    internal$iter_list[[iter]]$coalition_map = coalition_map

    Xtmp = X_dt_vS[, -..cnames_dt_vS]
    dt_vS = X_dt_vS[, ..cnames_dt_vS]

    # Must update vS_list since this is what will be used in the next iteration
    vS_list = list(dt_vS[-1,]) # Remove empty row since this is not part in the original vS_list
    internal$iter_list[[iter]]$vS_list = vS_list

    internal$iter_list[[iter]]$dt_vS_org = internal$iter_list[[iter]]$dt_vS
    internal$iter_list[[iter]]$dt_vS <- dt_vS

    n_coalitions_org = internal$iter_list[[iter]]$n_coalitions
    internal$iter_list[[iter]]$n_coalitions = nrow(Xtmp)
    internal$iter_list[[iter]]$n_coalitions_org = n_coalitions_org

    n_shapley_values = internal$parameters$n_shapley_values
    Xtmp[.N, coalitions_next := 1:n_shapley_values]
    repetitions <- Xtmp[-c(1, .N), sample_freq]

    # Have to update coal_samples since this is what will be used in the next iteration
    unique_coal_samples <- Xtmp[-c(1, .N), coalitions_next]

    coal_samples <- unlist(
      lapply(
        seq_along(unique_coal_samples),
        function(i) {
          rep(
            list(unique_coal_samples[[i]]),
            repetitions[i]
          )
        }
      ),
      recursive = FALSE
    )

    internal$iter_list[[iter]]$coal_samples_org = internal$iter_list[[iter]]$coal_samples
    internal$iter_list[[iter]]$coal_samples <- coal_samples




    return(internal)
}
