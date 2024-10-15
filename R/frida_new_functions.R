probfunc <- function(est, sd, shapley_threshold_val){
  prob <- 1 - (pnorm(shapley_threshold_val, mean = est, sd = sd) - pnorm(-shapley_threshold_val, mean = est, sd = sd))
  return(prob)
}

check_reduction <- function(internal){

    iter <- length(internal$iter_list)

    if (iter == 1){
        S_mapper = data.table(
                             feature_numbers = 1:ncol(internal$data$x_explain),
                             feature_names = colnames(internal$data$x_explain)
                             )
        internal$iter_list[[iter]]$S_mapper = S_mapper
    }

    shapley_threshold_val <- internal$parameters$adaptive_arguments$shapley_threshold_val
    shapley_threshold_prob <- internal$parameters$adaptive_arguments$shapley_threshold_prob
    dt_shapley_est <- internal$iter_list[[iter]]$dt_shapley_est
    dt_shapley_sd <- internal$iter_list[[iter]]$dt_shapley_sd

    kshap_est_mat = as.matrix(dt_shapley_est[, -"explain_id"])
    kshap_sd_mat = as.matrix(dt_shapley_sd[, -"explain_id"])

    prob_of_red <- probfunc(kshap_est_mat, kshap_sd_mat, shapley_threshold_val)

    if (!any(prob_of_red < shapley_threshold_prob)){
        # TODO: ensure tht keep in this iter is the same as in the prev iter.
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

    exclude_feature = which.min(prob_of_red[, -1])
    # exclude_feature_bin = min.ind(prob_of_red[, -1])
    exclude_feature_names = min.names(prob_of_red[, -1])

    # TODO: create a method for saving these in each iter
    # Keep the Shapley value estimate of the features that are excluded

    internal$iter_list[[iter]]$reduction <- list()

    keep = internal$iter_list[[iter]]$reduction$keep
    keep = c(keep, dt_shapley_est[, ..exclude_feature_names])
    internal$iter_list[[iter]]$reduction$keep = keep

    internal$iter_list[[iter]]$reduction$exclude_feature <- exclude_feature

    dt_vS_reduced <- reduce(internal)
    internal$iter_list[[iter]]$reduction$dt_vS_reduced <- dt_vS_reduced
    internal$iter_list[[iter]]$reduction$n_features <- internal$parameters$n_features - length(keep)

    internal$iter_list[[iter]]$reduction$prediction_zero <- internal$parameters$prediction_zero + sum(keep)

    return(internal)
}


reduce <- function(internal){
    next_S_mapper <- internal$iter_list[[iter]]$S_mapper[-exclude_feature, ]
    internal$iter_list[[iter]]$S_mapper <- next_S_mapper

    X <- internal$iter_list[[iter]]$X   # Should this be X og X_curr in new code?
    Xtmp <- X[, .(coalitions, id_coalition, coalition_size)]

    Xtmp[, coalitions_next := lapply(coalitions, function(x) x[!(x %in% exclude_feature)])]
    Xtmp[, coalitions_next := sapply(coalitions_next, match, next_S_mapper$feature_numbers)]
    Xtmp[, coalitions_next_char := sapply(coalitions_next, paste0, collapse = "_")]
    Xtmp[, coalitions_char := sapply(coalitions, paste0, collapse = "_")]

    # Uses 12|345 as replacement for 12|34, if 5 is removed
    Xtmp[, keep := !duplicated(coalitions_next_char)]
    Xtmp[, id_coalition_next := .GRP, by = coalitions_next_char]

    # If any of the reduced coalitions are equal to the full and zero coalition,
    # we remove these, and keep the original zero and full coalition
    id_coalition_next_keepers <- Xtmp[c(1, .N), id_coalition_next]
    Xtmp[id_coalition_next %in% id_coalition_next_keepers, keep := FALSE]
    Xtmp[c(1, .N), keep := TRUE]

    # TODO: should this be dt_vS or dt_vS_curr?
    dt_vS = internal$iter_list[[iter]]$dt_vS

    dt_vS = merge(dt_vS,
                  Xtmp[, .(id_coalition, id_coalition_next)],
                  by = "id_coalition")

    dt_vS[, p_hat1_1 := mean(p_hat1_1), by = id_coalition_next]

    dt_vS = unique(dt_vS[, .(id_coalition = id_coalition_next, p_hat1_1)])
}
