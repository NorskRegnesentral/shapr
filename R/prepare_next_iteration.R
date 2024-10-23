prepare_next_iteration <- function(internal) {
  iter <- length(internal$iter_list)
  converged <- internal$iter_list[[iter]]$converged
  paired_shap_sampling <- internal$parameters$paired_shap_sampling

  # TODO: a bit messy to do this here, should be moved somewhere else
  if (internal$parameters$adaptive){
    if (internal$parameters$adaptive_arguments$allow_feature_reduction){
      dropped_features <- internal$iter_list[[iter]]$shap_reduction$dropped_features
      if (nrow(dropped_features) < (iter)){
        dropped_features <- rbind(dropped_features, dropped_features[nrow(dropped_features), 1:ncol(dropped_features)])
        rownames(dropped_features) = paste("Iter", 1:(iter))
      }
    internal$iter_list[[iter]]$shap_reduction$dropped_features <- dropped_features
    }
  }


  if (converged == FALSE) {
    next_iter_list <- list()

    n_shapley_values <- internal$parameters$n_shapley_values
    reduction_factor_vec <- internal$parameters$adaptive_arguments$reduction_factor_vec
    fixed_n_coalitions_per_iter <- internal$parameters$adaptive_arguments$fixed_n_coalitions_per_iter
    max_n_coalitions <- internal$parameters$adaptive_arguments$max_n_coalitions


    est_remaining_coalitions <- internal$iter_list[[iter]]$est_remaining_coalitions
    reduction_factor <- internal$iter_list[[iter]]$reduction_factor
    current_n_coalitions <- internal$iter_list[[iter]]$n_coalitions
    current_coal_samples <- internal$iter_list[[iter]]$coal_samples

    if (is.null(fixed_n_coalitions_per_iter)) {
      proposal_next_n_coalitions <- current_n_coalitions + ceiling(est_remaining_coalitions * reduction_factor)
    } else {
      proposal_next_n_coalitions <- current_n_coalitions + fixed_n_coalitions_per_iter
    }

    # Thresholding if max_n_coalitions is reached
    proposal_next_n_coalitions <- min(
      max_n_coalitions,
      proposal_next_n_coalitions
    )

    if(paired_shap_sampling){
      proposal_next_n_coalitions <- ceiling(proposal_next_n_coalitions*0.5)*2
    }


    if ((proposal_next_n_coalitions) >= 2^n_shapley_values) {
      # Use all coalitions in the last iteration as the estimated number of samples is more than what remains
      next_iter_list$exact <- TRUE
      next_iter_list$n_coalitions <- 2^n_shapley_values
      next_iter_list$compute_sd <- FALSE
    } else {
      # Sample more keeping the current samples
      next_iter_list$exact <- FALSE
      next_iter_list$n_coalitions <- proposal_next_n_coalitions
      next_iter_list$compute_sd <- TRUE # TODO: should this be internal$parameters$adaptive_arguments$compute_sd?
    }

    next_iter_list$reduction_factor <- ifelse(
      length(reduction_factor_vec)>=iter,
      reduction_factor_vec[iter],
      reduction_factor_vec[length(reduction_factor_vec)]
    )

    next_iter_list$prev_coal_samples <- current_coal_samples

    if (internal$parameters$adaptive_arguments$allow_feature_reduction){
      next_iter_list$shap_reduction = list()
      next_iter_list$shap_reduction$reduced_dt_shapley_est <- internal$iter_list[[iter]]$shap_reduction$reduced_dt_shapley_est
      next_iter_list$shap_reduction$reduced_dt_shapley_sd <- internal$iter_list[[iter]]$shap_reduction$reduced_dt_shapley_sd
      next_iter_list$shap_reduction$sum_reduced_shapley_est <- internal$iter_list[[iter]]$shap_reduction$sum_reduced_shapley_est
      next_iter_list$shap_reduction$dropped_features <- internal$iter_list[[iter]]$shap_reduction$dropped_features
    }
  } else {
    next_iter_list <- list()
  }

  internal$iter_list[[iter + 1]] <- next_iter_list


  return(internal)
}
