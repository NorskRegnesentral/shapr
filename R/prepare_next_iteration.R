prepare_next_iteration <- function(internal) {
  iter <- length(internal$iter_list)
  converged <- internal$iter_list[[iter]]$converged


  if (converged == FALSE) {
    next_iter_list <- list()

    n_features <- internal$parameters$n_features
    reduction_factor_vec <- internal$parameters$adaptive_arguments$reduction_factor_vec
    fixed_n_combinations_per_iter <- internal$parameters$adaptive_arguments$fixed_n_combinations_per_iter

    est_remaining_combinations <- internal$iter_list[[iter]]$est_remaining_combinations
    reduction_factor <- internal$iter_list[[iter]]$reduction_factor
    current_n_combinations <- internal$iter_list[[iter]]$n_combinations


    X <- internal$iter_list[[iter]]$X


    if(is.null(fixed_n_combinations_per_iter)){
      proposal_next_n_combinations <- ceiling(est_remaining_combinations * reduction_factor)
    } else {
      proposal_next_n_combinations <- fixed_n_combinations_per_iter
    }

    if ((current_n_combinations + proposal_next_n_combinations) >= 2^n_features) {
      # Use all coalitions in the last iteration as the estimated number of samples is more than what remains
      next_iter_list$exact <- TRUE
      next_iter_list$n_combinations <- 2^n_features - current_n_combinations + 2
      next_iter_list$compute_sd <- FALSE
    } else {
      # Sample more keeping the current samples
      next_iter_list$exact <- FALSE
      next_iter_list$n_combinations <- proposal_next_n_combinations
      next_iter_list$compute_sd <- TRUE
    }

    next_iter_list$reduction_factor <- reduction_factor_vec[iter]

    # Storing the feature samples I have from before (not sure I need these if I run exact).
    # Could also be moved to shapley_setup as it is computed based on X only, and that is stored in previous iter_list
    repetitions <- X[-c(1, .N), sample_freq]
    unique_feature_samples <- X[-c(1, .N), features]

    next_iter_list$prev_feature_samples <- unlist(
      lapply(
        seq_along(unique_feature_samples),
        function(i) {
          rep(
            list(unique_feature_samples[[i]]),
            repetitions[i]
          )
        }
      ),
      recursive = FALSE
    )
  } else {
    next_iter_list <- list()
  }

  internal$iter_list[[iter + 1]] <- next_iter_list

  return(internal)
}
