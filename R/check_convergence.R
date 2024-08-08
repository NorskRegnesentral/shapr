check_convergence <- function(internal) {
  iter <- length(internal$iter_list)

  convergence_tolerance <- internal$parameters$adaptive_arguments$convergence_tolerance
  max_iter <- internal$parameters$adaptive_arguments$max_iter

  exact <- internal$iter_list[[iter]]$exact

  dt_shapley_est <- internal$iter_list[[iter]]$dt_shapley_est
  dt_shapley_sd <- internal$iter_list[[iter]]$dt_shapley_sd

  n_sampled_combinations <- internal$iter_list[[iter]]$n_combinations - 2 # Subtract the zero and full predictions

  max_sd <- dt_shapley_sd[, max(.SD), .SDcols = -1, by = .I]$V1 # Max per prediction
  max_sd0 <- max_sd * sqrt(n_sampled_combinations) # Scales UP the sd as it scales at this rate

  dt_shapley_est0 <- copy(dt_shapley_est)

  est_required_combs_per_ex_id <- est_required_combinations <- est_remaining_combinations <- NA

  if (isTRUE(exact)) {
    converged_exact <- TRUE
    converged_sd <- FALSE
  } else {
    converged_exact <- FALSE
    if (!is.null(convergence_tolerance)) {
      dt_shapley_est0[, maxval := max(.SD), .SDcols = -c(1, 2), by = .I]
      dt_shapley_est0[, minval := min(.SD), .SDcols = -c(1, 2), by = .I]
      dt_shapley_est0[, max_sd0 := max_sd0]
      dt_shapley_est0[, req_samples := (max_sd0 / ((maxval - minval) * convergence_tolerance))^2]
      est_required_combinations <- ceiling(dt_shapley_est0[, median(req_samples)]) # TODO:Consider other ways to do this
      est_remaining_combinations <- max(0, est_required_combinations - n_sampled_combinations)

      converged_sd <- (est_remaining_combinations == 0)

      est_required_combs_per_ex_id <- dt_shapley_est0[, req_samples]
      names(est_required_combs_per_ex_id) <- paste0(
        "req_samples_explain_id_",
        seq_along(est_required_combs_per_ex_id)
      )
    } else {
      converged_sd <- FALSE
    }
  }

  converged_max_iter <- (iter >= max_iter)

  converged <- converged_exact || converged_sd || converged_max_iter

  internal$iter_list[[iter]]$converged <- converged
  internal$iter_list[[iter]]$converged_exact <- converged_exact
  internal$iter_list[[iter]]$converged_sd <- converged_sd
  internal$iter_list[[iter]]$converged_max_iter <- converged_max_iter
  internal$iter_list[[iter]]$est_required_combinations <- est_required_combinations
  internal$iter_list[[iter]]$est_remaining_combinations <- est_remaining_combinations
  internal$iter_list[[iter]]$est_required_combs_per_ex_id <- as.list(est_required_combs_per_ex_id)

  internal$timing_list$check_convergence <- Sys.time()

  return(internal)
}
