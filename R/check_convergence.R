#' Checks the convergence according to the convergence threshold
#'
#' @inheritParams default_doc_export
#'
#' @export
#' @keywords internal
check_convergence <- function(internal) {
  iter <- length(internal$iter_list)

  convergence_tol <- internal$parameters$iterative_args$convergence_tol
  max_iter <- internal$parameters$iterative_args$max_iter
  max_n_coalitions <- internal$parameters$iterative_args$max_n_coalitions
  paired_shap_sampling <- internal$parameters$extra_computation_args$paired_shap_sampling
  n_shapley_values <- internal$parameters$n_shapley_values

  n_sampled_coalitions <- internal$iter_list[[iter]]$n_sampled_coalitions
  exact <- internal$iter_list[[iter]]$exact

  shap_names <- internal$parameters$shap_names
  shap_names_with_none <- c("none", shap_names)

  dt_shapley_est <- internal$iter_list[[iter]]$dt_shapley_est
  dt_shapley_sd <- internal$iter_list[[iter]]$dt_shapley_sd

  if (!all.equal(names(dt_shapley_est), names(dt_shapley_sd))) {
    stop("The column names of the dt_shapley_est and dt_shapley_df are not equal.")
  }

  max_sd <- dt_shapley_sd[, max(.SD, na.rm = TRUE), .SDcols = shap_names_with_none, by = .I]$V1 # Max per prediction
  max_sd0 <- max_sd * sqrt(n_sampled_coalitions) # Scales UP the sd as it scales at this rate

  dt_shapley_est0 <- copy(dt_shapley_est)

  est_required_coals_per_ex_id <- est_required_coalitions <- est_remaining_coalitions <- overall_conv_measure <- NA

  if (isTRUE(exact)) {
    converged_exact <- TRUE
    converged_sd <- FALSE
  } else {
    converged_exact <- FALSE
    if (!is.null(convergence_tol)) {
      dt_shapley_est0[, maxval := max(.SD, na.rm = TRUE), .SDcols = shap_names, by = .I]
      dt_shapley_est0[, minval := min(.SD, na.rm = TRUE), .SDcols = shap_names, by = .I]
      dt_shapley_est0[, max_sd0 := max_sd0]
      dt_shapley_est0[, req_samples := (max_sd0 / ((maxval - minval) * convergence_tol))^2]
      dt_shapley_est0[, conv_measure := max_sd0 / ((maxval - minval) * sqrt(n_sampled_coalitions))]
      dt_shapley_est0[, req_samples := min(req_samples, 2^n_shapley_values - 2)]

      est_required_coalitions <- ceiling(dt_shapley_est0[, median(req_samples)])
      if (isTRUE(paired_shap_sampling)) {
        est_required_coalitions <- ceiling(est_required_coalitions * 0.5) * 2
      }
      est_remaining_coalitions <- max(0, est_required_coalitions - (n_sampled_coalitions + 2))

      overall_conv_measure <- dt_shapley_est0[, median(conv_measure)]

      converged_sd <- (est_remaining_coalitions == 0)

      est_required_coals_per_ex_id <- dt_shapley_est0[, req_samples]
      names(est_required_coals_per_ex_id) <- paste0(
        "req_samples_explain_id_",
        seq_along(est_required_coals_per_ex_id)
      )
    } else {
      converged_sd <- FALSE
    }
  }

  converged_max_n_coalitions <- (n_sampled_coalitions + 2 >= max_n_coalitions)

  converged_max_iter <- (iter >= max_iter)

  converged <- converged_exact || converged_sd || converged_max_iter || converged_max_n_coalitions

  internal$iter_list[[iter]]$converged <- converged
  internal$iter_list[[iter]]$converged_exact <- converged_exact
  internal$iter_list[[iter]]$converged_sd <- converged_sd
  internal$iter_list[[iter]]$converged_max_iter <- converged_max_iter
  internal$iter_list[[iter]]$converged_max_n_coalitions <- converged_max_n_coalitions
  internal$iter_list[[iter]]$est_required_coalitions <- est_required_coalitions
  internal$iter_list[[iter]]$est_remaining_coalitions <- est_remaining_coalitions
  internal$iter_list[[iter]]$est_required_coals_per_ex_id <- as.list(est_required_coals_per_ex_id)
  internal$iter_list[[iter]]$overall_conv_measure <- overall_conv_measure

  internal$timing_list$check_convergence <- Sys.time()

  return(internal)
}
