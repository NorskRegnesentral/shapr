#' Prepare the Next Iteration of the Iterative Sampling Algorithm
#'
#' @inheritParams default_doc_export
#'
#' @return The (updated) internal list
#'
#' @export
#' @keywords internal
prepare_next_iteration <- function(internal) {
  iter <- length(internal$iter_list)
  converged <- internal$iter_list[[iter]]$converged
  paired_shap_sampling <- internal$parameters$extra_computation_args$paired_shap_sampling
  semi_deterministic_sampling <- internal$parameters$extra_computation_args$semi_deterministic_sampling
  type <- internal$parameters$type


  if (converged == FALSE) {
    next_iter_list <- list()

    n_shapley_values <- internal$parameters$n_shapley_values
    n_coal_next_iter_factor_vec <- internal$parameters$iterative_args$n_coal_next_iter_factor_vec
    fixed_n_coalitions_per_iter <- internal$parameters$iterative_args$fixed_n_coalitions_per_iter
    max_n_coalitions <- internal$parameters$iterative_args$max_n_coalitions


    est_remaining_coal_samp <- internal$iter_list[[iter]]$est_remaining_coal_samp
    n_coal_next_iter_factor <- internal$iter_list[[iter]]$n_coal_next_iter_factor
    n_sampled_coalitions <- internal$iter_list[[iter]]$n_sampled_coalitions
    current_n_coalitions <- n_sampled_coalitions + 2 # Used instead of n_coalitions to deal with forecast special case

    if (is.null(fixed_n_coalitions_per_iter)) {
      if (paired_shap_sampling) {
        proposal_new_n_coalitions <- max(2, round(est_remaining_coal_samp * n_coal_next_iter_factor * 0.5) * 2)
      } else {
        proposal_new_n_coalitions <- max(1, round(est_remaining_coal_samp * n_coal_next_iter_factor))
      }
    } else {
      proposal_new_n_coalitions <- fixed_n_coalitions_per_iter
    }

    remaining_coalitions <- max_n_coalitions - current_n_coalitions

    # Thresholding if max_n_coalitions is reached
    proposal_new_n_coalitions <- min(
      remaining_coalitions,
      proposal_new_n_coalitions
    )

    # Ensure an even number also after thresholding
    if (paired_shap_sampling) {
      proposal_new_n_coalitions <- round(proposal_new_n_coalitions * 0.5) * 2
    }

    if (proposal_new_n_coalitions >= 2^n_shapley_values - current_n_coalitions) {
      # Use all coalitions in the last iteration as the estimated number of samples is more than what remains
      next_iter_list$exact <- TRUE
      next_iter_list$n_coalitions <- 2^n_shapley_values
      next_iter_list$compute_sd <- FALSE
    } else {
      # Sample more keeping the current samples
      next_iter_list$exact <- FALSE
      next_iter_list$n_coalitions <- proposal_new_n_coalitions + current_n_coalitions
      next_iter_list$compute_sd <- TRUE
    }

    if (!is.null(n_coal_next_iter_factor_vec[1])) {
      next_iter_list$n_coal_next_iter_factor <- ifelse(
        length(n_coal_next_iter_factor_vec) >= iter,
        n_coal_next_iter_factor_vec[iter + 1],
        n_coal_next_iter_factor_vec[length(n_coal_next_iter_factor_vec)]
      )
    } else {
      next_iter_list$n_coal_next_iter_factor <- NULL
    }

    next_iter_list$new_n_coalitions <- next_iter_list$n_coalitions - current_n_coalitions

    next_iter_list$n_batches <- set_n_batches(next_iter_list$new_n_coalitions, internal)

    if (type == "forecast") {
      # Do not update dt_coal_samp_info for forecast as it is generated on the fly since the
      # number of features changes with the horizon.
      next_iter_list$prev_X_list <- internal$iter_list[[iter]]$X_list
    } else {
      next_iter_list$prev_X <- internal$iter_list[[iter]]$X
      next_iter_list$dt_coal_samp_info <-
        internal$objects$dt_coal_samp_info[next_iter_list$n_coalitions <= n_coal_max][1]
    }
  } else {
    next_iter_list <- list()
  }

  internal$iter_list[[iter + 1]] <- next_iter_list


  return(internal)
}
