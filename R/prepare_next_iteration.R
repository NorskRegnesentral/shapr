#' Prepares the next iteration of the iterative sampling algorithm
#'
#' @inheritParams default_doc_export
#'
#' @export
#' @keywords internal
prepare_next_iteration <- function(internal) {
  iter <- length(internal$iter_list)
  converged <- internal$iter_list[[iter]]$converged
  paired_shap_sampling <- internal$parameters$extra_computation_args$paired_shap_sampling


  if (converged == FALSE) {
    next_iter_list <- list()

    n_shapley_values <- internal$parameters$n_shapley_values
    n_coal_next_iter_factor_vec <- internal$parameters$iterative_args$n_coal_next_iter_factor_vec
    fixed_n_coalitions_per_iter <- internal$parameters$iterative_args$fixed_n_coalitions_per_iter
    max_n_coalitions <- internal$parameters$iterative_args$max_n_coalitions


    est_remaining_coalitions <- internal$iter_list[[iter]]$est_remaining_coalitions
    n_coal_next_iter_factor <- internal$iter_list[[iter]]$n_coal_next_iter_factor
    current_n_coalitions <- internal$iter_list[[iter]]$n_sampled_coalitions + 2 # Used instead of n_coalitions to
    # deal with forecast special case
    current_coal_samples <- internal$iter_list[[iter]]$coal_samples
    current_coal_samples_n_unique <- internal$iter_list[[iter]]$coal_samples_n_unique

    if (is.null(fixed_n_coalitions_per_iter)) {
      proposal_next_n_coalitions <- current_n_coalitions + ceiling(est_remaining_coalitions * n_coal_next_iter_factor)
    } else {
      proposal_next_n_coalitions <- current_n_coalitions + fixed_n_coalitions_per_iter
    }

    # Thresholding if max_n_coalitions is reached
    proposal_next_n_coalitions <- min(
      max_n_coalitions,
      proposal_next_n_coalitions
    )

    if (paired_shap_sampling) {
      proposal_next_n_coalitions <- ceiling(proposal_next_n_coalitions * 0.5) * 2
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
      next_iter_list$compute_sd <- TRUE
    }

    if (!is.null(n_coal_next_iter_factor_vec[1])) {
      next_iter_list$n_coal_next_iter_factor <- ifelse(
        length(n_coal_next_iter_factor_vec) >= iter,
        n_coal_next_iter_factor_vec[iter],
        n_coal_next_iter_factor_vec[length(n_coal_next_iter_factor_vec)]
      )
    } else {
      next_iter_list$n_coal_next_iter_factor <- NULL
    }

    next_iter_list$new_n_coalitions <- next_iter_list$n_coalitions - current_n_coalitions

    next_iter_list$n_batches <- set_n_batches(next_iter_list$new_n_coalitions, internal)


    next_iter_list$prev_coal_samples <- current_coal_samples
    next_iter_list$prev_coal_samples_n_unique <- current_coal_samples_n_unique
  } else {
    next_iter_list <- list()
  }

  internal$iter_list[[iter + 1]] <- next_iter_list


  return(internal)
}
