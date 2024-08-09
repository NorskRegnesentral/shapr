print_iter <- function(internal, print_iter_info, print_shapleyres) {


  iter <- length(internal$iter_list) - 1 # This function is called after the preparation of the next iteration

  converged <- internal$iter_list[[iter]]$converged
  converged_exact <- internal$iter_list[[iter]]$converged_exact
  converged_sd <- internal$iter_list[[iter]]$converged_sd
  converged_max_iter <- internal$iter_list[[iter]]$converged_max_iter
  converged_max_n_combinations <- internal$iter_list[[iter]]$converged_max_n_combinations

  if (print_iter_info) {
    convergence_tolerance <- internal$parameters$adaptive_arguments$convergence_tolerance

    current_n_combinations <- internal$iter_list[[iter]]$n_combinations
    est_remaining_combinations <- internal$iter_list[[iter]]$est_remaining_combinations
    est_required_combinations <- internal$iter_list[[iter]]$est_required_combinations

    next_n_combinations <- internal$iter_list[[iter + 1]]$n_combinations

    if (isFALSE(converged)) {
      cat(paste0(
        "\nIteration ", iter, "\n",
        "Not converged after ", current_n_combinations, " coalitions.\n"
      ))
      if(!is.null(convergence_tolerance)){
        cat(paste0(
          "Estimated remaining coalitions: ", est_remaining_combinations, "\n",
          "Estimated required coalitions: ", est_required_combinations, "\n"
        ))
      }
      cat(paste0(
        "Using ", next_n_combinations, " new coalitions in the next iteration.\n"
      ))
    } else {
      cat(paste0(
        "\nIteration ", iter, "\n",
        "Estimation stopped!\n"
      ))
      if (isTRUE(converged_exact)) {
        cat(paste0(
          "All (", current_n_combinations, ") coalitions used.\n"
        ))
      }
      if (isTRUE(converged_sd)) {
        cat(paste0(
          "Convergence tolerance reached after ", current_n_combinations, " coalitions.\n"
        ))
      }
      if (isTRUE(converged_max_iter)) {
        cat(paste0(
          "Maximum number of iterations reached after ", current_n_combinations, " coalitions.\n"
        ))
      }
      if (isTRUE(converged_max_n_combinations)) {
        cat(paste0(
          "Maximum number of coalitions (", current_n_combinations, ") reached.\n"
        ))
      }

    }
  }

  if (print_shapleyres) {
    n_explain <- internal$parameters$n_explain

    dt_shapley_est <- internal$iter_list[[iter]]$dt_shapley_est[, -1]
    dt_shapley_sd <- internal$iter_list[[iter]]$dt_shapley_sd[, -1]

    # Printing the current Shapley values
    matrix1 <- format(round(dt_shapley_est, 3), nsmall = 2, justify = "right")
    matrix2 <- format(round(dt_shapley_sd, 2), nsmall = 2, justify = "right")

    if (isTRUE(converged)) {
      cat("Final ")
    } else {
      cat("Current ")
    }

    if (converged_exact) {
      cat("estimated Shapley values:\n")
      print_dt <- as.data.table(matrix1)
      names(print_dt) <- names(dt_shapley_est)
      print(print_dt)
    } else {
      cat("estimated Shapley values (sd):\n")
      print_dt <- as.data.table(matrix(paste(matrix1, " (", matrix2, ") ", sep = ""), nrow = n_explain))
      names(print_dt) <- names(dt_shapley_est)
      print(print_dt)
    }
  }
}
