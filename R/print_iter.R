#' Prints iterative information
#'
#' @inheritParams default_doc_explain
#'
#' @export
#' @keywords internal
print_iter <- function(internal, print_iter_info, print_shapleyres) {
  # TODO: print_iter_info, print_shapleyres will be replaced by verbose extracted from internal
  iter <- length(internal$iter_list) - 1 # This function is called after the preparation of the next iteration

  converged <- internal$iter_list[[iter]]$converged
  converged_exact <- internal$iter_list[[iter]]$converged_exact
  converged_sd <- internal$iter_list[[iter]]$converged_sd
  converged_max_iter <- internal$iter_list[[iter]]$converged_max_iter
  converged_max_n_coalitions <- internal$iter_list[[iter]]$converged_max_n_coalitions

  saving_path <- internal$parameters$adaptive_arguments$saving_path
  testing <- internal$parameters$testing

  if (print_iter_info) {
    convergence_tolerance <- internal$parameters$adaptive_arguments$convergence_tolerance

    current_n_coalitions <- internal$iter_list[[iter]]$n_coalitions
    est_remaining_coalitions <- internal$iter_list[[iter]]$est_remaining_coalitions
    est_required_coalitions <- internal$iter_list[[iter]]$est_required_coalitions

    next_n_coalitions <- internal$iter_list[[iter + 1]]$n_coalitions

    if (isFALSE(converged)) {
      cat(paste0(
        "\nIteration ", iter, "\n",
        "Not converged after ", current_n_coalitions, " coalitions.\n"
      ))
      if (!is.null(convergence_tolerance)) {
        cat(paste0(
          "Estimated remaining coalitions: ", est_remaining_coalitions, "\n",
          "Estimated required coalitions: ", est_required_coalitions, "\n"
        ))
      }
      cat(paste0(
        "Using ", next_n_coalitions - current_n_coalitions, " new coalitions in the next iteration.\n"
      ))
    } else {
      cat(paste0(
        "\nIteration ", iter, "\n",
        "Estimation stopped!\n"
      ))
      if (isTRUE(converged_exact)) {
        cat(paste0(
          "All (", current_n_coalitions, ") coalitions used.\n"
        ))
      }
      if (isTRUE(converged_sd)) {
        cat(paste0(
          "Convergence tolerance reached after ", current_n_coalitions, " coalitions.\n"
        ))
      }
      if (isTRUE(converged_max_iter)) {
        cat(paste0(
          "Maximum number of iterations reached after ", current_n_coalitions, " coalitions.\n"
        ))
      }
      if (isTRUE(converged_max_n_coalitions)) {
        cat(paste0(
          "Maximum number of coalitions (", current_n_coalitions, ") reached.\n"
        ))
      }
    }

    # Printing saving_path unless testing is TRUE
    if (isFALSE(testing)) {
      cat(paste0(
        "Intermediate computations saved at ", saving_path, ".\n"
      ))
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
