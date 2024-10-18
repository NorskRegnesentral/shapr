#' Prints iterative information
#'
#' @inheritParams default_doc_explain
#'
#' @export
#' @keywords internal
print_iter <- function(internal) {
  verbose <- internal$parameters$verbose
  iter <- length(internal$iter_list) - 1 # This function is called after the preparation of the next iteration

  converged <- internal$iter_list[[iter]]$converged
  converged_exact <- internal$iter_list[[iter]]$converged_exact
  converged_sd <- internal$iter_list[[iter]]$converged_sd
  converged_max_iter <- internal$iter_list[[iter]]$converged_max_iter
  converged_max_n_coalitions <- internal$iter_list[[iter]]$converged_max_n_coalitions
  overall_conv_measure <- internal$iter_list[[iter]]$overall_conv_measure
  n_coal_next_iter_factor <- internal$iter_list[[iter]]$n_coal_next_iter_factor

  saving_path <- internal$parameters$output_args$saving_path
<<<<<<< HEAD
  convergence_tolerance <- internal$parameters$iterative_args$convergence_tolerance
  testing <- internal$parameters$testing

  if ("convergence" %in% verbose) {
    convergence_tolerance <- internal$parameters$iterative_args$convergence_tolerance
=======
  convergence_tol <- internal$parameters$iterative_args$convergence_tol
  testing <- internal$parameters$testing

  if ("convergence" %in% verbose) {
    convergence_tol <- internal$parameters$iterative_args$convergence_tol
>>>>>>> origin/shapr-1.0.0

    current_n_coalitions <- internal$iter_list[[iter]]$n_coalitions
    est_remaining_coalitions <- internal$iter_list[[iter]]$est_remaining_coalitions
    est_required_coalitions <- internal$iter_list[[iter]]$est_required_coalitions

    next_n_coalitions <- internal$iter_list[[iter + 1]]$n_coalitions
    next_new_n_coalitions <- internal$iter_list[[iter + 1]]$new_n_coalitions

    cli::cli_h3("Convergence info")

    if (isFALSE(converged)) {
      msg <- "Not converged after {current_n_coalitions} coalitions:\n"

      if (!is.null(convergence_tol)) {
        conv_nice <- signif(overall_conv_measure, 2)
<<<<<<< HEAD
        tol_nice <- format(signif(convergence_tolerance, 2), scientific = FALSE)
=======
        tol_nice <- format(signif(convergence_tol, 2), scientific = FALSE)
>>>>>>> origin/shapr-1.0.0
        n_coal_next_iter_factor_nice <- format(signif(n_coal_next_iter_factor * 100, 2), scientific = FALSE)
        msg <- paste0(
          msg,
          "Current convergence measure: {conv_nice} [needs {tol_nice}]\n",
          "Estimated remaining coalitions: {est_remaining_coalitions}\n",
          "(Concervatively) adding {n_coal_next_iter_factor_nice}% of that ({next_new_n_coalitions} coalitions) ",
          "in the next iteration."
        )
      }
      cli::cli_alert_info(msg)
    } else {
      msg <- "Converged after {current_n_coalitions} coalitions:\n"
      if (isTRUE(converged_exact)) {
        msg <- paste0(
          msg,
          "All ({current_n_coalitions}) coalitions used.\n"
        )
      }
      if (isTRUE(converged_sd)) {
        msg <- paste0(
          msg,
          "Convergence tolerance reached!\n"
        )
      }
      if (isTRUE(converged_max_iter)) {
        msg <- paste0(
          msg,
          "Maximum number of iterations reached!\n"
        )
      }
      if (isTRUE(converged_max_n_coalitions)) {
        msg <- paste0(
          msg,
          "Maximum number of coalitions reached!\n"
        )
      }
      cli::cli_alert_success(msg)
    }
  }

  if ("shapley" %in% verbose) {
    n_explain <- internal$parameters$n_explain

    dt_shapley_est <- internal$iter_list[[iter]]$dt_shapley_est[, -1]
    dt_shapley_sd <- internal$iter_list[[iter]]$dt_shapley_sd[, -1]

    # Printing the current Shapley values
    matrix1 <- format(round(dt_shapley_est, 3), nsmall = 2, justify = "right")
    matrix2 <- format(round(dt_shapley_sd, 2), nsmall = 2, justify = "right")

    if (isTRUE(converged)) {
      msg <- "Final "
    } else {
      msg <- "Current "
    }

    if (converged_exact) {
      msg <- paste0(msg, "estimated Shapley values")
      print_dt <- as.data.table(matrix1)
    } else {
      msg <- paste0(msg, "estimated Shapley values (sd)")
      print_dt <- as.data.table(matrix(paste(matrix1, " (", matrix2, ") ", sep = ""), nrow = n_explain))
    }

    cli::cli_h3(msg)
    names(print_dt) <- names(dt_shapley_est)
    print(print_dt)
  }
}
