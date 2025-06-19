format_convergence_info <- function(internal,iter){
  converged <- internal$iter_list[[iter]]$converged
  converged_exact <- internal$iter_list[[iter]]$converged_exact
  converged_sd <- internal$iter_list[[iter]]$converged_sd
  converged_max_iter <- internal$iter_list[[iter]]$converged_max_iter
  converged_max_n_coalitions <- internal$iter_list[[iter]]$converged_max_n_coalitions
  overall_conv_measure <- internal$iter_list[[iter]]$overall_conv_measure
  n_coal_next_iter_factor <- internal$iter_list[[iter]]$n_coal_next_iter_factor
  current_n_coalitions <- internal$iter_list[[iter]]$n_sampled_coalitions + 2
  est_remaining_coal_samp <- internal$iter_list[[iter]]$est_remaining_coal_samp
  est_required_coal_samp <- internal$iter_list[[iter]]$est_required_coal_samp

  convergence_tol <- internal$parameters$iterative_args$convergence_tol

  if (isFALSE(converged)) {

    next_new_n_coalitions <- internal$iter_list[[iter + 1]]$new_n_coalitions

    # No convergence
    msg <- "Not converged after {current_n_coalitions} coalitions:\n"

    if (!is.null(convergence_tol)) {
      conv_nice <- signif(overall_conv_measure, 2)
      tol_nice <- format(signif(convergence_tol, 2), scientific = FALSE)
      n_coal_next_iter_factor_nice <- format(signif(n_coal_next_iter_factor * 100, 2), scientific = FALSE)
      msg <- paste0(
        msg,
        "Current convergence measure: {conv_nice} [needs {tol_nice}]\n",
        "Estimated remaining coalitions: {est_remaining_coal_samp}\n",
        "(Conservatively) adding about {n_coal_next_iter_factor_nice}% of that ({next_new_n_coalitions} coalitions) ",
        "in the next iteration."
      )
    }
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

  }
  formatted_msg <- cli::format_inline(msg, .envir = environment())
  return(list(converged = converged,
              formatted_msg = formatted_msg))
}

format_shapley_info <- function(internal, iter){

  converged_exact <- internal$iter_list[[iter]]$converged_exact

  shap_names_with_none <- c("none", internal$parameters$shap_names)
  dt_shapley_est <- internal$iter_list[[iter]]$dt_shapley_est[, shap_names_with_none, with = FALSE]
  dt_shapley_sd <- internal$iter_list[[iter]]$dt_shapley_sd[, shap_names_with_none, with = FALSE]

  # Printing the current Shapley values
  matrix1 <- format(round(dt_shapley_est, 3), nsmall = 2, justify = "right")
  matrix2 <- format(round(dt_shapley_sd, 2), nsmall = 2, justify = "right")

  if (converged_exact) {
    print_dt <- as.data.table(matrix1)
  } else {
    print_dt <- as.data.table(matrix(paste(matrix1, " (", matrix2, ")", sep = ""), nrow = nrow(matrix1)))
  }

  names(print_dt) <- names(dt_shapley_est)

  output <- capture.output(print(print_dt))

  ret <- paste(output, collapse = "\n")

  return(ret)
}

#' Prints iterative information
#'
#' @inheritParams default_doc_export
#'
#' @return No return value (but prints iterative information)
#'
#' @export
#' @keywords internal
print_iter <- function(internal) {
  verbose <- internal$parameters$verbose
  iter <- length(internal$iter_list) - 1 # This function is called after the preparation of the next iteration

  converged <- internal$iter_list[[iter]]$converged
  converged_exact <- internal$iter_list[[iter]]$converged_exact

  if ("convergence" %in% verbose) {

    formatted_convergence_info <- format_convergence_info(internal,iter)$formatted_msg

    cli::cli_h3("Convergence info")
    if(isTRUE(converged)){
      cli::cli_alert_success(formatted_convergence_info)
    } else {
      cli::cli_alert_info(formatted_convergence_info)
    }
  }

  if ("shapley" %in% verbose) {
    formatted_shapley_info <- format_shapley_info(internal, iter)

    if (isTRUE(converged)) {
      msg <- "Final "
    } else {
      msg <- "Current "
    }

    if (converged_exact) {
      msg <- paste0(msg, "estimated Shapley values")
    } else {
      msg <- paste0(msg, "estimated Shapley values (sd)")
    }

    # Send it to rlang::inform (bypassing cli-formatting) to print correctly
    # Cannot use print as it does not obey suppressMessages()
    rlang::inform(paste0(msg,"\n", formatted_shapley_info))
  }
}
