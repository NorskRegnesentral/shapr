

#' Summary method for shapr objects
#'
#' @param object A shapr object.
#' @param digits Integer. Number of digits to round numerical outputs.
#' @param ... Currently unused.
#'
#' @return Invisibly returns a named list of summary components.
#' @export
summary.shapr <- function(x, digits = 4, ...) {
  stopifnot(inherits(x, "shapr"))

  internal <- x$internal
  iter <- length(internal$iter_list)

  iterative <- internal$parameters$iterative
  converged_exact <- internal$iter_list[[iter]]$converged_exact

  cli::cli_h1("Summary of Shapley value explanation")

  # Retrieve all needed results
  results <- get_results(x)

  # Display basic shapr info
  formatted_info_basic <- format_info_basic(internal)

  cli::cli_ul(formatted_info_basic)

  formatted_info_extra <- format_info_extra(internal)

  cli::cli_ul(formatted_info_extra)


  # TODO notes
  # Clean up the unused functions, and document those left
  # update explain_forecast with the new model_class stuff

  # Display convergence info
  formatted_convergence_info <- format_convergence_info(internal,iter)$formatted_msg

  cli::cli_h3("Convergence info")
  cli::cli_alert_success(formatted_convergence_info)

  # Display Shapley value res
  formatted_shapley_info <- format_shapley_info(internal, iter)

  if (converged_exact) {
    msg <- "Estimated Shapley values"
  } else {
    msg <- "Estimated Shapley values (sd in parantheses)"
  }

  cli::cli_h3(msg)

  # Using rlang::inform (bypassing cli-formatting) to print correctly
  # Cannot use print as it does not obey suppressMessages()
  rlang::inform(formatted_shapley_info)

  # TODO: Display MSE res???

  invisible(results)
}
