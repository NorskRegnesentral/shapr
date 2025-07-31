#' Summary method for shapr objects
#'
#' @param object A shapr object.
#' @param digits Integer. Number of digits to round numerical outputs.
#' @param ... Currently unused.
#'
#' @return Invisibly returns a named list of summary components.
#' See the details section of [get_results()] for details about each component.
#' @export
summary.shapr <- function(x, ...) {
  stopifnot(inherits(x, "shapr"))

  internal <- x$internal
  iter <- length(internal$iter_list)

  iterative <- internal$parameters$iterative
  converged_exact <- internal$iter_list[[iter]]$converged_exact

  # Retrieve all needed results
  results <- get_results(x)

  func_txt <- ifelse(results$calling_function == "explain", "{.fn shapr::explain}", "{.fn shapr::explain_forecast}")
  init_time <- results$timing$init_time

  cli::cli_h1("Summary of Shapley value explanation")
  cli::cli_ul(paste0("Computed with", func_txt, " at {.val {round(init_time)}}"))

  # Display basic shapr info
  formatted_info_basic <- format_info_basic(internal)

  cli::cli_ul(formatted_info_basic)

  formatted_info_extra <- format_info_extra(internal)

  cli::cli_ul(formatted_info_extra)

  # Display convergence info
  formatted_convergence_info <- format_convergence_info(internal, iter)

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

  # MSEv info
  MSEv_nice <- format(results$MSEv$MSEv, digits = 4, nsmall = 2)
  MSEv_sd_nice <- format(results$MSEv$MSEv_sd, digits = 4, nsmall = 2)

  cli::cli_h3("Estimated MSEv")
  cli::cli_alert_info(
    "The estimated MSE of v(S) = {MSEv_nice} (with sd = {MSEv_sd_nice})"
  )

  invisible(results)
}
