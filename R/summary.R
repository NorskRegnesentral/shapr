#' Summary method for shapr objects
#'
#' @param object A shapr object.
#' @param ... Currently unused.
#' @inheritParams default_doc_export
#'
#' @return Prints a formatted summary of the shapr object,
#' and invisibly returns a named list of summary components.
#' See the details section of [get_results()] for details about each component.
#'
#' @export
summary.shapr <- function(object, digits = 2L, nsmall = 0L, ...) {
  stopifnot(inherits(object, "shapr"))

  internal <- object$internal
  testing <- internal$parameters$testing
  iter <- length(internal$iter_list)

  iterative <- internal$parameters$iterative
  converged_exact <- internal$iter_list[[iter]]$converged_exact

  # Retrieve all needed results
  results <- get_results(object)

  func_txt <- ifelse(results$calling_function == "explain", "{.fn shapr::explain}", "{.fn shapr::explain_forecast}")
  init_time <- results$timing_summary$init_time
  nice_total_time <- results$timing_summary$nice_total_time
  if (is.null(init_time)) init_time <- 0
  if (is.null(nice_total_time)) nice_total_time <- ""


  cli::cli_h1("Summary of Shapley value explanation")
  if (isFALSE(testing)) {
    cli::cli_ul(paste0("Computed with", func_txt, " in {.field {nice_total_time}}, started {.val {round(init_time)}}"))
  } else {
    cli::cli_ul(paste0("Computed with", func_txt))
  }

  # Display basic shapr info
  formatted_info_basic <- format_info_basic(internal)

  cli::cli_ul(formatted_info_basic)

  formatted_info_extra <- format_info_extra(internal)

  cli::cli_ul(formatted_info_extra)

  # Display convergence info
  if (isTRUE(iterative)) {
    formatted_convergence_info <- format_convergence_info(internal, iter)
    cli::cli_h3("Convergence info")
    cli::cli_alert_success(formatted_convergence_info)
  }

  # Display Shapley value res
  formatted_shapley_info <- format_shapley_info(internal, iter, digits = digits, nsmall = nsmall)

  if (converged_exact) {
    msg <- "Estimated Shapley values"
  } else {
    msg <- "Estimated Shapley values (sd in parantheses)"
  }

  cli::cli_h3(msg)

  # Using rlang::inform (bypassing cli-formatting) to print correctly
  # Cannot use print as it does not obey suppressMessages()
  rlang::inform(formatted_shapley_info)

  # MSEv info (only when using explain())
  if (results$calling_function == "explain") {
    MSEv_nice <- num_str(format(results$MSEv$MSEv, digits = digits, nsmall = nsmall))
    MSEv_sd_nice <- num_str(format(results$MSEv$MSEv_sd, digits = digits, nsmall = nsmall))

    cli::cli_h3("Estimated MSEv")
    cli::cli_alert_success(
      "The estimated MSE of v(S) = {.val {MSEv_nice}} (with sd = {.val {MSEv_sd_nice}})"
    )
  }

  invisible(results)
}
