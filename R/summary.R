#' Summary Method for Shapr Objects
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
summary.shapr <- function(object, digits = 2L, ...) {
  stopifnot(inherits(object, "shapr"))

  internal <- object$internal
  testing <- internal$parameters$testing
  iter <- length(internal$iter_list)

  iterative <- internal$parameters$iterative
  converged_exact <- internal$iter_list[[iter]]$converged_exact

  # Retrieve all needed results
  results <- get_results(object)

  if (results$proglang == "R") {
    func_txt <- ifelse(results$calling_function == "explain", "{.fn shapr::explain}", "{.fn shapr::explain_forecast}")
  } else { # Python
    func_txt <- ifelse(results$calling_function == "explain", "{.fn shaprpy.explain}", "{.fn shaprpy.explain_forecast}")
  }

  init_time <- results$timing_summary$init_time
  total_time_str <- results$timing_summary$total_time_str
  if (is.null(init_time)) init_time <- 0
  if (is.null(total_time_str)) total_time_str <- ""


  cli::cli_h1("Summary of Shapley value explanation")
  if (isFALSE(testing)) {
    cli::cli_ul(paste0("Computed with ", func_txt, " in {.field {total_time_str}}, started {.val {round(init_time)}}"))
  } else {
    cli::cli_ul(paste0("Computed with ", func_txt))
  }

  # Display basic shapr info
  formatted_info_basic0 <- format_info_basic(internal)
  formatted_info_extra <- format_info_extra(internal)

  len_format0 <- length(formatted_info_basic0)

  # Append extra info second last (keep the temp path last)
  formatted_info_basic <- c(
    formatted_info_basic0[-len_format0],
    formatted_info_extra,
    formatted_info_basic0[len_format0]
  )

  cli::cli_ul(formatted_info_basic) # Display updated basic info


  # Display convergence info
  if (isTRUE(iterative)) {
    formatted_convergence_info <- format_convergence_info(internal, iter)
    cli::cli_h3("Convergence info")
    cli::cli_alert_success(formatted_convergence_info)
  }

  # Display Shapley value results
  formatted_shapley_info <- format_shapley_info(internal, iter, digits = digits)

  if (converged_exact) {
    msg <- "Estimated Shapley values"
  } else {
    msg <- "Estimated Shapley values (sd in parentheses)"
  }

  cli::cli_h3(msg)

  # Using rlang::inform (bypassing cli-formatting) to print correctly
  # Cannot use print as it does not obey suppressMessages()
  rlang::inform(formatted_shapley_info)

  # MSEv info (only when using explain())
  if (results$calling_function == "explain") {
    MSEv_nice <- num_str(format(results$MSEv$MSEv, digits = digits))
    MSEv_sd_nice <- num_str(format(results$MSEv$MSEv_sd, digits = digits))

    cli::cli_h3("Estimated MSEv")
    cli::cli_text(
      "Estimated MSE of v(S) = {.val {MSEv_nice}} (with sd = {.val {MSEv_sd_nice}})"
    )
  }

  invisible(results)
}
