#' Summary Method for Shapr Objects
#'
#' Provides a formatted summary of a shapr object and returns an object of class
#' \code{summary.shapr} containing the same information as returned by [get_results()].
#'
#' @param object A shapr object.
#' @param ... Currently unused.
#' @inheritParams default_doc_export
#'
#' @return An object of class \code{summary.shapr}, which is a named list
#' with the same accessible components as returned by [get_results()].
#' See [get_results()] for details about each component.
#'
#' @examples
#' \donttest{
#' # Load example data
#' data("airquality")
#' airquality <- airquality[complete.cases(airquality), ]
#' x_var <- c("Solar.R", "Wind", "Temp", "Month")
#' y_var <- "Ozone"
#'
#' # Split data into test and training data
#' data_train <- head(airquality, -3)
#' data_explain <- tail(airquality, 3)
#'
#' x_train <- data_train[, x_var]
#' x_explain <- data_explain[, x_var]
#'
#' # Fit a linear model
#' lm_formula <- as.formula(paste0(y_var, " ~ ", paste0(x_var, collapse = " + ")))
#' model <- lm(lm_formula, data = data_train)
#'
#' # Explain predictions
#' p <- mean(data_train[, y_var])
#'
#' explanation <- explain(
#'   model = model,
#'   x_explain = x_explain,
#'   x_train = x_train,
#'   approach = "gaussian",
#'   phi0 = p,
#'   n_MC_samples = 1e2
#' )
#'
#' # Call summary without assignment - prints formatted output to console
#' summary(explanation)
#'
#' # Assign to variable - returns shapr.summary with summary information for later use
#' expl_summary <- summary(explanation) # print(expl_summary) provides the formatted output
#'
#' # Access components from the summary object
#' expl_summary$shapley_est # Estimated Shapley values
#' expl_summary$timing_summary$total_time_secs # Total computation time
#' expl_summary$approach # Approach used
#' }
#'
#' @export
summary.shapr <- function(object, digits = 2L, ...) {
  stopifnot(inherits(object, "shapr"))

  # Retrieve all needed results
  results <- get_results(object)

  # Extract internal for formatting
  internal <- object$internal
  testing <- internal$parameters$testing
  iter <- length(internal$iter_list)
  iterative <- internal$parameters$iterative
  converged_exact <- internal$iter_list[[iter]]$converged_exact

  # Pre-compute all formatted components for printing
  if (results$proglang == "R") {
    func_txt <- ifelse(results$calling_function == "explain", "{.fn shapr::explain}", "{.fn shapr::explain_forecast}")
  } else { # Python
    func_txt <- ifelse(results$calling_function == "explain", "{.fn shaprpy.explain}", "{.fn shaprpy.explain_forecast}")
  }

  init_time <- results$timing_summary$init_time
  total_time_str <- results$timing_summary$total_time_str
  if (is.null(init_time)) init_time <- 0
  if (is.null(total_time_str)) total_time_str <- ""

  # Format basic info
  formatted_info_basic0 <- format_info_basic(internal)
  formatted_info_extra <- format_info_extra(internal)
  len_format0 <- length(formatted_info_basic0)
  formatted_info_basic <- c(
    formatted_info_basic0[-len_format0],
    formatted_info_extra,
    formatted_info_basic0[len_format0]
  )

  # Format convergence info (if applicable)
  formatted_convergence_info <- NULL
  if (isTRUE(iterative)) {
    formatted_convergence_info <- format_convergence_info(internal, iter)
  }

  # Format Shapley values
  formatted_shapley_info <- format_shapley_info(internal, iter, digits = digits)

  # Store all formatted components as an attribute (hidden from names())
  attr(results, "print_data") <- list2env(list(
    func_txt = func_txt,
    init_time = init_time,
    total_time_str = total_time_str,
    testing = testing,
    formatted_info_basic = formatted_info_basic,
    formatted_convergence_info = formatted_convergence_info,
    formatted_shapley_info = formatted_shapley_info,
    iterative = iterative,
    converged_exact = converged_exact,
    digits = digits
  ), parent = emptyenv())
  # Assign class
  class(results) <- c("summary.shapr", "list")

  results
}

#' Print Method for summary.shapr Objects
#'
#' @param x A summary.shapr object.
#' @param ... Currently unused.
#'
#' @return Invisibly returns the summary object.
#'
#' @export
print.summary.shapr <- function(x, ...) {
  stopifnot(inherits(x, "summary.shapr"))

  # Extract pre-formatted components from attribute
  pd <- attr(x, "print_data")

  # Display header
  cli::cli_h1("Summary of Shapley value explanation")
  if (isFALSE(pd$testing)) {
    cli::cli_ul(paste0(
      "Computed with ", pd$func_txt,
      " in {.field {pd$total_time_str}}, started {.val {round(pd$init_time)}}"
    ))
  } else {
    cli::cli_ul(paste0("Computed with ", pd$func_txt))
  }

  # Display basic info
  cli::cli_ul(pd$formatted_info_basic)

  # Display convergence info
  if (isTRUE(pd$iterative)) {
    cli::cli_h3("Convergence info")
    cli::cli_alert_success(pd$formatted_convergence_info)
  }

  # Display Shapley value results
  if (pd$converged_exact) {
    msg <- "Estimated Shapley values"
  } else {
    msg <- "Estimated Shapley values (sd in parentheses)"
  }

  cli::cli_h3(msg)

  # Using rlang::inform (bypassing cli-formatting) to print correctly
  # Cannot use print as it does not obey suppressMessages()
  rlang::inform(pd$formatted_shapley_info)

  # MSEv info (only when using explain())
  if (x$calling_function == "explain") {
    MSEv_nice <- num_str(format(x$MSEv$MSEv, digits = pd$digits))
    MSEv_sd_nice <- num_str(format(x$MSEv$MSEv_sd, digits = pd$digits))

    cli::cli_h3("Estimated MSEv")
    cli::cli_text(
      "Estimated MSE of v(S) = {.val {MSEv_nice}} (with sd = {.val {MSEv_sd_nice}})"
    )
  }

  invisible(x)
}
