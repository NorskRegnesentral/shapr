#' Summary method for shapr objects
#'
#' @param x A shapr object.
#' @param digits Integer. Number of digits for printed numbers.
#' @param ... Currently unused.
#'
#' @return Invisibly returns a list of key summary components.
#' @export
summary.shapr <- function(x, ...) {
  stopifnot(inherits(x, "shapr"))

  # Extract what we need
  internal <- x$internal
  model_class <- class(x$internal$model)[1]
  verbose_summary <-

  cli::cli_h1("shapr summary")

  # reuse existing internal CLI formatting logic
  cli_startup(internal = internal, model_class = model_class, verbose = "basic")

  # Optional extras: show global MSE
  mse <- tryCatch(get_results(x, "MSEv"), error = function(e) NULL)
  if (!is.null(mse)) {
    cli::cli_h2("Overall model error")
    cli::cli_text("MSE: {.val {round(mse, digits)}}")
  }

  invisible(x)
}
