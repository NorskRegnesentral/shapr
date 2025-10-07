#' Print Method for Shapr Objects
#'
#' @param x A shapr object
#' @param what Character. Which component to print.
#' Options are "shapley_est", "shapley_sd", "MSEv", "MSEv_explicand", "MSEv_coalition", and "timing_summary".
#' Defaults to "shapley_est".
#' Only one component can be printed at a time.
#' See the details section of [get_results()] for details about each component.
#' @param digits Integer.
#' Number of significant digits to display.
#' Defaults to 3.
#' @param ... Further arguments passed to [data.table::print.data.table()].
#'
#' @return The object is returned invisibly after printing selected output.
#' @export
print.shapr <- function(x, what = c(
                          "shapley_est", "shapley_sd", "MSEv",
                          "MSEv_explicand", "MSEv_coalition", "timing_summary"
                        ),
                        digits = 3L, ...) {
  what <- match.arg(what)
  value <- get_results(x, what) # Always return a single data.table

  print(value, digits = digits, ...)
  invisible(x)
}
