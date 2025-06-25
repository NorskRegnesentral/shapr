#' Print method for shapr objects
#'
#' @param x A shapr object
#' @param digits Scalar Integer.
#' Number of digits to display to the console
#' @param ... Unused
#' @return No return value (but prints the shapley values to the console)
#' @export
print.shapr <- function(x, digits = 4, ...) {
  shap <- copy(x$shapley_values_est)
  shapley_names <- x$internal$parameters$shapley_names
  cols <- c("none", shapley_names)
  shap[, (cols) := lapply(.SD, round, digits = digits + 2), .SDcols = cols]
  shap_names <- x$internal$parameters$shap_names
  cols <- c("none", shap_names)
  shap[, (cols) := lapply(.SD, round, digits = digits + 2), .SDcols = cols][]
  print(shap, digits = digits)
}
