#' @export
print.shapr <- function(x, digits = 4, ...) {
  print(x$shapley_values, digits = digits)
}
