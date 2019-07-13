#' @export
explain <- function(x, explainer, approach, prediction_zero, ...) {

  if (approach == "empirical") {
    class(x) <- "empirical"
  } else if (approach == "gaussian") {
    class(x) <- "gaussian"
  } else if (approach == "copula") {
    class(x) <- "copula"
  } else {
    stop("It seems that you passed a non-valid value for approach. It should be either 'empirical', 'gaussian' or 'copula'.")
  }

  UseMethod("explain", x)
}

#' @export
explain.empirical <- function(x, explainer, approach, prediction_zero, index_features) {

  # Get distance matrix ----------------
  explainer$D <- distance_matrix(
    explainer$Xtrain,
    x,
    explainer$X
  )

  return()
}

#' @export
explain.gaussian <- function(x, ...) {

}

#' @export
explain.copula <- function(x, ...) {

}
