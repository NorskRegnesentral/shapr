#' @export
explain <- function(x, explainer, ...) {

  UseMethod("shapr", x)
}

explain.empirical <- function() {

}

explain.gaussian <- function() {

}

explain.copula <- function() {

}
