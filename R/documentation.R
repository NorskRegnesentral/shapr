
#' Unexported documentation helper function
#'
#' @param internal List.
#' Holds all parameters, data, functions and computed objects used within
#' [`shapr:::explain()`][shapr::explain].
#' The list contains one or more of the elements `parameters`, `data`, `functions`, `output`.
#'
#' @param model Objects.
#' The model object that ought to be explained
#'
#' @return List `internal`
#' It holds all parameters, data, functions and computed objects used within
#' [`shapr:::explain()`][shapr::explain].
#' The list contains one or more of the elements `parameters`, `data`,
#' `functions`, `output`. [explain()]
#' [roxygen2::roxygenize()]
#' [explain()]
#'
#' @keywords internal
default_doc <- function() {
  NULL
}
