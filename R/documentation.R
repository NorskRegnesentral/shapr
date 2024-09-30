#' Unexported documentation helper function.
#'
#' @param internal List.
#' Holds all parameters, data, functions and computed objects used within [explain()]
#' The list contains one or more of the elements `parameters`, `data`, `objects`, `iter_list`, `iter_results`, `output`.
#'
#' @param model Objects.
#' The model object that ought to be explained.
#' See the documentation of [explain()] for details.
#'
#' @param predict_model Function.
#' The prediction function used when `model` is not natively supported.
#' See the documentation of [explain()] for details.
#'
#' @param output_size TODO: Document
#' @param extra TODO: Document
#'
#' @param ... Further arguments passed to `approach`-specific functions.
#'
#' @return List `internal`
#' It holds all parameters, data, and computed objects used within [explain()].
#' The list contains one or more of the elements `parameters`, `data`, `objects`, `output`.
#'
#'
#' @keywords internal
default_doc <- function(internal, model, predict_model, output_size, extra, ...) {
  NULL
}


#' Exported documentation helper function.
#'
#' @param internal List.
#' Not used directly, but passed through from [explain()].
#'
#' @param index_features Positive integer vector. Specifies the id_coalition to
#' apply to the present method. `NULL` means all coalitions. Only used internally.
#'
#' @keywords internal
default_doc_explain <- function(internal, index_features) {
  NULL
}


#' Documentation of the approach-specific parameters in [shapr::explain()]
#'
#' @description
#' This helper function displays the specific arguments applicable to the different
#' approaches. Note that when calling [shapr::explain()] from Python, the parameters
#' are renamed from the from `approach.parameter_name` to `approach_parameter_name`.
#' That is, an underscore has replaced the dot as the dot is reserved in Python.
#'
#' @inheritDotParams setup_approach.independence -internal
#' @inheritDotParams setup_approach.empirical -internal -predict_model -model
#' @inheritDotParams setup_approach.categorical -internal
#' @inheritDotParams setup_approach.copula -internal
#' @inheritDotParams setup_approach.ctree -internal
#' @inheritDotParams setup_approach.gaussian -internal
#' @inheritDotParams setup_approach.regression_separate -internal
#' @inheritDotParams setup_approach.regression_surrogate -internal
#' @inheritDotParams setup_approach.timeseries -internal
#' @inheritDotParams setup_approach.vaeac -internal
#'
#' @author Lars Henry Berge Olsen and Martin Jullum
explain_tripledot_docs <- function(...) {
  NULL
}
