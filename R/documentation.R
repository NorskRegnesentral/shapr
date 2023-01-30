
#' Unexported documentation helper function.
#'
#' @param internal List.
#' Holds all parameters, data, functions and computed objects used within [explain()]
#' The list contains one or more of the elements `parameters`, `data`, `objects`, `output`.
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
#' @param horizon TODO: Document
#'
#' @param ... Further arguments passed to `approach`-specific functions.
#'
#' @return List `internal`
#' It holds all parameters, data, and computed objects used within [explain()].
#' The list contains one or more of the elements `parameters`, `data`, `objects`, `output`.
#'
#'
#' @keywords internal
default_doc <- function() {
  NULL
}


#' Exported documentation helper function.
#'
#' @param internal Not used.
#' @keywords internal
default_doc_explain <- function(){
  NULL
}
