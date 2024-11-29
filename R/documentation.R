#' Unexported documentation helper function.
#'
#' @param internal List.
#' Holds all parameters, data, functions and computed objects used within [explain()]
#' The list contains one or more of the elements `parameters`, `data`, `objects`, `iter_list`, `timing_list`,
#' `main_timing_list`, `output`, and `iter_timing_list`.
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
#' @param iter Integer.
#' The iteration number. Only used internally.
#'
#' @param internal List.
#' Not used directly, but passed through from [explain()].
#'
#' @param index_features Positive integer vector. Specifies the id_coalition to
#' apply to the present method. `NULL` means all coalitions. Only used internally.
#'
#' @keywords internal
default_doc_explain <- function(internal, iter, index_features) {
  NULL
}


