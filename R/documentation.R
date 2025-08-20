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
#' @param x_explain Data.table with the features of the observation whose
#' predictions ought to be explained (test data).
#'
#' @param x_train Data.table with training data.
#'
#' @param n_features Positive integer.
#' The number of features.
#'
#' @param W_kernel Numeric matrix. Contains all non-scaled weights between training and test
#' observations for all coalitions. The dimension equals `n_train x m`.
#'
#' @param S Integer matrix of dimension `n_coalitions x m`, where `n_coalitions`
#' and `m` equals the total number of sampled/non-sampled coalitions and
#' the total number of unique features, respectively. Note that `m = ncol(x_train)`.
#'
#' @param dt_vS Data.table of dimension `n_coalitions` times `n_explain + 1` containing the contribution function
#' estimates. The first column is assumed to be named `id_coalition` and containing the ids of the coalitions.
#' The last row is assumed to be the full coalition, i.e., it contains the predicted responses for the observations
#' which are to be explained.
#'
#' @param model_class Character string.
#' The class of the model object, e.g., "lm", "glm", "xgboost", etc. obtained by `class(model)[1]`.
#'
#' @param output_size Scalar integer.
#' Specifies the dimension of the output from the prediction model for every observation.
#'
#' @param ... Further arguments passed to `approach`-specific functions.
#'
#' @return The `internal` list.
#' It holds all parameters, data, and computed objects used within [explain()].
#'
#'
#' @keywords internal
default_doc_internal <- function(internal,
                                 model,
                                 predict_model,
                                 x_explain,
                                 x_train,
                                 n_features,
                                 W_kernel,
                                 S,
                                 dt_vS,
                                 model_class,
                                 output_size,
                                 ...) {
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
#' @param digits Integer.
#' (Maximum) number of digits to be displayed after the decimal point.
#' Defaults to 2.
#'
#' @keywords internal
default_doc_export <- function(internal,
                               iter,
                               index_features,
                               digits) {
  NULL
}
