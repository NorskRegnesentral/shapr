#' @rdname predict_model
#' @export
predict_model.xgb.Booster <- function(x, newdata, ...) {
  if (!requireNamespace("xgboost", quietly = TRUE)) {
    cli::cli_abort("The {.pkg xgboost} package is required for predicting `xgboost` models")
  }

  predict(x, as.matrix(newdata))
}

#' @rdname get_model_specs
#' @export
get_model_specs.xgb.Booster <- function(x) {
  model_checker(x) # Checking if the model is supported

  feature_specs <- list()
  feature_specs$labels <- x$feature_names
  m <- length(feature_specs$labels)

  feature_specs$classes <- setNames(rep(NA, m), feature_specs$labels) # Not supported
  feature_specs$factor_levels <- setNames(vector("list", m), feature_specs$labels)

  return(feature_specs)
}

#' @rdname model_checker
#' @export
model_checker.xgb.Booster <- function(x) {
  if (!is.null(x$params$objective) &&
    (x$params$objective == "multi:softmax" || x$params$objective == "multi:softprob")
  ) {
    cli::cli_abort(
      paste0(
        "We currently don't support multi-classification using `xgboost`, i.e. ",
        "where `num_class` is greater than 2."
      )
    )
  }

  if (!is.null(x$params$objective) && x$params$objective == "reg:logistic") {
    cli::cli_abort(
      paste0(
        "We currently don't support standard classification, which predicts the class directly. ",
        "To train an `xgboost` model predicting the class probabilities, you'll need to change ",
        "the objective to `binary:logistic`"
      )
    )
  }
  return(NULL)
}
