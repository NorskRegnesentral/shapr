#' @rdname predict_model
#' @export
predict_model.xgb.Booster <- function(x, newdata, ...) {
  if (!requireNamespace("xgboost", quietly = TRUE)) {
    stop("The xgboost package is required for predicting xgboost models")
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
    stop(
      paste0(
        "\n",
        "We currently don't support multi-classification using xgboost, i.e.\n",
        "where num_class is greater than 2."
      )
    )
  }

  if (!is.null(x$params$objective) && x$params$objective == "reg:logistic") {
    stop(
      paste0(
        "\n",
        "We currently don't support standard classification, which predicts the class directly.\n",
        "To train an xgboost model predicting the class probabilities, you'll need to change \n",
        "the objective to 'binary:logistic'"
      )
    )
  }
  return(NULL)
}
