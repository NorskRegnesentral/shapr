#' @rdname predict_model
#' @export
predict_model.xgb.Booster <- function(x, newdata) {
  if (!requireNamespace("stats", quietly = TRUE)) {
    stop("The xgboost package is required for predicting xgboost models")
  }

  if (is.null(x$feature_list)) {
    predict(x, as.matrix(newdata))
  } else {
    newdata_dummy <- apply_dummies(feature_list = x$feature_list, testdata = newdata)
    predict(x, as.matrix(newdata_dummy))
  }
}

#' @rdname get_model_specs
#' @export
get_model_specs.xgb.Booster <- function(x) {
  model_checker(x) # Checking if the model is supported

  feature_list <- list()
  if (is.null(x[["feature_list"]])) {
    feature_list$labels <- x$feature_names
    m <- length(feature_list$labels)

    feature_list$classes <- setNames(rep(NA, m), feature_list$labels) # Not supported
    feature_list$factor_levels <- setNames(vector("list", m), feature_list$labels)
  } else {
    feature_list <- x$feature_list
  }

  return(feature_list)
}

#' @rdname model_checker
#' @export
model_checker.xgb.Booster <- function(x) {
  if (!is.null(x$params$objective) &&
      (x$params$objective == "multi:softmax" | x$params$objective == "multi:softprob")
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
