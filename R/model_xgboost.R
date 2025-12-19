#' @rdname predict_model
#' @export
predict_model.xgboost <- function(x, newdata, ...) {
  if (!requireNamespace("xgboost", quietly = TRUE)) {
    cli::cli_abort("The {.pkg xgboost} package is required for predicting `xgboost` models.")
  }

  predict(x, newdata)
}

#' @rdname get_model_specs
#' @export
get_model_specs.xgboost <- function(x) {
  model_checker(x) # Checking if the model is supported

  feature_specs <- list()
  feature_specs$labels <- xgboost::getinfo(x, "feature_name")
  m <- length(feature_specs$labels)

  feature_specs$classes <- xgboost::getinfo(x, "feature_type")

  if (is.null(feature_specs$classes)) {
    # Sometimes NULL is returned. Then we just set NA's (not supported)
    # Typically the case when data are passed to the model as a numeric matrix
    feature_specs$classes <- rep(NA, m)
  }

  # Mapping to generic classes
  feature_specs$classes[feature_specs$classes %in% c("int", "float")] <- "numeric"
  feature_specs$classes[feature_specs$classes %in% c("c")] <- "factor"

  # Set names
  names(feature_specs$classes) <- feature_specs$labels

  # Factor levels is not supported
  feature_specs$factor_levels <- setNames(as.list(rep(NA, m)), feature_specs$labels)

  return(feature_specs)
}

#' @rdname model_checker
#' @export
model_checker.xgboost <- function(x) {
  objective <- xgboost::xgb.config(x)$learner$objective$name

  if (!is.null(objective) &&
    (objective == "multi:softmax" || objective == "multi:softprob")
  ) {
    cli::cli_abort(
      paste0(
        "We currently do not support multiclass classification using `xgboost`, i.e., ",
        "when `num_class` is greater than 2."
      )
    )
  }
  return(NULL)
}

### Corresponding functions for the xgboost < 3.1.2.1 models (xgb.Booster)

#' @rdname predict_model
#' @export
predict_model.xgb.Booster <- function(x, newdata, ...) {
  if (!requireNamespace("xgboost", quietly = TRUE)) {
    cli::cli_abort("The {.pkg xgboost} package is required for predicting `xgb.Booster` models.")
  }

  if (utils::packageVersion("xgboost") >= "3.1.2.1") {
    predict_model.xgboost(x, newdata, ...)
  } else {
    predict(x, as.matrix(newdata))
  }
}

#' @rdname get_model_specs
#' @export
get_model_specs.xgb.Booster <- function(x) {
  if (utils::packageVersion("xgboost") >= "3.1.2.1") {
    return(get_model_specs.xgboost(x))
  }

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
  if (utils::packageVersion("xgboost") >= "3.1.2.1") {
    return(model_checker.xgboost(x))
  }

  if (!is.null(x$params$objective) &&
    (x$params$objective == "multi:softmax" || x$params$objective == "multi:softprob")
  ) {
    cli::cli_abort(
      paste0(
        "We currently do not support multiclass classification using `xgboost`, i.e., ",
        "when `num_class` is greater than 2."
      )
    )
  }

  if (!is.null(x$params$objective) && x$params$objective == "reg:logistic") {
    cli::cli_abort(
      paste0(
        "We currently do not support standard classification, which predicts the class directly. ",
        "To train an `xgboost` model that predicts class probabilities, change ",
        "the objective to `binary:logistic`."
      )
    )
  }
  return(NULL)
}
