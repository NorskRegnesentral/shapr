#' Predict on vector form
#'
#' @description Performs prediction of response \code{\link[stats]{lm}}, \code{\link[stats]{glm}},
#' \code{\link[ranger]{ranger}} and \code{\link[xgboost]{xgboost}} with binary or continuous response.
#'
#' @param x Object of class inheriting from one of the supported models. See details.
#' @param newdata A data frame (or matrix) in which to look for variables with which to predict.
#'
#' @details The following models are currently supported:
#' \itemize{
#' \item \code{\link[stats]{lm}}
#' \item \code{\link[stats]{glm}}
#' \item \code{\link[ranger]{ranger}}
#' \item \code{\link[mgcv]{mgcv}}
#' \item \code{\link[xgboost]{xgboost}}
#' }
#'
#' For more details on how to use a custom model see the package vignette:
#' \code{vignette("understanding_shapr", package = "shapr")}
#'
#' @return Numeric
#'
#' @export
#'
#' @author Martin Jullum
predict_model <- function(x, newdata) {
  UseMethod("predict_model", x)
}

#' @rdname predict_model
#' @name predict_model
#' @export
predict_model.default <- function(x, newdata) {

  str_error <- paste(
    "It seems that you passed a non-valid model object.",
    "See more information about which models that are supported",
    "by running ?predict_model."
  )
  stop(str_error)
}

#' @rdname predict_model
#' @name predict_model
#' @export
predict_model.lm <- function(x, newdata) {

  if (!requireNamespace('stats', quietly = TRUE)) {
    stop("The stats package is required for predicting stats models")
  }

  predict(x, as.data.frame(newdata))
}

#' @rdname predict_model
#' @name predict_model
#' @export
predict_model.glm <- function(x, newdata) {

  if (!requireNamespace('stats', quietly = TRUE)) {
    stop("The stats package is required for predicting stats models")
  }

  if (x$family[[1]] == "binomial") {
    predict(x, as.data.frame(newdata), type = "response")
  } else {
    predict(x, as.data.frame(newdata))
  }
}

#' @rdname predict_model
#' @name predict_model
#' @export
predict_model.ranger <- function(x, newdata) {

  if (!requireNamespace('ranger', quietly = TRUE)) {
    stop("The ranger package is required for predicting ranger models")
  }

  # Test model type
  model_type <- model_type(x)

  if (x$treetype == "Probability estimation") {
    predict(x, newdata)$predictions[, 2]
  } else {
    predict(x, newdata)$predictions
  }
}

#' @rdname predict_model
#' @name predict_model
#' @export
predict_model.xgb.Booster <- function(x, newdata) {

  if (!requireNamespace('stats', quietly = TRUE)) {
    stop("The xgboost package is required for predicting xgboost models")
  }

  # Test model type
  model_type <- model_type(x)

  predict(x, as.matrix(newdata))
}

#' @rdname predict_model
#' @name predict_model
#' @export
predict_model.mgcv <- function(x, newdata) {

  if (!requireNamespace('mgcv', quietly = TRUE)) {
    stop("The mgcv package is required for predicting mgcv models")
  }

  predict(x, newdata)
}

#' TODO: Add title & description
#' @export
model_type <- function(x) {
  UseMethod("model_type")
}

#' @rdname model_type
#' @name model_type
#' @export
model_type.default <- function(x) {
  stop("The model you passed to shapr is currently not supported.")
}

#' @rdname model_type
#' @name model_type
#' @export
model_type.lm <- function(x) {
  "regression"
}

#' @rdname model_type
#' @name model_type
#' @export
model_type.glm <- function(x) {
  ifelse(
    x$family[[1]] == "binomial",
    "classification",
    "regression"
  )
}

#' @rdname model_type
#' @name model_type
#' @export
model_type.ranger <- function(x) {

  if (x$treetype == "Classification") {
    stop(
      paste0(
        "\n",
        "We currently don't support standard classification, which predicts the class directly.\n",
        "To train a ranger model predicting the class probabilities, you'll need to grow a\n",
        "probability forest by setting probability = TRUE in ranger::ranger()."
      )
    )
  }

  if (x$treetype == "Probability estimation") {
    if (length(x$forest$levels) == 2) {
      "classification"
    } else {
      stop(
        paste0(
          "\n",
          "We currently don't support multi-classification using ranger, i.e.\n",
          "where length(model$forest$levels) is greater than 2."
        )
      )
    }
  } else {
    "regression"
  }
}

#' @rdname model_type
#' @name model_type
#' @export
model_type.mgcv <- function(x) {
  "regression"
}

#' @rdname model_type
#' @name model_type
#' @export
model_type.xgb.Booster <- function(x) {

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

  ifelse(
    !is.null(x$params$objective) && x$params$objective == "binary:logistic",
    "classification",
    "regression"
  )
}
