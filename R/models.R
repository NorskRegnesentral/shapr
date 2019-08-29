#' Predict on vector form
#'
#' @description Performs prediction of response \code{\link[stats]{lm}}, \code{\link[stats]{glm}},
#' \code{\link[ranger]{ranger}} and \code{\link[xgboost]{xgboost}} with binary or continuous response.
#'
#' @param x Object of class inheriting from \code{\link[stats]{lm}}, \code{\link[stats]{glm}},
#' \code{\link[ranger]{ranger}}, \code{\link[mgcv]{mgcv}} or \code{\link[xgboost]{xgboost}}.
#' @param newdata A data frame (or matrix) in which to look for variables with which to predict.
#'
#' @return Atomic vector
#'
#' @export
#'
#' @author Martin Jullum
predict_model <- function(x, newdata) {
  UseMethod("predict_model", x)
}

#' @rdname predict_model
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
#' @export
predict_model.lm <- function(x, newdata) {

  if (!requireNamespace('stats', quietly = TRUE)) {
    stop('The stats package is required for predicting stats models')
  }

  predict(x, newdata)
}

#' @rdname predict_model
#' @export
predict_model.glm <- function(x, newdata) {

  if (!requireNamespace('stats', quietly = TRUE)) {
    stop('The stats package is required for predicting stats models')
  }

  if (x$family[[1]] == "binomial") {
    predict(x, newdata, type = "response")
  } else {
    predict(x, newdata)
  }
}

#' @rdname predict_model
#' @export
predict_model.ranger <- function(x, newdata) {

  if (!requireNamespace('ranger', quietly = TRUE)) {
    stop('The ranger package is required for predicting ranger models')
  }

  if (x$treetype == "Probability estimation") {
    predict(x, newdata)$predictions[, 2]
  } else {
    predict(x, newdata)$predictions
  }
}

#' @rdname predict_model
#' @export
predict_model.xgb.Booster <- function(x, newdata) {

  if (!requireNamespace('stats', quietly = TRUE)) {
    stop('The xgboost package is required for predicting xgboost models')
  }

  predict(x, as.matrix(newdata))
}

#' @rdname predict_model
#' @export
predict_model.mgcv <- function(x, newdata) {

  if (!requireNamespace('mgcv', quietly = TRUE)) {
    stop('The mgcv package is required for predicting mgcv models')
  }

  predict(x, newdata)
}

#' @export
model_type <- function(x) {
  UseMethod("model_type")
}

#' @export
model_type.default <- function(x) {
  stop("The model you passed to shapr is currently not supported.")
}

#' @export
model_type.lm <- function(x) {
  "regression"
}

#' @export
model_type.glm <- function(x) {
  ifelse(
    x$family[[1]] == "binomial",
    "classification",
    "regression"
  )
}

#' @export
model_type.ranger <- function(x) {
  ifelse(
    x$family[[1]] == "binomial",
    "classification",
    "regression"
  )
}

#' @export
model_type.mgcv <- function(x) {
  "regression"
}

#' @export
model_type.xgb.Booster <- function(x) {

  ifelse(
    !is.null(x$treetype) && x$treetype == "Probability estimation",
    "classification",
    "regression"
  )
}
