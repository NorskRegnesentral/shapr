#' Generate predictions for different model classes
#'
#' @description Performs prediction of response \code{\link[stats]{lm}}, \code{\link[stats]{glm}},
#' \code{\link[ranger]{ranger}},  \code{\link[mgcv:gam]{mgcv::gam}} and \code{\link[xgboost]{xgboost}} with binary or
#' continuous response. See details for more information.
#'
#' @param x Model object for the model to be explained.
#' @param newdata A data frame (or matrix) in which to look for variables with which to predict.
#'
#' @details The following models are currently supported:
#' \itemize{
#' \item \code{\link[stats:lm]{stats::lm}}
#' \item \code{\link[stats:glm]{stats::glm}}
#' \item \code{\link[ranger:ranger]{ranger::ranger}}
#' \item \code{\link[mgcv:gam]{mgcv::gam}}
#' \item \code{\link[xgboost:xgboost]{xgboost::xgboost/xgboost::xgb.train}}
#' }
#'
#' The returned object \code{p} always satisfies the following properties:
#' \itemize{
#' \item \code{is.atomic(p)} equals \code{TRUE}
#' \item \code{is.double(p)} equals \code{TRUE}
#' }
#'
#' If you have a binary classification model we'll always return the probability prediction
#' for a single class.
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

  if (!requireNamespace("stats", quietly = TRUE)) {
    stop("The stats package is required for predicting stats models")
  }

  predict(x, as.data.frame(newdata))
}

#' @rdname predict_model
#' @export
predict_model.glm <- function(x, newdata) {

  if (!requireNamespace("stats", quietly = TRUE)) {
    stop("The stats package is required for predicting stats models")
  }

  if (x$family[[1]] == "binomial") {
    predict(x, as.data.frame(newdata), type = "response")
  } else {
    predict(x, as.data.frame(newdata))
  }
}

#' @rdname predict_model
#' @export
predict_model.ranger <- function(x, newdata) {

  if (!requireNamespace("ranger", quietly = TRUE)) {
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
#' @export
predict_model.xgb.Booster <- function(x, newdata) {

  if (!requireNamespace("stats", quietly = TRUE)) {
    stop("The xgboost package is required for predicting xgboost models")
  }

  # Test model type
  model_type <- model_type(x)

  predict(x, as.matrix(newdata))
}

#' @rdname predict_model
#' @export
predict_model.gam <- function(x, newdata) {

  if (!requireNamespace("mgcv", quietly = TRUE)) {
    stop("The mgcv package is required for predicting gam models")
  }

  if (x$family[[1]] == "binomial") {
    as.vector(
      predict(x, as.data.frame(newdata), type = "response")
    )
  } else {
    as.vector(
      predict(x, as.data.frame(newdata))
    )
  }

}

#' Define type of model
#'
#' @description The function checks whether the model given by \code{x} is
#' supported, and if it is a regression- or a classification model. If \code{x} is
#' not a supported model the function will return an error message, otherwise it will
#' return either \code{"regression"} or \code{"classification"}.
#'
#' @inheritParams predict_model
#'
#' @details See \code{\link{predict_model}} for more information about
#' what type of models \code{shapr} currently support.
#'
#' @return Either \code{"classification"} or \code{"regression"}.
#'
#' @export
model_type <- function(x) {
  UseMethod("model_type")
}

#' @rdname model_type
#' @export
model_type.default <- function(x) {
  stop("The model you passed to shapr is currently not supported.")
}

#' @rdname model_type
#' @export
model_type.lm <- function(x) {
  "regression"
}

#' @rdname model_type
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
#' @export
model_type.gam <- function(x) {
  ifelse(
    x$family[[1]] == "binomial",
    "classification",
    "regression"
  )
}

#' @rdname model_type
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

#' Fetches feature labels from a given model object
#'
#' @inheritParams predict_model
#' @param cnms Character vector. Represents the names of the columns in the data used for training/explaining.
#' @param feature_labels Character vector. Represents the labels of the features used for prediction.
#'
#' @keywords internal
#'
#' @export
features <- function(x, cnms, feature_labels = NULL) {
  UseMethod("features", x)
}

#' @rdname features
features.default <- function(x, cnms, feature_labels = NULL) {

  if (is.null(feature_labels)) {
    stop(
      paste0(
        "\nIt looks like you are using a custom model, and forgot to pass\n",
        "a valid value for the argument feature_labels when calling shapr().\n",
        "See ?shapr::shapr for more information about the argument."
      )
    )
  }

  if (!all(feature_labels %in% cnms)) {
    stop(
      paste0(
        "\nThere is mismatch between the column names in x and\n",
        "feature_labels. All elements in feature_labels should\n",
        "be present in colnames(x)."
      )
    )
  }

  feature_labels
}

#' @rdname features
features.lm <- function(x, cnms, feature_labels = NULL) {

  if (!is.null(feature_labels)) message_features_labels()

  nms <- tail(all.vars(x$terms), -1)
  if (!all(nms %in% cnms)) error_feature_labels()

  return(nms)
}

#' @rdname features
features.glm <- function(x, cnms, feature_labels = NULL) {

  if (!is.null(feature_labels)) message_features_labels()

  nms <- tail(all.vars(x$terms), -1)
  if (!all(nms %in% cnms)) error_feature_labels()

  return(nms)
}

#' @rdname features
features.ranger <- function(x, cnms, feature_labels = NULL) {

  if (!is.null(feature_labels)) message_features_labels()

  nms <- x$forest$independent.variable.names

  if (is.null(x$forest)) {
    stop(
      paste0(
        "\nIt looks like the model was fitted without saving the forest. Please set\n",
        "write.forest = TRUE when fitting a model using ranger::ranger()."
      )
    )
  }
  nms <- unique_features(nms)

  if (!all(nms %in% cnms)) error_feature_labels()

  return(nms)

}

#' @rdname features
features.gam <- function(x, cnms, feature_labels = NULL) {

  if (!is.null(feature_labels)) message_features_labels()

  nms <- tail(all.vars(x$terms), -1)

  if (!all(nms %in% cnms)) error_feature_labels()

  return(nms)
}

#' @rdname features
features.xgb.Booster <- function(x, cnms, feature_labels = NULL) {

  if (!is.null(feature_labels)) message_features_labels()

  nms <- x$feature_names

  if (!all(nms %in% cnms)) error_feature_labels()

  return(nms)
}

#' @keywords internal
message_features_labels <- function() {
  message(
    paste0(
      "\nYou have passed a supported model object, and therefore\n",
      "features_labels is ignored. The argument is only applicable when\n",
      "using a custom model. For more information see ?shapr::shapr."
    )
  )
}

#' @keywords internal
error_feature_labels <- function() {
  stop(
    paste0(
      "\nThere is mismatch between the column names in x and\n",
      "the returned elements from features(model). All elements\n",
      "from features(model) should be present in colnames(x).\n",
      "For more information see ?shapr::features"
    )
  )
}
