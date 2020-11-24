#' Generate predictions for different model classes
#'
#' @description Performs prediction of response \code{\link[stats]{lm}}, \code{\link[stats]{glm}},
#' \code{\link[ranger]{ranger}},  \code{\link[mgcv:gam]{mgcv::gam}} and
#' \code{\link[xgboost:xgb.train]{xgboost::xgb.train}} with binary or continuous
#' response. See details for more information.
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
#' \item \code{\link[xgboost:xgb.train]{xgboost::xgb.train}}
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
#' For more details on how to use a custom model see the package vignette: \cr
#' \code{vignette("understanding_shapr", package = "shapr")}
#'
#' @return Numeric
#'
#' @export
#' @keywords internal
#'
#' @author Martin Jullum
#' @examples
#'# Load example data
#' data("Boston", package = "MASS")
#' # Split data into test- and training data
#' x_train <- head(Boston, -3)
#' x_test <- tail(Boston, 3)
#' # Fit a linear model
#' model <- lm(medv ~ lstat + rm + dis + indus, data = x_train)
#'
#' # Predicting for a model with a standardized format
#' predict_model(x = model, newdata = x_test)
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

  if (model_type %in% c("cat_regression", "cat_classification")) {
    newdata_dummy <- apply_dummies(obj = x$dummylist, testdata = newdata)
    predict(x, as.matrix(newdata_dummy))
  } else {
    predict(x, as.matrix(newdata))
  }
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
#' @keywords internal
#'
#' @examples
#' # Load example data
#' data("Boston", package = "MASS")
#' # Split data into test- and training data
#' x_train <- head(Boston, -3)
#' # Fit a linear model
#' model <- lm(medv ~ lstat + rm + dis + indus, data = x_train)
#'
#' # Writing out the defined model type of the object
#' model_type(x = model)
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
    ifelse(is.null(x$dummylist), "classification", "cat_classification"),
    ifelse(is.null(x$dummylist), "regression", "cat_regression")
  )
}

#' Fetches feature labels from a given model object
#'
#' @inheritParams predict_model
#' @param feature_labels Character vector. Represents the labels of the features used for prediction.
#'
#' @keywords internal
#'
#' @export
#'
#' @examples
#'# Load example data
#' data("Boston", package = "MASS")
#' # Split data into test- and training data
#' x_train <- head(Boston, -3)
#' # Fit a linear model
#' model <- lm(medv ~ lstat + rm + dis + indus, data = x_train)
#'
#' cnms <- c("lstat", "rm", "dis", "indus")
#'
#' # Checking that features used by the model corresponds to cnms
#' features(x = model, cnms = cnms, feature_labels = NULL)
get_model_features <- function(x,feature_labels = NULL) {

  # Start with all checking for native models
  native_models <- substring(as.character(methods(get_model_features)),first = 20)
  is_native_model <- (class(x)[1] %in% native_models)

  if(is_native_model){
    if(!is.null(feature_labels)){
      message(
        paste0(
          "\nYou have passed a supported model object, and therefore\n",
          "features_labels is ignored. The argument is only applicable when\n",
          "using a custom model. For more information see ?shapr::shapr."
          )
        )
      }
    } else { # if custom model
    if(is.character(feature_labels)){

      message(
        paste0(
          "\nIt looks like you are using a custom model. Note that we currently\n",
          "do not check the class and validity of the features specified in\n",
          "feature_labels when calling shapr().\n"
        )
      )
    } else {
      stop(
        paste0(
          "\nIt looks like you are using a custom model, and forgot to pass\n",
          "a valid value for the argument feature_labels when calling shapr().\n",
          "See ?shapr::shapr for more information about the argument."
        )
      )
    }
  }

  UseMethod("get_model_features", x)
}

#' @rdname get_model_features
get_model_features.default <- function(x, feature_labels = NULL) {

  # For custom models
  feature_list = list()
  feature_list$labels <- feature_labels
  feature_list$classes <- rep(NA,length(feature_labels))
  feature_list$factor_levels = NULL

  return(feature_list)
}


#' @rdname get_model_features
#' @export
get_model_features.lm <- function(x, feature_labels = NULL) {

  feature_list = list()
  feature_list$labels <- labels(x$terms)
  feature_list$classes <- attr(x$terms,"dataClasses")[-1]
  feature_list$factor_levels <- setNames(vector("list", length(feature_list$labels)), feature_list$labels)
  feature_list$factor_levels[names(x$xlevels)] <- x$xlevels

  return(feature_list)
}

#' @rdname get_model_features
#' @export
get_model_features.glm <- function(x, feature_labels = NULL) {

  feature_list = list()
  feature_list$labels <- labels(x$terms)
  feature_list$classes <- attr(x$terms,"dataClasses")[-1]
  feature_list$factor_levels <- setNames(vector("list", length(feature_list$labels)), feature_list$labels)
  feature_list$factor_levels[names(x$xlevels)] <- x$xlevels

  return(feature_list)
}

#' @rdname get_model_features
#' @export
get_model_features.gam <- function(x, feature_labels = NULL) {

  feature_list = list()
  feature_list$labels <- labels(x$terms)
  feature_list$classes <- attr(x$terms,"dataClasses")[-1]
  feature_list$factor_levels <- setNames(vector("list", length(feature_list$labels)), feature_list$labels)
  feature_list$factor_levels[names(x$xlevels)] <- x$xlevels

  return(feature_list)
}

#' @rdname get_model_features
#' @export
get_model_features.ranger <- function(x, feature_labels = NULL) {

  # Additional check
  if (is.null(x$forest)) {
    stop(
      paste0(
        "\nIt looks like the model was fitted without saving the forest. Please set\n",
        "write.forest = TRUE when fitting a model using ranger::ranger()."
      )
    )
  }
  feature_list = list()
  feature_list$labels <- unique_features(x$forest$independent.variable.names)
  feature_list$classes <- setNames(rep(NA, length(feature_list$labels)),feature_list$labels) # Not supported
  feature_list$factor_levels <- setNames(vector("list", length(feature_list$labels)), feature_list$labels)
  feature_list$factor_levels[names(x$forest$covariate.levels)] <- x$forest$covariate.levels # Only provided when respect.unordered.factors == T

  return(feature_list)
}


#' @rdname get_model_features
#' @export
get_model_features.xgb.Booster <- function(x, feature_labels = NULL) {

  feature_list = list()

  if (is.null(x[["dummylist"]])) {
    feature_list$labels <- x$feature_names
    feature_list$classes <- setNames(rep(NA, length(feature_list$labels)),feature_list$labels) # Not supported
    feature_list$factor_levels <- setNames(vector("list", length(feature_list$labels)), feature_list$labels)
  } else {
    feature_list$labels <- x$dummylist$obj$features
    feature_list$classes <- x$dummylist$obj$class_vector
    feature_list$factor_levels <- setNames(vector("list", length(feature_list$labels)), feature_list$labels)
    feature_list$factor_levels[names(x$dummylist$obj$factor_list)] <- x$dummylist$obj$factor_list
  }

  return(feature_list)

}

#' Fetches feature labels from a given model object
#'
#' @param x matrix, data.frame or data.table The data to extract feature information from.
#'
#' @keywords internal
#'
#' @export
#'
#' @examples
#'# Load example data
#' data("Boston", package = "MASS")
#' # Split data into test- and training data
#' x_train <- as.data.table(head(Boston))
#' x_train[,rad:=as.factor(rad)]
#' get_data_features(x_train)
get_data_features <- function(x){

  x <- data.table::as.data.table(x)

  feature_list = list()
  feature_list$labels <- names(x)
  feature_list$classes <- unlist(lapply(x,class))
  feature_list$factor_levels = lapply(x,levels)

  return(feature_list)
}


#' @keywords internal
check_features <- function(f_list_1,f_list_2,
                           name_1 = "model",name_2 = "data",
                           use_first_list_as_truth=F){

  #### Check validity of f_lists ####

  if(!all(f_list_1$labels == names(f_list_1$classes))){
    stop(paste0(name_1," does not have matching labels and class names"))
  }
  if(!all(f_list_1$labels == names(f_list_1$factor_levels))){
    stop(paste0(name_1," does not have matching labels and factor level names"))
  }

  if(!all(f_list_2$labels == names(f_list_2$classes))){
    stop(paste0(name_2," does not have matching labels and class names"))
  }
  if(!all(f_list_2$labels == names(f_list_2$factor_levels))){
    stop(paste0(name_2," does not have matching labels and factor level names"))
  }


  #### Checking labels ####
  if (is.null(f_list_1$labels)) {
    stop(paste0(name_1," must have column names."))
  }
  if (is.null(f_list_2$labels)) {
    stop(paste0(name_2," must have column names."))
  }


  missing_1_in_2 <- f_list_1$labels[!(f_list_1$labels %in% f_list_2$labels)]
  missing_2_in_1 <- f_list_2$labels[!(f_list_2$labels %in% f_list_1$labels)]

  if (length(missing_1_in_2)>0) {
    stop(paste0("Feature(s) ",paste0(missing_1_in_2,collapse=", ")," in ",name_1," is not in ",name_2,"."))
  }

  # Also check also that the features in 2 are in 1
  if(!use_first_list_as_truth){
    if (length(missing_2_in_1)>0) {
      stop(paste0("Feature(s) ",paste0(missing_2_in_1,collapse=", ")," in ",name_2," is not in ",name_1,"."))
    }
  }


  #### Reorder f_List_2 to match f_list_1, also removing anything in the former which is not in the latter ####
  f_list_2_reordering = match(f_list_1$labels,f_list_2$labels)

  f_list_2$labels <- f_list_2$labels[f_list_2_reordering]
  f_list_2$classes <- f_list_2$classes[f_list_2_reordering]
  f_list_2$factor_levels <- f_list_2$factor_levels[f_list_2_reordering]

  # Computes level reordering based on original level order for f_list_1
  f_list_2_level_reordering <- mapply(FUN=function(x,y) match(x,y),
                                      f_list_1$factor_levels, f_list_2$factor_levels, SIMPLIFY = F)

  # Sorts the factor levels for easier comparison below
  f_list_1$factor_levels <- lapply(f_list_1$factor_levels,FUN=sort)
  f_list_2$factor_levels <- lapply(f_list_2$factor_levels,FUN=sort)

  # feature names must be unique
  if (any(duplicated(f_list_1$labels))) {
    stop(paste0("Both ",name_1," and ",name_2," must have unique column names."))
  }

  # Check if any features have empty names i.e ""
  if (any(f_list_1$labels == "")) {
    stop("One or more features is missing a name.")
  }

  #### Checking classes ####
  if(use_first_list_as_truth & any(is.na(f_list_1$classes))){
      message(paste0("The specified ",name_1," does not provide (all) feature classes. ",
                     "Feature class and any factor level checking is disabled."))
  } else {
    # Check if f_list_1 and f_list_2 have features with the same class
    if (!identical(f_list_1$classes,  f_list_2$classes)) {
      stop(paste0("The features in ",name_1," and ",name_2," must have the same classes."))
    }

    # Check if the features all have class "integer", "numeric" or "factor
    if (!all(f_list_1$classes %in% c("integer", "numeric", "factor"))) {
      invalid_class <- which(!(f_list_1$classes %in% c("integer", "numeric", "factor")))
      stop(paste0("Feature(s) ",paste0(invalid_class,collapse=", ")," in ",name_1," and ",name_2,
                  " is not of class integer, numeric or factor."))
    }

    # Checking factor levels #
    if (!identical(f_list_1$factor_levels, f_list_2$factor_levels)) {
      stop(paste0("The levels of the categorical features in ",name_1," and ",name_2," does not match."))
    }

  }

  # Decide what to return
  if(use_first_list_as_truth){
    ret <- list(
      label = f_list_2_reordering,
      factor_level = f_list_2_level_reordering
    )
  } else {
    ret <- NULL
  }

  return(ret)

}

