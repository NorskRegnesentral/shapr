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
    newdata_dummy <- apply_dummies(feature_list = x$feature_list, testdata = newdata)
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
  UseMethod("model_type", x)
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
    ifelse(is.null(x$feature_list), "classification", "cat_classification"),
    ifelse(is.null(x$feature_list), "regression", "cat_regression")
  )
}

#' Fetches feature labels from a given model object
#'
#' @inheritParams predict_model
#'
#' @keywords internal
#'
#' @export
#'
#' @examples
#'  # Load example data
#' data("Boston", package = "MASS")
#' # Split data into test- and training data
#' x_train <- data.table::as.data.table(head(Boston))
#' x_train[,rad:=as.factor(rad)]
#' model <- lm(medv ~ lstat + rm + rad + indus, data = x_train)
#'
#' get_model_specs(model)
get_model_specs <- function(model) {

  model_class <- NULL # Due to NSE notes in R CMD check

  required_model_objects <- c("model_type","predict_model","get_model_specs")

  # Start with all checking for native models
  model_info <- get_supported_models()[model_class==class(model)[1],]
  available_model_objects <- names(which(unlist(model_info[,2:4])))

  if(nrow(model_info)==0){
    stop(
      "You passed a nonsupported model to shapr. See ?shapr::shapr or the vignette\n",
      "for more information on how to run shapr with custom models."
    )
  }

  if(!(all(required_model_objects %in% available_model_objects)))
  {
    this_object_missing <- which(!(required_model_objects %in% available_model_objects))
    stop(
      paste0(
        "The following required model objects are not available for your custom model: ",
        paste0(required_model_objects[this_object_missing],collapse = ", "),".\n",
        "See ?shapr::shapr or the vignette for more information."
      )
    )
  }


  UseMethod("get_model_specs", model)
}

#' @rdname get_model_specs
get_model_specs.default <- function(model) {

  # For custom models
  stop("You should never get here. May I delete this function?")
  return(feature_list)
}


#' @rdname get_model_specs
#' @export
get_model_specs.lm <- function(model) {

  feature_list = list()
  feature_list$labels <- labels(model$terms)
  m <- length(feature_list$labels)

  feature_list$classes <- attr(model$terms,"dataClasses")[-1]
  feature_list$factor_levels <- setNames(vector("list", m), feature_list$labels)
  feature_list$factor_levels[names(model$xlevels)] <- model$xlevels
  feature_list$model_type <- model_type(model)
  feature_list$specs_type <- "model"

  return(feature_list)
}

#' @rdname get_model_specs
#' @export
get_model_specs.glm <- function(model) {

  feature_list = list()
  feature_list$labels <- labels(model$terms)
  m <- length(feature_list$labels)

  feature_list$classes <- attr(model$terms,"dataClasses")[-1]
  feature_list$factor_levels <- setNames(vector("list", m), feature_list$labels)
  feature_list$factor_levels[names(model$xlevels)] <- model$xlevels
  feature_list$model_type <- model_type(model)
  feature_list$specs_type <- "model"

  return(feature_list)
}

#' @rdname get_model_specs
#' @export
get_model_specs.gam <- function(model) {

  feature_list = list()
  feature_list$labels <- labels(model$terms)
  m <- length(feature_list$labels)

  feature_list$classes <- attr(model$terms,"dataClasses")[-1]
  feature_list$factor_levels <- setNames(vector("list", m), feature_list$labels)
  feature_list$factor_levels[names(model$xlevels)] <- model$xlevels
  feature_list$model_type <- model_type(model)
  feature_list$specs_type <- "model"

  return(feature_list)
}

#' @rdname get_model_specs
#' @export
get_model_specs.ranger <- function(model) {

  # Additional check
  if (is.null(model$forest)) {
    stop(
      paste0(
        "\nIt looks like the model was fitted without saving the forest. Please set\n",
        "write.forest = TRUE when fitting a model using ranger::ranger()."
      )
    )
  }
  feature_list = list()
  feature_list$labels <- unique_features(model$forest$independent.variable.names)
  m <- length(feature_list$labels)

  feature_list$classes <- setNames(rep(NA, m),feature_list$labels) # Not supported
  feature_list$factor_levels <- setNames(vector("list", m), feature_list$labels)
  feature_list$factor_levels[names(model$forest$covariate.levels)] <- model$forest$covariate.levels # Only provided when respect.unordered.factors == T
  feature_list$model_type <- model_type(model)
  feature_list$specs_type <- "model"

  return(feature_list)
}


#' @rdname get_model_specs
#' @export
get_model_specs.xgb.Booster <- function(model) {

  feature_list = list()

  if (is.null(model[["feature_list"]])) {
    feature_list$labels <- model$feature_names
    m <- length(feature_list$labels)

    feature_list$classes <- setNames(rep(NA, m),feature_list$labels) # Not supported
    feature_list$factor_levels <- setNames(vector("list", m), feature_list$labels)
  } else {
    feature_list <- model$feature_list
  }
  feature_list$model_type <- model_type(model)
  feature_list$specs_type <- "model"

  return(feature_list)

}




#' Provides a data.table with the supported models
#'
#'@keywords internal
get_supported_models <- function(){

  # Fixing NSE notes in R CMD check
  rn <- get_model_specs <- native_get_model_specs <- from <- NULL
  model_type <- native_model_type <-  NULL
  predict_model <- native_predict_model <- NULL
  native <- NULL

  DT_get_model_specs <- data.table::as.data.table(attr(methods(get_model_specs),"info"),keep.rownames = T)
  #DT_get_model_specs <- data.table::as.data.table(attr(.S3methods(get_model_specs,envir=globalenv()),"info"),keep.rownames = T)

  DT_get_model_specs[,rn:=substring(as.character(rn),first=17)]
  DT_get_model_specs[,get_model_specs:=1]
  DT_get_model_specs[,native_get_model_specs:=ifelse(from=="shapr",1,0)]
  DT_get_model_specs[,c("visible","from","generic","isS4"):=NULL]

  DT_model_type <- data.table::as.data.table(attr(methods(model_type),"info"),keep.rownames = T)
  #DT_model_type <- data.table::as.data.table(attr(.S3methods(model_type,envir=globalenv()),"info"),keep.rownames = T)

  DT_model_type[,rn:=substring(as.character(rn),first=12)]
  DT_model_type[,model_type:=1]
  DT_model_type[,native_model_type:=ifelse(from=="shapr",1,0)]
  DT_model_type[,c("visible","from","generic","isS4"):=NULL]

  DT_predict_model <- data.table::as.data.table(attr(methods(predict_model),"info"),keep.rownames = T)
  DT_predict_model[,rn:=substring(as.character(rn),first=15)]
  DT_predict_model[,predict_model:=1]
  DT_predict_model[,native_predict_model:=ifelse(from=="shapr",1,0)]
  DT_predict_model[,c("visible","from","generic","isS4"):=NULL]

  DT <- merge(DT_get_model_specs,DT_model_type,by="rn",all=T,allow.cartesian=T,nomatch=0)
  DT <- merge(DT,DT_predict_model,by="rn",all=T,nomatch=0)
  DT[,(colnames(DT)[-1]):=lapply(.SD,data.table::nafill,fill=0),.SDcols=colnames(DT)[-1]]
  DT[,native:=as.logical(native_get_model_specs*native_model_type*native_predict_model)]
  DT[,c("native_get_model_specs","native_model_type","native_predict_model"):=NULL]
  DT[,(colnames(DT)[2:4]):=lapply(.SD,as.logical),.SDcols=colnames(DT)[2:4]]
  data.table::setnames(DT,"rn","model_class")
  return(DT)
}


