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
#' @param feature_labels Character vector. Represents the labels of the features used for prediction.
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
#' get_model_specs(model, feature_labels = NULL)
get_model_specs <- function(model,feature_labels = NULL) {

  model_class <- NULL # Due to NSE notes in R CMD check

  required_model_objects <- c("model_type","predict_model")
  recommended_model_objects <- "get_model_specs"

  # Start with all checking for native models
  model_info <- get_supported_models()[model_class==class(model)[1],]

  if(nrow(model_info)==0){
    stop(
      "You passed a nonsupported model to shapr. See ?shapr::shapr or the vignette\n",
      "for more information on how to run shapr with custom models."
    )
  }

  if(model_info$native){
    if(!is.null(feature_labels)){
      message(
        paste0(
          "\nYou have passed a supported model object, and therefore\n",
          "features_labels is ignored. The argument is only applicable when\n",
          "using a custom model. See ?shapr::shapr or the vignette for more information."
        )
      )
    }
  } else { # if custom model


    available_model_objects <- names(which(unlist(model_info[,2:4])))
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
    if(!(recommended_model_objects %in% available_model_objects)){
      if(is.character(feature_labels)){
        message(
          paste0(
            "The following recommended model objects are not available for your custom model: ",
            paste0(recommended_model_objects,collapse = ", "),".\n",
            "The provided feature_labels is used, and there is no checking of feature classes and any\n",
            "factor levels. See ?shapr::shapr or the vignette for more information."
          )
        )
      } else { # If neither the recommended objects nor feature labels is provided
        stop(
          paste0(
            "Both the recommended model object ",recommended_model_objects," and feature_labels are\n",
            "missing for your custom model. Please provided at least one of them.\n",
            "See ?shapr::shapr or the vignette for more information."
          )
        )
      }
    } else {
      if(is.character(feature_labels)){ # If both recommended objects and feature labels is provided
        message(
          paste0(
            "\nYou have passed a custom model with all required and recommended model objects.\n",
            "features_labels is therefore ignored. The argument is only applicable when\n",
            "using a custom model without ",recommended_model_objects,"\n",
            "See ?shapr::shapr or the vignette for more information."
          )
        )
      }
    }
  }


  UseMethod("get_model_specs", model)
}

#' @rdname get_model_specs
get_model_specs.default <- function(model, feature_labels = NULL) {

  # For custom models
  feature_list = list()
  feature_list$labels <- feature_labels
  m <- length(feature_list$labels)

  feature_list$classes <- rep(NA,m)
  feature_list$factor_levels <- setNames(vector("list", m), feature_list$labels)
  feature_list$model_type <- model_type(model)
  feature_list$specs_type <- "model"

  return(feature_list)
}


#' @rdname get_model_specs
#' @export
get_model_specs.lm <- function(model, feature_labels = NULL) {

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
get_model_specs.glm <- function(model, feature_labels = NULL) {

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
get_model_specs.gam <- function(model, feature_labels = NULL) {

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
get_model_specs.ranger <- function(model, feature_labels = NULL) {

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
get_model_specs.xgb.Booster <- function(model, feature_labels = NULL) {

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
#' x_train <- data.table::as.data.table(head(Boston))
#' x_train[,rad:=as.factor(rad)]
#' get_data_specs(x_train)
get_data_specs <- function(x){

  x <- data.table::as.data.table(x)

  feature_list = list()
  feature_list$labels <- names(x)
  feature_list$classes <- unlist(lapply(x,class))
  feature_list$factor_levels = lapply(x,levels)
  feature_list$specs_type <- "data"

  # Defining all integer values as numeric
  feature_list$classes[feature_list$classes=="integer"] <- "numeric"

  return(feature_list)
}

#' Process (check and update) data according to specified feature list
#'
#' @param x matrix, data.frame or data.table. The data to check input for and update
#' according to the specification in \code{feature_list}.
#' @param feature_list List. Output from running \code{get_data_specs} or \code{get_model_specs}
#'
#' @return Checked and updated data \code{x} in data.table format.
#'
#' @keywords internal
#'
#' @export
#'
preprocess_data = function(x,feature_list){
  if(all(is.null(colnames(x)))){
    stop(paste0("The data is missing column names"))
  }

  x_dt <- data.table::as.data.table(x)

  feature_list_data <- get_data_specs(x_dt)

  updater <- check_features(feature_list,feature_list_data,
                            use_1_as_truth = T)
  update_data(x_dt,updater) # Updates x_dt by reference

  ret <- list(x_dt = x_dt,
              updated_feature_list = updater)

  return(ret)
}


#' Checks that two extracted feature lists have exactly the same properites
#'
#' @param f_list_1,f_list_2 List. As extracted from either \code{get_data_specs} or \code{get_model_specs}.
#' @param use_1_as_truth Logical. If TRUE, \code{f_list_2} is compared to \code{f_list_1}, i.e. additional elements
#' is allowed in \code{f_list_2}, and if \code{f_list_1}'s feature classes contains NA's, feature class check is
#' ignored regardless of what is specified in \code{f_list_1}. If FALSE, \code{f_list_1} and \code{f_list_2} are
#' equated and they need to contain exactly the same elements. Set to TRUE when comparing a model and data, and FALSE
#' when comparing two data sets.
#'
#' @return List. The \code{f_list_1} is returned as inserted if there all check are carried out, otherwise
#' \code{f_list_2} is used.
#'
#' @keywords internal
#'
#' @export
#'
#' @examples
#'
#'
#' # Load example data
#' data("Boston", package = "MASS")
#' # Split data into test- and training data
#' x_train <- data.table::as.data.table(head(Boston))
#' x_train[,rad:=as.factor(rad)]
#' data_features <- get_data_specs(x_train)
#' model <- lm(medv ~ lstat + rm + rad + indus, data = x_train)
#'
#' model_features <- get_model_specs(x = model, feature_labels = NULL)
#' check_features(model_features,data_features)
check_features <- function(f_list_1,f_list_2,
                           use_1_as_truth=T){

  name_1 <- f_list_1$specs_type
  name_2 <- f_list_2$specs_type

  if(name_1 == name_2){
    name_1 <- paste0("shapr-",name_1)
    name_2 <- paste0("test-",name_2)
  }

  #### Checking labels ####
  if (is.null(f_list_1$labels)) {
    stop(paste0(name_1," must have column names."))
  }
  if (is.null(f_list_2$labels)) {
    stop(paste0(name_2," must have column names."))
  }

  # feature names must be unique
  if (any(duplicated(f_list_1$labels))) {
    stop(paste0(name_1," must have unique column names."))
  }

  # feature names must be unique
  if (any(duplicated(f_list_2$labels))) {
    stop(paste0(name_2," must have unique column names."))
  }


  missing_1_in_2 <- f_list_1$labels[!(f_list_1$labels %in% f_list_2$labels)]
  missing_2_in_1 <- f_list_2$labels[!(f_list_2$labels %in% f_list_1$labels)]

  if (length(missing_1_in_2)>0) {
    stop(paste0("Feature(s) ",paste0(missing_1_in_2,collapse=", ")," in ",name_1," is not in ",name_2,"."))
  }

  # Also check also that the features in 2 are in 1
  if(!use_1_as_truth){
    if (length(missing_2_in_1)>0) {
      stop(paste0("Feature(s) ",paste0(missing_2_in_1,collapse=", ")," in ",name_2," is not in ",name_1,"."))
    }
  }


  #### Reorder f_List_2 to match f_list_1, also removing anything in the former which is not in the latter ####
  f_list_2_reordering = match(f_list_1$labels,f_list_2$labels)

  f_list_2$labels <- f_list_2$labels[f_list_2_reordering]
  f_list_2$classes <- f_list_2$classes[f_list_2_reordering]
  f_list_2$factor_levels <- f_list_2$factor_levels[f_list_2_reordering]

  # Sorts the factor levels for easier comparison below
  f_list_1$sorted_factor_levels <- lapply(f_list_1$factor_levels,FUN=sort)
  f_list_2$sorted_factor_levels <- lapply(f_list_2$factor_levels,FUN=sort)

  # Check if any features have empty names i.e ""
  if (any(f_list_1$labels == "")) {
    stop("One or more features is missing a name.")
  }


  #### Checking classes ####
  if(use_1_as_truth & any(is.na(f_list_1$classes))){
      message(paste0("The specified ",name_1," does not provide (all) feature classes. ",
                     "Feature class and any factor level checking is disabled, and those specifed in the ",
                     name_2," are passed on from here."))

    ret <- f_list_2

  } else {
    # Check if f_list_1 and f_list_2 have features with the same class
    if (!identical(f_list_1$classes,  f_list_2$classes)) {
      stop(paste0("The features in ",name_1," and ",name_2," must have the same classes."))
    }

    # Check if the features all have class "integer", "numeric" or "factor
    if (!all(f_list_1$classes %in% c("integer", "numeric", "factor"))) {
      invalid_class <- which(!(f_list_1$classes %in% c("integer", "numeric", "factor")))
      stop(paste0("Feature(s) ",paste0(f_list_1$labels[invalid_class],collapse=", ")," in ",name_1," and ",name_2,
                  " is not of class integer, numeric or factor."))
    }

    # Checking factor levels #
    if (!identical(f_list_1$sorted_factor_levels, f_list_2$sorted_factor_levels)) {
      stop(paste0("Some levels for factor features are not present in both ",name_1," and ",name_2,"."))
    }

    ret <- f_list_1
  }

  ret$sorted_factor_levels <- NULL # Not needed

  return(ret) #

}

#' Updates data by reference according to the updater argument.
#'
#' @description \code{data} is updated, i.e. unused columns and factor levels are removed as described in
#' \code{updater}. This is done by reference, i.e. updates the object being passed to data even if nothing is
#' returned by the function itself.
#'
#' @param data data.table. Data that ought to be updated.
#' @param updater List. The object should be the output from
#' \code{\link[shapr:check_features]{check_features()}}.
#'
#'
#' @return NULL.
#' @keywords internal
update_data = function(data,updater){
  # Operates on data by reference, so no copying of data here

  new_labels <- updater$labels
  factor_features <- which(updater$classes=="factor")
  factor_levels <- updater$factor_levels

  # Reorder and delete unused columns
  cnms_remove <- setdiff(colnames(data), new_labels)
  if (length(cnms_remove) > 0) {
    message(paste0("The columns(s) ",paste0(cnms_remove,collapse=", ")," is not used by the model and thus removed ",
                   "from the data."))

    data[, (cnms_remove) := NULL]
  }
  data.table::setcolorder(data, new_labels)

  # Reorderes the factor levels
  org_factor_levels <- lapply(data,levels)
  identical_levels <- mapply(FUN = "identical",org_factor_levels,factor_levels)
  if(any(!identical_levels)){
    changed_levels <- which(!identical_levels)
    message(paste0("Levels are reordered for the factor feature(s) ",
                   paste0(new_labels[changed_levels],collapse=", "),"."))

    for (i in changed_levels) {
      data.table::set(data,
                      j=i,
                      value = factor(unlist(data[,new_labels[i],with=F],use.names = F), levels = factor_levels[[i]]))
    }
  }

  return(NULL)
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


