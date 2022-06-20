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
#' For more details on how to explain other types of models (i.e. custom models), see the Advanced usage section
#' of the vignette: \cr
#' From R: \code{vignette("understanding_shapr", package = "shapr")}  \cr
#' Web: \url{https://norskregnesentral.github.io/shapr/articles/understanding_shapr.html#explain-custom-models}
#'
#' @return Numeric
#'
#' @export
#' @keywords internal
#'
#' @author Martin Jullum
#' @examples
#' if (requireNamespace("MASS", quietly = TRUE)) {
#'   # Load example data
#'   data("Boston", package = "MASS")
#'   # Split data into test- and training data
#'   x_train <- head(Boston, -3)
#'   x_explain <- tail(Boston, 3)
#'   # Fit a linear model
#'   model <- lm(medv ~ lstat + rm + dis + indus, data = x_train)
#'
#'   # Predicting for a model with a standardized format
#'   predict_model(x = model, newdata = x_explain)
#' }
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






#' Check that the type of model is supported by the explanation method
#'
#' @description The function checks whether the model given by \code{x} is supported.
#' If \code{x} is not a supported model the function will return an error message, otherwise it return NULL
#' (meaning all types of models with this class is supported)
#'
#' @inheritParams predict_model
#'
#' @details See \code{\link{predict_model}} for more information about
#' what type of models \code{shapr} currently support.
#'
#' @return Error or NULL
#'
#' @export
#' @keywords internal
#'
#' @examples
#' if (requireNamespace("MASS", quietly = TRUE)) {
#'   # Load example data
#'   data("Boston", package = "MASS")
#'   # Split data into test- and training data
#'   x_train <- head(Boston, -3)
#'   # Fit a linear model
#'   model <- lm(medv ~ lstat + rm + dis + indus, data = x_train)
#'
#'   # Checking the model object
#'   model_checker(x = model)
#' }
model_checker <- function(x) {
  UseMethod("model_checker", x)
}

#' @rdname model_checker
#' @export
model_checker.default <- function(x) {
  stop("The model class you passed to shapr is currently not supported.")
}






#' Fetches feature information from a given model object
#'
#' @inheritParams predict_model
#'
#' @details This function is used to extract the feature information to be checked against data passed to \code{shapr}
#' and \code{explain}. The function is called from \code{preprocess_data}.
#'
#' @return A list with the following elements:
#' \describe{
#'   \item{labels}{character vector with the feature names to compute Shapley values for}
#'   \item{classes}{a named character vector with the labels as names and the class type as elements}
#'   \item{factor_levels}{a named list with the labels as names and character vectors with the factor levels as elements
#'   (NULL if the feature is not a factor)}
#' }
#'
#' @author Martin Jullum
#'
#' @keywords internal
#' @export
#'
#' @examples
#' if (requireNamespace("MASS", quietly = TRUE)) {
#'   # Load example data
#'   data("Boston", package = "MASS")
#'   # Split data into test- and training data
#'   x_train <- data.table::as.data.table(head(Boston))
#'   x_train[, rad := as.factor(rad)]
#'   model <- lm(medv ~ lstat + rm + rad + indus, data = x_train)
#'
#'   get_model_specs(model)
#' }
get_model_specs <- function(x) {


  UseMethod("get_model_specs", x)
}

#' @rdname get_model_specs
get_model_specs.default <- function(x) {

  # For custom models where there is no
  return(list(labels = NA, classes = NA, factor_levels = NA))
}


#' Provides a data.table with the supported models
#'
#' @keywords internal
get_supported_models <- function() {

  # Fixing NSE notes in R CMD check
  rn <- get_model_specs <- native_get_model_specs <- from <- NULL
  predict_model <- native_predict_model <- NULL
  native <- NULL

  DT_get_model_specs <- data.table::as.data.table(attr(methods(get_model_specs), "info"), keep.rownames = T)

  DT_get_model_specs[, rn := substring(as.character(rn), first = 17)]
  DT_get_model_specs[, get_model_specs := 1]
  DT_get_model_specs[, c("visible", "from", "generic", "isS4") := NULL]

  DT_predict_model <- data.table::as.data.table(attr(methods(predict_model), "info"), keep.rownames = T)
  DT_predict_model[, rn := substring(as.character(rn), first = 15)]
  DT_predict_model[, predict_model := 1]
  DT_predict_model[, c("visible", "from", "generic", "isS4") := NULL]

  DT <- merge(DT_get_model_specs, DT_predict_model, by = "rn", all = T, allow.cartesian = T, nomatch = 0)
  DT[, (colnames(DT)[-1]) := lapply(.SD, data.table::nafill, fill = 0), .SDcols = colnames(DT)[-1]]
  DT[, (colnames(DT)[2:3]) := lapply(.SD, as.logical), .SDcols = colnames(DT)[2:3]]
  data.table::setnames(DT, "rn", "model_class")
  return(DT)
}