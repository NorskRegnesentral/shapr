#' Generate predictions for input data with specified model
#'
#' @description Performs prediction of response
#' [stats::lm()],
#' [stats::glm()],
#' [ranger::ranger()],
#' [mgcv::gam()],
#' [workflows::workflow()] (i.e., `tidymodels` models), and
#' [xgboost::xgb.train()] with binary or continuous
#' response. See details for more information.
#'
#' @param x Model object for the model to be explained.
#' @param newdata A data.frame/data.table with the features to predict from.
#' @param ... `newreg` and `horizon` parameters used in models passed to `[explain_forecast()]`
#'
#' @details The following models are currently supported:
#' \itemize{
#' \item [stats::lm()]
#' \item [stats::glm()]
#' \item [ranger::ranger()]
#' \item [mgcv::gam()]
#' \item [workflows::workflow()]
#' \item [xgboost::xgb.train()]
#' }
#'
#' If you have a binary classification model we'll always return the probability prediction
#' for a single class.
#'
#' If you are explaining a model not supported natively, you need to create the `[predict_model()]` function yourself,
#' and pass it on to as an argument to `[explain()]`.
#'
#' For more details on how to explain such non-supported models (i.e. custom models), see the Advanced usage section
#' of the general usage: \cr
#' From R: `vignette("general_usage", package = "shapr")`  \cr
#' Web: <https://norskregnesentral.github.io/shapr/articles/general_usage.html#explain-custom-models>
#'
#' @return Numeric. Vector of size equal to the number of rows in `newdata`.
#'
#' @export
#' @keywords internal
#'
#' @author Martin Jullum
#' @examples
#' # Load example data
#' data("airquality")
#' airquality <- airquality[complete.cases(airquality), ]
#' # Split data into test- and training data
#' x_train <- head(airquality, -3)
#' x_explain <- tail(airquality, 3)
#' # Fit a linear model
#' model <- lm(Ozone ~ Solar.R + Wind + Temp + Month, data = x_train)
#'
#' # Predicting for a model with a standardized format
#' predict_model(x = model, newdata = x_explain)
predict_model <- function(x, newdata, ...) {
  UseMethod("predict_model", x)
}

#' @rdname predict_model
#' @export
predict_model.default <- function(x, newdata, ...) {
  str_error <- paste(
    "It seems that you passed a non-valid model object.",
    "For more information about which models that are supported, see the documentation of {.fn shapr::predict_model}."
  )
  cli::cli_abort(str_error)
}






#' Check that the type of model is supported by the native implementation of the model class
#'
#' @description The function checks whether the model given by `x` is supported.
#' If `x` is not a supported model the function will return an error message, otherwise it return NULL
#' (meaning all types of models with this class is supported)
#'
#' @inheritParams predict_model
#'
#' @seealso See [predict_model()] for more information about what type of models `shapr` currently support.
#'
#' @return Error or NULL
#'
#' @keywords internal
model_checker <- function(x) {
  UseMethod("model_checker", x)
}

#' @rdname model_checker
#' @export
model_checker.default <- function(x) {
  cli::cli_abort("The model class you passed to shapr is currently not supported.")
}






#' Fetches feature information from natively supported models
#'
#' @inheritParams predict_model
#'
#' @description This function is used to extract the feature information from the model to be checked against the
#' corresponding feature information in the data passed to [explain()].
#'
#' NOTE: You should never need to call this function explicitly.
#' It is exported just to be easier accessible for users, see details.
#'
#' @details If you are explaining a model not supported natively, you may (optionally) enable such checking by
#' creating this function yourself and passing it on to [explain()].
#'
#' @return A list with the following elements:
#' \describe{
#'   \item{labels}{character vector with the feature names to compute Shapley values for}
#'   \item{classes}{a named character vector with the labels as names and the class type as elements}
#'   \item{factor_levels}{a named list with the labels as names and character vectors with the factor levels as elements
#'   (NULL if the feature is not a factor)}
#' }
#'
#' @seealso For model classes not supported natively, you NEED to create an analogue to [predict_model()]. See it's
#' help file for details.
#'
#' @author Martin Jullum
#'
#' @keywords internal
#' @export
#'
#' @examples
#' # Load example data
#' data("airquality")
#' airquality <- airquality[complete.cases(airquality), ]
#' # Split data into test- and training data
#' x_train <- head(airquality, -3)
#' x_explain <- tail(airquality, 3)
#' # Fit a linear model
#' model <- lm(Ozone ~ Solar.R + Wind + Temp + Month, data = x_train)
#' get_model_specs(model)
#'
get_model_specs <- function(x) {
  UseMethod("get_model_specs", x)
}

#' @rdname get_model_specs
#' @export
get_model_specs.default <- function(x) {
  # For custom models where there is no information
  return(list(labels = NA, classes = NA, factor_levels = NA))
}


#' Provides a data.table with the supported models
#'
#' @return A data.table with the supported models.
#' @export
get_supported_models <- function() {
  DT_get_model_specs <- data.table::as.data.table(attr(methods(get_model_specs), "info"), keep.rownames = TRUE)

  DT_get_model_specs[, rn := substring(as.character(rn), first = 17)]
  DT_get_model_specs[, get_model_specs := 1]
  DT_get_model_specs[, c("visible", "from", "generic", "isS4") := NULL]

  DT_predict_model <- data.table::as.data.table(attr(methods(predict_model), "info"), keep.rownames = TRUE)
  DT_predict_model[, rn := substring(as.character(rn), first = 15)]
  DT_predict_model[, predict_model := 1]
  DT_predict_model[, c("visible", "from", "generic", "isS4") := NULL]

  DT <- merge(DT_get_model_specs, DT_predict_model, by = "rn", all = TRUE, allow.cartesian = TRUE)
  DT[, (colnames(DT)[-1]) := lapply(.SD, data.table::nafill, fill = 0), .SDcols = colnames(DT)[-1]]
  DT[, (colnames(DT)[2:3]) := lapply(.SD, as.logical), .SDcols = colnames(DT)[2:3]]
  data.table::setnames(DT, "rn", "model_class")
  return(DT[])
}
