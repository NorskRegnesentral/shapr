#' @rdname predict_model
#' @export
predict_model.workflow <- function(x, newdata, ...) {
  if (!requireNamespace("workflows", quietly = TRUE)) {
    stop("The `workflows` package is required for predicting `workflows`")
  }
  predict(x, as.data.frame(newdata))
}

#' @rdname get_model_specs
#' @export
get_model_specs.workflow <- function(x) {
  model_checker(x) # Checking if the model is supported
  predictors <- x$pre$mold$predictors
  feature_specs <- list()
  feature_specs$labels <- colnames(predictors)
  feature_specs$classes <- sapply(predictors, class)
  feature_specs$classes[feature_specs$classes == "integer"] <- "numeric" # Integers to numerics, see `get_data_specs()`
  feature_specs$factor_levels <- sapply(predictors, levels)
  return(feature_specs)
}

#' @rdname model_checker
#' @export
model_checker.workflow <- function(x) {
  if (x$fit$actions$model$spec$mode != "regression") stop("We only support models with `mode = 'regression'`.")
  return(NULL)
}
