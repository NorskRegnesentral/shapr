#' @rdname predict_model
#' @export
predict_model.workflow <- function(x, newdata, ...) {
  if (!requireNamespace("workflows", quietly = TRUE)) {
    cli::cli_abort("The {.pkg workflows} package is required for predicting `workflows` models.")
  }
  predict(x, as.data.frame(newdata))$.pred
}

#' @rdname get_model_specs
#' @export
get_model_specs.workflow <- function(x) {
  model_checker(x) # Checking if the model is supported
  var_info <- x$pre$actions$recipe$recipe$var_info
  predictors <- var_info$variable[var_info$role == "predictor"]
  template <- x$pre$actions$recipe$recipe$template[predictors]
  feature_specs <- list()
  feature_specs$labels <- colnames(template)
  feature_specs$classes <- sapply(template, class)
  feature_specs$classes[feature_specs$classes == "integer"] <- "numeric" # Integers to numerics, see `get_data_specs()`
  feature_specs$factor_levels <- sapply(template, levels)
  return(feature_specs)
}

#' @rdname model_checker
#' @export
model_checker.workflow <- function(x) {
  if (x$fit$actions$model$spec$mode != "regression") {
    cli::cli_abort("Only models with `mode = 'regression'` are supported.")
  }
  return(NULL)
}
