#' @rdname predict_model
#' @export
predict_model.lm <- function(x, newdata, ...) {
  if (!requireNamespace("stats", quietly = TRUE)) {
    cli::cli_abort("The {.pkg stats} package is required for predicting stats models")
  }

  predict(x, as.data.frame(newdata))
}

#' @rdname get_model_specs
#' @export
get_model_specs.lm <- function(x) {
  model_checker(x) # Checking if the model is supported

  feature_specs <- list()
  feature_specs$labels <- all.vars(formula(x))[-1]
  m <- length(feature_specs$labels)

  feature_specs$classes <- attr(x$terms, "dataClasses")[-1]
  feature_specs$factor_levels <- setNames(vector("list", m), feature_specs$labels)
  feature_specs$factor_levels[names(x$xlevels)] <- x$xlevels

  return(feature_specs)
}


#' @rdname model_checker
#' @export
model_checker.lm <- function(x) {
  NULL
}
