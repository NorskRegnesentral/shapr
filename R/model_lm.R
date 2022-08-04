#' @rdname predict_model
#' @export
predict_model.lm <- function(x, newdata) {
  if (!requireNamespace("stats", quietly = TRUE)) {
    stop("The stats package is required for predicting stats models")
  }

  predict(x, as.data.frame(newdata))
}

#' @rdname get_model_specs
#' @export
get_model_specs.lm <- function(x) {
  model_checker(x) # Checking if the model is supported

  feature_spec <- list()
  feature_spec$labels <- labels(x$terms)
  m <- length(feature_spec$labels)

  feature_spec$classes <- attr(x$terms, "dataClasses")[-1]
  feature_spec$factor_levels <- setNames(vector("list", m), feature_spec$labels)
  feature_spec$factor_levels[names(x$xlevels)] <- x$xlevels

  return(feature_spec)
}


#' @rdname model_checker
#' @export
model_checker.lm <- function(x) {
  NULL
}
