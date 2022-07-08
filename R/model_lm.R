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

  feature_list <- list()
  feature_list$labels <- labels(x$terms)
  m <- length(feature_list$labels)

  feature_list$classes <- attr(x$terms, "dataClasses")[-1]
  feature_list$factor_levels <- setNames(vector("list", m), feature_list$labels)
  feature_list$factor_levels[names(x$xlevels)] <- x$xlevels

  return(feature_list)
}


#' @rdname model_checker
#' @export
model_checker.lm <- function(x) {
  NULL
}
