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

#' @rdname get_model_specs
#' @export
get_model_specs.gam <- function(x) {
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
model_checker.gam <- function(x) {
  NULL
}
