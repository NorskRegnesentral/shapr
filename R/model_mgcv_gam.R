#' @rdname predict_model
#' @export
predict_model.gam <- function(x, newdata, ...) {
  if (!requireNamespace("mgcv", quietly = TRUE)) {
    cli::cli_abort("The {.pkg mgcv} package is required for predicting `gam` models.")
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

  feature_specs <- list()
  feature_specs$labels <- all.vars(formula(x))[-1]
  m <- length(feature_specs$labels)

  feature_specs$classes <- attr(x$terms, "dataClasses")[-1]
  # Use same order as labels for classes
  feature_specs$classes <- feature_specs$classes[feature_specs$labels]
  feature_specs$factor_levels <- setNames(vector("list", m), feature_specs$labels)
  feature_specs$factor_levels[names(x$xlevels)] <- x$xlevels

  return(feature_specs)
}

#' @rdname model_checker
#' @export
model_checker.gam <- function(x) {
  NULL
}
