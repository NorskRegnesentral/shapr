#' @rdname predict_model
#' @export
predict_model.ar <- function(x, newdata, newreg, horizon, ...) {
  if (!requireNamespace("stats", quietly = TRUE)) {
    cli::cli_abort("The {.pkg stats} package is required for predicting stats models")
  }

  as.data.frame(t(apply(newdata, 1, function(n) predict(x, rev(n), n.ahead = horizon, se.fit = FALSE))))
}

#' @rdname get_model_specs
#' @export
get_model_specs.ar <- function(x) {
  model_checker(x) # Checking if the model is supported

  feature_specs <- list()
  feature_specs$labels <- rep(NA, length(x$ar))
  m <- length(feature_specs$labels)

  feature_specs$classes <- rep("numeric", m)
  names(feature_specs$classes) <- feature_specs$labels
  feature_specs$factor_levels <- setNames(vector("list", m), feature_specs$labels)
  feature_specs$factor_levels[names(x$xlevels)] <- NULL

  return(feature_specs)
}

#' @rdname model_checker
#' @export
model_checker.ar <- function(x) {
  NULL
}
