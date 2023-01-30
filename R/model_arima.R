#' @rdname predict_model
#' @export
predict_model.Arima <- predict_model.forecast_ARIMA <- function(x, newdata, newreg, horizon) {
  if (!requireNamespace("stats", quietly = TRUE)) {
    stop("The stats package is required for predicting stats models")
  }

  n_endo <- max(x$arma[1], x$arma[6] + x$arma[7] * x$arma[5] + 1)
  prediction <- matrix(NA, nrow(newdata), horizon)

  for (i in seq_len(nrow(newdata))) {
    endo <- as.numeric(newdata[i, 1:n_endo])
    exo <- matrix(as.numeric(newdata[i, -(1:n_endo)]), n_endo)

    if (ncol(exo) == 0) {
      x <- forecast::Arima(y = endo, model = x)
      prediction[i, ] <- predict(x, h = horizon)$pred
    } else {
      x <- forecast::Arima(y = endo, xreg = exo, model = x)
      xreg <- matrix(as.numeric(newreg[i, ]), horizon)
      prediction[i, ] <- predict(x, newxreg=xreg, h = horizon)$pred
    }
  }

  as.data.frame(prediction)
}

#' @rdname get_model_specs
#' @export
get_model_specs.Arima <- get_model_specs.forecast_ARIMA <- function(x) {
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
model_checker.Arima <- model_checker.forecast_ARIMA <- function(x) {
  NULL
}
