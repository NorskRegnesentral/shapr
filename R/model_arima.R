#' @rdname predict_model
#' @export
predict_model.Arima <- function(x, newdata, newreg, horizon, explain_idx, explain_lags, y, xreg, ...) {
  if (!requireNamespace("stats", quietly = TRUE)) {
    stop("The stats package is required for predicting stats models")
  }

  prediction <- matrix(NA, nrow(newdata), horizon)

  exp_idx <- -1
  for (i in seq_len(nrow(newdata))) {
    if (explain_idx[i] != exp_idx) {
      exp_idx <- explain_idx[i]
      y_hist <- y[seq_len(exp_idx)]
      xreg_hist <- xreg[seq_len(exp_idx)]
    }

    y_new <- as.numeric(newdata[i, 1:explain_lags$y])
    y_hist[seq.int(length.out = length(y_new), to = length(y_hist))] <- rev(y_new)

    if (explain_lags$y == ncol(newdata)) {
      x <- forecast::Arima(y = y_hist, model = x)
      prediction[i, ] <- predict(x, h = horizon)$pred
    } else {
      xreg_new <- as.numeric(newdata[i, -(1:explain_lags$y)])
      xreg_hist[seq.int(length.out = length(xreg_new), to = length(xreg_hist))] <- rev(xreg_new)

      x <- forecast::Arima(y = y_hist, xreg = xreg_hist, model = x)
      xreg_pred <- matrix(as.numeric(newreg[i, ]), horizon)
      prediction[i, ] <- predict(x, newxreg=xreg_pred, h = horizon)$pred
    }
  }

  as.data.frame(prediction)
}

#' @rdname predict_model
#' @export
predict_model.forecast_ARIMA <- function(x, newdata, newreg, horizon, ...) {
  predict_model.Arima(x, newdata, newreg, horizon, ...)
}


#' @rdname get_model_specs
#' @export
get_model_specs.Arima <- function(x) {
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


#' @rdname get_model_specs
#' @export
get_model_specs.forecast_ARIMA <- function(x) {
  get_model_specs.Arima(x)
  }


#' @rdname model_checker
#' @export
model_checker.Arima <- function(x) {
  NULL
}

#' @rdname model_checker
#' @export
model_checker.forecast_ARIMA <- function(x) {
  NULL
}

