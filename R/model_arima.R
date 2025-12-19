#' @rdname predict_model
#' @inheritParams explain_forecast
#' @export
predict_model.Arima <- function(x, newdata, newreg, horizon, explain_idx, explain_lags, y, xreg, ...) {
  if (!requireNamespace("forecast", quietly = TRUE)) {
    cli::cli_abort("The {.pkg forecast} package is required when explaining Arima models.")
  }

  prediction <- matrix(NA, length(explain_idx), horizon)
  newdata <- as.matrix(newdata, nrow = length(explain_idx))
  newreg <- as.matrix(newreg)
  newdata_y_cols <- seq_len(explain_lags$y)
  newdata_xreg_cols_list <- lapply(paste0("xreg", seq_along(explain_lags$xreg)), function(x) grep(x, colnames(newdata)))


  exp_idx <- -1
  for (i in seq_along(explain_idx)) {
    if (explain_idx[i] != exp_idx) {
      exp_idx <- explain_idx[i]
      y_hist <- y[seq_len(exp_idx)]
      xreg_hist <- xreg[seq_len(exp_idx), , drop = FALSE]
    }

    if (ncol(newdata) > 0) {
      y_new <- as.numeric(newdata[i, newdata_y_cols])
      y_hist[seq.int(length.out = length(y_new), to = length(y_hist))] <- rev(y_new)
    }

    if (ncol(xreg) == 0) {
      x <- forecast::Arima(y = y_hist, model = x)
      prediction[i, ] <- predict(x, h = horizon)$pred
    } else {
      for (j in seq_along(explain_lags$xreg)) {
        if (length(newdata_xreg_cols_list[[j]]) == 0) next
        xreg_new <- as.numeric(newdata[i, newdata_xreg_cols_list[[j]]])
        xreg_hist[seq.int(length.out = length(xreg_new), to = nrow(xreg_hist)), j] <- rev(xreg_new)
      }

      x <- forecast::Arima(y = y_hist, xreg = xreg_hist, model = x)
      xreg_pred <- matrix(newreg[i, ], horizon)
      prediction[i, ] <- predict(x, newxreg = xreg_pred, h = horizon)$pred
    }
  }

  if (horizon == 1) {
    prediction <- matrix(prediction, ncol = 1)
  }
  colnames(prediction) <- paste0("p_hat", seq_len(horizon))
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
