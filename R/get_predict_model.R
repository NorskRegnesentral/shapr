#' Get predict_model function
#'
#' @inheritParams default_doc_internal
#' @keywords internal
get_predict_model <- function(predict_model, model) {
  # Checks that predict_model is a proper function (R + py)
  # Extracts natively supported functions for predict_model if exists and not passed (R only)
  # Checks that predict_model provide the right output format (R and py)
  # Returns the predict_model to use subsequently (R only)

  model_class0 <- class(model)[1]

  # checks predict_model
  if (!(is.function(predict_model)) &&
    !(is.null(predict_model))) {
    cli::cli_abort("`predict_model` must be NULL or a function.")
  }

  supported_models <- get_supported_models()

  # Get native predict_model if not passed and exists
  if (is.null(predict_model)) {
    native_func_available <- supported_models[predict_model == TRUE, model_class0 %in% model_class]
    if (native_func_available) {
      predict_model <- get(paste0("predict_model.", model_class0))
    } else {
      cli::cli_abort(
        paste0(
          "You passed a model to {.fn shapr::explain} which is not natively supported, ",
          "and did not supply the 'predict_model' function to {.fn shapr::explain}. ",
          "See the documentation of {.fn shapr::explain} or the ",
          "{.vignette shapr::general_usage} vignette for more information on how to run shapr with custom models."
        )
      )
    }
  }
  return(predict_model)
}

#' Model testing function
#'
#' @inheritParams default_doc_internal
#' @keywords internal
test_predict_model <- function(x_test, predict_model, model, internal) {
  # Tests prediction with some data
  if (!is.null(internal$parameters$type) && internal$parameters$type == "forecast") {
    tmp <- tryCatch(predict_model(
      x = model,
      newdata = x_test[, .SD, .SDcols = seq_len(internal$data$n_endo), drop = FALSE],
      newreg = x_test[, .SD,
        .SDcols = seq_len(ncol(x_test) - internal$data$n_endo) + internal$data$n_endo,
        drop = FALSE
      ],
      horizon = internal$parameters$horizon,
      explain_idx = rep(internal$parameters$explain_idx[1], 2),
      y = internal$data$y,
      xreg = internal$data$xreg,
      explain_lags = internal$parameters$explain_lags,
    ), error = errorfun)
  } else {
    tmp <- tryCatch(predict_model(model, x_test), error = errorfun)
  }
  if (class(tmp)[1] == "error") {
    cli::cli_abort(paste0(
      "The predict_model function of class `", class(model), "` is invalid. ",
      "See the 'Advanced usage' section of ",
      "{.vignette shapr::general_usage} vignette ",
      "for more information on running shapr with custom models. ",
      "A basic function test threw the following error: ", as.character(tmp[[1]])
    ))
  }


  if (!((all(sapply(tmp, is.numeric))) &&
    (length(tmp) == 2 || (!is.null(dim(tmp)) && nrow(tmp) == 2 && ncol(tmp) == internal$parameters$output_size)))) {
    cli::cli_abort(
      paste0(
        "The predict_model function of class `", class(model),
        "` does not return a numeric output of the desired length ",
        "for single output models or a data.table of the correct ",
        "dimensions for a multiple output model. ",
        "See the 'Advanced usage' section of ",
        "{.vignette shapr::general_usage} vignette ",
        "for more information on running shapr with custom models."
      )
    )
  }
}
