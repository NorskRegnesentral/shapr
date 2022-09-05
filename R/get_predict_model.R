
#' Model testing function
#'
#' @inheritParams default_doc
#' @keywords internal
get_predict_model <- function(x_test, predict_model, model) {

  # Checks that predict_model is a proper function (R + py)
  # Extracts natively supported functions for predict_model if exists and not passed (R only)
  # Checks that predict_model provide the right output format (R and py)
  # Returns the predict_model to use subsequently (R only)

  model_class <- NULL # due to NSE

  model_class0 <- class(model)[1]

  # checks predict_model
  if (!(is.function(predict_model)) &&
    !(is.null(predict_model))) {
    stop("`predict_model` must be NULL or a function.")
  }

  supported_models <- get_supported_models()

  # Get native predict_model if not passed and exists
  if (is.null(predict_model)) {
    native_func_available <- supported_models[predict_model == TRUE, model_class0 %in% model_class]
    if (native_func_available) {
      predict_model <- get(paste0("predict_model.", model_class0))
    } else {
      stop(
        "You passed a model to explain() which is not natively supported, and did not supply the 'predict_model' ",
        "function to explain().\n",
        "See ?shapr::explain or the vignette for more information on how to run shapr with custom models."
      )
    }
  }


  # Tests prediction with some data
  tmp <- tryCatch(predict_model(model, x_test), error = errorfun)
  if (class(tmp)[1] == "error") {
    stop(paste0(
      "The predict_model function of class `", class(model), "` is invalid.\n",
      "See the 'Advanced usage' section of the vignette:\n",
      "vignette('understanding_shapr', package = 'shapr')\n",
      "for more information on running shapr with custom models.\n",
      "A basic function test threw the following error:\n", as.character(tmp[[1]])
    ))
  }

  if (!(all(is.numeric(tmp)) &&
    length(tmp) == 2)) {
    stop(
      paste0(
        "The predict_model function of class `", class(model),
        "` does not return a numeric output of the desired length.\n",
        "See the 'Advanced usage' section of the vignette:\n",
        "vignette('understanding_shapr', package = 'shapr')\n\n",
        "for more information on running shapr with custom models.\n"
      )
    )
  }
  return(predict_model)
}
