
#' Model testing function
#'
#' @inheritParams default_doc
#' @keywords internal
check_model <- function(internal, model) {

  ignore_model <- internal$parameters$ignore_model
  x_train <- internal$data$x_train
  predict_model <- internal$funcs$predict_model

  if(ignore_model){
    stop("`ignore_model=TRUE` is only allowed in the Python wrapper of the shapr package.")
  }

    if(is.null(model)){
    stop("Argument `model` is missing.")
  }

  x <- head(x_train, 2)
  tmp <- predict_model(model, x)
  if (!(all(is.numeric(tmp)) &&
        length(tmp) == 2)) {
    stop(
      paste0(
        "The predict_model function of class ", class(model), " is invalid.\n",
        "See the 'Advanced usage' section of the vignette:\n",
        "vignette('understanding_shapr', package = 'shapr')\n",
        "for more information on running shapr with custom models.\n"
      )
    )
  }
}
