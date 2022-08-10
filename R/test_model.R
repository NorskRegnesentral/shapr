
#' Model testing function
#'
#' @inheritParams default_doc
#' @keywords internal
check_model <- function(internal, model) {

  is_python <- internal$parameters$is_python
  x_train <- internal$data$x_train
  predict_model <- internal$funcs$predict_model

  if(is_python){
    stop("`is_python=TRUE` is only allowed in the Python wrapper of the shapr package.")
  }

  if(is.null(model)){
    stop("Argument `model` is missing.")
  }

  x <- head(x_train, 2)
  tmp <- tryCatch(predict_model(model, x),error = errorfun)
  if(class(tmp)[1]=="error"){
    stop(paste0("The predict_model function of class `", class(model), "` is invalid.\n",
                "See the 'Advanced usage' section of the vignette:\n",
                "vignette('understanding_shapr', package = 'shapr')\n",
                "for more information on running shapr with custom models.\n",
                "A basic function test threw the following error:\n",as.character(tmp[[1]])))
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
}
