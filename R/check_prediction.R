
#' Model testing function
#'
#' @inheritParams default_doc
#' @keywords internal
check_prediction_R <- function(x_test, predict_model, model) {

  tmp <- tryCatch(predict_model(model, x_test),error = errorfun)
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
