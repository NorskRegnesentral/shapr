#' @export
test_model <- function(internal,model){
  x <- head(internal$data$x_train, 2)
  tmp <- internal$funcs$predict_model(model, x)
  if (!(all(is.numeric(tmp)) & length(tmp) == 2)) {
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
