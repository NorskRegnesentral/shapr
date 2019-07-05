#' Scale training and test data
#'
#' @inheritParams global_arguments
#'
#' @return List
#'
#' @export
#'
#' @author Nikolai Sellereite
scale_data <- function(Xtrain, Xtest, scale = TRUE) {
  if (!is.data.table(Xtrain)) {
    Xtrain <- as.data.table(Xtrain)
  }
  if (!is.data.table(Xtest)) {
    Xtest <- as.data.table(Xtest)
  }

  if (scale) {
    nms <- colnames(Xtrain)
    setcolorder(Xtest, nms)
    sd <- Xtrain[, unname(sapply(.SD, sd, na.rm = TRUE))]
    Xtrain[, (nms) := .SD / sd]
    Xtest[, (nms) := .SD / sd]
  }

  return(list(Xtrain = Xtrain, Xtest = Xtest))
}

