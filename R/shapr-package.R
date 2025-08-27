#' @importFrom data.table data.table fread fwrite setnames := year month
#' uniqueN setkey as.data.table copy between is.data.table setcolorder rbindlist
#'
#' @importFrom graphics plot hist rect
#'
#' @importFrom utils head tail methods modifyList
#'
#' @importFrom stats predict as.formula formula model.matrix model.frame setNames
#' contrasts embed sd qt pt rnorm median
#'
#' @importFrom Rcpp sourceCpp
#'
#' @importFrom utils capture.output relist
#'
#' @keywords internal
#'
#' @useDynLib shapr, .registration = TRUE
NULL

#' @keywords internal
"_PACKAGE"

#' Auxiliary function for the vignettes
#' @description Ask whether the vignettes have been built using the
#' `rebuild-long-running-vignette.R` script.
#' This is only useful when using devtools to release `shapr` to CRAN.
#' See [devtools::release()] for more information.
#' @keywords internal
release_questions <- function() {
  c(paste0(
    "Did you rebuild the vignettes using `rebuild-long-running-vignette.R`?"
  ))
}
