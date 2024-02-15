#' @importFrom data.table data.table fread fwrite setnames := year month
#' uniqueN setkey as.data.table copy between is.data.table setcolorder rbindlist
#'
#' @importFrom graphics plot hist rect
#'
#' @importFrom utils head tail methods modifyList
#'
#' @importFrom stats predict
#'
#' @importFrom stats as.formula
#'
#' @importFrom stats formula
#'
#' @importFrom stats model.matrix
#'
#' @importFrom stats model.frame
#'
#' @importFrom stats setNames
#'
#' @importFrom stats contrasts
#'
#' @importFrom stats embed
#'
#' @importFrom stats sd qt pt
#'
#' @importFrom stats rnorm
#'
#' @importFrom Rcpp sourceCpp
#'
#' @importFrom methods formalArgs
#'
#' @keywords internal
#'
#' @useDynLib shapr, .registration = TRUE
NULL

#' @keywords internal
"_PACKAGE"


#' Auxiliary function for the vaeac vignette
#' @description Function that question if the vaeac vignette has been built using `rebuild-long-running-vignette.R`.
#' This is only useful when using devtools to release packages to cran. See [devtools::release()] for more information.
release_questions <- function() {
  c("Did you re-build the `understanding_shapr_vaeac.Rmd` using `rebuild-long-running-vignette.R`?")
}
