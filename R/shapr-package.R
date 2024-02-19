#' @importFrom data.table data.table as.data.table is.data.table := setnames setkey copy setcolorder rbindlist
#'
#' @importFrom graphics hist
#'
#' @importFrom utils head tail methods modifyList
#'
#' @importFrom stats predict as.formula formula setNames embed sd qt pt rnorm
#'
#' @importFrom Rcpp sourceCpp
#'
#' @keywords internal
#'
#' @useDynLib shapr, .registration = TRUE
NULL

#' @keywords internal
"_PACKAGE"

#' Auxiliary function for the vaeac vignette
#' @description Function that question if the main and vaeac vignette has been built using the
#' `rebuild-long-running-vignette.R` function. This is only useful when using devtools to release
#' `shapr` to cran. See [devtools::release()] for more information.
release_questions <- function() {
  c(paste0(
    "Did you re-build the `understanding_shapr.Rmd` and `understanding_shapr_vaeac.Rmd` vignettes using ",
    "`rebuild-long-running-vignette.R`?"
  ))
}


