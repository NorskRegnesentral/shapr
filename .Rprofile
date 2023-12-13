#' Helper function for package development
#'
#' This is a manual extension of [testthat::snapshot_review()] which works for the \code{.rds} files used in
#' this package.
#'
#' @param path Character
#' @param ... Additional arguments passed to [waldo::compare()]
#' Gives the relative path to the test files to review
#'
snapshot_review_man <- function(path, tolerance = NULL, ...) {
  changed <- testthat:::snapshot_meta(path)
  these_rds <- (tools::file_ext(changed$name) == "rds")
  if (any(these_rds)) {
    for (i in which(these_rds)) {
      old <- readRDS(changed[i, "cur"])
      new <- readRDS(changed[i, "new"])

      cat(paste0("Difference for check ", changed[i, "name"], " in test ", changed[i, "test"], "\n"))
      print(waldo::compare(old, new, max_diffs = 50, tolerance = tolerance, ...))
      browser()
    }
  }
}
