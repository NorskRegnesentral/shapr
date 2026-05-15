if (requireNamespace("testthat", quietly = TRUE)) {
  testthat::set_max_fails(Inf)
}

#' Helper function for package development
#'
#' This is a manual extension of [testthat::snapshot_review()] which works for the \code{.rds} files used in
#' this package.
#'
#' @param path Character
#' @param ... Additional arguments passed to [waldo::compare()]
#' Gives the relative path to the test files to review
#'
snapshot_review_man <- function(path, tolerance = 10^(-5), max_diffs = 200, ...) {
  if (requireNamespace("testthat", quietly = TRUE) && requireNamespace("waldo", quietly = TRUE)) {
    changed <- testthat:::snapshot_meta(path)
    these_rds <- (tools::file_ext(changed$name) == "rds")
    if (any(these_rds)) {
      for (i in which(these_rds)) {
        old <- readRDS(changed[i, "cur"])
        new <- readRDS(changed[i, "new"])

        cat(paste0("Difference for check ", changed[i, "name"], " in test ", changed[i, "test"], "\n"))
        print(waldo::compare(old, new, max_diffs = max_diffs, tolerance = tolerance, ...))
        browser()
      }
    }
  }
}

# Bootstrap the VS Code R extension session watcher for radian terminals.
local({
  if (!interactive() || Sys.getenv("TERM_PROGRAM") != "vscode" || Sys.getenv("RSTUDIO") != "") {
    return(invisible())
  }

  vscode_home <- Sys.getenv(if (.Platform$OS.type == "windows") "USERPROFILE" else "HOME")
  init_r <- file.path(vscode_home, ".vscode-R", "init.R")
  if (!file.exists(init_r)) {
    return(invisible())
  }

  source(init_r)

  first_sys <- get0(".First.sys", envir = globalenv(), inherits = FALSE, ifnotfound = NULL)
  if (is.function(first_sys) && !"tools:vscode" %in% search()) {
    tryCatch(
      first_sys(),
      error = function(e) {
        message("VS Code R session watcher failed to initialize: ", conditionMessage(e))
      }
    )
  }
})
