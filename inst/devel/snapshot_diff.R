# inst/devel/snapshot_diff.R
#
# Developer helper for snapshot testing.
#
# Usage (from repo root):
#   Rscript inst/devel/snapshot_diff.R
#   Rscript inst/devel/snapshot_diff.R --md
#   Rscript inst/devel/snapshot_diff.R --rds
#   Rscript inst/devel/snapshot_diff.R --review-md
#   Rscript inst/devel/snapshot_diff.R --review-rds
#   Rscript inst/devel/snapshot_diff.R --run-tests
#   Rscript inst/devel/snapshot_diff.R --run-tests --file tests/testthat/test-forecast-output.R
#   Rscript inst/devel/snapshot_diff.R --run-tests --review-md --review-rds
#   Rscript inst/devel/snapshot_diff.R --accept

test_path <- file.path("tests", "testthat")
snap_dir <- file.path(test_path, "_snaps")

args <- commandArgs(trailingOnly = TRUE)

run_tests <- "--run-tests" %in% args
review_md <- "--review-md" %in% args
review_rds <- "--review-rds" %in% args
accept_snapshots <- "--accept" %in% args
only_md <- "--md" %in% args
only_rds <- "--rds" %in% args

file_arg_index <- match("--file", args)
test_files <- character()

if (!is.na(file_arg_index)) {
  if (file_arg_index == length(args)) {
    stop("`--file` must be followed by a test file path.", call. = FALSE)
  }
  test_files <- args[file_arg_index + 1]
}

if (length(test_files) == 0) {
  test_files <- list.files(
    path = test_path,
    pattern = "^test-.*\\.R$",
    full.names = TRUE,
    recursive = TRUE
  )
}

divider <- function(label) {
  cat(strrep("-", 72), "\n")
  cat(sprintf("  %s\n", label))
  cat(strrep("-", 72), "\n")
}

summarise_test_results <- function(results) {
  rows <- do.call(rbind, lapply(results, as.data.frame))

  if (is.null(rows) || nrow(rows) == 0) {
    rows <- data.frame(
      failed = integer(),
      error = logical(),
      warning = integer(),
      skipped = logical(),
      passed = integer()
    )
  }

  summary <- list(
    failures = sum(rows$failed, na.rm = TRUE),
    errors = sum(rows$error, na.rm = TRUE),
    warnings = sum(rows$warning, na.rm = TRUE),
    skipped = sum(rows$skipped, na.rm = TRUE),
    passed = sum(rows$passed, na.rm = TRUE)
  )

  return(summary)
}

print_test_summary <- function(summary) {
  cat(strrep("=", 72), "\n")
  cat("Test summary\n")
  cat(strrep("=", 72), "\n")
  cat(sprintf("Passed:   %d\n", summary$passed))
  cat(sprintf("Skipped:  %d\n", summary$skipped))
  cat(sprintf("Warnings: %d\n", summary$warnings))
  cat(sprintf("Failures: %d\n", summary$failures))
  cat(sprintf("Errors:   %d\n", summary$errors))
}

print_snapshot_summary <- function(new_snapshots) {
  cat(strrep("=", 72), "\n")
  cat("Snapshot summary\n")
  cat(strrep("=", 72), "\n")
  cat(sprintf("Changed .md snapshots:  %d\n", length(new_snapshots$md)))
  cat(sprintf("Changed .rds snapshots: %d\n", length(new_snapshots$rds)))
}

run_test_file <- function(file) {
  result <- tryCatch(
    testthat::test_file(file, package = "shapr"),
    error = function(error) {
      cat(sprintf("Error while running %s:\n%s\n", file, conditionMessage(error)))
      data.frame(
        file = basename(file),
        context = NA_character_,
        test = "file-level error",
        nb = 0,
        failed = 0,
        skipped = FALSE,
        error = TRUE,
        warning = 0,
        passed = 0
      )
    }
  )

  return(result)
}

collect_new_snapshots <- function() {
  list(
    rds = list.files(snap_dir, pattern = "\\.new\\.rds$", recursive = TRUE, full.names = TRUE),
    md = list.files(snap_dir, pattern = "\\.new\\.md$", recursive = TRUE, full.names = TRUE)
  )
}

snapshot_name <- function(new_path) {
  current_path <- sub("\\.new\\.([^.]+)$", ".\\1", new_path)
  relative_path <- sub(paste0("^", snap_dir, "/"), "", current_path)

  if (dirname(relative_path) == ".") {
    return(basename(relative_path))
  }

  return(relative_path)
}

print_rds_diffs <- function(new_rds) {
  for (new_path in new_rds) {
    old_path <- sub("\\.new\\.rds$", ".rds", new_path)
    rel_new <- sub(paste0("^", snap_dir, "/"), "", new_path)

    divider(rel_new)

    if (!file.exists(old_path)) {
      cat("  NEW snapshot (no previous reference).\n\n")
      next
    }

    old_obj <- readRDS(old_path)
    new_obj <- readRDS(new_path)

    diff <- waldo::compare(old_obj, new_obj, x_arg = "old", y_arg = "new")

    if (length(diff) == 0) {
      cat("  Objects are identical (comparison tolerance may differ from testthat's).\n\n")
    } else {
      print(diff)
      cat("\n")
    }
  }
}

print_md_diffs <- function(new_md) {
  for (new_path in new_md) {
    old_path <- sub("\\.new\\.md$", ".md", new_path)
    rel_new <- sub(paste0("^", snap_dir, "/"), "", new_path)

    divider(rel_new)

    if (!file.exists(old_path)) {
      cat("  NEW snapshot (no previous reference).\n\n")
      cat(readLines(new_path, warn = FALSE), sep = "\n")
      cat("\n")
      next
    }

    old_lines <- readLines(old_path, warn = FALSE)
    new_lines <- readLines(new_path, warn = FALSE)

    diff <- waldo::compare(old_lines, new_lines, x_arg = "old", y_arg = "new")

    if (length(diff) == 0) {
      cat("  Files are identical.\n\n")
    } else {
      print(diff)
      cat("\n")
    }
  }
}

review_md_snapshots <- function(new_md) {
  if (length(new_md) == 0) {
    cat("No failing .md snapshots found in", snap_dir, "\n")
    return(invisible())
  }

  testthat::snapshot_review(files = vapply(new_md, snapshot_name, character(1)), path = test_path)
  return(invisible())
}

review_rds_snapshots <- function(new_rds, tolerance = 10^(-5), max_diffs = 200) {
  if (length(new_rds) == 0) {
    cat("No failing .rds snapshots found in", snap_dir, "\n")
    return(invisible())
  }

  for (new_path in new_rds) {
    old_path <- sub("\\.new\\.rds$", ".rds", new_path)
    rel_new <- sub(paste0("^", snap_dir, "/"), "", new_path)

    divider(rel_new)

    if (!file.exists(old_path)) {
      cat("  NEW snapshot (no previous reference).\n\n")
      next
    }

    old <- readRDS(old_path)
    new <- readRDS(new_path)

    print(waldo::compare(old, new, max_diffs = max_diffs, tolerance = tolerance))
    cat("\nPress Enter to continue to the next .rds snapshot...")
    invisible(readline())
  }

  return(invisible())
}

accept_selected_snapshots <- function(new_rds, new_md) {
  files <- c(
    vapply(new_rds, snapshot_name, character(1)),
    vapply(new_md, snapshot_name, character(1))
  )

  if (length(files) == 0) {
    cat("No failing snapshots found in", snap_dir, "\n")
    return(invisible())
  }

  testthat::snapshot_accept(files = files, path = test_path)
  return(invisible())
}

if (run_tests) {
  Sys.setenv(NOT_CRAN = "true")

  library(testthat)
  library(shapr)

  test_results <- list()

  for (file in test_files) {
    print(paste0("Running snapshots for: ", file))
    Sys.sleep(1)
    test_results[[file]] <- run_test_file(file)
  }

  test_summary <- summarise_test_results(test_results)
  print_test_summary(test_summary)

  tests_failed <- test_summary$failures > 0 || test_summary$errors > 0
} else {
  tests_failed <- FALSE
}

new_snapshots <- collect_new_snapshots()

new_rds <- if (only_md) character() else new_snapshots$rds
new_md <- if (only_rds) character() else new_snapshots$md

if (tests_failed && (review_md || review_rds)) {
  cat("Skipping snapshot review because the test run had failures or errors.\n")
  cat("Fix the failing tests first, then re-run the review task.\n")
} else if (review_md) {
  review_md_snapshots(new_md)
}

if (!tests_failed && review_rds) {
  review_rds_snapshots(new_rds)
}

final_snapshots <- collect_new_snapshots()

if (run_tests || review_md || review_rds) {
  print_snapshot_summary(final_snapshots)
}

if (accept_snapshots) {
  accept_selected_snapshots(new_rds, new_md)
  final_snapshots <- collect_new_snapshots()
}

show_diff <- !run_tests && !review_md && !review_rds && !accept_snapshots

if (show_diff) {
  if (length(new_rds) == 0 && length(new_md) == 0) {
    cat("No failing snapshots found in", snap_dir, "\n")
    quit(status = 0)
  }

  cat(sprintf("Found %d failing snapshot(s).\n\n", length(new_rds) + length(new_md)))
  print_rds_diffs(new_rds)
  print_md_diffs(new_md)

  cat(strrep("=", 72), "\n")
  cat("To accept selected new snapshots, re-run with:\n")
  cat("  Rscript inst/devel/snapshot_diff.R --accept\n")
  cat("  Rscript inst/devel/snapshot_diff.R --rds --accept\n")
  cat("  Rscript inst/devel/snapshot_diff.R --md --accept\n")
}

unresolved_snapshots <- length(final_snapshots$md) > 0 || length(final_snapshots$rds) > 0

if (exists("tests_failed") && tests_failed) {
  quit(status = 1)
}

if ((run_tests || review_md || review_rds) && unresolved_snapshots) {
  quit(status = 1)
}
