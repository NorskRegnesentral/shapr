# inst/devel/snapshot_diff.R
#
# Terminal-based snapshot diff tool.
#
# Finds all *.new.rds and *.new.md files produced by failing testthat snapshot
# tests and prints a human-readable diff for each using waldo::compare().
#
# Usage (from repo root):
#   Rscript inst/devel/snapshot_diff.R            # show diffs only
#   Rscript inst/devel/snapshot_diff.R --accept   # show diffs, then accept all
#
# Alternatively, run via the VS Code task "R: Diff failing snapshots (terminal)".

snap_dir <- file.path("tests", "testthat", "_snaps")
args <- commandArgs(trailingOnly = TRUE)
accept_all <- "--accept" %in% args

# ---- Collect failing snapshots -----------------------------------------------

new_rds <- list.files(snap_dir, pattern = "\\.new\\.rds$", recursive = TRUE, full.names = TRUE)
new_md  <- list.files(snap_dir, pattern = "\\.new\\.md$",  recursive = TRUE, full.names = TRUE)

if (length(new_rds) == 0 && length(new_md) == 0) {
  cat("No failing snapshots found in", snap_dir, "\n")
  quit(status = 0)
}

cat(sprintf("Found %d failing snapshot(s).\n\n", length(new_rds) + length(new_md)))

# ---- Helper ------------------------------------------------------------------

divider <- function(label) {
  cat(strrep("-", 72), "\n")
  cat(sprintf("  %s\n", label))
  cat(strrep("-", 72), "\n")
}

# ---- RDS diffs ---------------------------------------------------------------

for (new_path in new_rds) {
  old_path <- sub("\\.new\\.rds$", ".rds", new_path)
  rel_new  <- sub(paste0("^", snap_dir, "/"), "", new_path)

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

# ---- MD diffs ----------------------------------------------------------------

for (new_path in new_md) {
  old_path <- sub("\\.new\\.md$", ".md", new_path)
  rel_new  <- sub(paste0("^", snap_dir, "/"), "", new_path)

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

# ---- Accept ------------------------------------------------------------------

if (accept_all) {
  cat(strrep("=", 72), "\n")
  cat("Accepting all new snapshots...\n")
  testthat::snapshot_accept(path = "tests/testthat")
  cat("Done.\n")
} else {
  cat(strrep("=", 72), "\n")
  cat("To accept all new snapshots, re-run with --accept:\n")
  cat("  Rscript inst/devel/snapshot_diff.R --accept\n")
  cat("Or use the VS Code task: 'R: Accept all new snapshots'\n")
  cat("Or interactively: testthat::snapshot_review('tests/testthat')\n")
}
