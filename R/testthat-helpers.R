#' @keywords internal
compare_rds <- function(old, new) {
  old <- readRDS(old)
  new <- readRDS(new)

  check <- all.equal(old, new, tolerance = 10^(-5)) # Increase tolerance
  ifelse(is.character(check), FALSE, check)
}

#' @keywords internal
expect_snapshot_rds <- function(code, name = "tmp", digits = 4) {
  name_full <- paste0(name, ".rds")
  path <- file.path(tempdir(), name_full)

  testthat::announce_snapshot_file(path = path)


  testthat::expect_snapshot(print(
    {
      out <- code
    },
    digits = digits
  )) # Test output + warnings/messages

  saveRDS(out, file = path)

  testthat::expect_snapshot_file(path, compare = compare_rds) # Test the returned object
}
