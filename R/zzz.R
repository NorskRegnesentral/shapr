.onLoad <- function(libname = find.package("shapr"), pkgname = "shapr") {

  # CRAN Note avoidance
  utils::globalVariables(
    c(
      "ID", "N", "W", "features", "keep", "m", "mphat", "nfeatures", "phat",
      "test", "Xtest", "Xtrain", "weight", "test_id", "sample_id",
      "comb", "p_hat", "w", "wcomb", "wcum", "id", "k",

      ".", ".N", ".I", ".GRP", ".SD"
    )
  )
  invisible()
}
