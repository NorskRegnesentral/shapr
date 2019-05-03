.onLoad <- function(libname = find.package("shapr"), pkgname = "shapr") {

  # CRAN Note avoidance
  if (getRversion() >= "2.15.1") {
    utils::globalVariables(
      c(
        "id", "n", "w_mat", "features", "keep", "m", "mphat", "nfeatures", "phat",
        "test", "xtest", "xtrain", "weight", "test_id", "sample_id",
        "comb", "p_hat", "w", "wcomb", "wcum", "id", "k",

        ".", ".N", ".I", ".GRP", ".SD"
      )
    )
  }
  invisible()
}
