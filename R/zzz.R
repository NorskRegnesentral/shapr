.onLoad <- function(libname = find.package("shapr"), pkgname = "shapr") {

  # CRAN Note avoidance
  if (getRversion() >= "2.15.1") {
    utils::globalVariables(
      c(
        ".", ".N", ".I", ".GRP", ".SD"
      )
    )
  }
  invisible()
}
