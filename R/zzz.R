.onLoad <- function(libname = find.package("shapr"), pkgname = "shapr") {

  # CRAN Note avoidance
  utils::globalVariables(
    c(
      ".", ".N", ".I", ".GRP", ".SD"
    )
  )
  invisible()
}
