.onLoad <- function(libname = find.package("shapr"), pkgname = "shapr"){

    # CRAN Note avoidance
    if (getRversion() >= "2.15.1")
        utils::globalVariables(

            c("ID", "N", "W", "comb", "keep", "m", "mphat", "num_var", "phat",
              "test", "testData", "trainData", "w",

              ".", ".N", ".I", ".GRP", ".SD")
        )
    invisible()
}
