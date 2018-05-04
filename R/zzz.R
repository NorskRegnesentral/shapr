.onLoad <- function(libname = find.package("shapr"), pkgname = "shapr"){

    # CRAN Note avoidance
    if (getRversion() >= "2.15.1")
        utils::globalVariables(

            c("ID", "N", "W", "features", "keep", "m", "mphat", "nfeatures", "phat",
              "test", "Xtest", "Xtrain", "weight", "test_id", "sample_id",

              ".", ".N", ".I", ".GRP", ".SD")
        )
    invisible()
}
