# This R file is based on https://www.kloppenborg.ca/2021/06/long-running-vignettes/ and
# https://ropensci.org/blog/2019/12/08/precompute-vignettes/ to allow for the `vaeac` vignette
# to be pre-built such that is not run every time we check and/or build the package.

old_wd <- getwd()

setwd("vignettes/")
knitr::knit("understanding_shapr_vaeac.Rmd.orig", output = "understanding_shapr_vaeac.Rmd")
knitr::purl("understanding_shapr_vaeac.Rmd.orig", output = "understanding_shapr_vaeac.R")

knitr::knit("understanding_shapr.Rmd.orig", output = "understanding_shapr.Rmd")
knitr::purl("understanding_shapr.Rmd.orig", output = "understanding_shapr.R")

setwd(old_wd)
