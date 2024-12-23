# This R file is based on https://www.kloppenborg.ca/2021/06/long-running-vignettes/ and
# https://ropensci.org/blog/2019/12/08/precompute-vignettes/ to allow for the `vaeac` vignette
# to be pre-built such that is not run every time we check and/or build the package.

old_wd <- getwd()

setwd("vignettes/")

knitr::knit("general_usage.Rmd.orig", output = "general_usage.Rmd")
# knitr::purl("general_usage.Rmd.orig", output = "general_usage.R") # Don't need this

knitr::knit("vaeac.Rmd.orig", output = "vaeac.Rmd")
# knitr::purl("vaeac.Rmd.orig", output = "vaeac.R") # Don't need this

knitr::knit("regression.Rmd.orig", output = "regression.Rmd")
# knitr::purl("regression.Rmd.orig", output = "regression.R") # Don't need this

knitr::knit("asymmetric_causal.Rmd.orig", output = "asymmetric_causal.Rmd")
# knitr::purl("asymmetric_causal.Rmd.orig", output = "asymmetric_causal.R")

setwd(old_wd)
