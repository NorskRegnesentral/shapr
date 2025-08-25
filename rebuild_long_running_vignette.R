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


# Additional processing of images to reduce size using WebP format

library(webp)

vignettes <- c("general_usage", "vaeac", "regression", "asymmetric_causal")
for (v in vignettes){
  ### Converting png files to WebP
  pngs <- list.files(paste0("figure_",v), "\\.png$", full.names = TRUE)
  for (f in pngs) {
    # read PNG into array
    arr <- png::readPNG(f)

    # output filename with .webp extension
    out <- sub("\\.png$", ".webp", f)

    # write WebP with desired quality
    write_webp(arr, out, quality = 80)

    # remove the original PNG
    unlink(f)
  }

  ### Replace .png with .webp in the Rmd file
  rmd <- readLines(paste0(v,".Rmd"))
  rmd <- gsub("\\.png", ".webp", rmd)
  writeLines(rmd, paste0(v,".Rmd"))

}







setwd(old_wd)
