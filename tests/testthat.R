# CRAN OMP THREAD LIMIT
Sys.setenv("OMP_THREAD_LIMIT" = 1)

library(testthat)
library(shapr)

test_check("shapr")
