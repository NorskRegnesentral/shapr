library(shapr)

context("test-shaley.R")

test_that("Test functions in shapley.R", {

    ## Example -----------
    x <- 1

    ## Test results -----------
    expect_equal(x - 1, 0)
    expect_error(x - "a")
})
