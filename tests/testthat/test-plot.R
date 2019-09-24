library(shapr)

context("test-plot.R")

test_that("Test plot.shapr", {

  # Example -----------
  x <- matrix(c(4.98, 9.14, 4.03, 2.94, 5.33,
                6.575, 6.421, 7.185, 6.998, 7.147,
                4.0900, 4.9671, 4.9671, 6.0622, 6.0622,
                2.31, 7.07, 7.07, 2.18, 2.18),
              ncol = 4
  )

  colnames(x) <- c("lstat", "rm", "dis", "indus")

  explanation <- list()
  explanation$p <- c(31.30145, 23.25194, 33.11547, 33.43015, 31.72984)
  explanation$dt <- data.table::data.table(
    "none" = rep(22.00, 5),
    "lstat" = c(5.2632, 0.1672, 5.9888, 8.2142, 0.5060),
    "rm" = c(-1.2527, -0.7088, 5.5451, 0.7508, 5.6875),
    "dis" = c(0.2920, 0.9689, 0.5660, 0.1893, 0.8432),
    "indus" = c(4.5529, 0.3787, -1.4304, 1.8298, 2.2471)
  )
  explanation$x_test <- x
  attr(explanation, "class") <- c("shapr", "list")


  # Test -----------
  p <- plot(explanation, plot_phi0 = FALSE)

  expect_equal(colnames(x), unique(as.character(p$data$variable)))
  expect_equal(explanation$p, unique(p$data$pred))
  expect_equal(sort(as.vector(as.matrix(explanation$dt[, -c("none")]))), sort(p$data$phi))

  p <- plot(explanation, plot_phi0 = TRUE)

  expect_equal(colnames(explanation$dt), unique(as.character(p$data$variable)))
  expect_equal(explanation$p, unique(p$data$pred))
  expect_equal(sort(as.vector(as.matrix(explanation$dt))), sort(p$data$phi))

  p <- plot(explanation, plot_phi0 = TRUE, top_k_features = 2)

  expect_equal(2, max(p$data$rank))
})
