library(shapr)

context("test-plot.R")

test_that("Test plot.shapr", {

  # Example -----------
  x <- matrix(c(
    4.98, 9.14, 4.03, 2.94, 5.33,
    6.575, 6.421, 7.185, 6.998, 7.147,
    4.0900, 4.9671, 4.9671, 6.0622, 6.0622,
    2.31, 7.07, 7.07, 2.18, 2.18
  ),
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


  # With groups --------
  x_var <- c("lstat", "rm", "dis",
             "indus", "nox",
             "tax")
  group1 <- list(c(1, 2, 3),
                 c(4, 5),
                 c(6))

  group2 <- list(c(1),
                 c(2),
                 c(3),
                 c(4),
                 c(5),
                 c(6))

  group1_names <- lapply(group1, function(x) {
    x_var[x]
    })
  group2_names <- lapply(group2, function(x) {
    x_var[x]
    })


  ## Example 1 -------
  explanation0 <- list()
  explanation0$dt <- data.table::data.table("none" = rep(22.446, 3),
                                            "group1" = c(3.14, -1.022, 9.66),
                                            "group2" = c(3.817, -0.208, -0.424),
                                            "group3" = c(-0.263, 1.866, 1.7901))


  explanation0$p <- c(29.14010, 23.08229, 33.47549)

  explanation0$x_test <- data.table::data.table("lstat" = c(4.98, 9.14, 4.03),
                                                "rm" = c(6.575, 6.421, 7.185),
                                                "dis" = c(4.090, 4.967, 4.967),
                                                "indus" = c(2.31, 7.07, 7.07),
                                                "nox" = c(0.538, 0.469, 0.469),
                                                "tax" = c(296, 242, 242))
  attr(explanation0, "class") <- c("shapr", "list")
  plot0 <- plot(explanation0)

  ## Example 2 -------
  explanation1 <- list()
  explanation1$dt <- data.table::data.table("none" = rep(22.446, 3),
                                            "group1" = c(6.4273167, -0.2043953, 5.2757339),
                                            "group2" = c(-1.7740620, -0.8058347, 6.2492723),
                                            "group3" = c(-0.6324422, 0.3347352, 0.2075207),
                                            "group4" = c(4.6871282, -0.4634629, -1.3892780),
                                            "group5" = c(-1.5839274, 0.7739448, 0.1856827),
                                            "group6" = c(-0.4299092, 1.0013031, 0.5005543))


  explanation1$p <- c(29.14010, 23.08229, 33.47549)

  explanation1$x_test <- data.table::data.table("lstat" = c(4.98, 9.14, 4.03),
                                                "rm" = c(6.575, 6.421, 7.185),
                                                "dis" = c(4.090, 4.967, 4.967),
                                                "indus" = c(2.31, 7.07, 7.07),
                                                "nox" = c(0.538, 0.469, 0.469),
                                                "tax" = c(296, 242, 242))

  attr(explanation1, "class") <- c("shapr", "list")
  plot1 <- plot(explanation1)

  ## Test
  expect_equal(length(group1_names), length(unique(as.character(plot0$data$variable))) - 1)
  expect_equal(length(group2_names), length(unique(as.character(plot1$data$variable))) - 1)

  expect_equal(explanation0$p, unique(plot0$data$pred))
  expect_equal(explanation1$p, unique(plot1$data$pred))

  expect_equal(sort(as.vector(as.matrix(explanation0$dt))), sort(plot0$data$phi))
  expect_equal(sort(as.vector(as.matrix(explanation1$dt))), sort(plot1$data$phi))

})
