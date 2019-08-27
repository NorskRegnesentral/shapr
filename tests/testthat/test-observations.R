library(shapr)
library(testthat)
library(MASS)
library(mvtnorm)
library(data.table)

context("test-observations.R")

test_that("Test observation_impute", {
  # Example 1 -----------------
  n=60
  m=100
  Sigma = cov(matrix(mvrnorm(m*n,0,1),nrow=n))
  Xtrain = mvrnorm(n,mu=rep(0,m),Sigma=Sigma)
  Xtest=mvrnorm(1,mu=rep(0,m),S)
  S= t(combn())
  res1.1 = shapr:::observation_impute(W_kernel, S, Xtrain, Xtest, w_threshold = .7, noSamp_MC = 1e3)
  res1.2 = observation_impute(W_kernel, S, Xtrain, Xtest, w_threshold = .7)


  # Example 2 ------------------

  res2.1 = observation_impute(W_kernel, S, Xtrain, Xtest, w_threshold = .7, noSamp_MC = 1e3)
  res2.2 = observation_impute(W_kernel, S, Xtrain, Xtest,noSamp_MC = 1e3)

  # Example 3 ------------------

  # Tests ----------------------
  expect_equal(res1.1, res1.2)
  expect_equal(res2.1, res2.2)
  expect_error(observation_impute(W_kernel=1, S, Xtrain, Xtest))

})
