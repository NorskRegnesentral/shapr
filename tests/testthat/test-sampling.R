library(shapr)
library(MASS)
library(mvtnorm)

context("test-sample_combinations.R")

test_that("Test sample_combinations", {

  # Example -----------
  ntrain <- 10
  ntest <- 10
  nsamples <- 7
  joint_sampling <- FALSE
  cnms <- c("samp_train", "samp_test")

  x <- sample_combinations(ntrain, ntest, nsamples, joint_sampling)


  # Tests -----------
  expect_true(is.data.frame(x))
  expect_equal(names(x), cnms)
  expect_equal(nrow(x), nsamples)

  # Expect all unique values when nsamples < ntrain
  expect_true(length(unique(x$samp_train)) == nsamples)
  expect_true(length(unique(x$samp_test)) == nsamples)

  expect_true(max(x$samp_train) <= ntrain)
  expect_true(max(x$samp_test) <= ntest)

  # Example -----------
  ntrain <- 5
  ntest <- 5
  nsamples <- 7
  joint_sampling <- FALSE

  x <- sample_combinations(ntrain, ntest, nsamples, joint_sampling)

  # Tests -----------
  expect_true(max(x$samp_train) <= ntrain)
  expect_true(max(x$samp_test) <= ntest)
  expect_equal(nrow(x), nsamples)

  # Example -----------
  ntrain <- 5
  ntest <- 5
  nsamples <- 7
  joint_sampling <- TRUE

  x <- sample_combinations(ntrain, ntest, nsamples, joint_sampling)

  # Tests -----------
  expect_true(max(x$samp_train) <= ntrain)
  expect_true(max(x$samp_test) <= ntest)
  expect_equal(nrow(x), nsamples)


})

test_that("test sample_gaussian",{
  # Example 1 -----------
  # Check that the given features are not resampled, but kept as is.
  m <- 10
  noSamp <- 50
  mu=rep(1,m)
  Sigma=cov(matrix(rnorm(noSamp*m),noSamp,m))
  Xtest=mvrnorm(1,mu,Sigma)
  given.ind = 4
  set.seed(1)
  ret = sample_gaussian(given.ind,noSamp, mu, Sigma, m, Xtest)
  X_given <- Xtest[given.ind]
  res1.1 = as.data.table(matrix(rep(X_given, each = noSamp),byrow=T))
  res1.2 = as.data.table(ret[, ..given.ind])
  colnames(res1.1)=colnames(res1.2)


  # Example 2 -------------
  # Check that conditioning upon all variables simply returns the test observation.
  given.ind=1:m
  x2=as.data.table(matrix(Xtest, ncol = m, nrow = 1))
  res2=sample_gaussian(given.ind,noSamp, mu, Sigma, m, Xtest)


  # Example 3 -------------
  # Check that ensuring conditional covariance matrix symmetry is FALSE by default.
  given.ind=4:7
  set.seed(1)
  res3.1=sample_gaussian(given.ind, noSamp, mu, Sigma, m, Xtest, ensure_condcov_symmetry = F)
  set.seed(1)
  res3.2=sample_gaussian(given.ind, noSamp, mu, Sigma, m, Xtest)
  set.seed(1)
  res3.3 =sample_gaussian(given.ind, noSamp, mu, Sigma, m, Xtest, ensure_condcov_symmetry = T)

  # Tests ------------------
  expect_equal(res1.1,res1.2)
  expect_equal(x2,res2)
  expect_identical(res3.1,res3.2)
  expect_false(sum(res3.1!=res3.3)==0) # Expect different results
  expect_error(sample_gaussian(m+1,noSamp, mu, Sigma, m, Xtest))
  expect_true(data.table::is.data.table(res3.2))
})

test_that("test sample_copula",{
  # Example 1 --------------
  # Check that the given features are not resampled, but kept as is.
  m <- 10
  n=40
  noSamp <- 50
  mu=rep(1,m)
  Sigma=cov(matrix(rnorm(n*m),n,m))
  Xtrain=mvrnorm(n,mu,Sigma)
  Xtest=mvrnorm(1,mu,Sigma)
  Xtest_Gauss = mvrnorm(1,mu,Sigma)
  given.ind = 3:6
  set.seed(1)
  ret = sample_copula(given.ind, noSamp, mu, Sigma, m, Xtest_Gauss, Xtrain, Xtest)
  X_given <- Xtest[given.ind]
  res1.1 = as.data.table(matrix(rep(X_given, each = noSamp),nrow=noSamp))
  res1.2 = as.data.table(ret[, ..given.ind])
  colnames(res1.1)=colnames(res1.2)

  # Example 2 --------------
  # Check that conditioning upon all variables simply returns the test observation.
  given.ind=1:m
  x2=as.data.table(matrix(Xtest, ncol = m, nrow = 1))
  res2=sample_copula(given.ind, noSamp, mu, Sigma, m, Xtest_Gauss, Xtrain, Xtest)

  # Example 3 --------------
  # Check that the colnames are preserved.
  given.ind = c(1,2,3,5,6)
  Xtest=t(as.data.frame(Xtest))
  colnames(Xtest)=1:m
  res3 = sample_copula(given.ind, noSamp, mu, Sigma, m, Xtest_Gauss, Xtrain, Xtest)


  # Tests ------------------
  expect_equal(res1.1,res1.2)
  expect_equal(x2,res2)
  expect_identical(colnames(res3),colnames(Xtest))
  expect_error(sample_copula(m+1,noSamp, mu, Sigma, m, Xtest_Gauss,Xtrain,Xtest))
  expect_true(data.table::is.data.table(res2))

})
