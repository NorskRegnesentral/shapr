#' Sample conditional variables using the Gaussian copula approach
#'
#' @param given_ind Vector. The indices of the features to condition upon.
#' @param p Positive integer. The number of features.
#' @param Xtest_Gauss_trans Vector with the Gaussian transformed features of the observation whose predictions ought to be explained (test data). Dimension \code{1xp} or \code{px1}.
#' @param Xtest Matrix, data.frame or data.table with the features of the observation whose predictions ought to be explained (test data). Dimension \code{1xp} or \code{px1}.
#'
#' @inheritParams global_arguments
#'
#' @keywords internal
#'
#' @return data.table with \code{noSamp_MC} (conditional) Gaussian samples
#'
#' @author Martin Jullum
sample_copula <- function(given_ind, noSamp_MC, mu, Sigma, p, Xtest_Gauss_trans, Xtrain, Xtest) {
  # Handles the unconditional and full conditional separtely when predicting
  if (length(given_ind) %in% c(0, p)) {
    ret <- matrix(Xtest, ncol = p, nrow = 1)
  } else {
    dependent_ind <- (1:length(mu))[-given_ind]
    X_given <- Xtest_Gauss_trans[given_ind]
    X_given_orig <- Xtest[given_ind]

    tmp <- condMVNorm::condMVN(
      mean = mu,
      sigma = Sigma,
      dependent.ind = dependent_ind,
      given.ind = given_ind,
      X.given = X_given
    )

    ret0_z <- mvnfast::rmvn(n = noSamp_MC, mu = tmp$condMean, sigma = tmp$condVar)

    ret0_x <- apply(
      X = rbind(ret0_z, Xtrain[, dependent_ind, drop = F]),
      MARGIN = 2,
      FUN = inv_gaussian_transform,
      n_z = noSamp_MC
    )

    ret <- matrix(NA, ncol = p, nrow = noSamp_MC)
    ret[, given_ind] <- rep(X_given_orig, each = noSamp_MC)
    ret[, dependent_ind] <- ret0_x
  }
  colnames(ret) <- colnames(Xtest)
  return(as.data.table(ret))
}


#' Sample conditional Gaussian variables
#'
#' @param given_ind Vector. The indices of the features to condition upon.
#' @param p Positive integer. The number of features.
#' @param ensure_condcov_symmetry Logical. If \code{true}, \code{symmpart} is used on the conditional covariance matrix to ensure symmetry.
#' @param Xtest Matrix, data.frame or data.table with the features of the observation whose predictions ought to be explained (test data). Dimension \code{1xp} or \code{px1}.
#'
#' @inheritParams global_arguments
#'
#' @keywords internal
#'
#' @return data.table with \code{noSamp_MC} (conditional) Gaussian samples
#'
#' @author Martin Jullum
sample_gaussian <- function(given_ind, noSamp_MC, mu, Sigma, p, Xtest, ensure_condcov_symmetry = F) {

  # Handles the unconditional and full conditional separtely when predicting
  if (length(given_ind) %in% c(0, p)) {
    ret <- matrix(Xtest, ncol = p, nrow = 1)
  } else {
    dependent_ind <- (1:length(mu))[-given_ind]
    X_given <- Xtest[given_ind]
    tmp <- condMVNorm::condMVN(
      mean = mu,
      sigma = Sigma,
      dependent.ind = dependent_ind,
      given.ind = given_ind,
      X.given = X_given
    )
    if (ensure_condcov_symmetry) {
      tmp$condVar <- Matrix::symmpart(tmp$condVar)
    }

    ret0 <- mvnfast::rmvn(n = noSamp_MC, mu = tmp$condMean, sigma = tmp$condVar)

    ret <- matrix(NA, ncol = p, nrow = noSamp_MC)
    ret[, given_ind] <- rep(X_given, each = noSamp_MC)
    ret[, dependent_ind] <- ret0
  }
  colnames(ret) <- colnames(Xtest)
  return(as.data.table(ret))
}

#' Helper function to sample a combination of training and testing rows, which does not risk
#' getting the same observation twice. Need to improve this help file.
#'
#' @inheritParams global_arguments
#'
#' @param ntrain Positive integer. Number of training observations to sample from.
#'
#' @param ntest Positive integer. Number of test observations to sample from.
#'
#' @param nsamples Positive integer. Number of samples.
#'
#' @param joint_sampling Logical. Indicates whether train- and test data should be sampled
#' separately or in a joint sampling space. If they are sampled separately (which typically
#' would be used when optimizing more than one distribution at once) we sample with replacement
#' if \code{nsamples > ntrain}. Note that this solution is not optimal. Be careful if you're
#' doing optimization over every test observation when \code{nsamples > ntrain}.
#'
#' @keywords internal
#'
#' @return Data.frame. Contains \code{nsamples} rows of re-sampled train and test observations.
#'
#' @author Martin Jullum
sample_combinations <- function(ntrain, ntest, nsamples, joint_sampling = TRUE) {

  if (!joint_sampling) {

    # Sample training data
    samp_train <- sample(
      x = ntrain,
      size = nsamples,
      replace = ifelse(nsamples < ntrain, FALSE, TRUE)
    )

    # Sample test data
    samp_test <- sample(
      x = ntest,
      size = nsamples,
      replace = ifelse(nsamples < ntrain, nsamples > ntest, TRUE)
    )
  } else {

    n <- ntrain * ntest
    if (nsamples < n) {
      input_samp <- sample(
        x = n,
        size = nsamples,
        replace = FALSE
      )
    } else {
      input_samp <- seq(n)
    }

    samp_train <- (input_samp - 1) %% ntrain + 1
    samp_test <- (input_samp - 1) %/% ntrain + 1
  }
  ret <- data.frame(samp_train = samp_train, samp_test = samp_test)

  return(ret)
}
