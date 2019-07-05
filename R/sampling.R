#' Sample conditional variables using the Gaussian copula approach
#'
#' @param given_ind Vector
#' @param p Positive integer
#'
#' @inheritParams global_arguments
#'
#' @return data.table with \code{noSamp_MC} (conditional) Gaussian samples
#'
#' @export
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
#' @param given_ind Vector
#' @param p Positive integer
#'
#' @inheritParams global_arguments
#'
#' @return data.table with \code{noSamp_MC} (conditional) Gaussian samples
#'
#' @export
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

