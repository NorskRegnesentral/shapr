#' Sample conditional variables using the Gaussian copula approach
#'
#' @param index_given Vector. The indices of the features to condition upon.
#' @param p Positive integer. The number of features.
#' @param x_test_gaussian Vector with the Gaussian transformed features of the observation whose
#' predictions ought to be explained (test data). Dimension \code{1xp} or \code{px1}.
#' @param x_test Matrix, data.frame or data.table with the features of the observation whose
#' predictions ought to be explained (test data). Dimension \code{1xp} or \code{px1}.
#'
#' @inheritParams global_arguments
#'
#' @return data.table with \code{n_samples} (conditional) Gaussian samples
#'
#' @keywords internal
#'
#' @examples
#' # TODO: Add simple example
#'
#' @author Martin Jullum
sample_copula <- function(index_given, n_samples, mu, cov_mat, p, x_test_gaussian, x_train, x_test) {
  # Handles the unconditional and full conditional separtely when predicting
  if (length(index_given) %in% c(0, p)) {
    ret <- matrix(x_test, ncol = p, nrow = 1)
  } else {
    dependent_ind <- (1:length(mu))[-index_given]

    tmp <- condMVNorm::condMVN(
      mean = mu,
      sigma = cov_mat,
      dependent.ind = dependent_ind,
      given.ind = index_given,
      X.given = x_test_gaussian[index_given]
    )

    ret0_z <- mvnfast::rmvn(n = n_samples, mu = tmp$condMean, sigma = tmp$condVar)

    ret0_x <- apply(
      X = rbind(ret0_z, x_train[, dependent_ind, drop = F]),
      MARGIN = 2,
      FUN = inv_gaussian_transform,
      n_z = n_samples
    )

    ret <- matrix(NA, ncol = p, nrow = n_samples)
    ret[, index_given] <- rep(x_test[index_given], each = n_samples)
    ret[, dependent_ind] <- ret0_x
  }
  colnames(ret) <- colnames(x_test)
  return(as.data.table(ret))
}


#' Sample conditional Gaussian variables
#'
#' @param index_given Vector. The indices of the features to condition upon.
#' @param p Positive integer. The number of features.
#' @param ensure_condcov_symmetry Logical. If \code{true}, \code{symmpart} is used on the
#' conditional covariance matrix to ensure symmetry.
#' @param x_test Matrix, data.frame or data.table with the features of the observation whose
#' predictions ought to be explained (test data). Dimension \code{1xp} or \code{px1}.
#'
#' @inheritParams global_arguments
#'
#' @return data.table with \code{n_samples} (conditional) Gaussian samples
#'
#' @keywords internal
#'
#' @examples
#' # TODO: Add simple example
#'
#' @author Martin Jullum
sample_gaussian <- function(index_given, n_samples, mu, cov_mat, p, x_test, ensure_condcov_symmetry = F) {

  # Handles the unconditional and full conditional separtely when predicting
  cnms <- colnames(x_test)
  if (length(index_given) %in% c(0, p)) {
    ret <- matrix(x_test, ncol = p, nrow = 1)
  } else {
    dependent_ind <- (1:length(mu))[-index_given]
    x_test_gaussian <- x_test[index_given]
    tmp <- condMVNorm::condMVN(
      mean = mu,
      sigma = cov_mat,
      dependent.ind = dependent_ind,
      given.ind = index_given,
      X.given = x_test_gaussian
    )
    if (ensure_condcov_symmetry) {
      tmp[["condVar"]] <- Matrix::symmpart(tmp$condVar)
    }

    ret0 <- mvnfast::rmvn(n = n_samples, mu = tmp$condMean, sigma = tmp$condVar)

    ret <- matrix(NA, ncol = p, nrow = n_samples)
    ret[, index_given] <- rep(x_test_gaussian, each = n_samples)
    ret[, dependent_ind] <- ret0
  }
  colnames(ret) <- cnms
  return(as.data.table(ret))
}

#' Helper function to sample a combination of training and testing rows, which does not risk
#' getting the same observation twice. Need to improve this help file.
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
#' @return Data.frame. Contains \code{nsamples} rows of re-sampled train and test observations.
#'
#' @keywords internal
#'
#' @examples
#' # TODO: Add simple example
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

#' Sample ctree variables
#'
#' @param tree
#' @param n_samples
#' @param x_test
#' @param x_train
#' @param features
#' @param p
#'
#' @inheritParams global_arguments
#'
#' @return data.table with \code{n_samples} (conditional) Gaussian samples
#'
#' @keywords internal
#'
#' @examples
#' # TODO: Add simple example
#'
#' @author Annabelle Redelmeier

sample_ctree <- function(tree,
                         n_samples,
                         x_test,
                         x_train,
                         features,
                         p) {


  datact <- tree$tree
  # print(tree$given_ind)


  cnms <- colnames(x_test)
  if (length(tree$given_ind) %in% c(0, p)) {
    ret <- matrix(x_test, ncol = p, nrow = 1)
  } else {
    given_ind <- tree$given_ind
    given_ind.vec <- rep(0, length(x_test))
    given_ind.vec[given_ind] <- 1

    dependent_ind <- tree$dependent_ind

    x_test_given <- x_test[given_ind]

    xp <- data.table(matrix(x_test_given, nrow = 1, ncol = length(x_test_given)))
    colnames(xp) <- paste0("V", given_ind) # this is important for where() below

    # x0 <- x_test
    # dependent_ind <- (1:dim(x_train)[2])[-given.ind.vec]
    # X_given <- x_test[given.ind]

    # xp <- data.table(matrix(x_test, nrow = 1, ncol = length(X_given)))
    # colnames(xp) <- paste0("V", given.ind)

    fit.nodes <- where(datact)
    nodes <- unique(fit.nodes)
    no.nodes <- length(nodes)
    pred.nodes <- where(object = datact, newdata = xp) ## newdata must be a data.frame and have the same colnames as x

    rowno <- 1:dim(x_train)[1]
    # newrowno <- vector("integer", n_samples)

    newrowno <- sample(rowno[fit.nodes == pred.nodes], n_samples, replace = TRUE)

    depDT <- data.table(matrix(x_train[newrowno, dependent_ind], ncol = length(dependent_ind)))
    givenDT <- data.table(matrix(x_test[1, given_ind], ncol = length(given_ind)))

    ret <- data.table(matrix(0, nrow = n_samples, ncol = length(x_test)))
    ret[, paste0("V", dependent_ind) := depDT]
    ret[, paste0("V", given_ind) := givenDT]

  }
  colnames(ret) <- cnms

  return(as.data.table(ret))

}

#' Make all conditional inference trees
#'
#' @param given_ind
#' @param x_train
#' @param comb
#' @param minbucket
#' @param mincriterion
#' @param minsplit
#'
#' @inheritParams global_arguments
#'
#' @return data.table with \code{n_samples} (conditional) Gaussian samples
#'
#' @keywords internal
#'
#' @examples
#' # TODO: Add simple example
#'
#' @author Annabelle Redelmeier
#'
simulateAllTrees <- function(given_ind,
                             x_train,
                             comb,
                             minbucket,
                             mincriterion,
                             minsplit){

  if (length(given_ind) %in% c(0, ncol(x_train))) {
    # ret <- matrix(Xtest, ncol = p, nrow = 1)
    datact = list()
    dependent_ind <- (1:dim(x_train)[2])[-given_ind]
  } else {


    # x0 <- Xtest
    dependent_ind <- (1:dim(x_train)[2])[-given_ind]
    # X_given <- Xtest[given_ind]

    ## for ctree model
    if(!is.null(comb)){
      if(length(given_ind) <= comb$comb_indici){
        mincriterion <- comb$comb_mincriterion[1] # alpha = 0.05 - split tree if p < 0.05
      } else {
        mincriterion <- comb$comb_mincriterion[2] # alpha = 0.20 - split tree if p < 0.20
      }
    }

    ## model
    if(length(dependent_ind) == 1){

      x <- x_train[, given_ind, with = FALSE]
      y <- x_train[, dependent_ind, with = FALSE]
      # xp <- data.table(matrix(X_given, ncol = length(X_given), nrow = 1))

      df <- data.table(cbind(y, x))

      colnames(df) <- c("Y", paste0("V", given_ind))
      # colnames(xp) <- paste0("V", given_ind)

      datact <- party::ctree(Y ~ ., data = df, controls = ctree_control(minbucket = minbucket, mincriterion = mincriterion))

    } else{

      x <- x_train[, given_ind, with = FALSE]
      y <- x_train[, dependent_ind, with = FALSE]
      # xp <- data.table(matrix(X_given, nrow = 1, ncol = length(X_given)))

      df <- data.table(cbind(y, x))

      colnames(df) <- c(paste0("Y", 1:ncol(y)), paste0("V", given_ind))
      # colnames(xp) <- paste0("V", given_ind)

      ## this was changed
      ynam <- paste0("Y", 1:ncol(y)) # ncol(y)
      fmla <- as.formula(paste(paste(ynam, collapse= "+"), "~ ."))

      datact <- party::ctree(fmla, data = df, controls = ctree_control(minbucket = minbucket, mincriterion = mincriterion))

    }

  }

  return(list(tree = datact, given_ind = given_ind, dependent_ind = dependent_ind)) # return the whole tree
}
