#' Sample conditional variables using the Gaussian copula approach
#'
#' @param index_given Integer vector. The indices of the features to condition upon. Note that
#' \code{min(index_given) >= 1} and \code{max(index_given) <= m}.
#' @param m Positive integer. The total number of features.
#' @param x_test_gaussian Numeric matrix. Contains the observation whose predictions ought to be explained (test data),
#' after quantile-transforming them to standard Gaussian variables.
#' @param x_test Numeric matrix. Contains the features of the observation whose
#' predictions ought to be explained (test data).
#'
#' @return data.table
#'
#' @keywords internal
#'
#' @examples
#' m <- 10
#' n <- 40
#' n_samples <- 50
#' mu <- rep(1, m)
#' cov_mat <- cov(matrix(rnorm(n * m), n, m))
#' x_train <- MASS::mvrnorm(n, mu, cov_mat)
#' x_test <- MASS::mvrnorm(1, mu, cov_mat)
#' x_test_gaussian <- MASS::mvrnorm(1, mu, cov_mat)
#' index_given <- 3:6
#' ret <- shapr:::sample_copula(index_given, n_samples, mu, cov_mat,
#'   m = m,
#'   x_test_gaussian, x_train, x_test
#' )
#' @author Martin Jullum
sample_copula <- function(index_given, n_samples, mu, cov_mat, m, x_test_gaussian, x_train, x_test) {
  # Handles the unconditional and full conditional separtely when predicting
  if (length(index_given) %in% c(0, m)) {
    ret <- matrix(x_test, ncol = m, nrow = 1)
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

    ret <- matrix(NA, ncol = m, nrow = n_samples)
    ret[, index_given] <- rep(x_test[index_given], each = n_samples)
    ret[, dependent_ind] <- ret0_x
  }
  colnames(ret) <- colnames(x_test)
  return(as.data.table(ret))
}


#' Sample conditional Gaussian variables
#'
#' @inheritParams sample_copula
#'
#' @return data.table
#'
#' @keywords internal
#'
#' @examples
#' m <- 10
#' n_samples <- 50
#' mu <- rep(1, m)
#' cov_mat <- cov(matrix(rnorm(n_samples * m), n_samples, m))
#' x_test <- matrix(MASS::mvrnorm(1, mu, cov_mat), nrow = 1)
#' cnms <- paste0("x", seq(m))
#' colnames(x_test) <- cnms
#' index_given <- c(4, 7)
#' r <- shapr:::sample_gaussian(index_given, n_samples, mu, cov_mat, m, x_test)
#' @author Martin Jullum
sample_gaussian <- function(index_given, n_samples, mu, cov_mat, m, x_test) {

  # Check input
  stopifnot(is.matrix(x_test))

  # Handles the unconditional and full conditional separtely when predicting
  cnms <- colnames(x_test)
  if (length(index_given) %in% c(0, m)) return(data.table::as.data.table(x_test))

  dependent_ind <- (1:length(mu))[-index_given]
  x_test_gaussian <- x_test[index_given]
  tmp <- condMVNorm::condMVN(
    mean = mu,
    sigma = cov_mat,
    dependent.ind = dependent_ind,
    given.ind = index_given,
    X.given = x_test_gaussian
  )

  # Makes the conditional covariance matrix symmetric in the rare case where numerical instability made it unsymmetric
  if (!isSymmetric(tmp[["condVar"]])) {
    tmp[["condVar"]] <- Matrix::symmpart(tmp$condVar)
  }

  ret0 <- mvnfast::rmvn(n = n_samples, mu = tmp$condMean, sigma = tmp$condVar)

  ret <- matrix(NA, ncol = m, nrow = n_samples)
  ret[, index_given] <- rep(x_test_gaussian, each = n_samples)
  ret[, dependent_ind] <- ret0

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
#' @return data.frame
#'
#' @keywords internal
#'
#' @examples
#' ntrain <- 10
#' ntest <- 10
#' nsamples <- 7
#' joint_sampling <- FALSE
#' cnms <- c("samp_train", "samp_test")
#' x <- shapr:::sample_combinations(ntrain, ntest, nsamples, joint_sampling)
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

#' Sample ctree variables from a given conditional inference tree
#'
#' @param tree List. Contains tree which is an object of type ctree built from the party package.
#' Also contains given_ind, the features to condition upon.
#'
#' @param n_samples Numeric. Indicates how many samples to use for MCMC.
#'
#' @param x_test Matrix, data.frame or data.table with the features of the observation whose
#' predictions ought to be explained (test data). Dimension \code{1xp} or \code{px1}.
#'
#' @param x_train Matrix, data.frame or data.table with training data.
#'
#' @param p Positive integer. The number of features.
#'
#' @param sample Boolean. True indicates that the method samples from the terminal node
#' of the tree whereas False indicates that the method takes all the samples if it is less than n_samples.
#'
#' @return data.table with \code{n_samples} (conditional) Gaussian samples
#'
#' @keywords internal
#'
#' @examples
#' # TODO: Add simple example
#' @author Annabelle Redelmeier
sample_ctree <- function(tree,
                         n_samples,
                         x_test,
                         x_train,
                         p,
                         sample) {
  datact <- tree$tree

  cnms <- colnames(x_test)
  if (length(tree$given_ind) %in% c(0, p)) {
    ret <- x_test # matrix(x_test, ncol = p, nrow = 1)
  } else {
    given_ind <- tree$given_ind
    # given_ind_vec <- rep(0, length(x_test)) ## I don't think we actually use this?
    # given_ind_vec[given_ind] <- 1

    dependent_ind <- tree$dependent_ind

    x_test_given <- x_test[, given_ind, drop = FALSE, with = FALSE]

    xp <- x_test_given # data.table(matrix(x_test_given, nrow = 1, ncol = length(x_test_given)))  # this is changed by Martin
    colnames(xp) <- paste0("V", given_ind) # this is important for where() below

    fit.nodes <- party::where(object = datact)
    ## I don't think you actually need this?
    # nodes <- unique(fit.nodes)
    # no.nodes <- length(nodes)
    pred.nodes <- party::where(object = datact, newdata = xp) ## newdata must be a data.frame and have the same colnames as x

    rowno <- 1:dim(x_train)[1]

    # newrowno <- sample(rowno[fit.nodes == pred.nodes], n_samples, replace = TRUE)
    # depDT <- data.table::data.table(matrix(x_train[newrowno, dependent_ind], ncol = length(dependent_ind)))
    # givenDT <- data.table::data.table(matrix(x_test[1, given_ind], ncol = length(given_ind)))
    # ret <- data.table::data.table(matrix(0, nrow = n_samples, ncol = length(x_test)))
    # ret[, paste0("V", dependent_ind) := depDT]
    # ret[, paste0("V", given_ind) := givenDT]

    if (!sample) {
      if (length(rowno[fit.nodes == pred.nodes]) <= n_samples) {
        depDT <- data.table::data.table(x_train[rowno[fit.nodes == pred.nodes], dependent_ind, drop = FALSE, with = FALSE])
        givenDT <- data.table::data.table(x_test[1, given_ind, drop = FALSE, with = FALSE])

        ret <- cbind(depDT, givenDT)
        setcolorder(ret, colnames(x_train))

        # ret <- data.table::data.table(matrix(0, nrow = length(rowno[fit.nodes == pred.nodes]), ncol = length(x_test)))
        # ret[, paste0("V", dependent_ind) := depDT]
        # ret[, paste0("V", given_ind) := givenDT]
      } else {
        newrowno <- sample(rowno[fit.nodes == pred.nodes], n_samples, replace = TRUE)

        depDT <- data.table::data.table(x_train[newrowno, dependent_ind, drop = FALSE, with = FALSE])
        givenDT <- data.table::data.table(x_test[1, given_ind, drop = FALSE, with = FALSE])

        # ret <- data.table::data.table(matrix(0, nrow = n_samples, ncol = length(x_test)))
        # ret[, paste0("V", dependent_ind) := depDT]
        # ret[, paste0("V", given_ind) := givenDT]

        ret <- cbind(depDT, givenDT)
        setcolorder(ret, colnames(x_train))
      }
    } else {
      newrowno <- sample(rowno[fit.nodes == pred.nodes], n_samples, replace = TRUE)

      depDT <- data.table::data.table(x_train[newrowno, dependent_ind, drop = FALSE, with = FALSE])
      givenDT <- data.table::data.table(x_test[1, given_ind, drop = FALSE, with = FALSE])

      # ret <- data.table::data.table(matrix(0, nrow = n_samples, ncol = length(x_test)))
      # ret[, paste0("V", dependent_ind) := depDT]
      # ret[, paste0("V", given_ind) := givenDT]

      ret <- cbind(depDT, givenDT)
      setcolorder(ret, colnames(x_train))
    }
  }
  colnames(ret) <- cnms

  return(as.data.table(ret))
}

#' Make all conditional inference trees
#'
#' @param given_ind Numeric value. Indicates which features are conditioned on.
#'
#' @param x_train Numeric vector. Indicates the specific values of features for individual i.
#'
#' @param comb_indici Numeric value. (Optional) Contains the splitting point corresponding to where to change the
#' \code{comb_mincriterion}.
#' If \code{NULL}, the \code{mincriterion} is constant for every combination.
#' This is depreciated and will be deleted soon.
#'
#' @param comb_mincriterion Numeric vector. (Optional) Contains the different mincriterions to use for each
#' combination.
#' If \code{NULL}, the \code{mincriterion} is constant for every combination.
#' This is depreciated and will be deleted soon.
#'
#' @param mincriterion Numeric value or vector equal to 1 - alpha where alpha is the nominal level of the conditional independence tests.
#' Can also be a vector equal to the length of the number of features indicating which mincriterion to use
#' when conditioning on various numbers of features.
#'
#' @param minsplit Numeric value. Equal to the value that the sum of the left and right daughter nodes need to exceed.
#'
#' @param minbucket Numeric value. Equal to the minimum sum of weights in a terminal node.
#'
#' @return List with conditional inference tree and the variables conditioned/not conditioned on.
#'
#' @keywords internal
#'
#' @examples
#' # TODO: Add simple example
#' @author Annabelle Redelmeier
#'
#' @export
simulateAllTrees <- function(given_ind,
                             x_train,
                             comb_indici,
                             comb_mincriterion,
                             mincriterion,
                             minsplit,
                             minbucket) {
  dependent_ind <- (1:dim(x_train)[2])[-given_ind]

  if (length(given_ind) %in% c(0, ncol(x_train))) {
    datact = list()
  } else {

    ## currently no tests made to make sure that comb_indici and comb_mincriterion both exist
    ## if only one is provided, no split is made.
    if (!is.null(comb_indici) & !is.null(comb_mincriterion)) {
      if (length(given_ind) <= comb_indici) {
        mincriterion <- comb_mincriterion[1] # if alpha = 0.05 --> split tree if p < 0.05
      } else {
        mincriterion <- comb_mincriterion[2]
      }
    }

    if (length(dependent_ind) == 1) {
      x <- x_train[, given_ind, with = FALSE]
      y <- x_train[, dependent_ind, with = FALSE]

      df <- data.table(cbind(y, x))

      colnames(df) <- c("Y", paste0("V", given_ind))

      datact <- party::ctree(Y ~ ., data = df, controls = party::ctree_control(minbucket = minbucket, mincriterion = mincriterion))
    } else {
      x <- x_train[, given_ind, with = FALSE]
      y <- x_train[, dependent_ind, with = FALSE]

      df <- data.table::data.table(cbind(y, x))

      colnames(df) <- c(paste0("Y", 1:ncol(y)), paste0("V", given_ind))

      ynam <- paste0("Y", 1:ncol(y))
      fmla <- as.formula(paste(paste(ynam, collapse = "+"), "~ ."))

      datact <- party::ctree(fmla, data = df, controls = party::ctree_control(minbucket = minbucket, mincriterion = mincriterion))
    }
  }

  return(list(tree = datact, given_ind = given_ind, dependent_ind = dependent_ind)) # return the whole tree
}
