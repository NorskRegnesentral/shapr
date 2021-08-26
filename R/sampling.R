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
#' @author Martin Jullum
sample_gaussian <- function(index_given, n_samples, mu, cov_mat, m, x_test) {

  # Check input
  stopifnot(is.matrix(x_test))

  # Handles the unconditional and full conditional separately when predicting
  cnms <- colnames(x_test)
  if (length(index_given) %in% c(0, m)) {
    return(data.table::as.data.table(x_test))
  }

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


#' Sample conditional Gaussian variables following a causal chain graph with do-calculus.
#'
#' @inheritParams sample_copula
#'
#' @param causal_ordering List of vectors specifying (partial) causal ordering. Each element in
#' the list is a component in the order, which can contain one or more variable indices in a vector.
#' For example, in list(1, c(2, 3)), 2 > 1 and 3 > 1, but 2 and 3 are not comparable.
#' @param confounding Logical vector specifying which variables are affected by confounding.
#' Confounding must be speficied globally with a single TRUE / FALSE value for all components,
#' or separately for each causal component in the causal ordering.
#'
#' @return data.table
#'
#' @keywords internal
#'
#' @author Tom Heskes, Ioan Gabriel Bucur
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
#' causal_ordering <- list(c(1:3), c(4:6), c(7:10))
#' confounding <- c(TRUE, FALSE, TRUE)
#' r <- shapr:::sample_causal(
#'   index_given, n_samples, mu, cov_mat, m, x_test,
#'   causal_ordering, confounding
#' )
sample_causal <- function(index_given, n_samples, mu, cov_mat, m, x_test,
                          causal_ordering, confounding) {

  # Check input
  stopifnot(is.matrix(x_test))
  stopifnot(is.list(causal_ordering))
  stopifnot(is.logical(confounding))

  if (length(confounding) > 1 && length(confounding) != length(causal_ordering)) {
    stop("Confounding must be specified globally (one value for all components), or separately for each component in the causal ordering.")
  }

  # In case of global confounding value, replicate it across components.
  if (length(confounding) == 1) {
    confounding <- rep(confounding, length(causal_ordering))
  }

  if (!base::setequal(unlist(causal_ordering), seq(m))) {
    stop(paste("Incomplete or incorrect partial causal_ordering specified for", m, "variables"))
  }

  # Handles the unconditional and full conditional separately when predicting
  if (length(index_given) %in% c(0, m)) {
    return(data.table::as.data.table(x_test))
  }

  dependent_ind <- setdiff(1:length(mu), index_given)
  xall <- matrix(NA, ncol = m, nrow = n_samples)
  xall[, index_given] <- rep(x_test[index_given], each = n_samples)

  for(i in seq(length(causal_ordering))) {

    # check overlap between dependent_ind and component
    to_be_sampled <- intersect(causal_ordering[[i]], dependent_ind)
    if (length(to_be_sampled) > 0) {
      # condition upon all variables in ancestor components
      to_be_conditioned <- unlist(causal_ordering[0:(i-1)])

      # back to conditioning if confounding is FALSE or no conditioning if confounding is TRUE
      if (!confounding[i]) {
        # add intervened variables in the same component
        to_be_conditioned <- union(intersect(causal_ordering[[i]], index_given), to_be_conditioned)
      }
      if (length(to_be_conditioned) == 0) {
        # draw new samples from marginal distribution
        newsamples <- mvnfast::rmvn(n_samples, mu=mu[to_be_sampled], sigma=as.matrix(cov_mat[to_be_sampled,to_be_sampled]))
      } else {

        # compute conditional Gaussian
        C <- cov_mat[to_be_sampled,to_be_conditioned, drop=FALSE]
        D <- cov_mat[to_be_conditioned, to_be_conditioned]
        CDinv <- C %*% solve(D)
        cVar <- cov_mat[to_be_sampled,to_be_sampled] - CDinv %*% t(C)
        if (!isSymmetric(cVar)) {
          cVar <- Matrix::symmpart(cVar)
        }

        # draw new samples from conditional distribution
        mu_sample <- matrix(rep(mu[to_be_sampled],each=n_samples),nrow=n_samples)
        mu_cond <- matrix(rep(mu[to_be_conditioned],each=n_samples),nrow=n_samples)
        cMU <- mu_sample + t(CDinv %*% t(xall[,to_be_conditioned] - mu_cond))
        newsamples <- mvnfast::rmvn(n_samples, mu=matrix(0,1,length(to_be_sampled)), sigma=as.matrix(cVar))
        newsamples <- newsamples + cMU

      }
      xall[,to_be_sampled] <- newsamples
    }
  }

  colnames(xall) <- colnames(x_test)
  return(as.data.table(xall))
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
#' of the tree whereas False indicates that the method takes all the observations if it is
#' less than n_samples.
#'
#' @return data.table with \code{n_samples} (conditional) Gaussian samples
#'
#' @keywords internal
#'
#' @author Annabelle Redelmeier
#'
#' @examples
#' if (requireNamespace("MASS", quietly = TRUE) & requireNamespace("party", quietly = TRUE)) {
#'   m <- 10
#'   n <- 40
#'   n_samples <- 50
#'   mu <- rep(1, m)
#'   cov_mat <- cov(matrix(rnorm(n * m), n, m))
#'   x_train <- data.table::data.table(MASS::mvrnorm(n, mu, cov_mat))
#'   x_test <- MASS::mvrnorm(1, mu, cov_mat)
#'   x_test_dt <- data.table::setDT(as.list(x_test))
#'   given_ind <- c(4, 7)
#'   dependent_ind <- (1:dim(x_train)[2])[-given_ind]
#'   x <- x_train[, given_ind, with = FALSE]
#'   y <- x_train[, dependent_ind, with = FALSE]
#'   df <- data.table::data.table(cbind(y, x))
#'   colnames(df) <- c(paste0("Y", 1:ncol(y)), paste0("V", given_ind))
#'   ynam <- paste0("Y", 1:ncol(y))
#'   fmla <- as.formula(paste(paste(ynam, collapse = "+"), "~ ."))
#'   datact <- party::ctree(fmla, data = df, controls = party::ctree_control(
#'     minbucket = 7,
#'     mincriterion = 0.95
#'   ))
#'   tree <- list(tree = datact, given_ind = given_ind, dependent_ind = dependent_ind)
#'   shapr:::sample_ctree(
#'     tree = tree, n_samples = n_samples, x_test = x_test_dt, x_train = x_train,
#'     p = length(x_test), sample = TRUE
#'   )
#' }
sample_ctree <- function(tree,
                         n_samples,
                         x_test,
                         x_train,
                         p,
                         sample) {
  datact <- tree$tree
  using_partykit <- (class(datact)[1] != "BinaryTree")

  cnms <- colnames(x_test)
  if (length(tree$given_ind) %in% c(0, p)) {
    ret <- x_test
  } else {
    given_ind <- tree$given_ind

    dependent_ind <- tree$dependent_ind

    x_test_given <- x_test[,
      given_ind,
      drop = FALSE,
      with = FALSE
    ] #
    xp <- x_test_given
    colnames(xp) <- paste0("V", given_ind) # this is important for where() below

    if (using_partykit) {
      fit.nodes <- predict(
        object = datact,
        type = "node"
      )
      # newdata must be data.frame + have the same colnames as x
      pred.nodes <- predict(
        object = datact, newdata = xp,
        type = "node"
      )
    } else {
      fit.nodes <- party::where(object = datact)
      # newdata must be data.frame + have the same colnames as x
      pred.nodes <- party::where(object = datact, newdata = xp)
    }

    rowno <- 1:nrow(x_train)

    use_all_obs <- !sample & (length(rowno[fit.nodes == pred.nodes]) <= n_samples)

    if (use_all_obs) {
      newrowno <- rowno[fit.nodes == pred.nodes]
    } else {
      newrowno <- sample(rowno[fit.nodes == pred.nodes], n_samples,
        replace = TRUE
      )
    }

    depDT <- data.table::data.table(x_train[newrowno,
      dependent_ind,
      drop = FALSE,
      with = FALSE
    ])

    givenDT <- data.table::data.table(x_test[1,
      given_ind,
      drop = FALSE,
      with = FALSE
    ])
    ret <- cbind(depDT, givenDT)
    data.table::setcolorder(ret, colnames(x_train))
    colnames(ret) <- cnms
  }

  return(data.table::as.data.table(ret))
}

#' Make all conditional inference trees
#'
#' @param given_ind Numeric value. Indicates which features are conditioned on.
#'
#' @param x_train Numeric vector. Indicates the specific values of features for individual i.
#'
#' @param mincriterion Numeric value or vector equal to 1 - alpha where alpha is the nominal level of the conditional
#' independence tests.
#' Can also be a vector equal to the length of the number of features indicating which mincriterion to use
#' when conditioning on various numbers of features.
#'
#' @param minsplit Numeric value. Equal to the value that the sum of the left and right daughter nodes need to exceed.
#'
#' @param minbucket Numeric value. Equal to the minimum sum of weights in a terminal node.
#'
#' @param use_partykit String. In some semi-rare cases \code{partyk::ctree} runs into an error related to the LINPACK
#' used by R. To get around this problem, one may fall back to using the newer (but slower) \code{partykit::ctree}
#' function, which is a reimplementation of the same method. Setting this parameter to \code{"on_error"} (default)
#' falls back to  \code{partykit::ctree}, if \code{party::ctree} fails. Other options are \code{"never"}, which always
#' uses \code{party::ctree}, and \code{"always"}, which always uses \code{partykit::ctree}. A warning message is
#' created whenever \code{partykit::ctree} is used.
#'
#' @return List with conditional inference tree and the variables conditioned/not conditioned on.
#'
#' @keywords internal
#' @author Annabelle Redelmeier, Martin Jullum
#'
#' @export
#'
#' @examples
#' if (requireNamespace("MASS", quietly = TRUE) & requireNamespace("party", quietly = TRUE)) {
#'   m <- 10
#'   n <- 40
#'   n_samples <- 50
#'   mu <- rep(1, m)
#'   cov_mat <- cov(matrix(rnorm(n * m), n, m))
#'   x_train <- data.table::data.table(MASS::mvrnorm(n, mu, cov_mat))
#'   given_ind <- c(4, 7)
#'   mincriterion <- 0.95
#'   minsplit <- 20
#'   minbucket <- 7
#'   sample <- TRUE
#'   create_ctree(
#'     given_ind = given_ind, x_train = x_train,
#'     mincriterion = mincriterion, minsplit = minsplit,
#'     minbucket = minbucket, use_partykit = "on_error"
#'   )
#' }
create_ctree <- function(given_ind,
                         x_train,
                         mincriterion,
                         minsplit,
                         minbucket,
                         use_partykit = "on_error") {
  dependent_ind <- (1:dim(x_train)[2])[-given_ind]

  if (length(given_ind) %in% c(0, ncol(x_train))) {
    datact <- list()
  } else {
    y <- x_train[, dependent_ind, with = FALSE]
    x <- x_train[, given_ind, with = FALSE]
    df <- data.table::data.table(cbind(y, x))
    colnames(df) <- c(paste0("Y", 1:ncol(y)), paste0("V", given_ind))

    ynam <- paste0("Y", 1:ncol(y))
    fmla <- as.formula(paste(paste(ynam, collapse = "+"), "~ ."))

    # Run party:ctree if that works. If that fails, run partykit instead
    if (use_partykit == "on_error") {
      datact <- tryCatch(expr = {
        party::ctree(fmla,
          data = df,
          controls = party::ctree_control(
            minbucket = minbucket,
            mincriterion = mincriterion
          )
        )
      }, error = function(ex) {
        warning("party::ctree ran into the error: ", ex, "Using partykit::ctree instead!")
        partykit::ctree(fmla,
          data = df,
          control = partykit::ctree_control(
            minbucket = minbucket,
            mincriterion = mincriterion,
            splitstat = "maximum"
          )
        )
      })
    } else if (use_partykit == "never") {
      datact <- party::ctree(fmla,
        data = df,
        controls = party::ctree_control(
          minbucket = minbucket,
          mincriterion = mincriterion
        )
      )
    } else if (use_partykit == "always") {
      warning("Using partykit::ctree instead of party::ctree!")
      datact <- partykit::ctree(fmla,
        data = df,
        control = partykit::ctree_control(
          minbucket = minbucket,
          mincriterion = mincriterion,
          splitstat = "maximum"
        )
      )
    } else {
      stop("use_partykit needs to be one of 'on_error', 'never', or 'always'. See ?create_ctree for details.")
    }
  }
  return(list(tree = datact, given_ind = given_ind, dependent_ind = dependent_ind))
}
