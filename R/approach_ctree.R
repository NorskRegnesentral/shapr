#' @keywords internal
setup_approach.ctree <- function(internal,
                                 mincriterion = 0.95,
                                 minsplit = 20,
                                 minbucket = 7,
                                 sample = TRUE, ...) {
  defaults <- mget(c("mincriterion", "minsplit", "minbucket", "sample"))

  internal <- insert_defaults(internal, defaults)


  return(internal)
}


#' @param index_features List. Default is NULL but if either various methods are being used or various mincriterion are
#' used for different numbers of conditioned features, this will be a list with the features to pass.
#'
#' @param  mc_cores Integer. Only for class \code{ctree} currently. The number of cores to use in paralellization of the
#' tree building (\code{create_ctree}) and tree sampling (\code{sample_ctree}). Defaults to 1. Note: Uses
#' parallel::mclapply which relies on forking, i.e. uses only 1 core on Windows systems.
#'
#' @param  mc_cores_create_ctree Integer. Same as \code{mc_cores}, but specific for the tree building function
#' #' Defaults to \code{mc_cores}.
#'
#' @param  mc_cores_sample_ctree Integer. Same as \code{mc_cores}, but specific for the tree building prediction
#' function.
#' Defaults to \code{mc_cores}.
#'
#' @rdname prepare_data
#' @export
prepare_data.ctree <- function(internal, index_features = NULL, ...) {
  id <- id_combination <- w <- NULL # due to NSE notes in R CMD check

  x_train <- internal$data$x_train
  x_explain <- internal$data$x_explain
  n_explain <- internal$parameters$n_explain
  n_samples <- internal$parameters$n_samples
  n_features <- internal$parameters$n_features
  mincriterion <- internal$parameters$mincriterion
  minsplit <- internal$parameters$minsplit
  minbucket <- internal$parameters$minbucket
  sample <- internal$parameters$sample
  labels <- internal$parameters$feature_list$labels

  X <- internal$objects$X


  dt_l <- list()


  if (is.null(index_features)) {
    features <- X$features
  } else {
    features <- X$features[index_features]
  }


  # this is a list of all 2^M trees (where number of features = M)
  all_trees <- lapply(
    X = features,
    FUN = create_ctree,
    x_train = x_train,
    mincriterion = mincriterion,
    minsplit = minsplit,
    minbucket = minbucket
  )

  for (i in seq_len(n_explain)) {
    l <- lapply(
      X = all_trees,
      FUN = sample_ctree,
      n_samples = n_samples,
      x_explain = x_explain[i, , drop = FALSE],
      x_train = x_train,
      p = n_features,
      sample = sample
    )

    dt_l[[i]] <- data.table::rbindlist(l, idcol = "id_combination")
    dt_l[[i]][, w := 1 / n_samples]
    dt_l[[i]][, id := i]
    if (!is.null(index_features)) dt_l[[i]][, id_combination := index_features[id_combination]]
  }

  dt <- data.table::rbindlist(dt_l, use.names = TRUE, fill = TRUE)
  dt[id_combination %in% c(1, 2^n_features), w := 1.0]

  # only return unique dt
  dt2 <- dt[, sum(w), by = c("id_combination", labels, "id")]
  setnames(dt2, "V1", "w")

  return(dt2)
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

#' Sample ctree variables from a given conditional inference tree
#'
#' @param tree List. Contains tree which is an object of type ctree built from the party package.
#' Also contains given_ind, the features to condition upon.
#'
#' @param n_samples Numeric. Indicates how many samples to use for MCMC.
#'
#' @param x_explain Matrix, data.frame or data.table with the features of the observation whose
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
#'   x_explain <- MASS::mvrnorm(1, mu, cov_mat)
#'   x_explain_dt <- data.table::setDT(as.list(x_explain))
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
#'     tree = tree, n_samples = n_samples, x_explain = x_explain_dt, x_train = x_train,
#'     p = length(x_explain), sample = TRUE
#'   )
#' }
sample_ctree <- function(tree,
                         n_samples,
                         x_explain,
                         x_train,
                         p,
                         sample) {
  datact <- tree$tree
  using_partykit <- (class(datact)[1] != "BinaryTree")

  cnms <- colnames(x_explain)
  if (length(tree$given_ind) %in% c(0, p)) {
    ret <- x_explain
  } else {
    given_ind <- tree$given_ind

    dependent_ind <- tree$dependent_ind

    x_explain_given <- x_explain[,
      given_ind,
      drop = FALSE,
      with = FALSE
    ] #
    xp <- x_explain_given
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

    givenDT <- data.table::data.table(x_explain[1,
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
