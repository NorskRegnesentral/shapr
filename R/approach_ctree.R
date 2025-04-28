#' @rdname setup_approach
#'
#' @param ctree.mincriterion Numeric scalar or vector.
#' Either a scalar or vector of length equal to the number of features in the model.
#' The value is equal to 1 - \eqn{\alpha} where \eqn{\alpha} is the nominal level of the conditional independence tests.
#' If it is a vector, this indicates which value to use when conditioning on various numbers of features.
#' The default value is 0.95.
#'
#' @param ctree.minsplit Numeric scalar.
#' Determines minimum value that the sum of the left and right daughter nodes required for a split.
#' The default value is 20.
#'
#' @param ctree.minbucket Numeric scalar.
#' Determines the minimum sum of weights in a terminal node required for a split
#' The default value is 7.
#'
#' @param ctree.sample Boolean.
#' If `TRUE` (default), then the method always samples `n_MC_samples` observations from the leaf nodes
#' (with replacement).
#' If `FALSE` and the number of observations in the leaf node is less than `n_MC_samples`,
#' the method will take all observations in the leaf.
#' If `FALSE` and the number of observations in the leaf node is more than `n_MC_samples`,
#' the method will sample `n_MC_samples` observations (with replacement).
#' This means that there will always be sampling in the leaf unless
#' `sample = FALSE` *and* the number of obs in the node is less than `n_MC_samples`.
#'
#' @inheritParams default_doc_export
#'
#' @export
setup_approach.ctree <- function(internal,
                                 ctree.mincriterion = 0.95,
                                 ctree.minsplit = 20,
                                 ctree.minbucket = 7,
                                 ctree.sample = TRUE, ...) {
  defaults <- mget(c("ctree.mincriterion", "ctree.minsplit", "ctree.minbucket", "ctree.sample"))

  internal <- insert_defaults(internal, defaults)


  return(internal)
}


#' @inheritParams default_doc_internal
#'
#' @rdname prepare_data
#' @export
#' @keywords internal
prepare_data.ctree <- function(internal, index_features = NULL, ...) {
  x_train <- internal$data$x_train
  x_explain <- internal$data$x_explain
  n_explain <- internal$parameters$n_explain
  n_MC_samples <- internal$parameters$n_MC_samples
  n_features <- internal$parameters$n_features
  ctree.mincriterion <- internal$parameters$ctree.mincriterion
  ctree.minsplit <- internal$parameters$ctree.minsplit
  ctree.minbucket <- internal$parameters$ctree.minbucket
  ctree.sample <- internal$parameters$ctree.sample
  labels <- internal$objects$feature_specs$labels

  iter <- length(internal$iter_list)

  X <- internal$iter_list[[iter]]$X


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
    mincriterion = ctree.mincriterion,
    minsplit = ctree.minsplit,
    minbucket = ctree.minbucket
  )

  for (i in seq_len(n_explain)) {
    l <- lapply(
      X = all_trees,
      FUN = sample_ctree,
      n_MC_samples = n_MC_samples,
      x_explain = x_explain[i, , drop = FALSE],
      x_train = x_train,
      n_features = n_features,
      sample = ctree.sample
    )

    dt_l[[i]] <- data.table::rbindlist(l, idcol = "id_coalition")
    dt_l[[i]][, w := 1 / n_MC_samples]
    dt_l[[i]][, id := i]
    if (!is.null(index_features)) dt_l[[i]][, id_coalition := index_features[id_coalition]]
  }

  dt <- data.table::rbindlist(dt_l, use.names = TRUE, fill = TRUE)
  dt[id_coalition %in% c(1, 2^n_features), w := 1.0]

  # only return unique dt
  dt2 <- dt[, sum(w), by = c("id_coalition", labels, "id")]
  setnames(dt2, "V1", "w")

  return(dt2)
}

#' Build all the conditional inference trees
#'
#' @param given_ind Integer vector.
#' Indicates which features are conditioned on.
#'
#' @param use_partykit String. In some semi-rare cases [party::ctree()] runs into an error related to the LINPACK
#' used by R. To get around this problem, one may fall back to using the newer (but slower) [partykit::ctree()]
#' function, which is a reimplementation of the same method. Setting this parameter to `"on_error"` (default)
#' falls back to  [partykit::ctree()], if [party::ctree()] fails. Other options are `"never"`, which always
#' uses [party::ctree()], and `"always"`, which always uses [partykit::ctree()]. A warning message is
#' created whenever [partykit::ctree()] is used.
#'
#' @inheritParams default_doc_internal
#'
#' @return List with conditional inference tree and the variables conditioned/not conditioned on.
#'
#' @details See the documentation of the [setup_approach.ctree()] function for undocumented parameters.
#'
#' @keywords internal
#' @author Annabelle Redelmeier, Martin Jullum
create_ctree <- function(given_ind,
                         x_train,
                         mincriterion,
                         minsplit,
                         minbucket,
                         use_partykit = "on_error") {
  dependent_ind <- seq_len(ncol(x_train))[-given_ind]

  if (length(given_ind) %in% c(0, ncol(x_train))) {
    datact <- list()
  } else {
    y <- x_train[, dependent_ind, with = FALSE]
    x <- x_train[, given_ind, with = FALSE]
    df <- data.table::data.table(cbind(y, x))
    colnames(df) <- c(paste0("Y", seq_len(ncol(y))), paste0("V", given_ind))

    ynam <- paste0("Y", seq_len(ncol(y)))
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
        msg1 <- paste0("{.fn party::ctree} ran into the error: ", ex)
        msg2 <- "Using {.fn partykit::ctree} instead!"
        cli::cli_warn(c("!" = msg1, " " = msg2), immediate. = TRUE)
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
      msg <- "Using {.fn partykit::ctree} instead of {.fn party::ctree}!"
      cli::cli_warn(c("!" = msg), immediate. = TRUE)
      datact <- partykit::ctree(fmla,
        data = df,
        control = partykit::ctree_control(
          minbucket = minbucket,
          mincriterion = mincriterion,
          splitstat = "maximum"
        )
      )
    } else {
      cli::cli_abort(
        paste0(
          "`use_partykit` needs to be one of 'on_error', 'never', or 'always'. ",
          "See {.fn shapr::create_ctree} for details."
        )
      )
    }
  }
  return(list(tree = datact, given_ind = given_ind, dependent_ind = dependent_ind))
}

#' Sample ctree variables from a given conditional inference tree
#'
#'
#' @param tree List. Contains tree which is an object of type ctree built from the party package.
#' Also contains given_ind, the features to condition upon.
#'
#' @param n_MC_samples Scalar integer.
#' Corresponds to the number of samples from the leaf node.
#' See an exception when sample = FALSE in [setup_approach.ctree()].
#'
#' @inheritParams default_doc_internal
#'
#' @details See the documentation of the [setup_approach.ctree()] function for undocumented parameters.
#'
#' @return data.table with `n_MC_samples` (conditional) Gaussian samples
#'
#' @keywords internal
#'
#' @author Annabelle Redelmeier
sample_ctree <- function(tree,
                         n_MC_samples,
                         x_explain,
                         x_train,
                         n_features,
                         sample) {
  datact <- tree$tree
  using_partykit <- (class(datact)[1] != "BinaryTree")

  cnms <- colnames(x_explain)
  if (length(tree$given_ind) %in% c(0, n_features)) {
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
      # xp here needs to contain the response variables as well, for some reason
      x_explain_dependent <- x_explain[,
        dependent_ind,
        drop = FALSE,
        with = FALSE
      ]

      colnames(x_explain_dependent) <- paste0("Y", seq_along(dependent_ind))
      xp2 <- cbind(xp, x_explain_dependent)

      fit.nodes <- predict(
        object = datact,
        type = "node"
      )
      # newdata must be data.frame + have the same colnames as x
      pred.nodes <- predict(
        object = datact, newdata = xp2,
        type = "node"
      )
    } else {
      fit.nodes <- party::where(object = datact)
      # newdata must be data.frame + have the same colnames as x
      pred.nodes <- party::where(object = datact, newdata = xp)
    }

    rowno <- seq_len(nrow(x_train))

    use_all_obs <- !sample & (length(rowno[fit.nodes == pred.nodes]) <= n_MC_samples)

    if (use_all_obs) {
      newrowno <- rowno[fit.nodes == pred.nodes]
    } else {
      newrowno <- sample(rowno[fit.nodes == pred.nodes], n_MC_samples,
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
