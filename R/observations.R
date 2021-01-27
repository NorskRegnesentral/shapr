#' Generate permutations of training data using test observations
#'
#' @param W_kernel Numeric matrix. Contains all nonscaled weights between training and test
#' observations for all feature combinations. The dimension equals \code{n_train x m}.
#' @param S Integer matrix of dimension \code{n_combinations x m}, where \code{n_combinations}
#' and \code{m} equals the total number of sampled/non-sampled feature combinations and
#' the total number of unique features, respectively. Note that \code{m = ncol(x_train)}.
#' @param x_train Numeric matrix
#' @param x_test Numeric matrix
#' @param w_threshold Numeric vector of length 1, where \code{w_threshold > 0} and
#' \code{w_threshold <= 1}. If \code{w_threshold = .8} we will choose the \code{K} samples with
#' the largest weight so that the sum of the weights accounts for 80\% of the total weight.
#'
#' @return data.table
#'
#' @keywords internal
#'
#' @author Nikolai Sellereite
observation_impute <- function(W_kernel, S, x_train, x_test, w_threshold = .7, n_samples = 1e3) {

  # Check input
  stopifnot(is.matrix(W_kernel) & is.matrix(S))
  stopifnot(nrow(W_kernel) == nrow(x_train))
  stopifnot(ncol(W_kernel) == nrow(S))
  stopifnot(all(S %in% c(0, 1)))
  index_s <- index_x_train <- id_combination <- weight <- w <- wcum <- NULL # due to NSE notes in R CMD check

  # Find weights for all combinations and training data
  dt <- data.table::as.data.table(W_kernel)
  nms_vec <- seq(ncol(dt))
  names(nms_vec) <- colnames(dt)
  dt[, index_x_train := .I]
  dt_melt <- data.table::melt(
    dt,
    id.vars = "index_x_train",
    variable.name = "id_combination",
    value.name = "weight",
    variable.factor = FALSE
  )
  dt_melt[, index_s := nms_vec[id_combination]]

  # Remove training data with small weight
  knms <- c("index_s", "weight")
  data.table::setkeyv(dt_melt, knms)
  dt_melt[, weight := weight / sum(weight), by = "index_s"]
  if (w_threshold < 1) {
    dt_melt[, wcum := cumsum(weight), by = "index_s"]
    dt_melt <- dt_melt[wcum > 1 - w_threshold][, wcum := NULL]
  }
  dt_melt <- dt_melt[, tail(.SD, n_samples), by = "index_s"]

  # Generate data used for prediction
  dt_p <- observation_impute_cpp(
    index_xtrain = dt_melt[["index_x_train"]],
    index_s = dt_melt[["index_s"]],
    xtrain = x_train,
    xtest = x_test,
    S = S
  )

  # Add keys
  dt_p <- data.table::as.data.table(dt_p)
  data.table::setnames(dt_p, colnames(x_train))
  dt_p[, id_combination := dt_melt[["index_s"]]]
  dt_p[, w := dt_melt[["weight"]]]

  return(dt_p)
}

#' Generate data used for predictions
#'
#' @param x Explainer object. See \code{\link{explain}} for more information.
#'
#' @param n_samples Positive integer. Indicating the maximum number of samples to use in the
#' Monte Carlo integration for every conditional expectation.
#'
#' @param seed Positive integer. If \code{NULL} the seed will be inherited from the calling environment.
#'
#' @param index_features Positive integer vector. Specifies the indices of combinations to apply to the present method.
#' \code{NULL} means all combinations. Only used internally.
#'
#' @param x_test_gaussian Matrix. Test data quantile-transformed to standard Gaussian variables. Only applicable if
#' \code{approach = "empirical"}.
#'
#' @param ... Currently not used.
#'
#' @export
#' @keywords internal
prepare_data <- function(x, ...) {
  class(x) <- x$approach
  UseMethod("prepare_data", x)
}

#' @rdname prepare_data
#' @export
prepare_data.empirical <- function(x, seed = 1, n_samples = 1e3, index_features = NULL, ...) {
  id <- id_combination <- w <- NULL # due to NSE notes in R CMD check

  # Get distance matrix ----------------
  if (is.null(index_features)) {
    index_features <- x$X[, .I]
  }

  x$D <- distance_matrix(
    x$x_train,
    x$x_test,
    x$X$features[index_features]
  )

  # Setup
  if (!is.null(seed)) set.seed(seed)
  n_col <- nrow(x$x_test)
  no_empirical <- nrow(x$S[index_features, ])

  h_optim_mat <- matrix(NA, ncol = n_col, nrow = no_empirical)
  h_optim_DT <- as.data.table(h_optim_mat)
  data.table::setnames(h_optim_DT, paste0("Testobs_", seq(nrow(x$x_test))))
  varcomb <- NULL # due to NSE notes in R CMD check
  h_optim_DT[, varcomb := .I]
  kernel_metric <- ifelse(x$type == "independence", x$type, "gaussian")

  if (kernel_metric == "independence") {
    x$w_threshold <- 1
    paste0("w_threshold force set to 1 for kernel_metric = 'independence'")
  } else if (kernel_metric == "gaussian") {
    if (x$type == "fixed_sigma") {
      h_optim_mat[, ] <- x$fixed_sigma_vec
    } else {
      if (x$type == "AICc_each_k") {
        h_optim_mat <- compute_AICc_each_k(x, h_optim_mat)
      } else if (x$type == "AICc_full") {
        h_optim_mat <- compute_AICc_full(x, h_optim_mat)
      } else {
        stop("type must be equal to 'independence', 'fixed_sigma', 'AICc_each_k' or 'AICc_full'.")
      }
    }
  }
  dt_l <- list()
  for (i in seq(n_col)) {
    D <- x$D[, i, ]
    h_optim_vec <- h_optim_mat[, i]
    h_optim_vec[is.na(h_optim_vec)] <- 1

    if (kernel_metric == "independence") {
      D <- D[sample.int(nrow(D)), ]
      h_optim_vec <- mean(D) * 1000
    }

    val <- t(t(-0.5 * D) / h_optim_vec^2)
    W_kernel <- exp(val)
    S <- x$S[index_features, ]

    ## Get imputed data
    dt_l[[i]] <- observation_impute(
      W_kernel = W_kernel,
      S = S,
      x_train = as.matrix(x$x_train),
      x_test = x$x_test[i, , drop = FALSE],
      w_threshold = x$w_threshold,
      n_samples = n_samples
    )

    dt_l[[i]][, id := i]
    if (!is.null(index_features)) dt_l[[i]][, id_combination := index_features[id_combination]]
  }

  dt <- data.table::rbindlist(dt_l, use.names = TRUE, fill = TRUE)
  return(dt)
}

#' @rdname prepare_data
#' @export
prepare_data.gaussian <- function(x, seed = 1, n_samples = 1e3, index_features = NULL, ...) {
  id <- id_combination <- w <- NULL # due to NSE notes in R CMD check

  n_xtest <- nrow(x$x_test)
  dt_l <- list()
  if (!is.null(seed)) set.seed(seed)
  if (is.null(index_features)) {
    features <- x$X$features
  } else {
    features <- x$X$features[index_features]
  }

  for (i in seq(n_xtest)) {
    l <- lapply(
      X = features,
      FUN = sample_gaussian,
      n_samples = n_samples,
      mu = x$mu,
      cov_mat = x$cov_mat,
      m = ncol(x$x_test),
      x_test = x$x_test[i, , drop = FALSE]
    )

    dt_l[[i]] <- data.table::rbindlist(l, idcol = "id_combination")
    dt_l[[i]][, w := 1 / n_samples]
    dt_l[[i]][, id := i]
    if (!is.null(index_features)) dt_l[[i]][, id_combination := index_features[id_combination]]
  }

  dt <- data.table::rbindlist(dt_l, use.names = TRUE, fill = TRUE)
  return(dt)
}

#' @rdname prepare_data
#' @export
prepare_data.copula <- function(x, x_test_gaussian = 1, seed = 1, n_samples = 1e3, index_features = NULL, ...) {
  id <- id_combination <- w <- NULL # due to NSE notes in R CMD check
  n_xtest <- nrow(x$x_test)
  dt_l <- list()
  if (!is.null(seed)) set.seed(seed)
  if (is.null(index_features)) {
    features <- x$X$features
  } else {
    features <- x$X$features[index_features]
  }

  for (i in seq(n_xtest)) {
    l <- lapply(
      X = features,
      FUN = sample_copula,
      n_samples = n_samples,
      mu = x$mu,
      cov_mat = x$cov_mat,
      m = ncol(x$x_test),
      x_test = x$x_test[i, , drop = FALSE],
      x_train = as.matrix(x$x_train),
      x_test_gaussian = x_test_gaussian[i, , drop = FALSE]
    )

    dt_l[[i]] <- data.table::rbindlist(l, idcol = "id_combination")
    dt_l[[i]][, w := 1 / n_samples]
    dt_l[[i]][, id := i]
    if (!is.null(index_features)) dt_l[[i]][, id_combination := index_features[id_combination]]
  }
  dt <- data.table::rbindlist(dt_l, use.names = TRUE, fill = TRUE)
  return(dt)
}

#' @param n_samples Integer. The number of obs to sample from the leaf if \code{sample} = TRUE or if \code{sample}
#' = FALSE but \code{n_samples} is less than the number of obs in the leaf.
#'
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
prepare_data.ctree <- function(x, seed = 1, n_samples = 1e3, index_features = NULL,
                               mc_cores = 1, mc_cores_create_ctree = mc_cores,
                               mc_cores_sample_ctree = mc_cores, ...) {
  id <- id_combination <- w <- NULL # due to NSE notes in R CMD check

  n_xtest <- nrow(x$x_test)
  dt_l <- list()

  if (!is.null(seed)) set.seed(seed)
  if (is.null(index_features)) {
    features <- x$X$features
  } else {
    features <- x$X$features[index_features]
  }


  # this is a list of all 2^M trees (where number of features = M)
  all_trees <- parallel::mclapply(
    X = features,
    FUN = create_ctree,
    x_train = x$x_train,
    mincriterion = x$mincriterion,
    minsplit = x$minsplit,
    minbucket = x$minbucket,
    mc.cores = mc_cores_create_ctree,
    mc.set.seed = FALSE
  )

  for (i in seq(n_xtest)) {
    l <- parallel::mclapply(
      X = all_trees,
      FUN = sample_ctree,
      n_samples = n_samples,
      x_test = x$x_test[i, , drop = FALSE],
      x_train = x$x_train,
      p = ncol(x$x_test),
      sample = x$sample,
      mc.cores = mc_cores_sample_ctree,
      mc.set.seed = FALSE
    )

    dt_l[[i]] <- data.table::rbindlist(l, idcol = "id_combination")
    dt_l[[i]][, w := 1 / n_samples]
    dt_l[[i]][, id := i]
    if (!is.null(index_features)) dt_l[[i]][, id_combination := index_features[id_combination]]
  }

  dt <- data.table::rbindlist(dt_l, use.names = TRUE, fill = TRUE)
  dt[id_combination %in% c(1, 2^ncol(x$x_test)), w := 1.0]

  # only return unique dt
  dt2 <- dt[, sum(w), by = c("id_combination", colnames(x$x_test), "id")]
  setnames(dt2, "V1", "w")

  return(dt2)
}


#' @keywords internal
compute_AICc_each_k <- function(x, h_optim_mat) {
  id_combination <- n_features <- NULL # due to NSE notes in R CMD check
  stopifnot(
    data.table::is.data.table(x$X),
    !is.null(x$X[["id_combination"]]),
    !is.null(x$X[["n_features"]])
  )

  optimsamp <- sample_combinations(
    ntrain = nrow(x$x_train),
    ntest = nrow(x$x_test),
    nsamples = x$n_samples_aicc,
    joint_sampling = FALSE
  )
  x$n_samples_aicc <- nrow(optimsamp)
  nloops <- nrow(x$x_test) # No of observations in test data

  # Optimization is done only once for all distributions which conditions on
  # exactly k variables
  these_k <- unique(x$X$n_features[-c(1, nrow(x$S))])

  for (i in these_k) {
    these_cond <- x$X[n_features == i, id_combination]
    cutters <- 1:x$n_samples_aicc
    no_cond <- length(these_cond)
    cond_samp <- cut(
      x = cutters,
      breaks = stats::quantile(cutters, (0:no_cond) / no_cond),
      include.lowest = TRUE,
      labels = these_cond
    )
    cond_samp <- as.numeric(levels(cond_samp))[cond_samp]

    # Loop over each observation to explain
    for (loop in 1:nloops) {
      this.optimsamp <- optimsamp
      this.optimsamp$samp_test <- loop

      j <- 1
      X_list <- X.pred.list <- mcov_list <- list()
      for (this_cond in unique(cond_samp)) {
        these_inds <- which(cond_samp == this_cond)
        these_train <- this.optimsamp$samp_train[these_inds]
        these_test <- this.optimsamp$samp_test[these_inds]

        these_train <- 1:nrow(x$x_train)
        these_test <- sample(x = these_test, size = nrow(x$x_train), replace = TRUE)
        current_cond_samp <- rep(unique(cond_samp), each = nrow(x$x_train))

        S <- x$S[this_cond, ]
        S.cols <- which(as.logical(S))
        Sbar.cols <- which(as.logical(1 - S))

        X_list[[j]] <- as.matrix(subset(x$x_train, select = S.cols)[these_train, ])
        mcov_list[[j]] <- stats::cov(X_list[[j]])

        Xtrain.Sbar <- subset(x$x_train, select = Sbar.cols)[these_train, ]
        Xtest.S <- subset(x$x_test, select = S.cols)[these_test, ]
        X.pred.list[[j]] <- cbind(Xtrain.Sbar, Xtest.S)

        # Ensure colnames are correct:
        varname <- colnames(x$x_train)[-which(colnames(x$x_train) %in% colnames(Xtrain.Sbar))]
        colnames(X.pred.list[[j]]) <- c(colnames(Xtrain.Sbar), varname)

        j <- j + 1
      }
      # Combining the X's for doing prediction
      X.pred <- rbindlist(X.pred.list, use.names = T)
      X.nms <- colnames(x$x_train)
      setcolorder(X.pred, X.nms)
      # Doing prediction jointly (for speed), and then splitting them back into the y_list
      pred <- predict_model(x$model, X.pred)
      y_list <- split(pred, current_cond_samp)
      names(y_list) <- NULL
      ## Doing the numerical optimization -------
      nlm.obj <- suppressWarnings(stats::nlminb(
        start = x$start_aicc,
        objective = aicc_full_cpp,
        X_list = X_list,
        mcov_list = mcov_list,
        S_scale_dist = T,
        y_list = y_list,
        negative = F,
        lower = 0,
        control = list(
          eval.max = x$eval_max_aicc
        )
      ))
      h_optim_mat[these_cond, loop] <- nlm.obj$par
    }
  }
  return(h_optim_mat)
}


#' @keywords internal
compute_AICc_full <- function(x, h_optim_mat) {
  ntest <- nrow(x$x_test)
  if (is.null(dim(x$x_test))) {
    nloops <- 1
    ntest <- 1
  }
  optimsamp <- sample_combinations(
    ntrain = nrow(x$x_train),
    ntest = ntest,
    nsamples = x$n_samples_aicc,
    joint_sampling = FALSE
  )
  x$n_samples_aicc <- nrow(optimsamp)
  nloops <- nrow(x$x_test) # No of observations in test data

  ind_of_vars_to_cond_on <- 2:(nrow(x$S) - 1)
  for (i in ind_of_vars_to_cond_on) {
    S <- x$S[i, ]
    S.cols <- which(as.logical(S))
    Sbar.cols <- which(as.logical(1 - S))

    # Loop over each observation to explain:
    for (loop in 1:nloops) {
      this.optimsamp <- optimsamp
      this.optimsamp$samp_test <- loop

      these_train <- this.optimsamp$samp_train
      these_test <- this.optimsamp$samp_test

      these_train <- 1:nrow(x$x_train)
      these_test <- sample(x = these_test, size = nrow(x$x_train), replace = T)

      X_list <- list(as.matrix(subset(x$x_train, select = S.cols)[these_train, ]))
      mcov_list <- list(stats::cov(X_list[[1]]))

      Xtrain.Sbar <- subset(x$x_train, select = Sbar.cols)[these_train, ]
      Xtest.S <- subset(x$x_test, select = S.cols)[these_test, ]
      X.pred <- cbind(Xtrain.Sbar, Xtest.S)

      # Ensure colnames are correct:
      varname <- colnames(x$x_train)[-which(colnames(x$x_train) %in% colnames(Xtrain.Sbar))]
      colnames(X.pred) <- c(colnames(Xtrain.Sbar), varname)

      X.nms <- colnames(x$x_train)
      setcolorder(X.pred, X.nms)

      pred <- predict_model(x$model, X.pred)
      y_list <- list(pred)

      ## Running the nonlinear optimization
      nlm.obj <- suppressWarnings(stats::nlminb(
        start = x$start_aicc,
        objective = aicc_full_cpp,
        X_list = X_list,
        mcov_list = mcov_list,
        S_scale_dist = T,
        y_list = y_list,
        negative = F,
        lower = 0,
        control = list(
          eval.max = x$eval_max_aicc
        )
      ))


      h_optim_mat[i, loop] <- nlm.obj$par
    }
  }
  return(h_optim_mat)
}
