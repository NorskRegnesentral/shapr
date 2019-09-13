#' Get predictions
#'
#' @inheritParams global_arguments
#'
#' @return List
#'
#' @export
#'
#' @author Nikolai Sellereite
observation_impute <- function(W_kernel, S, Xtrain, Xtest, w_threshold = .7, noSamp_MC = 1e3) {

  ## Find weights for all combinations and training data
  DT <- as.data.table(W_kernel)
  DT[, ID := .I]
  DT <- data.table::melt(data = DT, id.vars = "ID", variable.name = "comb", value.name = "w", variable.factor = FALSE)

  ## Remove training data with small weight
  setkey(DT, comb, w)
  DT[, w := w / sum(w), comb]
  DT[, wcum := cumsum(w), comb]
  DT <- DT[wcum > 1 - w_threshold][, wcum := NULL]
  DT <- DT[, tail(.SD, noSamp_MC), comb]
  DT[, comb := gsub(comb, pattern = "V", replacement = ""), comb]
  DT[, wcomb := as.integer(comb), comb][, comb := NULL]

  ## Generate data used for prediction
  DTp <- observation_impute_cpp(
    index_xtrain = DT[["ID"]],
    index_s = DT[["wcomb"]],
    xtrain = Xtrain,
    xtest = Xtest,
    S = S
  )

  ## Add keys
  DTp <- as.data.table(DTp)
  setnames(DTp, colnames(Xtrain))
  DTp[, wcomb := DT[["wcomb"]]]
  DTp[, w := DT[["w"]]]

  return(DTp)
}

#' Generate data used for predictions
#' @name prepare_data
#' @export
prepare_data <- function(x, ...) {
  class(x) <- x$approach
  UseMethod("prepare_data", x)
}

#' @rdname prepare_data
#' @name prepare_data
#' @export
prepare_data.empirical <- function(x, seed = 1, n_samples = 1e3, index_features = NULL, ...) {

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
  n_col <- nrow(x$x_test)
  no_empirical <- nrow(x$S[index_features, ])

  h_optim_mat <- matrix(NA, ncol = n_col, nrow = no_empirical)
  h_optim_DT <- as.data.table(h_optim_mat)
  data.table::setnames(h_optim_DT, paste0("Testobs_", seq(nrow(x$x_test))))
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
      D <- D[sample(nrow(D)), ]
      h_optim_vec <- mean(D) * 1000
    }

    val <- t(t(-0.5 * D) / h_optim_vec^2)
    W_kernel <- exp(val)
    S <- x$S[index_features, ]

    ## Get imputed data
    dt_l[[i]] <- observation_impute(
      W_kernel = W_kernel,
      S = S,
      Xtrain = as.matrix(x$x_train),
      Xtest = x$x_test[i, , drop = FALSE],
      w_threshold = x$w_threshold,
      noSamp_MC = n_samples
    )

    dt_l[[i]][, id := i]
    if (!is.null(index_features)) dt_l[[i]][, wcomb := index_features[wcomb]]
  }

  dt <- data.table::rbindlist(dt_l, use.names = TRUE, fill = TRUE)
  dt[wcomb %in% c(1, 2^ncol(x$x_test)), w := 1.0]
  return(dt)
}

#' @rdname prepare_data
#' @name prepare_data
#' @export
prepare_data.gaussian <- function(x, seed = 1, n_samples = 1e3, index_features = NULL, ...) {

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
      noSamp_MC = n_samples,
      mu = x$mu,
      Sigma = x$cov_mat,
      p = ncol(x$x_test),
      Xtest = x$x_test[i, , drop = FALSE],
      ensure_condcov_symmetry = FALSE
    )

    dt_l[[i]] <- data.table::rbindlist(l, idcol = "wcomb")
    dt_l[[i]][, w := 1 / n_samples]
    dt_l[[i]][, id := i]
    if (!is.null(index_features)) dt_l[[i]][, wcomb := index_features[wcomb]]
  }
  dt <- data.table::rbindlist(dt_l, use.names = TRUE, fill = TRUE)
  dt[wcomb %in% c(1, 2^ncol(x$x_test)), w := 1.0]
  return(dt)
}

#' @rdname prepare_data
#' @name prepare_data
#' @export
prepare_data.copula <- function(x, x_test = 1, seed = 1, n_samples = 1e3, index_features = NULL, ...) {

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
      noSamp_MC = n_samples,
      mu = x$mu,
      Sigma = x$cov_mat,
      p = ncol(x$x_test),
      Xtest = x$x_test[i, , drop = FALSE],
      Xtrain = as.matrix(x$x_train),
      Xtest_Gauss_trans = x_test[i, , drop = FALSE]
    )

    dt_l[[i]] <- data.table::rbindlist(l, idcol = "wcomb")
    dt_l[[i]][, w := 1 / n_samples]
    dt_l[[i]][, id := i]
    if (!is.null(index_features)) dt_l[[i]][, wcomb := index_features[wcomb]]
  }
  dt <- data.table::rbindlist(dt_l, use.names = TRUE, fill = TRUE)
  dt[wcomb %in% c(1, 2^ncol(x$x_test)), w := 1.0]
  return(dt)
}

#' @export
compute_AICc_each_k <- function(x, h_optim_mat) {
  optimsamp <- sample_combinations(
    ntrain = nrow(x$x_train),
    ntest = nrow(x$x_test),
    nsamples = x$AICc_no_samp_per_optim,
    joint_sampling = FALSE
  )
  x$AICc_no_samp_per_optim <- nrow(optimsamp)
  nloops <- nrow(x$x_test) # No of observations in test data

  # Optimization is done only once for all distributions which conditions on
  # exactly k variables
  these_k <- unique(x$X$nfeatures[-c(1, nrow(x$S))])

  for (i in these_k) {
    these_cond <- x$X[nfeatures == i, ID]
    cutters <- 1:x$AICc_no_samp_per_optim
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
        these_test <- sample(x = these_test, size = nrow(x$x_train), replace = T)
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
        start = x$AIC_optim_startval,
        objective = aicc_full_cpp,
        X_list = X_list,
        mcov_list = mcov_list,
        S_scale_dist = T,
        y_list = y_list,
        negative = F,
        lower = 0,
        control = list(
          eval.max = x$AIC_optim_max_eval
        )
      ))
      h_optim_mat[these_cond, loop] <- nlm.obj$par
    }
  }
  return(h_optim_mat)
}


#' @export
compute_AICc_full <- function(x, h_optim_mat) {
  ntest <- nrow(x$x_test)
  if (is.null(dim(x$x_test))) {
    nloops <- 1
    ntest <- 1
  }
  optimsamp <- sample_combinations(
    ntrain = nrow(x$x_train),
    ntest = ntest,
    nsamples = x$AICc_no_samp_per_optim,
    joint_sampling = FALSE
  )
  x$AICc_no_samp_per_optim <- nrow(optimsamp)
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
        start = x$AIC_optim_startval,
        objective = aicc_full_cpp,
        X_list = X_list,
        mcov_list = mcov_list,
        S_scale_dist = T,
        y_list = y_list,
        negative = F,
        lower = 0,
        control = list(
          eval.max = x$AIC_optim_max_eval
        )
      ))


      h_optim_mat[i, loop] <- nlm.obj$par
    }
  }
  return(h_optim_mat)
}
