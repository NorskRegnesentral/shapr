#' @rdname setup_approach
#'
#' @param empirical.type Character. (default = `"fixed_sigma"`)
#' Should be equal to either `"independence"`,`"fixed_sigma"`, `"AICc_each_k"` `"AICc_full"`.
#' TODO: Describe better what the methods do here.
#'
#' @param empirical.eta Numeric. (default = 0.95)
#' Needs to be `0 < eta <= 1`.
#' Represents the minimum proportion of the total empirical weight that data samples should use.
#' If e.g. `eta = .8` we will choose the `K` samples with the largest weight so that the sum of the weights
#' accounts for 80\% of the total weight.
#' `eta` is the \eqn{\eta} parameter in equation (15) of Aas et al (2021).
#'
#' @param empirical.fixed_sigma Positive numeric scalar. (default = 0.1)
#' Represents the kernel bandwidth in the distance computation used when conditioning on all different combinations.
#' Only used when `empirical.type = "fixed_sigma"`
#'
#' @param empirical.n_samples_aicc Positive integer. (default = 1000)
#' Number of samples to consider in AICc optimization.
#' Only used for `empirical.type` is either `"AICc_each_k"` or `"AICc_full"`.
#'
#' @param empirical.eval_max_aicc Positive integer. (default = 20)
#' Maximum number of iterations when optimizing the AICc.
#' Only used for `empirical.type` is either `"AICc_each_k"` or `"AICc_full"`.
#'
#' @param empirical.start_aicc Numeric. (default = 0.1)
#' Start value of the `sigma` parameter when optimizing the AICc.
#' Only used for `empirical.type` is either `"AICc_each_k"` or `"AICc_full"`.
#'
#'
#' @param empirical.cov_mat Numeric matrix. (Optional, default = NULL)
#' Containing the covariance matrix of the data generating distribution used to define the Mahalanobis distance.
#' `NULL` means it is estimated from `x_train`.
#'
#' @inheritParams default_doc_explain
#' @inheritParams default_doc
#'
#' @export
setup_approach.empirical <- function(internal,
                                     empirical.type = "fixed_sigma",
                                     empirical.eta = 0.95,
                                     empirical.fixed_sigma = 0.1,
                                     empirical.n_samples_aicc = 1000,
                                     empirical.eval_max_aicc = 20,
                                     empirical.start_aicc = 0.1,
                                     empirical.cov_mat = NULL,
                                     model = NULL,
                                     predict_model = NULL, ...) {
  # TODO: Can I avoid passing model and predict_model (using ...) as they clutter the help file

  defaults <- mget(c(
    "empirical.eta", "empirical.type", "empirical.fixed_sigma",
    "empirical.n_samples_aicc", "empirical.eval_max_aicc", "empirical.start_aicc"
  ))

  internal <- insert_defaults(internal, defaults)

  # Checking if factor features are present
  feature_specs <- internal$objects$feature_specs
  if (any(feature_specs$classes == "factor")) {
    factor_features <- names(which(feature_specs$classes == "factor"))
    factor_approaches <- get_factor_approaches()
    stop(
      paste0(
        "The following feature(s) are factor(s): ", factor_features, ".\n",
        "approach = 'empirical' does not support factor features.\n",
        "Please change approach to one of ", paste0(factor_approaches, collapse = ", "), "."
      )
    )
  }


  if (internal$parameters$empirical.type == "independence") {
    warning(paste0(
      "Using empirical.type = 'independence' for approach = 'empirical' is deprecated.\n",
      "Please use approach = 'independence' instead."
    ))
  }

  if (internal$parameters$empirical.type %in% c("AICc_each_k", "AICc_full") && internal$parameters$is_python == TRUE) {
    stop(paste0(
      "empirical.type = ", internal$parameters$empirical.type,
      " for approach = 'empirical' is not available in Python.\n",
    ))
  }

  if (!(length(internal$parameters$empirical.fixed_sigma) == 1 &&
    is.numeric(internal$parameters$empirical.fixed_sigma) &&
    internal$parameters$empirical.fixed_sigma > 0)) {
    stop(
      "empirical.fixed_sigma must be a positive numeric of length 1.\n"
    )
  }


  x_train <- internal$data$x_train

  # If empirical.cov_mat is not provided directly, use sample covariance of training data
  if (is.null(empirical.cov_mat)) {
    internal$parameters$empirical.cov_mat <- get_cov_mat(x_train)
  }

  internal$tmp <- list(
    model = model,
    predict_model = predict_model
  )

  return(internal)
}


#' @rdname prepare_data
#' @export
prepare_data.empirical <- function(internal, index_features = NULL, ...) {
  x_train <- internal$data$x_train
  x_explain <- internal$data$x_explain

  empirical.cov_mat <- internal$parameters$empirical.cov_mat
  X <- internal$objects$X
  S <- internal$objects$S

  n_explain <- internal$parameters$n_explain
  empirical.type <- internal$parameters$empirical.type
  empirical.eta <- internal$parameters$empirical.eta
  empirical.fixed_sigma <- internal$parameters$empirical.fixed_sigma
  n_samples <- internal$parameters$n_samples

  model <- internal$tmp$model
  predict_model <- internal$tmp$predict_model


  if (is.null(index_features)) {
    index_features <- X[, .I]
  }

  # Get distance matrix ----------------
  D <- distance_matrix(
    x_train,
    x_explain,
    X$features[index_features],
    mcov = empirical.cov_mat
  )


  # Setup
  n_col <- nrow(x_explain)
  no_empirical <- nrow(S[index_features, , drop = FALSE])


  kernel_metric <- ifelse(empirical.type == "independence", empirical.type, "gaussian")

  S0 <- S[index_features, , drop = FALSE]


  # Increased efficiency for simplest and most common use case
  if (kernel_metric == "gaussian" && empirical.type == "fixed_sigma" && length(empirical.fixed_sigma) == 1) {
    W_kernel_full <- exp(-0.5 * D / empirical.fixed_sigma^2)
    dt_l <- list()

    for (i in seq(n_col)) {
      ## Get imputed data
      dt_l[[i]] <- observation_impute(
        W_kernel = as.matrix(W_kernel_full[, i, ]),
        S = S0,
        x_train = as.matrix(x_train),
        x_explain = as.matrix(x_explain[i, , drop = FALSE]),
        empirical.eta = empirical.eta,
        n_samples = n_samples
      )

      dt_l[[i]][, id := i]
      if (!is.null(index_features)) dt_l[[i]][, id_combination := index_features[id_combination]]
    }
  } else {
    h_optim_mat <- matrix(NA, ncol = n_col, nrow = no_empirical)


    if (kernel_metric == "independence") {
      empirical.eta <- 1
      message(
        "\nSuccess with message:\nempirical.eta force set to 1 for empirical.type = 'independence'"
      )
    } else if (kernel_metric == "gaussian") {
      if (empirical.type == "fixed_sigma") {
        h_optim_mat[, ] <- empirical.fixed_sigma
      } else {
        if (empirical.type == "AICc_each_k") {
          h_optim_mat <- compute_AICc_each_k(internal, model, predict_model, index_features)
        } else if (empirical.type == "AICc_full") {
          h_optim_mat <- compute_AICc_full(internal, model, predict_model, index_features)
        } else {
          stop("empirical.type must be equal to 'independence', 'fixed_sigma', 'AICc_each_k' or 'AICc_full'.")
        }
      }
    }
    dt_l <- list()
    for (i in seq(n_col)) {
      D0 <- D[, i, ]
      h_optim_vec <- h_optim_mat[, i]
      h_optim_vec[is.na(h_optim_vec)] <- 1

      if (kernel_metric == "independence") {
        D0 <- D0[sample.int(nrow(D)), ] + stats::runif(n = nrow(D) * ncol(D))
        h_optim_vec <- mean(D) * 1000
      }

      val <- t(t(-0.5 * D0) / h_optim_vec^2)
      W_kernel <- exp(val)

      ## Get imputed data
      dt_l[[i]] <- observation_impute(
        W_kernel = W_kernel,
        S = S0,
        x_train = as.matrix(x_train),
        x_explain = as.matrix(x_explain[i, , drop = FALSE]),
        empirical.eta = empirical.eta,
        n_samples = n_samples
      )

      dt_l[[i]][, id := i]
      if (!is.null(index_features)) dt_l[[i]][, id_combination := index_features[id_combination]]
    }
  }

  dt <- data.table::rbindlist(dt_l, use.names = TRUE, fill = TRUE)

  # Clear out objects created by Rcpp functions. For some unknown reason this reduces memory usage when n_batches>1
  rm(D)
  rm(dt_l)
  gc()

  return(dt)
}

#' Generate permutations of training data using test observations
#'
#' @param W_kernel Numeric matrix. Contains all nonscaled weights between training and test
#' observations for all feature combinations. The dimension equals `n_train x m`.
#' @param S Integer matrix of dimension `n_combinations x m`, where `n_combinations`
#' and `m` equals the total number of sampled/non-sampled feature combinations and
#' the total number of unique features, respectively. Note that `m = ncol(x_train)`.
#' @param x_train Numeric matrix
#' @param x_explain Numeric matrix
#'
#' @inheritParams explain
#'
#' @return data.table
#'
#' @keywords internal
#'
#' @author Nikolai Sellereite
observation_impute <- function(W_kernel, S, x_train, x_explain, empirical.eta = .7, n_samples = 1e3) {
  # Check input
  stopifnot(is.matrix(W_kernel) & is.matrix(S))
  stopifnot(nrow(W_kernel) == nrow(x_train))
  stopifnot(ncol(W_kernel) == nrow(S))
  stopifnot(all(S %in% c(0, 1)))
  index_s <- index_x_train <- id_combination <- weight <- w <- wcum <- NULL # due to NSE notes in R CMD check

  # Find weights for all combinations and training data
  dt <- data.table::as.data.table(W_kernel)
  nms_vec <- seq_len(ncol(dt))
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
  if (empirical.eta < 1) {
    dt_melt[, wcum := cumsum(weight), by = "index_s"]
    dt_melt <- dt_melt[wcum > 1 - empirical.eta][, wcum := NULL]
  }
  dt_melt <- dt_melt[, tail(.SD, n_samples), by = "index_s"]

  # Generate data used for prediction
  dt_p <- observation_impute_cpp(
    index_xtrain = dt_melt[["index_x_train"]],
    index_s = dt_melt[["index_s"]],
    xtrain = x_train,
    xtest = x_explain, # TODO: change this to xexplain
    S = S
  )

  # Add keys
  dt_p <- data.table::as.data.table(dt_p)
  data.table::setnames(dt_p, colnames(x_train))
  dt_p[, id_combination := dt_melt[["index_s"]]]
  dt_p[, w := dt_melt[["weight"]]]

  return(dt_p)
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
#' if `nsamples > ntrain`. Note that this solution is not optimal. Be careful if you're
#' doing optimization over every test observation when `nsamples > ntrain`.
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


#' @keywords internal
compute_AICc_each_k <- function(internal, model, predict_model, index_features) {
  x_train <- internal$data$x_train
  x_explain <- internal$data$x_explain
  n_train <- internal$parameters$n_train
  n_explain <- internal$parameters$n_explain
  empirical.n_samples_aicc <- internal$parameters$empirical.n_samples_aicc
  n_combinations <- internal$parameters$n_combinations
  n_features <- internal$parameters$n_features
  labels <- internal$objects$feature_specs$labels
  empirical.start_aicc <- internal$parameters$empirical.start_aicc
  empirical.eval_max_aicc <- internal$parameters$empirical.eval_max_aicc

  X <- internal$objects$X
  S <- internal$objects$S

  stopifnot(
    data.table::is.data.table(X),
    !is.null(X[["id_combination"]]),
    !is.null(X[["n_features"]])
  )

  optimsamp <- sample_combinations(
    ntrain = n_train,
    ntest = n_explain,
    nsamples = empirical.n_samples_aicc,
    joint_sampling = FALSE
  )
  empirical.n_samples_aicc <- nrow(optimsamp)
  nloops <- n_explain # No of observations in test data

  h_optim_mat <- matrix(NA, ncol = n_features, nrow = n_combinations)

  if (is.null(index_features)) {
    index_features <- X[, .I]
  }

  # Optimization is done only once for all distributions which conditions on
  # exactly k variables
  these_k <- unique(X[, n_features[index_features]])

  for (i in these_k) {
    these_cond <- X[index_features][n_features == i, id_combination]
    cutters <- seq_len(empirical.n_samples_aicc)
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

        these_train <- seq_len(n_train)
        these_test <- sample(x = these_test, size = n_train, replace = TRUE)
        current_cond_samp <- rep(unique(cond_samp), each = n_train)

        this_S <- S[this_cond, ]
        S.cols <- which(as.logical(this_S))
        Sbar.cols <- which(as.logical(1 - this_S))

        X_list[[j]] <- as.matrix(subset(x_train, select = S.cols)[these_train, ])
        mcov_list[[j]] <- stats::cov(X_list[[j]])

        Xtrain.Sbar <- subset(x_train, select = Sbar.cols)[these_train, ]
        Xtest.S <- subset(x_explain, select = S.cols)[these_test, ]
        X.pred.list[[j]] <- cbind(Xtrain.Sbar, Xtest.S)

        # Ensure colnames are correct:
        varname <- labels[-which(labels %in% colnames(Xtrain.Sbar))]
        colnames(X.pred.list[[j]]) <- c(colnames(Xtrain.Sbar), varname)

        j <- j + 1
      }
      # Combining the X's for doing prediction
      X.pred <- rbindlist(X.pred.list, use.names = TRUE)
      X.nms <- labels
      setcolorder(X.pred, X.nms)
      # Doing prediction jointly (for speed), and then splitting them back into the y_list
      pred <- predict_model(model, X.pred)
      y_list <- split(pred, current_cond_samp)
      names(y_list) <- NULL
      ## Doing the numerical optimization -------
      nlm.obj <- suppressWarnings(stats::nlminb(
        start = empirical.start_aicc,
        objective = aicc_full_cpp,
        X_list = X_list,
        mcov_list = mcov_list,
        S_scale_dist = TRUE,
        y_list = y_list,
        negative = FALSE,
        lower = 0,
        control = list(
          eval.max = empirical.eval_max_aicc
        )
      ))
      h_optim_mat[these_cond, loop] <- nlm.obj$par
    }
  }
  return(h_optim_mat[index_features, , drop = FALSE])
}


#' @keywords internal
compute_AICc_full <- function(internal, model, predict_model, index_features) {
  x_train <- internal$data$x_train
  x_explain <- internal$data$x_explain
  n_train <- internal$parameters$n_train
  n_explain <- internal$parameters$n_explain
  empirical.n_samples_aicc <- internal$parameters$empirical.n_samples_aicc
  n_combinations <- internal$parameters$n_combinations
  n_features <- internal$parameters$n_features
  labels <- internal$objects$feature_specs$labels
  empirical.start_aicc <- internal$parameters$empirical.start_aicc
  empirical.eval_max_aicc <- internal$parameters$empirical.eval_max_aicc

  X <- internal$objects$X
  S <- internal$objects$S


  ntest <- n_explain
  if (is.null(dim(x_explain))) {
    nloops <- 1
    ntest <- 1
  }
  optimsamp <- sample_combinations(
    ntrain = n_train,
    ntest = ntest,
    nsamples = empirical.n_samples_aicc,
    joint_sampling = FALSE
  )
  nloops <- n_explain # No of observations in test data

  h_optim_mat <- matrix(NA, ncol = n_features, nrow = n_combinations)

  if (is.null(index_features)) {
    index_features <- X[, .I]
  }


  ind_of_vars_to_cond_on <- index_features
  for (i in ind_of_vars_to_cond_on) {
    S0 <- S[i, ]
    S.cols <- which(as.logical(S0))
    Sbar.cols <- which(as.logical(1 - S0))

    # Loop over each observation to explain:
    for (loop in 1:nloops) {
      this.optimsamp <- optimsamp
      this.optimsamp$samp_test <- loop

      these_train <- this.optimsamp$samp_train
      these_test <- this.optimsamp$samp_test

      these_train <- seq_len(n_train)
      these_test <- sample(x = these_test, size = n_train, replace = TRUE)

      X_list <- list(as.matrix(subset(x_train, select = S.cols)[these_train, ]))
      mcov_list <- list(stats::cov(X_list[[1]]))

      Xtrain.Sbar <- subset(x_train, select = Sbar.cols)[these_train, ]
      Xtest.S <- subset(x_explain, select = S.cols)[these_test, ]
      X.pred <- cbind(Xtrain.Sbar, Xtest.S)

      # Ensure colnames are correct:
      varname <- labels[-which(labels %in% colnames(Xtrain.Sbar))]
      colnames(X.pred) <- c(colnames(Xtrain.Sbar), varname)

      X.nms <- labels
      setcolorder(X.pred, X.nms)

      pred <- predict_model(model, X.pred)
      y_list <- list(pred)

      ## Running the nonlinear optimization
      nlm.obj <- suppressWarnings(stats::nlminb(
        start = empirical.start_aicc,
        objective = aicc_full_cpp,
        X_list = X_list,
        mcov_list = mcov_list,
        S_scale_dist = TRUE,
        y_list = y_list,
        negative = FALSE,
        lower = 0,
        control = list(
          eval.max = empirical.eval_max_aicc
        )
      ))


      h_optim_mat[i, loop] <- nlm.obj$par
    }
  }
  return(h_optim_mat[index_features, , drop = FALSE])
}

#' @keywords internal
distance_matrix <- function(x_train, x_explain = NULL, list_features, mcov) {
  if (is.null(x_explain)) {
    return(NULL)
  }

  if (is.null(dim(x_explain))) {
    x_explain <- t(as.matrix(x_explain))
  }

  # Note that D equals D_S(,)^2 in the paper
  D <- mahalanobis_distance_cpp(
    featureList = list_features,
    Xtrain_mat = as.matrix(x_train),
    Xtest_mat = as.matrix(x_explain),
    mcov = mcov,
    S_scale_dist = TRUE
  )


  # Normalize distance rows to ensure numerical stability in later operations
  colmin <- apply(X = D, MARGIN = c(2, 3), FUN = min)
  for (i in seq_len(dim(D)[3])) {
    D[, , i] <- t(t(D[, , i]) - colmin[, i])
  }

  return(D)
}
