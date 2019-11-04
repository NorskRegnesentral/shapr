#' Calculate Shapley weight
#'
#' @inheritParams global_arguments
#'
#' @return Numeric
#'
#' @export
#'
#' @author Nikolai Sellereite
shapley_weights <- function(m, N, s, weight_zero_m = 10^6) {
  x <- (m - 1) / (N * s * (m - s))
  x[!is.finite(x)] <- weight_zero_m
  x
}

#' Calculate weighted matrix
#'
#' @param X data.table
#'
#' @return Matrix
#'
#' @export
#'
#' @author Nikolai Sellereite, Martin Jullum
weight_matrix <- function(X, use_shapley_weights_in_W = TRUE, normalize_W_weights = TRUE) {
  if (use_shapley_weights_in_W) {
    w <- X[["shapley_weight"]] * X[["no"]]
  } else {
    w <- X[["no"]]
    w[c(1, length(w))] <- X[["shapley_weight"]][c(1, length(w))]
  }

  if (normalize_W_weights) {
    w[-c(1, length(w))] <- w[-c(1, length(w))] / sum(w[-c(1, length(w))])
  }

  W <- weight_matrix_cpp(
    features = X[["features"]],
    m = X[.N][["nfeatures"]],
    n = X[, .N],
    w = w
  )

  return(W)
}


#' Create an explainer object with Shapley weights for test data.
#'
#' @inheritParams global_arguments
#'
#' @param x An \code{ntrain x p} numeric matrix or data.frame, where \code{p = ncol(x)} (total number of explanatory variables).Contains the variables used for training the model
#' (i.e. the explanatory variables). Note that the response variable should not be part of
#' \code{x}.
#'
#' @param model The model whose predictions we want to explain.
#'
#' @param n_combinations Integer. The number of feature combinations to sample. If \code{NULL},
#' the exact method is used and all combinations are considered. The maximum number of
#' combinations equals \code{2^p}.
#'
#' @return A list to be used by \code{explain} to compute the kernel SHAP values (\code{Kshap}).
#'
#' @export
#'
#' @author Nikolai Sellereite
#'
shapr <- function(x,
                  model,
                  n_combinations = NULL) {

  # Checks input argument
  if (!is.matrix(x) & !is.data.frame(x)) {
    stop("x should be a matrix or a dataframe.")
  }

  # Setup
  explainer <- as.list(environment())
  explainer$exact <- ifelse(is.null(n_combinations), TRUE, FALSE)
  explainer$n_features <- ncol(x)
  explainer$model_type <- model_type(model)

  # Checks model and features
  explainer$p <- predict_model(model, head(x))

  # Converts to data.table, otherwise copy to x_train  --------------
  x_train <- data.table::as.data.table(x)

  # Get all combinations ----------------
  dt_combinations <- feature_combinations(
    m = explainer$n_features,
    exact = explainer$exact,
    n_combinations = n_combinations,
    shapley_weight_inf_replacement = 10^6,
    reduce_dim = TRUE
  )

  # Get weighted matrix ----------------
  weighted_mat <- weight_matrix(
    X = dt_combinations,
    use_shapley_weights_in_W = ifelse(explainer$exact, TRUE, FALSE),
    normalize_W_weights = TRUE
  )

  ## Get feature matrix ---------
  feature_matrix <- feature_matrix_cpp(
    features = dt_combinations[["features"]],
    nfeatures = explainer$n_features
  )

  explainer$S <- feature_matrix
  explainer$W <- weighted_mat
  explainer$X <- dt_combinations
  explainer$x_train <- x_train
  explainer$x <- NULL
  explainer$p <- NULL

  attr(explainer, "class") <- c("explainer", "list")

  return(explainer)
}

#' @keywords internal
distance_matrix <- function(x_train, x_test = NULL, list_features) {
  if (is.null(x_test)) return(NULL)

  # Get covariance matrix
  mcov <- stats::cov(x_train)
  if (is.null(dim(x_test))) {
    x_test <- t(as.matrix(x_test))
  }
  # Note that D equals D_S(,)^2 in the paper
  D <- mahalanobis_distance_cpp(
    featureList = list_features,
    Xtrain_mat = as.matrix(x_train),
    Xtest_mat = as.matrix(x_test),
    mcov = mcov,
    S_scale_dist = TRUE
  )

  # Normalize distance rows to ensure numerical stability in later operations
  colmin <- apply(X = D, MARGIN = c(2, 3), FUN = min)
  for (i in 1:dim(D)[3]) {
    D[, , i] <- t(t(D[, , i]) - colmin[, i])
  }

  return(D)
}

#' Note that this function is deprecated, but we'll keep it for a week
#' to check that results are stable.
#'
#' TODO: Delete this function from the codebase
#'
#' @keywords internal
#'
#' @export
compute_kshap <- function(model,
                          l,
                          noSamp_MC = 1e3,
                          verbose = FALSE,
                          cond_approach = "empirical",
                          empirical_settings = list(
                            type = "fixed_sigma",
                            fixed_sigma_vec = 0.1,
                            n_samples_aicc = 1000,
                            eval_max_aicc = 20,
                            start_aicc = 0.1,
                            w_threshold = 0.95
                          ),
                          pred_zero,
                          mu = NULL,
                          Sigma = NULL) {
  tt <- proc.time()

  ll <- list()

  if (is.character(cond_approach)) {
    cond_approach_list <- list(1:nrow(l$S))
    names(cond_approach_list) <- cond_approach
  }
  if (is.list(cond_approach)) {
    cond_approach_list <- cond_approach
  }

  if ("empirical" %in% names(cond_approach_list)) {
    these_empirical <- cond_approach_list$empirical

    exclude_emp <- (these_empirical %in% c(1, nrow(l$S)))

    these_empirical <- these_empirical[!exclude_emp]

    no_empirical <- length(these_empirical)
    h_optim_mat <- matrix(NA, ncol = nrow(l$Xtest), nrow = no_empirical) # Each test observation has one column

    # Checking whether any of the distance are not pre-computed.
    if (any(!(these_empirical %in% l$D_for_these_varcomb))) {
      str_msg <- paste0(these_empirical[!(these_empirical %in% l$D_for_these_varcomb)], collapse = ", ")
      paste0("Distance not pre-computed for varcomb ", str_msg)
    }

    # Reducing and re-ordering the D-array
    l$D <- l$D[, , match(these_empirical, l$D_for_these_varcomb)]
    # Note that the D-array corresponds to exactly the covariate combinations specified in
    # these_empirical


    if (empirical_settings$type == "independence") {
      kernel_metric <- "independence"
      empirical_settings$w_threshold <- 1
      paste0("empirical_settings$w_threshold force set to 1 for empirical_settings$type = 'independence'")
    } else {
      kernel_metric <- "Gaussian"

      if (empirical_settings$type == "fixed_sigma") {
        if (length(empirical_settings$fixed_sigma_vec) == 1) {
          empirical_settings$fixed_sigma_vec <- rep(empirical_settings$fixed_sigma_vec, no_empirical)
        } else {
          empirical_settings$fixed_sigma_vec <- empirical_settings$fixed_sigma_vec[!exclude_emp]
        }

        h_optim_mat[, ] <- empirical_settings$fixed_sigma_vec
      } else {

        # Procedure for sampling a combination of an index in the training and the test sets
        optimsamp <- sample_combinations(
          ntrain = nrow(l$Xtrain),
          ntest = nrow(l$Xtest),
          nsamples = empirical_settings$n_samples_aicc,
          joint_sampling = FALSE
        )

        # Updating parameter (only if it is larger than nTrain*nTest)
        empirical_settings$n_samples_aicc <- nrow(optimsamp)

        nloops <- nrow(l$Xtest)

        # Include test here that empirical settings is defined as it should be
        if (empirical_settings$type == "AICc_each_k") {
          # Optimization is done only once for all distributions which conditions on
          # exactly k variables
          these_k <- unique(l$X$nfeatures[these_empirical])

          for (i in these_k) {
            these_cond <- l$X[ID %in% these_empirical][nfeatures == i, ID]
            cutters <- 1:empirical_settings$n_samples_aicc
            no_cond <- length(these_cond)

            cond_samp <- cut(
              x = cutters,
              breaks = stats::quantile(cutters, (0:no_cond) / no_cond),
              include.lowest = TRUE,
              labels = these_cond
            )
            cond_samp <- as.numeric(levels(cond_samp))[cond_samp]


            for (loop in 1:nloops) {
              this.optimsamp <- optimsamp
              this.optimsamp$samp_test <- loop

              j <- 1
              X_list <- X.pred.list <- mcov_list <- list()
              for (this_cond in unique(cond_samp)) {
                these_inds <- which(cond_samp == this_cond)
                these_train <- this.optimsamp$samp_train[these_inds]
                these_test <- this.optimsamp$samp_test[these_inds]

                # Hacky way to handle the situation when optimizing in the usual way. Needs to be improved!
                these_train <- 1:nrow(l$Xtrain)
                these_test <- sample(x = these_test, size = nrow(l$Xtrain), replace = T)
                current_cond_samp <- rep(unique(cond_samp), each = nrow(l$Xtrain))

                S <- l$S[this_cond, ]

                S.cols <- which(as.logical(S))
                Sbar.cols <- which(as.logical(1 - S))

                X_list[[j]] <- as.matrix(subset(l$Xtrain, select = S.cols)[these_train, ])
                mcov_list[[j]] <- stats::cov(X_list[[j]])

                Xtrain.Sbar <- subset(l$Xtrain, select = Sbar.cols)[these_train, ]
                Xtest.S <- subset(l$Xtest, select = S.cols)[these_test, ]
                X.pred.list[[j]] <- cbind(Xtrain.Sbar, Xtest.S)

                j <- j + 1
              }

              # Combining the X's for doing prediction
              X.pred <- rbindlist(X.pred.list, use.names = T)
              X.nms <- colnames(l$Xtrain)
              setcolorder(X.pred, X.nms)
              # Doing prediction jointly (for speed), and then splitting them back into the y_list
              pred <- predict_model(model, X.pred)
              y_list <- split(pred, current_cond_samp)
              names(y_list) <- NULL


              ## Doing the numerical optimization -------
              nlm.obj <- suppressWarnings(stats::nlminb(
                start = empirical_settings$start_aicc,
                objective = aicc_full_cpp,
                X_list = X_list,
                mcov_list = mcov_list,
                S_scale_dist = T,
                y_list = y_list,
                negative = F,
                lower = 0,
                control = list(
                  eval.max = empirical_settings$eval_max_aicc,
                  trace = verbose
                )
              ))



              h_optim_mat[match(these_cond, these_empirical), loop] <- nlm.obj$par
            }
          }
        }
        if (empirical_settings$type == "AICc_full") {
          for (i in these_empirical) {
            S <- l$S[i, ]

            S.cols <- which(as.logical(S))
            Sbar.cols <- which(as.logical(1 - S))

            for (loop in 1:nloops) {
              this.optimsamp <- optimsamp
              this.optimsamp$samp_test <- loop

              these_train <- this.optimsamp$samp_train
              these_test <- this.optimsamp$samp_test

              # Hacky way to handle the situation when optimizing in the usual way. Needs to be improved!
              these_train <- 1:nrow(l$Xtrain)
              these_test <- sample(x = these_test, size = nrow(l$Xtrain), replace = T)

              X_list <- list(as.matrix(subset(l$Xtrain, select = S.cols)[these_train, ]))
              mcov_list <- list(stats::cov(X_list[[1]]))

              Xtrain.Sbar <- subset(l$Xtrain, select = Sbar.cols)[these_train, ]
              Xtest.S <- subset(l$Xtest, select = S.cols)[these_test, ]
              X.pred <- cbind(Xtrain.Sbar, Xtest.S)

              X.nms <- colnames(l$Xtrain)
              setcolorder(X.pred, X.nms)

              pred <- predict_model(model, X.pred)
              y_list <- list(pred)

              ## Running the nonlinear optimization

              nlm.obj <- suppressWarnings(stats::nlminb(
                start = empirical_settings$start_aicc,
                objective = aicc_full_cpp,
                X_list = X_list,
                mcov_list = mcov_list,
                S_scale_dist = T,
                y_list = y_list,
                negative = F,
                lower = 0,
                control = list(
                  eval.max = empirical_settings$eval_max_aicc,
                  trace = verbose
                )
              ))


              h_optim_mat[match(i, these_empirical), loop] <- nlm.obj$par
            }
          }
        }
      }
    }
    h_optim_DT <- data.table(varcomb = these_empirical, h_optim_mat)
    colnames(h_optim_DT)[-1] <- paste0("Testobs_", 1:nrow(l$Xtest))
  } else {
    h_optim_mat <- NULL
    h_optim_DT <- NULL
  }
  if (is.null(mu)) {
    # Using the mean of the training data in the Gaussian approach if not provided directly
    mu <- colMeans(l$Xtrain)
  }
  if (is.null(Sigma)) {
    # Using the sample covariance of the training data in the Gaussian approach if not provided directly
    Sigma <- stats::cov(l$Xtrain)
  }

  if (any(eigen(Sigma)$values <= 1e-06)) {
    # Make matrix positive definite if not, or close to not.
    Sigma <- as.matrix(Matrix::nearPD(Sigma)$mat)
  }
  Xtest.mat <- as.matrix(l$Xtest)
  Xtrain.mat <- as.matrix(l$Xtrain)

  # Only needed for copula method, but is not time consuming anyway
  Xtrain_Gauss_trans <- apply(X = l$Xtrain, MARGIN = 2, FUN = gaussian_transform)
  Xtest_Gauss_trans <- apply(
    X = rbind(l$Xtest, l$Xtrain),
    MARGIN = 2,
    FUN = gaussian_transform_separate,
    n_y = nrow(l$Xtest)
  )
  if (is.null(dim(Xtest_Gauss_trans))) {
    Xtest_Gauss_trans <- t(as.matrix(Xtest_Gauss_trans))
  }
  mu_Gauss_trans <- rep(0, ncol(l$Xtrain))
  Sigma_Gauss_trans <- stats::cov(Xtrain_Gauss_trans)

  if (any(eigen(Sigma_Gauss_trans)$values <= 1e-06)) {
    Sigma_Gauss_trans <- as.matrix(Matrix::nearPD(Sigma_Gauss_trans)$mat)
  }
  set.seed(1)
  for (i in l$Xtest[, .I]) {
    # This may be parallelized when the prediction function is not parallelized.
    if (verbose > 0) {
      print(sprintf("%d out of %d", i, l$Xtest[, .N]))
    }

    ll[[i]] <- predictions(
      model = model,
      D = l$D[, i, ],
      h_optim_vec = h_optim_mat[, i],
      kernel_metric = kernel_metric,
      S = l$S,
      Xtrain = Xtrain.mat,
      Xtest = Xtest.mat[i, , drop = FALSE],
      w_threshold = empirical_settings$w_threshold,
      noSamp_MC = noSamp_MC,
      verbose = verbose,
      cond_approach_list = cond_approach_list,
      feature_list = l$X$features,
      pred_zero = pred_zero,
      mu = mu,
      Sigma = Sigma,
      mu_Gauss_trans = mu_Gauss_trans,
      Sigma_Gauss_trans = Sigma_Gauss_trans,
      Xtest_Gauss_trans = Xtest_Gauss_trans[i, , drop = FALSE]
    )
    ll[[i]][, id := i]
  }


  DT <- rbindlist(ll)

  Kshap <- matrix(0, nrow = nrow(l$Xtest), ncol = nrow(l$W))
  for (i in l$Xtest[, .I]) {
    Kshap[i, ] <- l$W %*% DT[id == i, k]
  }

  # Makes data.table from Kshap
  Kshap <- as.data.table(Kshap)
  colnames(Kshap) <- c("none", colnames(l$Xtrain))


  # Makes vector with the full prediction that is decomposed
  pred_vec <- DT[wcomb == 2^ncol(l$Xtrain), k]

  tt <- proc.time() - tt

  ret_list <- list(
    Kshap = Kshap,
    pred_vec = pred_vec,
    other_objects = list(ll = ll, DT = DT, h_optim_DT = h_optim_DT, comp_time = tt)
  )
  return(ret_list)
}

#' DELETE
#' @keywords interal
#' @export
prepare_kshap <- function(Xtrain,
                          Xtest,
                          exact = TRUE,
                          noSamp = NULL,
                          shapley_weight_inf_replacement = 10^6,
                          compute_distances_for_no_var = 0:ncol(Xtrain)) {

  ## Convert data to data.table format --------------
  if (!is.data.table(Xtrain)) {
    Xtrain <- as.data.table(Xtrain)
  }
  if (!is.data.table(Xtest)) {
    Xtest <- as.data.table(Xtest)
  }

  ## Get all combinations ----------------
  X <- feature_combinations(
    m = ncol(Xtrain),
    exact = exact,
    n_combinations = noSamp,
    shapley_weight_inf_replacement = shapley_weight_inf_replacement,
    reduce_dim = TRUE
  )

  ## Get weighted matrix ----------------
  W <- weight_matrix(X, use_shapley_weights_in_W = ifelse(exact, T, F), normalize_W_weights = T)

  mcov <- stats::cov(Xtrain)
  # Note that we could move distance_metric if-test here and replace by diag(m) if "Euclidean"
  # once you see everything works fine

  if (!is.null(compute_distances_for_no_var[1])) {
    # Only compute the distances if the empirical approach is used
    D <- mahalanobis_distance_cpp(
      featureList = X[nfeatures %in% compute_distances_for_no_var, features],
      Xtrain_mat = as.matrix(Xtrain),
      Xtest_mat = as.matrix(Xtest),
      mcov = mcov,
      S_scale_dist = T
    )
    # Note that this is D_S(,)^2 in the paper

    ## Normalize the distance rows to ensure numerical stability in later operations
    colmin <- apply(X = D, MARGIN = c(2, 3), FUN = min)
    for (i in 1:dim(D)[3]) {
      D[, , i] <- t(t(D[, , i]) - colmin[, i])
    }
  } else {
    D <- NULL
  }
  # Get feature matrix
  S <- feature_matrix_cpp(
    features = X[["features"]],
    nfeatures = ncol(Xtrain)
  )
  return(list(
    D = D, S = S, W = W, X = X, Xtrain = Xtrain, Xtest = Xtest,
    D_for_these_varcomb = X[nfeatures %in% compute_distances_for_no_var, which = TRUE]
  ))
}
