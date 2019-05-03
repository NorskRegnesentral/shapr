#' Calculate Shapley weight
#'
#' @inheritParams global_arguments
#'
#' @return Numeric
#'
#' @export
#'
#' @author Nikolai Sellereite
shapley_weights <- function(m, n, s) {
  (m - 1) / (n * s * (m - s))
}

#' Get combinations
#'
#' @inheritParams global_arguments
#'
#' @details
#' The returned data.table contains the following columns
#' \describe{
#' \item{id}{Positive integer. Unique key for combination}
#' \item{features}{List}
#' \item{nfeatures}{Positive integer}
#' \item{n}{Positive integer}
#' }
#'
#' @return data.table
#'
#' @export
#'
#' @author Nikolai Sellereite, Martin Jullum
feature_combinations <- function(m,
                                 exact = TRUE,
                                 no_samp = 200,
                                 shapley_weight_inf_replacement = 10^6,
                                 reduce_dim = TRUE) {
  if (!exact && no_samp > (2^m - 2) && !replace) {
    no_samp <- 2^m - 2
    cat(paste0("noSamp is larger than 2^m = ", 2^m, ". Using exact instead."))
  }
  if (exact == TRUE) {
    n <- 2^m
    x <- data.table(id = 1:n)
    combinations <- lapply(0:m, utils::combn, x = m, simplify = FALSE)
    x[, features := unlist(combinations, recursive = FALSE)]
    x[, nfeatures := length(features[[1]]), id]
    x[, n := .N, nfeatures]
    x[!(nfeatures %in% c(0, m)), shapley_weight := shapley_weights(m = m, n = n, s = nfeatures)]
    x[nfeatures %in% c(0, m), shapley_weight := shapley_weight_inf_replacement]
    x[, no := 1]
  } else {
    ## Find weights for given number of features ----------
    dt_zero <- data.table(nfeatures = head(1:m, -1))
    dt_zero[, n := unlist(lapply(nfeatures, choose, n = m))]
    dt_zero[, shapley_weight := shapley_weights(m = m, n = n, s = nfeatures)]
    dt_zero[, samp_weight := shapley_weight * n]
    dt_zero[, samp_weight := samp_weight / sum(samp_weight)]

    ## Sample number of features ----------
    x <- data.table(
      nfeatures = sample(
        x = dt_zero[["nfeatures"]],
        size = no_samp,
        replace = TRUE,
        prob = dt_zero[["samp_weight"]]
      )
    )

    ## Sample specific set of features # Not optimal, as it is a bit slow for no_samp -------
    setkey(x, nfeatures)
    samp <- sapply(X = x$nfeatures, FUN = function(x) {
      aa <- rep(NA, m)
      aa[1:x] <- sample(x = 1:m, size = x)
      aa
    })
    samp <- t(apply(X = samp, MARGIN = 2, FUN = sort, na.last = T))
    samp_list <- apply(X = samp, MARGIN = 1, FUN = function(x) {
      x[!is.na(x)]
    })

    x <- cbind(x, samp)
    x[, no := .N, by = mget(paste0("V", 1:m))] # Counting repetitions of the same sample

    if (reduce_dim) {
      is_dup <- duplicated(x)
      x[, features := samp_list]
      x <- x[!is_dup, ]
    } else {
      x[, no := 1]
      x[, features := samp_list]
    }

    x[, paste0("V", 1:m) := NULL]
    x[, id := .I]

    nms <- c("id", "nfeatures", "features", "no")
    setcolorder(x, nms)

    ## Add zero features and m features ----------
    x_zero_all <- data.table(
      id = seq(x[, max(id)] + 1, length.out = 2),
      num_var = c(0, m),
      comb = c(list(numeric(0)), list(1:m)),
      no = 1
    )
    x <- rbindlist(list(x, x_zero_all))
    setkey(x, nfeatures)

    ## Add number of combinations
    x <- merge(x = x, y = dt_zero[, .(nfeatures, n, shapley_weight)], all.x = TRUE, on = "nfeatures")
    nms <- c("id", "features", "nfeatures", "n", "shapley_weight", "no")
    setcolorder(x, nms)
    x[, id := .I]
    x[nfeatures %in% c(0, m), `:=`(
      shapley_weight = shapley_weight_inf_replacement,
      n = 1
    )]
  }
  return(x)
}

#' Calculate Shapley weights
#'
#' @param x data.table
#'
#' @return data.table
#'
#' @export
#'
#' @author Nikolai Sellereite
observation_weights <- function(x, m) {
  x[-c(1, .N), weight := shapley_weights(m = m, n = n, s = nfeatures), id]
  x[c(1, .N), weight := 10^6]

  return(x)
}

#' Get weighted matrix
#'
#' @param x data.table
#'
#' @return Matrix
#'
#' @export
#'
#' @author Nikolai Sellereite, Martin Jullum
weight_matrix <- function(x, use_shapley_weights_in_w = T, normalize_w_weights = T) {
  if (use_shapley_weights_in_w) {
    w <- x[["shapley_weight"]] * x[["no"]]
  } else {
    w <- x[["no"]]
    w[c(1, length(w))] <- x[["shapley_weight"]][c(1, length(w))]
  }

  if (normalize_w_weights) {
    w[-c(1, length(w))] <- w[-c(1, length(w))] / sum(w[-c(1, length(w))])
  }

  w <- weight_matrix_cpp(
    features = x[["features"]],
    m = x[.N][["nfeatures"]],
    n = x[, .N],
    w = w
  )

  return(w)
}

#' Scale training and test data
#'
#' @inheritParams global_arguments
#'
#' @return List
#'
#' @export
#'
#' @author Nikolai Sellereite
scale_data <- function(xtrain, xtest, scale = TRUE) {
  if (!is.data.table(xtrain)) {
    xtrain <- as.data.table(xtrain)
  }
  if (!is.data.table(xtest)) {
    xtest <- as.data.table(xtest)
  }

  if (scale) {
    nms <- colnames(xtrain)
    setcolorder(xtest, nms)
    sd <- xtrain[, unname(sapply(.SD, sd, na.rm = TRUE))]
    xtrain[, (nms) := .SD / sd]
    xtest[, (nms) := .SD / sd]
  }

  return(list(xtrain = xtrain, xtest = xtest))
}

#' Get predictions
#'
#' @inheritParams global_arguments
#'
#' @return List
#'
#' @export
#'
#' @author Nikolai Sellereite
observation_impute <- function(w_kernel, s, xtrain, xtest, w_threshold = .7, no_samp_mc = 1e3) {

  ## Find weights for all combinations and training data
  dt <- as.data.table(w_kernel)
  dt[, id := .I]
  dt <- data.table::melt(data = dt, id.vars = "id", variable.name = "comb", value.name = "w", variable.factor = FALSE)

  ## Remove training data with small weight
  setkey(dt, comb, w)
  dt[, w := w / sum(w), comb]
  dt[, wcum := cumsum(w), comb]
  dt <- dt[wcum > 1 - w_threshold][, wcum := NULL]
  dt <- dt[, tail(.SD, no_samp_mc), comb]
  dt[, comb := gsub(comb, pattern = "V", replacement = ""), comb]
  dt[, wcomb := as.integer(comb), comb][, comb := NULL]

  ## Generate data used for prediction
  dt_p <- observation_impute_cpp(
    id = dt[["id"]],
    Comb = dt[["wcomb"]],
    xtrain = xtrain,
    xtest = xtest,
    s = s
  )

  ## Add keys
  dt_p <- as.data.table(dt_p)
  setnames(dt_p, colnames(xtrain))
  dt_p[, wcomb := dt[["wcomb"]]]
  dt_p[, w := dt[["w"]]]

  return(dt_p)
}


#' Transforms new data to standardized normal (dimension 1) based on other data transformations
#'
#' Handled in this way in order to allow using the apply function over this function
#' @param zx Vector where the first part is the Gaussian data, and last part is
#' the data with the original transformation
#' @param n_y How many elements of \code{yx} that belongs to the y-part (new data)
#' @param type The quantile type used when back-transforming. 7 (default) is the default in stats::quantile().
#'
#' @return Vector of transformed new data
#' @export
#'
#' @author Martin Jullum
inv_gaussian_transform <- function(zx, n_z, type = 7) {
  ind <- 1:n_z
  z <- zx[ind]
  x <- zx[-ind]
  u <- stats::pnorm(z)
  x_new <- stats::quantile(x, u, type = type)
  return(x_new)
}


#' Transforms new data to standardized normal (dimension 1) based on other data transformations
#'
#' Handled in this way in order to allow using the apply function over this function
#' @param yx Vector where the first part is the new data to transform,
#' and last part is the data with the original transformation
#' @param n_z How many elements of \code{zx} that belongs to the z-part (Gaussian data)
#'
#' @return Vector of back-transformed Gaussian data
#' @export
#'
#' @author Martin Jullum
gaussian_transform_separate <- function(yx, n_y) {
  ind <- 1:n_y
  y <- yx[ind]
  x <- yx[-ind]
  tmp <- rank(c(y, x))[1:length(y)]
  tmp <- tmp - rank(tmp) + 0.5
  u_y <- tmp / (length(x) + 1)
  z_y <- stats::qnorm(u_y)
  return(z_y)
}

#' Transforms a sample to standardized normal (dimension 1)
#'
#' @param x Vector of data to transform
#'
#' @return Vector of transformed data
#' @export
#'
#' @author Martin Jullum
gaussian_transform <- function(x) {
  u <- rank(x) / (length(x) + 1)
  z <- stats::qnorm(u)
  return(z)
}


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
sample_copula <- function(given_ind, no_samp_mc, mu, sigma, p, xtest_gauss_trans, xtrain, xtest) {
  # Handles the unconditional and full conditional separtely when predicting
  if (length(given_ind) %in% c(0, p)) {
    ret <- matrix(xtest, ncol = p, nrow = 1)
  } else {
    dependent_ind <- (1:length(mu))[-given_ind]
    x_given <- xtest_gauss_trans[given_ind]
    x_given_orig <- xtest[given_ind]

    tmp <- condMVNorm::condMVN(
      mean = mu,
      sigma = sigma,
      dependent.ind = dependent_ind,
      given.ind = given_ind,
      x.given = x_given
    )

    ret0_z <- mvnfast::rmvn(n = no_samp_mc, mu = tmp$condMean, sigma = tmp$cond_var)

    ret0_x <- apply(
      X = rbind(ret0_z, xtrain[, dependent_ind, drop = F]),
      MARGIN = 2,
      FUN = inv_gaussian_transform,
      n_z = no_samp_mc
    )

    ret <- matrix(NA, ncol = p, nrow = no_samp_mc)
    ret[, given_ind] <- rep(x_given_orig, each = no_samp_mc)
    ret[, dependent_ind] <- ret0_x
  }
  colnames(ret) <- colnames(xtest)
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
sample_gaussian <- function(given_ind, no_samp_mc, mu, sigma, p, xtest, ensure_condcov_symmetry = F) {

  # Handles the unconditional and full conditional separtely when predicting
  if (length(given_ind) %in% c(0, p)) {
    ret <- matrix(xtest, ncol = p, nrow = 1)
  } else {
    dependent_ind <- (1:length(mu))[-given_ind]
    x_given <- xtest[given_ind]
    tmp <- condMVNorm::condMVN(
      mean = mu,
      sigma = sigma,
      dependent.ind = dependent_ind,
      given.ind = given_ind,
      x.given = x_given
    )
    if (ensure_condcov_symmetry) {
      tmp$cond_var <- Matrix::symmpart(tmp$cond_var)
    }

    ret0 <- mvnfast::rmvn(n = no_samp_mc, mu = tmp$condMean, sigma = tmp$cond_var)

    ret <- matrix(NA, ncol = p, nrow = no_samp_mc)
    ret[, given_ind] <- rep(x_given, each = no_samp_mc)
    ret[, dependent_ind] <- ret0
  }
  colnames(ret) <- colnames(xtest)
  return(as.data.table(ret))
}

#' Get predictions
#'
#' @param feature_list List
#' @param pred_zero Numeric
#' @inheritParams global_arguments
#'
#' @return List
#'
#' @export
#'
#' @author Nikolai Sellereite, Martin Jullum
predictions <- function(model,
                        D,
                        h_optim_vec,
                        kernel_metric,
                        s,
                        xtrain,
                        xtest,
                        w_threshold = .7,
                        no_samp_mc = 1e3,
                        verbose = FALSE,
                        cond_approach_list,
                        feature_list,
                        pred_zero,
                        mu,
                        sigma,
                        mu_gauss_trans = mu_gauss_trans,
                        sigma_gauss_trans = sigma_gauss_trans,
                        xtest_gauss_trans,
                        ensure_condcov_symmetry = F) {
  p <- ncol(xtrain)

  dt_p_gaussian <- dt_p_copula <- dt_p_empirical <- NULL

  if ("Gaussian" %in% names(cond_approach_list)) {
    ## Assume Gaussian distributed variables and sample from the various conditional distributions
    these_wcomb <- cond_approach_list$Gaussian
    these_wcomb <- these_wcomb[!(these_wcomb %in% c(1, nrow(s)))]

    samp_list <- lapply(
      X = feature_list[these_wcomb],
      FUN = sample_gaussian,
      no_samp_mc = no_samp_mc,
      mu = mu,
      sigma = sigma,
      p = p,
      xtest = xtest,
      ensure_condcov_symmetry = ensure_condcov_symmetry
    )

    dt_p_gaussian <- rbindlist(samp_list, idcol = "wcomb")
    dt_p_gaussian[, wcomb := these_wcomb[wcomb]] # Correcting originally assigned wcomb
    dt_p_gaussian[, w := 1 / no_samp_mc]
  }
  if ("copula" %in% names(cond_approach_list)) {
    these_wcomb <- cond_approach_list$copula
    these_wcomb <- these_wcomb[!(these_wcomb %in% c(1, nrow(s)))]


    samp_list <- lapply(
      X = feature_list[these_wcomb],
      FUN = sample_copula,
      no_samp_mc = no_samp_mc,
      mu = mu_gauss_trans,
      sigma = sigma_gauss_trans,
      p = p,
      xtest_gauss_trans = xtest_gauss_trans,
      xtrain = xtrain,
      xtest = xtest
    )

    dt_p_copula <- rbindlist(samp_list, idcol = "wcomb")
    dt_p_copula[, wcomb := these_wcomb[wcomb]] # Correcting originally assigned wcomb
    dt_p_copula[, w := 1 / no_samp_mc]
  }

  if ("empirical" %in% names(cond_approach_list)) {
    these_wcomb <- cond_approach_list$empirical
    these_wcomb <- these_wcomb[!(these_wcomb %in% c(1, nrow(s)))]

    no_wcomb <- length(these_wcomb)

    # Handle the computation of all training-test weights for ALL combinations here, before looping
    if (kernel_metric == "independence") {
      # Just random noise to "fake" a distance between observations
      w_kernel <- array(stats::runif(no_wcomb * nrow(xtrain)), dim = c(nrow(xtrain), no_wcomb))
    }
    if (kernel_metric == "Gaussian") {
      val <- t(t(-0.5 * D) / h_optim_vec^2)
      w_kernel <- exp(val)
      # To avoid numerical problems for small sigma values, we need to substract some constant from
      # val here. Check if it is possible to do this per column/row of l$D[,i,]
    }

    ## Get imputed data
    dt_p_empirical <- observation_impute(
      w_kernel = w_kernel,
      s = s[these_wcomb, ],
      xtrain = xtrain,
      xtest = xtest,
      w_threshold = w_threshold,
      no_samp_mc = no_samp_mc
    )
    dt_p_empirical[, wcomb := these_wcomb[wcomb]] # Correcting originally assigned wcomb
  }

  ## Performing prediction
  nms <- colnames(xtest)

  dt_p <- rbind(dt_p_gaussian, dt_p_copula, dt_p_empirical)
  dt_p <- merge(dt_p, data.table(wcomb = c(1, 2^p), w = 1), all = T)
  setkey(dt_p, wcomb)


  dt_p[!(wcomb %in% c(1, 2^p)), p_hat := prediction_vector(model = model, data = .SD), .SDcols = nms]
  dt_p[wcomb == 2^p, p_hat := prediction_vector(model = model, data = as.data.frame(xtest))]
  dt_p[wcomb == 1, p_hat := pred_zero]

  ## Get mean probability
  dt_res <- dt_p[, .(k = sum((p_hat * w) / sum(w))), wcomb]
  setkey(dt_res, wcomb)

  return(dt_res)
}


#' Predict on vector form
#'
#' @description Performs prediction of response \code{\link[stats]{lm}}, \code{\link[stats]{glm}},
#' \code{\link[ranger]{ranger}} and \code{\link[xgboost]{xgboost}} with binary or continuous response.
#' Output the prediction on vector form. May let the user provide this function to handle any
#' prediction model in the future.
#'
#' @inheritParams global_arguments
#' @param data data.table or data.frame with data to perform prediction
#' @return Vector of predictions
#'
#' @export
#'
#'
#' @author Martin Jullum
prediction_vector <- function(model, data) {
  ## Figure out which model type we're using
  model_class <- head(class(model), 1)

  if (model_class == "glm") {
    if (model$family[[1]] == "binomial") {
      ret <- predict(model, newdata = data, type = "response")
    } else {
      ret <- predict(model, newdata = data)
    }
  }
  if (model_class == "lm") {
    ret <- predict(model, newdata = data)
  }
  if (model_class == "ranger") {
    if (model$treetype == "Probability estimation") {
      ret <- predict(model, data = data)$predictions[, 2]
    } else {
      ret <- predict(model, data = data)$predictions
    }
  }
  if (model_class == "xgb.Booster") {
    ret <- predict(model, newdata = as.matrix(data))
  }

  if (model_class == "gam") {
    ret <- predict(model, newdata = data)
  }

  return(ret)
}

#' Computes kernel SHAP values for test data
#'
#' @inheritParams global_arguments
#' @param empirical_settings List. Specifying the settings when using the empirical method to
#' compute the conditional expectations.
#' @param pred_zero The prediction value for unseen data, typically equal to the mean of the
#' response
#' @param ensure_condcov_symmetry Logical. Whether to ensure that the conditional covariance
#' matrices in the Gaussian and copula approaches are symmetric. Typically only needed if the
#' original covariance is just barely positive definite.
#'
#' @details If \code{cond_approach} is a list, the elements in the list refers to the rows in
#' \code{l$X} that ought to be included in each of the approaches!
#'
#' @return List with kernel SHAP values (\code{Kshap}) and other object used to perform
#' the computation (helpful for debugging etc.)
#'
#' @export
#'
#' @author Martin Jullum
compute_kshap <- function(model,
                          l,
                          no_samp_mc = 1e3,
                          verbose = FALSE,
                          cond_approach = "empirical",
                          empirical_settings = list(
                            type = "fixed_sigma",
                            fixed_sigma_vec = 0.1,
                            aicc_no_samp_per_optim = 1000,
                            AIC_optim_max_eval = 20,
                            AIC_optim_startval = 0.1,
                            w_threshold = 0.95
                          ),
                          pred_zero,
                          mu = NULL,
                          sigma = NULL,
                          ensure_condcov_symmetry = F) {
  tt <- proc.time()

  ll <- list()

  if (is.character(cond_approach)) {
    cond_approach_list <- list(1:nrow(l$s))
    names(cond_approach_list) <- cond_approach
  }
  if (is.list(cond_approach)) {
    cond_approach_list <- cond_approach
  }

  if ("empirical" %in% names(cond_approach_list)) {
    these_empirical <- cond_approach_list$empirical
    exclude_emp <- (these_empirical %in% c(1, nrow(l$s)))

    these_empirical <- these_empirical[!exclude_emp]

    no_empirical <- length(these_empirical)
    h_optim_mat <- matrix(NA, ncol = nrow(l$xtest), nrow = no_empirical) # Each test observation has one column

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
          n_train = nrow(l$xtrain),
          n_test = nrow(l$xtest),
          nosamp = empirical_settings$aicc_no_samp_per_optim,
          separate = T
        )

        # Updating parameter (only if it is larger than nTrain*nTest)
        empirical_settings$aicc_no_samp_per_optim <- nrow(optimsamp)

        nloops <- nrow(l$xtest)

        # Include test here that empirical settings is defined as it should be
        if (empirical_settings$type == "AICc_each_k") {
          # Optimization is done only once for all distributions which conditions on
          # exactly k variables
          these_k <- unique(l$x$nfeatures[these_empirical])

          for (i in these_k) {
            these_cond <- l$x[id %in% these_empirical][nfeatures == i, id]

            cutters <- 1:empirical_settings$aicc_no_samp_per_optim
            no_cond <- length(these_cond)

            cond_samp <- cut(
              x = cutters,
              breaks = stats::quantile(cutters, (0:no_cond) / no_cond),
              include.lowest = TRUE,
              labels = these_cond
            )
            cond_samp <- as.numeric(levels(cond_samp))[cond_samp]


            for (loop in 1:nloops) {
              this_optimsamp <- optimsamp
              this_optimsamp$samp_test <- loop

              j <- 1
              x_list <- x_pred_list <- mcov_list <- list()
              for (this_cond in unique(cond_samp)) {
                these_inds <- which(cond_samp == this_cond)
                these_train <- this_optimsamp$samp_train[these_inds]
                these_test <- this_optimsamp$samp_test[these_inds]

                # Hacky way to handle the situation when optimizing in the usual way. Needs to be improved!
                these_train <- 1:nrow(l$xtrain)
                these_test <- sample(x = these_test, size = nrow(l$xtrain), replace = T)
                current_cond_samp <- rep(unique(cond_samp), each = nrow(l$xtrain))

                s <- l$s[this_cond, ]

                s_cols <- which(as.logical(s))
                sbar_cols <- which(as.logical(1 - s))

                x_list[[j]] <- as.matrix(subset(l$xtrain, select = s_cols)[these_train, ])
                mcov_list[[j]] <- stats::cov(x_list[[j]])

                xtrain_sbar <- subset(l$xtrain, select = sbar_cols)[these_train, ]
                xtest_s <- subset(l$xtest, select = s_cols)[these_test, ]
                x_pred_list[[j]] <- cbind(xtrain_sbar, xtest_s)

                j <- j + 1
              }

              # Combining the X's for doing prediction
              x_pred <- rbindlist(x_pred_list, use.names = T)
              x_nms <- colnames(l$xtrain)
              setcolorder(x_pred, x_nms)
              # Doing prediction jointly (for speed), and then splitting them back into the y_list
              pred <- prediction_vector(model = model, data = x_pred)
              y_list <- split(pred, current_cond_samp)
              names(y_list) <- NULL


              ## Doing the numerical optimization -------
              nlm_obj <- suppressWarnings(stats::nlminb(
                start = empirical_settings$AIC_optim_startval,
                objective = aicc_full_cpp,
                x_list = x_list,
                mcov_list = mcov_list,
                S_scale_dist = T,
                y_list = y_list,
                negative = F,
                lower = 0,
                control = list(
                  eval.max = empirical_settings$AIC_optim_max_eval,
                  trace = verbose
                )
              ))



              h_optim_mat[match(these_cond, these_empirical), loop] <- nlm_obj$par
            }
          }
        }

        if (empirical_settings$type == "AICc_full") {
          for (i in these_empirical) {
            s <- l$s[i, ]

            s_cols <- which(as.logical(s))
            sbar_cols <- which(as.logical(1 - s))

            for (loop in 1:nloops) {
              this_optimsamp <- optimsamp
              this_optimsamp$samp_test <- loop

              these_train <- this_optimsamp$samp_train
              these_test <- this_optimsamp$samp_test

              # Hacky way to handle the situation when optimizing in the usual way. Needs to be improved!
              these_train <- 1:nrow(l$xtrain)
              these_test <- sample(x = these_test, size = nrow(l$xtrain), replace = T)

              x_list <- list(as.matrix(subset(l$xtrain, select = s_cols)[these_train, ]))
              mcov_list <- list(stats::cov(x_list[[1]]))

              xtrain_sbar <- subset(l$xtrain, select = sbar_cols)[these_train, ]
              xtest_s <- subset(l$xtest, select = s_cols)[these_test, ]
              x_pred <- cbind(xtrain_sbar, xtest_s)

              x_nms <- colnames(l$xtrain)
              setcolorder(x_pred, x_nms)

              pred <- prediction_vector(model = model, data = x_pred)
              y_list <- list(pred)

              ## Running the nonlinear optimization

              nlm_obj <- suppressWarnings(stats::nlminb(
                start = empirical_settings$AIC_optim_startval,
                objective = aicc_full_cpp,
                x_list = x_list,
                mcov_list = mcov_list,
                S_scale_dist = T,
                y_list = y_list,
                negative = F,
                lower = 0,
                control = list(
                  eval.max = empirical_settings$AIC_optim_max_eval,
                  trace = verbose
                )
              ))


              h_optim_mat[match(i, these_empirical), loop] <- nlm_obj$par
            }
          }
        }
      }
    }

    h_optim_dt <- data.table(varcomb = these_empirical, h_optim_mat)
    colnames(h_optim_dt)[-1] <- paste0("Testobs_", 1:nrow(l$xtest))
  } else {
    h_optim_mat <- NULL
    h_optim_dt <- NULL
  }

  if (is.null(mu)) {
    # Using the mean of the training data in the Gaussian approach if not provided directly
    mu <- colMeans(l$xtrain)
  }
  if (is.null(sigma)) {
    # Using the sample covariance of the training data in the Gaussian approach if not provided directly
    sigma <- stats::cov(l$xtrain)
  }

  if (any(eigen(sigma)$values <= 1e-06)) {
    # Make matrix positive definite if not, or close to not.
    sigma <- as.matrix(Matrix::nearPD(sigma)$mat)
  }
  xtest_mat <- as.matrix(l$xtest)
  xtrain_mat <- as.matrix(l$xtrain)

  # Only needed for copula method, but is not time consuming anyway
  xtrain_gauss_trans <- apply(X = l$xtrain, MARGIN = 2, FUN = gaussian_transform)
  xtest_gauss_trans <- apply(
    X = rbind(l$xtest, l$xtrain),
    MARGIN = 2,
    FUN = gaussian_transform_separate,
    n_y = nrow(l$xtest)
  )

  mu_gauss_trans <- rep(0, ncol(l$xtrain))
  sigma_gauss_trans <- stats::cov(xtrain_gauss_trans)
  if (any(eigen(sigma_gauss_trans)$values <= 1e-06)) {
    sigma_gauss_trans <- as.matrix(Matrix::nearPD(sigma_gauss_trans)$mat)
  }

  for (i in l$xtest[, .I]) {
    # This may be parallelized when the prediction function is not parallelized.
    if (verbose > 0) {
      print(sprintf("%d out of %d", i, l$xtest[, .N]))
    }

    ll[[i]] <- predictions(
      model = model,
      D = l$D[, i, ],
      h_optim_vec = h_optim_mat[, i],
      kernel_metric = kernel_metric,
      s = l$s,
      xtrain = xtrain_mat,
      xtest = xtest_mat[i, , drop = FALSE],
      w_threshold = empirical_settings$w_threshold,
      no_samp_mc = no_samp_mc,
      verbose = verbose,
      cond_approach_list = cond_approach_list,
      feature_list = l$x$features,
      pred_zero = pred_zero,
      mu = mu,
      sigma = sigma,
      mu_gauss_trans = mu_gauss_trans,
      sigma_gauss_trans = sigma_gauss_trans,
      xtest_gauss_trans = xtest_gauss_trans[i, , drop = FALSE],
      ensure_condcov_symmetry = ensure_condcov_symmetry
    )
    ll[[i]][, id := i]
  }

  dt <- rbindlist(ll)

  kshap <- matrix(0, nrow = nrow(l$xtest), ncol = nrow(l$w))
  for (i in l$xtest[, .I]) {
    kshap[i, ] <- l$w %*% dt[id == i, k]
  }

  # Makes data.table from kshap
  kshap <- as.data.table(kshap)
  colnames(kshap) <- c("none", colnames(l$xtrain))


  # Makes vector with the full prediction that is decomposed
  pred_vec <- dt[wcomb == 2^ncol(l$xtrain), k]

  tt <- proc.time() - tt

  ret_list <- list(
    kshap = kshap,
    pred_vec = pred_vec,
    other_objects = list(ll = ll, dt = dt, h_optim_dt = h_optim_dt, comp_time = tt)
  )
  return(ret_list)
}




#' Get Shapley weights for test data
#'
#' @inheritParams global_arguments
#' @param compute_distances_for_no_var  If equal to \code{NULL} no distances are computed
#'
#' @return Matrix
#'
#' @export
#'
#' @author Nikolai Sellereite
prepare_kshap <- function(xtrain,
                          xtest,
                          exact = TRUE,
                          no_samp = NULL,
                          shapley_weight_inf_replacement = 10^6,
                          compute_distances_for_no_var = 0:ncol(xtrain)) {

  ## Convert data to data.table format --------------
  if (!is.data.table(xtrain)) {
    xtrain <- as.data.table(xtrain)
  }
  if (!is.data.table(xtest)) {
    xtest <- as.data.table(xtest)
  }

  ## Get all combinations ----------------
  x <- feature_combinations(
    m = ncol(xtrain),
    exact = exact,
    no_samp = no_samp,
    shapley_weight_inf_replacement = shapley_weight_inf_replacement,
    reduce_dim = TRUE
  )

  ## Get weighted matrix ----------------
  w <- weight_matrix(x, use_shapley_weights_in_w = ifelse(exact, T, F), normalize_w_weights = T)

  mcov <- stats::cov(xtrain)
  # Note that we could move distance_metric if-test here and replace by diag(m) if "Euclidean"
  # once you see everything works fine

  if (!is.null(compute_distances_for_no_var[1])) {
    # Only compute the distances if the empirical approach is used
    D <- mahalanobis_distance_cpp(
      featureList = x[nfeatures %in% compute_distances_for_no_var, features],
      xtrain_mat = as.matrix(xtrain),
      xtest_mat = as.matrix(xtest),
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


  ## Get feature matrix ---------
  s <- feature_matrix_cpp(
    features = x[["features"]],
    nfeatures = ncol(xtrain)
  )

  return(list(
    D = D, s = s, w = w, x = x, xtrain = xtrain, xtest = xtest,
    D_for_these_varcomb = x[nfeatures %in% compute_distances_for_no_var, which = TRUE]
  ))
}
