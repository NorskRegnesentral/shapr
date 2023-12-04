# Libraries -------------------------------------------------------------------------------------------------------
library(data.table)
library(shapr)
library(rbenchmark)


# Old and new version ---------------------------------------------------------------------------------------------
prepare_data_gaussian_old <- function(internal, index_features = NULL, ...) {
  x_train <- internal$data$x_train
  x_explain <- internal$data$x_explain
  n_explain <- internal$parameters$n_explain
  gaussian.cov_mat <- internal$parameters$gaussian.cov_mat
  n_samples <- internal$parameters$n_samples
  gaussian.mu <- internal$parameters$gaussian.mu
  n_features <- internal$parameters$n_features

  X <- internal$objects$X

  x_explain0 <- as.matrix(x_explain)
  dt_l <- list()

  if (is.null(index_features)) {
    features <- X$features
  } else {
    features <- X$features[index_features]
  }

  for (i in seq_len(n_explain)) {
    cat(sprintf("%d,", i))
    l <- lapply(
      X = features,
      FUN = shapr:::sample_gaussian,
      n_samples = n_samples,
      mu = gaussian.mu,
      cov_mat = gaussian.cov_mat,
      m = n_features,
      x_explain = x_explain0[i, , drop = FALSE]
    )

    dt_l[[i]] <- data.table::rbindlist(l, idcol = "id_combination")
    dt_l[[i]][, w := 1 / n_samples]
    dt_l[[i]][, id := i]
    if (!is.null(index_features)) dt_l[[i]][, id_combination := index_features[id_combination]]
  }

  dt <- data.table::rbindlist(dt_l, use.names = TRUE, fill = TRUE)
  return(dt)
}


# In this version we improve the method by only computing the conditional covariance matrices once.
prepare_data_gaussian_new_v1 <- function(internal, index_features, ...) {
  # This function assumes that index_features will never include the empty and
  # grand coalitions. This is valid 21/11/23 as `batch_prepare_vS()` removes the
  # grand coalition before calling the `prepare_data()` function and the empty
  # coalition is never included in the `internal$objects$S_batch` list.

  # Extract objects that we are going to use
  x_explain <- internal$data$x_explain
  S <- internal$objects$S
  mu <- internal$parameters$gaussian.mu
  cov_mat <- internal$parameters$gaussian.cov_mat
  x_explain_mat <- as.matrix(internal$data$x_explain)
  n_explain <- internal$parameters$n_explain
  n_features <- internal$parameters$n_features
  n_samples <- internal$parameters$n_samples
  feature_names <- internal$parameters$feature_names
  n_combinations <- internal$parameters$n_combinations

  # Extract the relevant coalitions specified in `index_features` from `S`.
  # This will always be called as `index_features` is never NULL.
  S <- if (!is.null(index_features)) S[index_features, , drop = FALSE]

  # Generate a data table containing all Monte Carlo samples for all test observations and coalitions
  dt <- data.table::rbindlist(
    # Iterate over the coalitions
    lapply(
      seq_len(nrow(S)),
      function(S_ind) {
        # This function generates the conditional samples Xsbar | Xs = Xs_star
        # and combine those values with the unconditional values.
        cat(sprintf("%d,", S_ind))

        # Get boolean representations if the features are in the S and the Sbar sets
        S_now <- as.logical(S[S_ind, ])
        Sbar_now <- !as.logical(S[S_ind, ])

        # Remove:
        # Do not need to treat the empty and grand coalitions different as they will never be present
        # if (sum(S_now) %in% c(0, n_features)) {
        #   return(data.table::as.data.table(cbind("id" = seq(n_explain), x_explain)))
        # }

        # Extract the features we condition on
        x_S_star <- x_explain_mat[, S_now, drop = FALSE]

        # Extract the mean values for the features in the two sets
        mu_S <- mu[S_now]
        mu_Sbar <- mu[Sbar_now]

        # Extract the relevant parts of the covariance matrix
        cov_mat_SS <- cov_mat[S_now, S_now, drop = FALSE]
        cov_mat_SSbar <- cov_mat[S_now, Sbar_now, drop = FALSE]
        cov_mat_SbarS <- cov_mat[Sbar_now, S_now, drop = FALSE]
        cov_mat_SbarSbar <- cov_mat[Sbar_now, Sbar_now, drop = FALSE]

        # Compute the covariance matrix multiplication factors/terms and the conditional covariance matrix
        cov_mat_SbarS_cov_mat_SS_inv <- cov_mat_SbarS %*% solve(cov_mat_SS)
        cond_cov_mat_Sbar_given_S <- cov_mat_SbarSbar - cov_mat_SbarS_cov_mat_SS_inv %*% cov_mat_SSbar

        # Ensure that the conditional covariance matrix symmetric in the
        # rare case where numerical instability made it unsymmetrical.
        if (!isSymmetric(cond_cov_mat_Sbar_given_S)) {
          cond_cov_mat_Sbar_given_S <- Matrix::symmpart(cond_cov_mat_Sbar_given_S)
        }

        # Compute the conditional mean of Xsbar given Xs = Xs_star
        x_Sbar_mean <- mu_Sbar + cov_mat_SbarS_cov_mat_SS_inv %*% (t(x_S_star) - mu_S)

        # Allocate an empty matrix used in mvnfast:::rmvnCpp to store the generated MC samples.
        B <- matrix(nrow = n_samples, ncol = sum(Sbar_now))
        class(B) <- "numeric"

        # Create a data.table containing the MC samples for all test observations for one coalition
        data.table::rbindlist(

          # Loop over the different test observations
          lapply(seq(n_explain), function(idx_now) {
            # Sample the MC samples from the conditional Gaussian distribution for one test observation.
            .Call("rmvnCpp",
                  n_ = n_samples,
                  mu_ = x_Sbar_mean[, idx_now],
                  sigma_ = cond_cov_mat_Sbar_given_S,
                  ncores_ = 1,
                  isChol_ = FALSE,
                  A_ = B,
                  PACKAGE = "mvnfast"
            )
            # Combine the generated values with the values we conditioned on
            ret <- matrix(NA, ncol = n_features, nrow = n_samples)
            ret[, S_now] <- rep(c(x_explain_mat[idx_now, S_now]), each = n_samples)
            ret[, Sbar_now] <- B

            # Set names of the columns and convert to a data.table
            colnames(ret) <- feature_names
            as.data.table(ret)
          }),
          use.names = TRUE, idcol = "id", fill = TRUE
        )
      }
    ),
    idcol = "id_combination"
  )

  # Update the id_combination. This will always be called as `index_features` is never NULL.
  if (!is.null(index_features)) dt[, id_combination := index_features[id_combination]]

  # Add uniform weights
  dt[, w := 1 / n_samples]

  # Remove:
  # This is not needed when we assume that the empty and grand coalitions will never be present
  # dt[id_combination %in% c(1, n_combinations), w := 1]

  # Return the MC samples
  return(dt)
}

# This is similar to v1, but we compute the Cholensky decomposition only once for each coalitions.
# In v1, it is computed n_explain times.
prepare_data_gaussian_new_v2 <- function(internal, index_features, ...) {
  # This function assumes that index_features will never include the empty and
  # grand coalitions. This is valid 21/11/23 as `batch_prepare_vS()` removes the
  # grand coalition before calling the `prepare_data()` function and the empty
  # coalition is never included in the `internal$objects$S_batch` list.

  # Extract objects that we are going to use
  x_explain <- internal$data$x_explain
  S <- internal$objects$S
  mu <- internal$parameters$gaussian.mu
  cov_mat <- internal$parameters$gaussian.cov_mat
  x_explain_mat <- as.matrix(internal$data$x_explain)
  n_explain <- internal$parameters$n_explain
  n_features <- internal$parameters$n_features
  n_samples <- internal$parameters$n_samples
  feature_names <- internal$parameters$feature_names
  n_combinations <- internal$parameters$n_combinations

  # Extract the relevant coalitions specified in `index_features` from `S`.
  # This will always be called as `index_features` is never NULL.
  S <- if (!is.null(index_features)) S[index_features, , drop = FALSE]

  # Generate a data table containing all Monte Carlo samples for all test observations and coalitions
  dt <- data.table::rbindlist(
    # Iterate over the coalitions
    lapply(
      seq_len(nrow(S)),
      function(S_ind) {
        # This function generates the conditional samples Xsbar | Xs = Xs_star
        # and combine those values with the unconditional values.
        cat(sprintf("%d,", S_ind))

        # Get boolean representations if the features are in the S and the Sbar sets
        S_now <- as.logical(S[S_ind, ])
        Sbar_now <- !as.logical(S[S_ind, ])

        # Remove:
        # Do not need to treat the empty and grand coalitions different as they will never be present
        # if (sum(S_now) %in% c(0, n_features)) {
        #   return(data.table::as.data.table(cbind("id" = seq(n_explain), x_explain)))
        # }

        # Extract the features we condition on
        x_S_star <- x_explain_mat[, S_now, drop = FALSE]

        # Extract the mean values for the features in the two sets
        mu_S <- mu[S_now]
        mu_Sbar <- mu[Sbar_now]

        # Extract the relevant parts of the covariance matrix
        cov_mat_SS <- cov_mat[S_now, S_now, drop = FALSE]
        cov_mat_SSbar <- cov_mat[S_now, Sbar_now, drop = FALSE]
        cov_mat_SbarS <- cov_mat[Sbar_now, S_now, drop = FALSE]
        cov_mat_SbarSbar <- cov_mat[Sbar_now, Sbar_now, drop = FALSE]

        # Compute the covariance matrix multiplication factors/terms and the conditional covariance matrix
        cov_mat_SbarS_cov_mat_SS_inv <- cov_mat_SbarS %*% solve(cov_mat_SS)
        cond_cov_mat_Sbar_given_S <- cov_mat_SbarSbar - cov_mat_SbarS_cov_mat_SS_inv %*% cov_mat_SSbar

        # Ensure that the conditional covariance matrix symmetric in the
        # rare case where numerical instability made it unsymmetrical.
        if (!isSymmetric(cond_cov_mat_Sbar_given_S)) {
          cond_cov_mat_Sbar_given_S <- Matrix::symmpart(cond_cov_mat_Sbar_given_S)
        }

        # Compute the conditional mean of Xsbar given Xs = Xs_star
        x_Sbar_mean <- mu_Sbar + cov_mat_SbarS_cov_mat_SS_inv %*% (t(x_S_star) - mu_S)

        # Allocate an empty matrix used in mvnfast:::rmvnCpp to store the generated MC samples.
        B <- matrix(nrow = n_samples, ncol = sum(Sbar_now))
        class(B) <- "numeric"

        # Compute the Cholensky decomposition
        cond_cov_mat_Sbar_given_S_chol <- chol(cond_cov_mat_Sbar_given_S)

        # Create a data.table containing the MC samples for all test observations for one coalition
        data.table::rbindlist(

          # Loop over the different test observations
          lapply(seq(n_explain), function(idx_now) {
            # Sample the MC samples from the conditional Gaussian distribution for one test observation.
            .Call("rmvnCpp",
                  n_ = n_samples,
                  mu_ = x_Sbar_mean[, idx_now],
                  sigma_ = cond_cov_mat_Sbar_given_S_chol,
                  ncores_ = 1,
                  isChol_ = TRUE,
                  A_ = B,
                  PACKAGE = "mvnfast"
            )
            # Combine the generated values with the values we conditioned on
            ret <- matrix(NA, ncol = n_features, nrow = n_samples)
            ret[, S_now] <- rep(c(x_explain_mat[idx_now, S_now]), each = n_samples)
            ret[, Sbar_now] <- B

            # Set names of the columns and convert to a data.table
            colnames(ret) <- feature_names
            as.data.table(ret)
          }),
          use.names = TRUE, idcol = "id", fill = TRUE
        )
      }
    ),
    idcol = "id_combination"
  )

  # Update the id_combination. This will always be called as `index_features` is never NULL.
  if (!is.null(index_features)) dt[, id_combination := index_features[id_combination]]

  # Add uniform weights
  dt[, w := 1 / n_samples]

  # Remove:
  # This is not needed when we assume that the empty and grand coalitions will never be present
  # dt[id_combination %in% c(1, n_combinations), w := 1]

  # Return the MC samples
  return(dt)
}

# Here we improve the method speed by only sampling once per coalition
# and only add the test-observation-dependent mean in a secondary call.
prepare_data_gaussian_new_v3 <- function(internal, index_features, ...) {
  # This function assumes that index_features will never include the empty and
  # grand coalitions. This is valid 21/11/23 as `batch_prepare_vS()` removes the
  # grand coalition before calling the `prepare_data()` function and the empty
  # coalition is never included in the `internal$objects$S_batch` list.

  # Extract objects that we are going to use
  x_explain <- internal$data$x_explain
  S <- internal$objects$S
  mu <- internal$parameters$gaussian.mu
  cov_mat <- internal$parameters$gaussian.cov_mat
  x_explain_mat <- as.matrix(internal$data$x_explain)
  n_explain <- internal$parameters$n_explain
  n_features <- internal$parameters$n_features
  n_samples <- internal$parameters$n_samples
  feature_names <- internal$parameters$feature_names
  n_combinations <- internal$parameters$n_combinations

  # Extract the relevant coalitions specified in `index_features` from `S`.
  # This will always be called as `index_features` is never NULL.
  S <- if (!is.null(index_features)) S[index_features, , drop = FALSE]

  # Generate a data table containing all Monte Carlo samples for all test observations and coalitions
  dt <- data.table::rbindlist(
    # Iterate over the coalitions
    lapply(
      seq_len(nrow(S)),
      function(S_ind) {
        # This function generates the conditional samples Xsbar | Xs = Xs_star
        # and combine those values with the unconditional values.
        cat(sprintf("%d,", S_ind))

        # Get boolean representations if the features are in the S and the Sbar sets
        S_now <- as.logical(S[S_ind, ])
        Sbar_now <- !as.logical(S[S_ind, ])

        # Remove:
        # Do not need to treat the empty and grand coalitions different as they will never be present
        # if (sum(S_now) %in% c(0, n_features)) {
        #   return(data.table::as.data.table(cbind("id" = seq(n_explain), x_explain)))
        # }

        # Extract the features we condition on
        x_S_star <- x_explain_mat[, S_now, drop = FALSE]

        # Extract the mean values for the features in the two sets
        mu_S <- mu[S_now]
        mu_Sbar <- mu[Sbar_now]

        # Extract the relevant parts of the covariance matrix
        cov_mat_SS <- cov_mat[S_now, S_now, drop = FALSE]
        cov_mat_SSbar <- cov_mat[S_now, Sbar_now, drop = FALSE]
        cov_mat_SbarS <- cov_mat[Sbar_now, S_now, drop = FALSE]
        cov_mat_SbarSbar <- cov_mat[Sbar_now, Sbar_now, drop = FALSE]

        # Compute the covariance matrix multiplication factors/terms and the conditional covariance matrix
        cov_mat_SbarS_cov_mat_SS_inv <- cov_mat_SbarS %*% solve(cov_mat_SS)
        cond_cov_mat_Sbar_given_S <- cov_mat_SbarSbar - cov_mat_SbarS_cov_mat_SS_inv %*% cov_mat_SSbar

        # Ensure that the conditional covariance matrix symmetric in the
        # rare case where numerical instability made it unsymmetrical.
        if (!isSymmetric(cond_cov_mat_Sbar_given_S)) {
          cond_cov_mat_Sbar_given_S <- Matrix::symmpart(cond_cov_mat_Sbar_given_S)
        }

        # Compute the conditional mean of Xsbar given Xs = Xs_star
        x_Sbar_mean <- mu_Sbar + cov_mat_SbarS_cov_mat_SS_inv %*% (t(x_S_star) - mu_S)

        # rbenchmark::benchmark(
        # t(sweep(x_S_star, 2, mu_S, FUN = "-")),
        # t(x_S_star) - mu_S)

        # Allocate an empty matrix used in mvnfast:::rmvnCpp to store the generated MC samples.
        B <- matrix(nrow = n_samples, ncol = sum(Sbar_now))
        class(B) <- "numeric"

        .Call("rmvnCpp",
              n_ = n_samples,
              mu_ = rep(0, length(mu_Sbar)),
              sigma_ = cond_cov_mat_Sbar_given_S,
              ncores_ = 1,
              isChol_ = FALSE,
              A_ = B,
              PACKAGE = "mvnfast"
        )

        # Transpose her and untranspose later for faster matrix addition in `t(B + x_Sbar_mean[, idx_now])`
        # as it seems to be faster than using `sweep(B, 2, x_Sbar_mean[, idx_now], FUN = "+")` on the
        # original B (i.e., not transposed B).
        B <- t(B)

        # Create a data.table containing the MC samples for all test observations for one coalition
        data.table::rbindlist(

          # Loop over the different test observations
          lapply(seq(n_explain), function(idx_now) {
            # Combine the generated values with the values we conditioned on
            ret <- matrix(NA, ncol = n_features, nrow = n_samples)
            ret[, S_now] <- rep(c(x_explain_mat[idx_now, S_now]), each = n_samples)
            ret[, Sbar_now] <- t(B + x_Sbar_mean[, idx_now])

            # Set names of the columns and convert to a data.table
            colnames(ret) <- feature_names
            as.data.table(ret)
          }),
          use.names = TRUE, idcol = "id", fill = TRUE
        )
      }
    ),
    idcol = "id_combination"
  )

  # Update the id_combination. This will always be called as `index_features` is never NULL.
  if (!is.null(index_features)) dt[, id_combination := index_features[id_combination]]

  # Add uniform weights
  dt[, w := 1 / n_samples]

  # Remove:
  # This is not needed when we assume that the empty and grand coalitions will never be present
  # dt[id_combination %in% c(1, n_combinations), w := 1]

  # Return the MC samples
  return(dt)
}

# Same as v3, but we now use R to compute Cholensky
prepare_data_gaussian_new_v4 <- function(internal, index_features, ...) {
  # This function assumes that index_features will never include the empty and
  # grand coalitions. This is valid 21/11/23 as `batch_prepare_vS()` removes the
  # grand coalition before calling the `prepare_data()` function and the empty
  # coalition is never included in the `internal$objects$S_batch` list.

  # Extract objects that we are going to use
  x_explain <- internal$data$x_explain
  S <- internal$objects$S
  mu <- internal$parameters$gaussian.mu
  cov_mat <- internal$parameters$gaussian.cov_mat
  x_explain_mat <- as.matrix(internal$data$x_explain)
  n_explain <- internal$parameters$n_explain
  n_features <- internal$parameters$n_features
  n_samples <- internal$parameters$n_samples
  feature_names <- internal$parameters$feature_names
  n_combinations <- internal$parameters$n_combinations

  # Extract the relevant coalitions specified in `index_features` from `S`.
  # This will always be called as `index_features` is never NULL.
  S <- if (!is.null(index_features)) S[index_features, , drop = FALSE]

  # Generate a data table containing all Monte Carlo samples for all test observations and coalitions
  dt <- data.table::rbindlist(
    # Iterate over the coalitions
    lapply(
      seq_len(nrow(S)),
      function(S_ind) {
        # This function generates the conditional samples Xsbar | Xs = Xs_star
        # and combine those values with the unconditional values.
        cat(sprintf("%d,", S_ind))

        # Get boolean representations if the features are in the S and the Sbar sets
        S_now <- as.logical(S[S_ind, ])
        Sbar_now <- !as.logical(S[S_ind, ])

        # Remove:
        # Do not need to treat the empty and grand coalitions different as they will never be present
        # if (sum(S_now) %in% c(0, n_features)) {
        #   return(data.table::as.data.table(cbind("id" = seq(n_explain), x_explain)))
        # }

        # Extract the features we condition on
        x_S_star <- x_explain_mat[, S_now, drop = FALSE]

        # Extract the mean values for the features in the two sets
        mu_S <- mu[S_now]
        mu_Sbar <- mu[Sbar_now]

        # Extract the relevant parts of the covariance matrix
        cov_mat_SS <- cov_mat[S_now, S_now, drop = FALSE]
        cov_mat_SSbar <- cov_mat[S_now, Sbar_now, drop = FALSE]
        cov_mat_SbarS <- cov_mat[Sbar_now, S_now, drop = FALSE]
        cov_mat_SbarSbar <- cov_mat[Sbar_now, Sbar_now, drop = FALSE]

        # Compute the covariance matrix multiplication factors/terms and the conditional covariance matrix
        cov_mat_SbarS_cov_mat_SS_inv <- cov_mat_SbarS %*% solve(cov_mat_SS)
        cond_cov_mat_Sbar_given_S <- cov_mat_SbarSbar - cov_mat_SbarS_cov_mat_SS_inv %*% cov_mat_SSbar

        # Ensure that the conditional covariance matrix symmetric in the
        # rare case where numerical instability made it unsymmetrical.
        if (!isSymmetric(cond_cov_mat_Sbar_given_S)) {
          cond_cov_mat_Sbar_given_S <- Matrix::symmpart(cond_cov_mat_Sbar_given_S)
        }

        # Compute the conditional mean of Xsbar given Xs = Xs_star
        x_Sbar_mean <- mu_Sbar + cov_mat_SbarS_cov_mat_SS_inv %*% (t(x_S_star) - mu_S)

        # Allocate an empty matrix used in mvnfast:::rmvnCpp to store the generated MC samples.
        B <- matrix(nrow = n_samples, ncol = sum(Sbar_now))
        class(B) <- "numeric"

        .Call("rmvnCpp",
              n_ = n_samples,
              mu_ = rep(0, length(mu_Sbar)),
              sigma_ = chol(cond_cov_mat_Sbar_given_S),
              ncores_ = 1,
              isChol_ = TRUE,
              A_ = B,
              PACKAGE = "mvnfast"
        )

        # Transpose her and untranspose later for faster matrix addition in `t(B + x_Sbar_mean[, idx_now])`
        # as it seems to be faster than using `sweep(B, 2, x_Sbar_mean[, idx_now], FUN = "+")` on the
        # original B (i.e., not transposed B).
        B <- t(B)

        # Create a data.table containing the MC samples for all test observations for one coalition
        data.table::rbindlist(

          # Loop over the different test observations
          lapply(seq(n_explain), function(idx_now) {
            # Combine the generated values with the values we conditioned on
            ret <- matrix(NA, ncol = n_features, nrow = n_samples)
            ret[, S_now] <- rep(c(x_explain_mat[idx_now, S_now]), each = n_samples)
            ret[, Sbar_now] <- t(B + x_Sbar_mean[, idx_now])

            # Set names of the columns and convert to a data.table
            colnames(ret) <- feature_names
            as.data.table(ret)
          }),
          use.names = TRUE, idcol = "id", fill = TRUE
        )
      }
    ),
    idcol = "id_combination"
  )

  # Update the id_combination. This will always be called as `index_features` is never NULL.
  if (!is.null(index_features)) dt[, id_combination := index_features[id_combination]]

  # Add uniform weights
  dt[, w := 1 / n_samples]

  # Remove:
  # This is not needed when we assume that the empty and grand coalitions will never be present
  # dt[id_combination %in% c(1, n_combinations), w := 1]

  # Return the MC samples
  return(dt)
}

# Here we only want to generate the data once. So we generate n_samples from N(0, I),
# and then use Cholensky to transform to N(O, Sigma_{Sbar|S}), and then add the means.
prepare_data_gaussian_new_v5 <- function(internal, index_features, ...) {
  # This function assumes that index_features will never include the empty and
  # grand coalitions. This is valid 21/11/23 as `batch_prepare_vS()` removes the
  # grand coalition before calling the `prepare_data()` function and the empty
  # coalition is never included in the `internal$objects$S_batch` list.

  # Extract objects that we are going to use
  x_explain <- internal$data$x_explain
  S <- internal$objects$S
  mu <- internal$parameters$gaussian.mu
  cov_mat <- internal$parameters$gaussian.cov_mat
  x_explain_mat <- as.matrix(internal$data$x_explain)
  n_explain <- internal$parameters$n_explain
  n_features <- internal$parameters$n_features
  n_samples <- internal$parameters$n_samples
  feature_names <- internal$parameters$feature_names
  n_combinations <- internal$parameters$n_combinations

  # Extract the relevant coalitions specified in `index_features` from `S`.
  # This will always be called as `index_features` is never NULL.
  S <- if (!is.null(index_features)) S[index_features, , drop = FALSE]

  # Allocate an empty matrix used in mvnfast:::rmvnCpp to store the generated MC samples.
  B <- matrix(nrow = n_samples, ncol = n_features)
  class(B) <- "numeric"

  .Call("rmvnCpp",
        n_ = n_samples,
        mu_ = rep(0, n_features),
        sigma_ = diag(n_features),
        ncores_ = 1,
        isChol_ = TRUE,
        A_ = B,
        PACKAGE = "mvnfast"
  )

  # Generate a data table containing all Monte Carlo samples for all test observations and coalitions
  dt <- data.table::rbindlist(
    # Iterate over the coalitions
    lapply(
      seq_len(nrow(S)),
      function(S_ind) {
        # This function generates the conditional samples Xsbar | Xs = Xs_star
        # and combine those values with the unconditional values.
        cat(sprintf("%d,", S_ind))

        # Get boolean representations if the features are in the S and the Sbar sets
        S_now <- as.logical(S[S_ind, ])
        Sbar_now <- !as.logical(S[S_ind, ])

        # Remove:
        # Do not need to treat the empty and grand coalitions different as they will never be present
        # if (sum(S_now) %in% c(0, n_features)) {
        #   return(data.table::as.data.table(cbind("id" = seq(n_explain), x_explain)))
        # }

        # Extract the features we condition on
        x_S_star <- x_explain_mat[, S_now, drop = FALSE]

        # Extract the mean values for the features in the two sets
        mu_S <- mu[S_now]
        mu_Sbar <- mu[Sbar_now]

        # Extract the relevant parts of the covariance matrix
        cov_mat_SS <- cov_mat[S_now, S_now, drop = FALSE]
        cov_mat_SSbar <- cov_mat[S_now, Sbar_now, drop = FALSE]
        cov_mat_SbarS <- cov_mat[Sbar_now, S_now, drop = FALSE]
        cov_mat_SbarSbar <- cov_mat[Sbar_now, Sbar_now, drop = FALSE]

        # Compute the covariance matrix multiplication factors/terms and the conditional covariance matrix
        cov_mat_SbarS_cov_mat_SS_inv <- cov_mat_SbarS %*% solve(cov_mat_SS)
        cond_cov_mat_Sbar_given_S <- cov_mat_SbarSbar - cov_mat_SbarS_cov_mat_SS_inv %*% cov_mat_SSbar

        # Ensure that the conditional covariance matrix symmetric in the
        # rare case where numerical instability made it unsymmetrical.
        if (!isSymmetric(cond_cov_mat_Sbar_given_S)) {
          cond_cov_mat_Sbar_given_S <- Matrix::symmpart(cond_cov_mat_Sbar_given_S)
        }

        # Compute the conditional mean of Xsbar given Xs = Xs_star
        x_Sbar_mean <- mu_Sbar + cov_mat_SbarS_cov_mat_SS_inv %*% t(sweep(x_S_star, 2, mu_S, FUN = "-"))

        # Transform the samples to be from N(O, Sigma_Sbar|S)
        # Transpose her and untranspose later for faster matrix addition in `t(B + x_Sbar_mean[, idx_now])`
        # as it seems to be faster than using `sweep(B, 2, x_Sbar_mean[, idx_now], FUN = "+")` on the
        # original B (i.e., not transposed B).
        B_now <- t(B[, Sbar_now] %*% chol(cond_cov_mat_Sbar_given_S))

        # Create a data.table containing the MC samples for all test observations for one coalition
        data.table::rbindlist(

          # Loop over the different test observations
          lapply(seq(n_explain), function(idx_now) {
            # Combine the generated values with the values we conditioned on
            ret <- matrix(NA, ncol = n_features, nrow = n_samples)
            ret[, S_now] <- rep(c(x_explain_mat[idx_now, S_now]), each = n_samples)
            ret[, Sbar_now] <- t(B_now + x_Sbar_mean[, idx_now])

            # Set names of the columns and convert to a data.table
            colnames(ret) <- feature_names
            as.data.table(ret)
          }),
          use.names = TRUE, idcol = "id", fill = TRUE
        )
      }
    ),
    idcol = "id_combination"
  )

  # Update the id_combination. This will always be called as `index_features` is never NULL.
  if (!is.null(index_features)) dt[, id_combination := index_features[id_combination]]

  # Add uniform weights
  dt[, w := 1 / n_samples]

  # Remove:
  # This is not needed when we assume that the empty and grand coalitions will never be present
  # dt[id_combination %in% c(1, n_combinations), w := 1]

  # Return the MC samples
  return(dt)
}

# Here we only want to generate the data once. So we generate n_samples*n_batches from N(0, I),
# and then use Cholensky to transform to N(O, Sigma_{Sbar|S}), and then add the means.
prepare_data_gaussian_new_v6 <- function(internal, index_features, ...) {
  # This function assumes that index_features will never include the empty and
  # grand coalitions. This is valid 21/11/23 as `batch_prepare_vS()` removes the
  # grand coalition before calling the `prepare_data()` function and the empty
  # coalition is never included in the `internal$objects$S_batch` list.

  # Extract objects that we are going to use
  x_explain <- internal$data$x_explain
  S <- internal$objects$S
  mu <- internal$parameters$gaussian.mu
  cov_mat <- internal$parameters$gaussian.cov_mat
  x_explain_mat <- as.matrix(internal$data$x_explain)
  n_explain <- internal$parameters$n_explain
  n_features <- internal$parameters$n_features
  n_samples <- internal$parameters$n_samples
  feature_names <- internal$parameters$feature_names
  n_combinations <- internal$parameters$n_combinations

  # Extract the relevant coalitions specified in `index_features` from `S`.
  # This will always be called as `index_features` is never NULL.
  S <- if (!is.null(index_features)) S[index_features, , drop = FALSE]
  n_combinations_in_this_batch <- nrow(S)

  # Allocate an empty matrix used in mvnfast:::rmvnCpp to store the generated MC samples.
  B <- matrix(nrow = n_samples * n_combinations_in_this_batch, ncol = n_features)
  class(B) <- "numeric"

  .Call("rmvnCpp",
        n_ = n_samples * n_combinations_in_this_batch,
        mu_ = rep(0, n_features),
        sigma_ = diag(n_features),
        ncores_ = 1,
        isChol_ = TRUE,
        A_ = B,
        PACKAGE = "mvnfast"
  )

  # Indices of the start for the combinations
  B_indices <- n_samples * (seq(0, n_combinations_in_this_batch)) + 1

  # Generate a data table containing all Monte Carlo samples for all test observations and coalitions
  dt <- data.table::rbindlist(
    # Iterate over the coalitions
    lapply(
      seq_len(nrow(S)),
      function(S_ind) {
        # This function generates the conditional samples Xsbar | Xs = Xs_star
        # and combine those values with the unconditional values.
        cat(sprintf("%d,", S_ind))

        # Get boolean representations if the features are in the S and the Sbar sets
        S_now <- as.logical(S[S_ind, ])
        Sbar_now <- !as.logical(S[S_ind, ])

        # Remove:
        # Do not need to treat the empty and grand coalitions different as they will never be present
        # if (sum(S_now) %in% c(0, n_features)) {
        #   return(data.table::as.data.table(cbind("id" = seq(n_explain), x_explain)))
        # }

        # Extract the features we condition on
        x_S_star <- x_explain_mat[, S_now, drop = FALSE]

        # Extract the mean values for the features in the two sets
        mu_S <- mu[S_now]
        mu_Sbar <- mu[Sbar_now]

        # Extract the relevant parts of the covariance matrix
        cov_mat_SS <- cov_mat[S_now, S_now, drop = FALSE]
        cov_mat_SSbar <- cov_mat[S_now, Sbar_now, drop = FALSE]
        cov_mat_SbarS <- cov_mat[Sbar_now, S_now, drop = FALSE]
        cov_mat_SbarSbar <- cov_mat[Sbar_now, Sbar_now, drop = FALSE]

        # Compute the covariance matrix multiplication factors/terms and the conditional covariance matrix
        cov_mat_SbarS_cov_mat_SS_inv <- cov_mat_SbarS %*% solve(cov_mat_SS)
        cond_cov_mat_Sbar_given_S <- cov_mat_SbarSbar - cov_mat_SbarS_cov_mat_SS_inv %*% cov_mat_SSbar

        # Ensure that the conditional covariance matrix symmetric in the
        # rare case where numerical instability made it unsymmetrical.
        if (!isSymmetric(cond_cov_mat_Sbar_given_S)) {
          cond_cov_mat_Sbar_given_S <- Matrix::symmpart(cond_cov_mat_Sbar_given_S)
        }

        # Compute the conditional mean of Xsbar given Xs = Xs_star
        x_Sbar_mean <- mu_Sbar + cov_mat_SbarS_cov_mat_SS_inv %*% t(sweep(x_S_star, 2, mu_S, FUN = "-"))

        # Transform the samples to be from N(O, Sigma_Sbar|S)
        # Extract the relevant samples for this combination
        # Transpose her and untranspose later for faster matrix addition in `t(B + x_Sbar_mean[, idx_now])`
        # as it seems to be faster than using `sweep(B, 2, x_Sbar_mean[, idx_now], FUN = "+")` on the
        # original B (i.e., not transposed B).
        B_now <- t(B[B_indices[S_ind]:(B_indices[S_ind + 1] - 1), Sbar_now] %*% chol(cond_cov_mat_Sbar_given_S))

        # Create a data.table containing the MC samples for all test observations for one coalition
        data.table::rbindlist(

          # Loop over the different test observations
          lapply(seq(n_explain), function(idx_now) {
            # Combine the generated values with the values we conditioned on
            ret <- matrix(NA, ncol = n_features, nrow = n_samples)
            ret[, S_now] <- rep(c(x_explain_mat[idx_now, S_now]), each = n_samples)
            ret[, Sbar_now] <- t(B_now + x_Sbar_mean[, idx_now])

            # Set names of the columns and convert to a data.table
            colnames(ret) <- feature_names
            as.data.table(ret)
          }),
          use.names = TRUE, idcol = "id", fill = TRUE
        )
      }
    ),
    idcol = "id_combination"
  )

  # Update the id_combination. This will always be called as `index_features` is never NULL.
  if (!is.null(index_features)) dt[, id_combination := index_features[id_combination]]

  # Add uniform weights
  dt[, w := 1 / n_samples]

  # Remove:
  # This is not needed when we assume that the empty and grand coalitions will never be present
  # dt[id_combination %in% c(1, n_combinations), w := 1]

  # Return the MC samples
  return(dt)
}

# Compare the methods ---------------------------------------------------------------------------------------------

## Setup -----------------------------------------------------------------------------------------------------------

n_samples <- 1000
n_samples <- 25000
n_train <- 1000
n_test <- 500
M <- 8
rho <- 0.5
betas <- c(0, rep(1, M))

# We use the Gaussian approach
approach <- "gaussian"

# Mean of the multivariate Gaussian distribution
mu <- rep(0, times = M)
mu <- seq(M)

# Create the covariance matrix
sigma <- matrix(rho, ncol = M, nrow = M) # Old
for (i in seq(1, M - 1)) {
  for (j in seq(i + 1, M)) {
    sigma[i, j] <- sigma[j, i] <- rho^abs(i - j)
  }
}
diag(sigma) <- 1

# Set seed for reproducibility
seed_setup <- 1996
set.seed(seed_setup)

# Make Gaussian data
data_train <- data.table(mvtnorm::rmvnorm(n = n_train, mean = mu, sigma = sigma))
data_test <- data.table(mvtnorm::rmvnorm(n = n_test, mean = mu, sigma = sigma))
colnames(data_train) <- paste("X", seq(M), sep = "")
colnames(data_test) <- paste("X", seq(M), sep = "")

# Make the response
response_train <- as.vector(cbind(1, as.matrix(data_train)) %*% betas)
response_test <- as.vector(cbind(1, as.matrix(data_test)) %*% betas)

# Put together the data
data_train_with_response <- copy(data_train)[, y := response_train]
data_test_with_response <- copy(data_test)[, y := response_test]

# Fit a LM model
predictive_model <- lm(y ~ ., data = data_train_with_response)

# Get the prediction zero, i.e., the phi0 Shapley value.
prediction_zero <- mean(response_train)

model <- predictive_model
x_explain <- data_test
x_train <- data_train
keep_samp_for_vS <- FALSE
predict_model <- NULL
get_model_specs <- NULL
timing <- TRUE
n_combinations <- NULL
group <- NULL
feature_specs <- get_feature_specs(get_model_specs, model)
n_batches <- 1
seed <- 1

internal <- setup(
  x_train = x_train,
  x_explain = x_explain,
  approach = approach,
  prediction_zero = prediction_zero,
  n_combinations = n_combinations,
  group = group,
  n_samples = n_samples,
  n_batches = n_batches,
  seed = seed,
  feature_specs = feature_specs,
  keep_samp_for_vS = keep_samp_for_vS,
  predict_model = predict_model,
  get_model_specs = get_model_specs,
  timing = timing,
  gaussian.mu = mu,
  gaussian.cov_mat = sigma
)

# Gets predict_model (if not passed to explain)
predict_model <- get_predict_model(
  predict_model = predict_model,
  model = model
)

# Sets up the Shapley (sampling) framework and prepares the
# conditional expectation computation for the chosen approach
# Note: model and predict_model are ONLY used by the AICc-methods of approach empirical to find optimal parameters
internal <- setup_computation(internal, model, predict_model)



## Compare time ----------------------------------------------------------------------------------------------------

# Recall that old version iterate over the observations and then the coalitions.
# While the new version iterate over the coalitions and then the observations.
# The latter lets us reuse the computed conditional distributions for all observations.
look_at_coalitions <- seq(1, 2^M - 2)
look_at_coalitions <- seq(1, 2^M - 2, 10)
look_at_coalitions <- seq(1, 2^M - 2, 25)
time_old <- system.time({
  res_old <- prepare_data_gaussian_old(internal = internal,
                                       index_features = internal$objects$S_batch$`1`[look_at_coalitions])})
res_old <- NULL
# Set to NULL as it is many GB when we look at many combinations in one batch and the methods slow down due to
# little available memory. The same below.

time_new_v1 <- system.time({
  res_new_v1 <- prepare_data_gaussian_new_v1(
    internal = internal,
    index_features = internal$objects$S_batch$`1`[look_at_coalitions])})
res_new_v1 <- NULL

time_new_v2 <- system.time({
  res_new_v2 <- prepare_data_gaussian_new_v2(
    internal = internal,
    index_features = internal$objects$S_batch$`1`[look_at_coalitions])})
res_new_v2 <- NULL

time_new_v3 <- system.time({
  res_new_v3 <- prepare_data_gaussian_new_v3(
    internal = internal,
    index_features = internal$objects$S_batch$`1`[look_at_coalitions])})
res_new_v3 <- NULL

time_new_v4 <- system.time({
  res_new_v4 <- prepare_data_gaussian_new_v4(
    internal = internal,
    index_features = internal$objects$S_batch$`1`[look_at_coalitions])})
res_new_v4 <- NULL

time_new_v5 <- system.time({
  res_new_v5 <- prepare_data_gaussian_new_v5(
    internal = internal,
    index_features = internal$objects$S_batch$`1`[look_at_coalitions])})
res_new_v5 <- NULL

time_new_v6 <- system.time({
  res_new_v6 <- prepare_data_gaussian_new_v6(
    internal = internal,
    index_features = internal$objects$S_batch$`1`[look_at_coalitions])})
res_new_v6 <- NULL

# Create a table of the times. Less is better
times <- rbind(time_old, time_new_v1, time_new_v2, time_new_v3, time_new_v4, time_new_v5, time_new_v6)
times

# Look at the relative time compared to the old method. Larger value is better.
# Tells us how many times faster the new version is.
times_relative <- t(sapply(seq_len(nrow(times)), function(idx) times[1, ] / times[idx, ]))
rownames(times_relative) <- paste0(rownames(times), "_rel")
times_relative

# ALL COALITIONS (look_at_coalitions = seq(1, 2^M-2))
#             user.self sys.self elapsed user.child sys.child
# time_old      195.319   34.193  244.74      0.000     0.000
# time_new_v1    69.243   31.176  114.28      0.000     0.000
# time_new_v2    67.820   31.743  116.54      0.000     0.000
# time_new_v3    61.005   33.955  110.45      0.000     0.000
# time_new_v4    61.426   31.499  105.11      0.000     0.000
# time_new_v5    59.670   33.061  106.08      0.000     0.000
# time_new_v6    61.810   33.342  109.48      0.000     0.000
#                 user.self sys.self elapsed user.child sys.child
# time_old_rel       1.0000   1.0000  1.0000        NaN       NaN
# time_new_v1_rel    2.8208   1.0968  2.1416        NaN       NaN
# time_new_v2_rel    2.8800   1.0772  2.1000        NaN       NaN
# time_new_v3_rel    3.2017   1.0070  2.2159        NaN       NaN
# time_new_v4_rel    3.1797   1.0855  2.3284        NaN       NaN
# time_new_v5_rel    3.2733   1.0342  2.3071        NaN       NaN
# time_new_v6_rel    3.1600   1.0255  2.2354        NaN       NaN


# 26 coalitions (look_at_coalitions = seq(1, 2^M-2, 10))
#             user.self sys.self elapsed user.child sys.child
# time_old       25.913    2.797  30.399          0         0
# time_new_v1     7.071    1.624   8.997          0         0
# time_new_v2     6.653    1.461   8.244          0         0
# time_new_v3     5.700    1.690   7.521          0         0
# time_new_v4     5.877    1.826   7.852          0         0
# time_new_v5     5.522    1.594   7.286          0         0
# time_new_v6     5.559    1.668   7.335          0         0
#                 user.self sys.self elapsed user.child sys.child
# time_old_rel       1.0000   1.0000  1.0000        NaN       NaN
# time_new_v1_rel    3.6647   1.7223  3.3788        NaN       NaN
# time_new_v2_rel    3.8949   1.9144  3.6874        NaN       NaN
# time_new_v3_rel    4.5461   1.6550  4.0419        NaN       NaN
# time_new_v4_rel    4.4092   1.5318  3.8715        NaN       NaN
# time_new_v5_rel    4.6927   1.7547  4.1722        NaN       NaN
# time_new_v6_rel    4.6614   1.6769  4.1444        NaN       NaN


# 11 coalitions (look_at_coalitions = seq(1, 2^M-2, 25))
#             user.self sys.self elapsed user.child sys.child
# time_old       11.251    1.187  12.961      0.000     0.000
# time_new_v1     3.273    0.873   4.306      0.000     0.000
# time_new_v2     3.043    0.690   4.011      0.000     0.000
# time_new_v3     2.677    0.794   3.587      0.000     0.000
# time_new_v4     2.598    0.759   3.460      0.000     0.000
# time_new_v5     2.574    0.752   3.613      0.000     0.000
# time_new_v6     2.303    0.669   3.009      0.000     0.000
#                 user.self sys.self elapsed user.child sys.child
# time_old_rel       1.0000   1.0000  1.0000        NaN       NaN
# time_new_v1_rel    3.4375   1.3597  3.0100        NaN       NaN
# time_new_v2_rel    3.6973   1.7203  3.2314        NaN       NaN
# time_new_v3_rel    4.2028   1.4950  3.6133        NaN       NaN
# time_new_v4_rel    4.3306   1.5639  3.7460        NaN       NaN
# time_new_v5_rel    4.3710   1.5785  3.5873        NaN       NaN
# time_new_v6_rel    4.8854   1.7743  4.3074        NaN       NaN


## Compare mean ----------------------------------------------------------------------------------------------------
look_at_coalition <- 25
one_coalition_time_old <- system.time({
  one_coalition_res_old <- prepare_data_gaussian_old(
    internal = internal,
    index_features = internal$objects$S_batch$`1`[look_at_coalition])})
one_coalition_time_old2 <- system.time({
  one_coalition_res_old2 <- prepare_data_gaussian_old(
    internal = internal,
    index_features = internal$objects$S_batch$`1`[look_at_coalition])})

one_coalition_time_new_v1 <- system.time({
  one_coalition_res_new_v1 <- prepare_data_gaussian_new_v1(
    internal = internal,
    index_features = internal$objects$S_batch$`1`[look_at_coalition])})

one_coalition_time_new_v2 <- system.time({
  one_coalition_res_new_v2 <- prepare_data_gaussian_new_v2(
    internal = internal,
    index_features = internal$objects$S_batch$`1`[look_at_coalition])})

one_coalition_time_new_v3 <- system.time({
  one_coalition_res_new_v3 <- prepare_data_gaussian_new_v3(
    internal = internal,
    index_features = internal$objects$S_batch$`1`[look_at_coalition])})

one_coalition_time_new_v4 <- system.time({
  one_coalition_res_new_v4 <- prepare_data_gaussian_new_v4(
    internal = internal,
    index_features = internal$objects$S_batch$`1`[look_at_coalition])})

one_coalition_time_new_v5 <- system.time({
  one_coalition_res_new_v5 <- prepare_data_gaussian_new_v5(
    internal = internal,
    index_features = internal$objects$S_batch$`1`[look_at_coalition])})

one_coalition_time_new_v6 <- system.time({
  one_coalition_res_new_v6 <- prepare_data_gaussian_new_v6(
    internal = internal,
    index_features = internal$objects$S_batch$`1`[look_at_coalition])})

rbind(one_coalition_time_old,
      one_coalition_time_old2,
      one_coalition_time_new_v1,
      one_coalition_time_new_v2,
      one_coalition_time_new_v3,
      one_coalition_time_new_v4,
      one_coalition_time_new_v5,
      one_coalition_time_new_v6)

internal$objects$S[internal$objects$S_batch$`1`[look_at_coalition], , drop = FALSE]
means_old <- one_coalition_res_old[, lapply(.SD, mean), .SDcols = paste0("X", seq(M)), by = list(id_combination, id)]
means_old2 <- one_coalition_res_old2[, lapply(.SD, mean), .SDcols = paste0("X", seq(M)), by = list(id_combination, id)]
means_v1 <- one_coalition_res_new_v1[, lapply(.SD, mean), .SDcols = paste0("X", seq(M)), by = list(id_combination, id)]
means_v2 <- one_coalition_res_new_v2[, lapply(.SD, mean), .SDcols = paste0("X", seq(M)), by = list(id_combination, id)]
means_v3 <- one_coalition_res_new_v3[, lapply(.SD, mean), .SDcols = paste0("X", seq(M)), by = list(id_combination, id)]
means_v4 <- one_coalition_res_new_v4[, lapply(.SD, mean), .SDcols = paste0("X", seq(M)), by = list(id_combination, id)]
means_v5 <- one_coalition_res_new_v5[, lapply(.SD, mean), .SDcols = paste0("X", seq(M)), by = list(id_combination, id)]
means_v6 <- one_coalition_res_new_v6[, lapply(.SD, mean), .SDcols = paste0("X", seq(M)), by = list(id_combination, id)]

# They are all in the same ballpark, so the differences are due to sampling.
# This is supported by the fact that mean_old and mean_old2 use the same old code, and the difference there is the
# same as for the new methods.
# A larger n_samples makes these closer to 0 (I have done that and for other means too)
max(abs(means_old - means_old2))
max(abs(means_old - means_v1))
max(abs(means_old - means_v2))
max(abs(means_old - means_v3))
max(abs(means_old - means_v4))
max(abs(means_old - means_v5))
max(abs(means_old - means_v6))
