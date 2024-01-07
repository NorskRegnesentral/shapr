# Libraries -------------------------------------------------------------------------------------------------------
# library(shapr)
# library(rbenchmark)
library(data.table)
devtools::load_all(".")

# Old R code ------------------------------------------------------------------------------------------------------
#' @inheritParams default_doc
#' @rdname prepare_data
#' @export
prepare_data.copula_old <- function(internal, index_features = NULL, ...) {
  X <- internal$objects$X
  x_train <- internal$data$x_train
  x_explain <- internal$data$x_explain
  n_explain <- internal$parameters$n_explain
  n_samples <- internal$parameters$n_samples
  n_features <- internal$parameters$n_features
  copula.mu <- internal$parameters$copula.mu
  copula.cov_mat <- internal$parameters$copula.cov_mat
  copula.x_explain_gaussian <- internal$data$copula.x_explain_gaussian

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
      FUN = sample_copula_old,
      n_samples = n_samples,
      mu = copula.mu,
      cov_mat = copula.cov_mat,
      m = n_features,
      x_explain = x_explain0[i, , drop = FALSE],
      x_train = as.matrix(x_train),
      x_explain_gaussian = copula.x_explain_gaussian[i, , drop = FALSE]
    )
    dt_l[[i]] <- data.table::rbindlist(l, idcol = "id_combination")
    dt_l[[i]][, w := 1 / n_samples]
    dt_l[[i]][, id := i]
    if (!is.null(index_features)) dt_l[[i]][, id_combination := index_features[id_combination]]
  }

  dt <- data.table::rbindlist(dt_l, use.names = TRUE, fill = TRUE)

  return(dt)
}

#' Sample conditional variables using the Gaussian copula approach
#'
#' @param index_given Integer vector. The indices of the features to condition upon. Note that
#' `min(index_given) >= 1` and `max(index_given) <= m`.
#' @param m Positive integer. The total number of features.
#' @param x_explain_gaussian Numeric matrix. Contains the observation whose predictions ought
#' to be explained (test data),
#' after quantile-transforming them to standard Gaussian variables.
#' @param x_explain Numeric matrix. Contains the features of the observation whose
#' predictions ought to be explained (test data).
#'
#' @return data.table
#'
#' @keywords internal
#'
#' @author Martin Jullum
sample_copula_old <- function(index_given, n_samples, mu, cov_mat, m, x_explain_gaussian, x_train, x_explain) {
  # Handles the unconditional and full conditional separtely when predicting
  if (length(index_given) %in% c(0, m)) {
    ret <- matrix(x_explain, ncol = m, nrow = 1)
  } else {
    dependent_ind <- (seq_len(length(mu)))[-index_given]

    tmp <- condMVNorm::condMVN(
      mean = mu,
      sigma = cov_mat,
      dependent.ind = dependent_ind,
      given.ind = index_given,
      X.given = x_explain_gaussian[index_given]
    )

    ret0_z <- mvnfast::rmvn(n = n_samples, mu = tmp$condMean, sigma = tmp$condVar)

    ret0_x <- apply(
      X = rbind(ret0_z, x_train[, dependent_ind, drop = FALSE]),
      MARGIN = 2,
      FUN = inv_gaussian_transform_old,
      n_z = n_samples,
      type = 5
    )

    ret <- matrix(NA, ncol = m, nrow = n_samples)
    ret[, index_given] <- rep(x_explain[index_given], each = n_samples)
    ret[, dependent_ind] <- ret0_x
  }
  colnames(ret) <- colnames(x_explain)
  return(as.data.table(ret))
}


#' Transforms new data to a standardized normal distribution
#'
#' @param zx Numeric vector. The first `n_z` items are the Gaussian data, and the last part is
#' the data with the original transformation.
#' @param n_z Positive integer. Number of elements of `zx` that belongs to new data.
#'
#' @return Numeric vector of length `n_z`
#'
#' @keywords internal
#'
#' @author Martin Jullum
inv_gaussian_transform_old <- function(zx, n_z, type) {
  if (n_z >= length(zx)) stop("n_z should be less than length(zx)")
  ind <- 1:n_z
  z <- zx[ind]
  x <- zx[-ind]
  u <- stats::pnorm(z)
  x_new <- stats::quantile(x, probs = u, type = type)
  return(as.double(x_new))
}




# Setup -----------------------------------------------------------------------------------------------------------
{
  n_samples <- 1000
  n_train <- 1000
  n_test <- 6
  M <- 8
  rho <- 0.5
  betas <- c(0, rep(1, M))

  # We use the Gaussian copula approach
  approach <- "copula"

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
    timing = timing
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
}

# Compare ---------------------------------------------------------------------------------------------------------

# Recall that old version iterate over the observations and then the coalitions.
# While the new version iterate over the coalitions and then the observations.
# The latter lets us reuse the computed conditional distributions for all observations.
look_at_coalitions <- seq(1, 2^M - 2)
look_at_coalitions <- seq(1, 2^M - 2, 10)
#look_at_coalitions <- seq(1, 2^M - 2, 25)

# The old R code
time_old <- system.time({
  res_old <- prepare_data.copula_old(
    internal = internal,
    index_features = internal$objects$S_batch$`1`[look_at_coalitions])})

# The new C++ code
time_new <- system.time({
  res_new <- prepare_data.copula(
    internal = internal,
    index_features = internal$objects$S_batch$`1`[look_at_coalitions])})
setorderv(res_new, c("id", "id_combination"))

# Time
time_old
time_new

# Relative speedup of new method
time_old/time_new

# Aggregate the MC sample values for each explicand and combination
res_old = res_old[,w:=NULL]
res_new = res_new[,w:=NULL]
res_old_agr = res_old[, lapply(.SD, mean), by = c("id", "id_combination")]
res_new_agr = res_new[, lapply(.SD, mean), by = c("id", "id_combination")]

# Difference
res_old_agr - res_new_agr

# Max absolute difference
max(abs(res_old_agr - res_new_agr))

# Max absolute relative difference
max(abs((res_old_agr - res_new_agr)/res_new_agr))
