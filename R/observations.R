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

#' @export
prepare_data <- function(x, ...){
  class(x) <- x$approach
  UseMethod("prepare_data", x)
}

#' @export
prepare_data.empirical <- function(x){

  # Setup
  n_col <- nrow(x$x_test)
  h_optim_mat <- matrix(NA, ncol = n_col, nrow = nrow(x$S))
  h_optim_DT <- as.data.table(h_optim_mat)
  data.table::setnames(h_optim_DT, paste0("Testobs_", seq(nrow(x$x_test))))
  h_optim_DT[, varcomb := .I]
  kernel_metric <- ifelse(x$type == "independence", x$type, "gaussian")

  if (kernel_metric == "independence") {
    # 1. Check that this method works and give the same results as previous steps
    x$w_threshold <- 1

  } else if (kernel_metric == "gaussian") {


    if (x$type == "fixed_sigma") {

      # 2. Check that this method works and give the same results as previous steps
      h_optim_mat[, ] <- x$fixed_sigma_vec

    } else {

      if (empirical_settings$type == "AICc_each_k") {

        # 3. Add functionality to Find h_optim_mat for this option

      } else if (empirical_settings$type == "AICc_full") {

        # 4. Add functionality to Find h_optim_mat for this option

      } else {
        stop("Some error message")
      }
    }
  } else {
    stop("Some error message")
  }

  dt_l <- list()
  for (i in seq(n_col)) {

    D <- x$D[, i, ]
    h_optim_vec <- h_optim_mat[, i]

    if (kernel_metric == "independence") {

      D <- D[sample(nrow(D)),]
      h_optim_vec <- mean(D)*1000

    }

    val <- t(t(-0.5 * D) / h_optim_vec^2)
    W_kernel <- exp(val)

    ## Get imputed data
    dt_l[[i]] <- observation_impute(
      W_kernel = W_kernel,
      S = x$S,
      Xtrain = x$x,
      Xtest = x$x_test[i, , drop = FALSE],
      w_threshold = x$w_threshold,
      noSamp_MC = x$n_samples
    )
    dt_l[[i]][, id := i]

  }

  dt <- data.table::rbindlist(dt_l, use.names = TRUE, fill = TRUE)
  dt[wcomb %in% c(1, max(wcomb)), w := 1.0]

  return(dt)
}

#' @export
prepare_data.gaussian <- function(x){

  n_xtest <- nrow(x$x_test)
  dt_l <- list()
  for (i in seq(n_xtest)) {
    l <- lapply(
      X = x$X$features,
      FUN = sample_gaussian,
      noSamp_MC = x$n_samples,
      mu = x$mu,
      Sigma = x$cov_mat,
      p = ncol(x$x_test),
      Xtest = x$x_test,
      ensure_condcov_symmetry = TRUE
    )

    dt_l[[i]] <- data.table::rbindlist(l, idcol = "wcomb")
    dt_l[[i]][, w := 1 / x$n_samples]
    dt_l[[i]][, id := i]
  }
  dt <- data.table::rbindlist(dt_l, use.names = TRUE, fill = TRUE)
  dt[wcomb %in% c(1, max(wcomb)), w := 1.0]

  return(dt)
}

#' @export
prepare_data.copula <- function(x, x_test){

  n_xtest <- nrow(x$x_test)
  dt_l <- list()

  for (i in seq(n_xtest)) {

    l <- lapply(
      X = x$X$features,
      FUN = sample_copula,
      noSamp_MC = x$n_samples,
      mu = x$mu,
      Sigma = x$cov_mat,
      p = x$n_features,
      Xtest = x$x_test[i,, drop = FALSE],
      Xtrain = as.matrix(x$x_train),
      Xtest_Gauss_trans = x_test
    )

    dt_l[[i]] <- data.table::rbindlist(l, idcol = "wcomb")
    dt_l[[i]][, w := 1 / x$n_samples]
    dt_l[[i]][, id := i]
  }
  dt <- data.table::rbindlist(dt_l, use.names = TRUE, fill = TRUE)
  dt[wcomb %in% c(1, max(wcomb)), w := 1.0]

  return(dt)
}
