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

  kernel_metric <- ifelse(x$type == "independence", x$type, "gaussian")

  # Handle the computation of all training-test weights for ALL combinations here, before looping
  if (kernel_metric == "independence") {

    # Adds random noise to "fake" a distance between observations
    n <- no_wcomb * nrow(x$x_train)
    W_kernel <- array(
      stats::runif(n),
      dim = c(nrow(x$x_train), no_wcomb)
    )
  } else if(kernel_metric == "gaussian") {

    val <- t(t(-0.5 * x$D) / (x$h_optim_vec)^2)
    W_kernel <- exp(val)
    # To avoid numerical problems for small sigma values, we need to substract some constant from
    # val here. Check if it is possible to do this per column/row of l$D[,i,]
  } else {
    stop("It seems that you've passed a non-valid value when using kernel_metric")
  }

  # Generate permutations of data
  dt <- observation_impute(
    W_kernel = W_kernel,
    S = S[these_wcomb, ],
    Xtrain = Xtrain,
    Xtest = Xtest,
    w_threshold = w_threshold,
    noSamp_MC = noSamp_MC
  )

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
    # dt[, wcomb := these_wcomb[wcomb]] # Correcting originally assigned wcomb
    dt_l[[i]][, w := 1 / x$n_samples]
    dt_l[[i]][, id := i]
  }
  dt <- data.table::rbindlist(dt_l, use.names = TRUE, fill = TRUE)

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
    # dt[, wcomb := these_wcomb[wcomb]] # Correcting originally assigned wcomb
    dt_l[[i]][, w := 1 / x$n_samples]
    dt_l[[i]][, id := i]
  }
  dt <- data.table::rbindlist(dt_l, use.names = TRUE, fill = TRUE)

}
