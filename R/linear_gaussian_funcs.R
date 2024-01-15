# Here I put both the setup functios and the compute_linear_gaussian functions




#' @rdname setup_approach
#'
#' @inheritParams default_doc_explain
#' @inheritParams setup_approach.gaussian
#'
#' @export
setup_linear_gaussian <- function(internal,
                                  gaussian.mu = NULL,
                                  gaussian.cov_mat = NULL, ...) {

  x_train <- internal$data$x_train
  n_explain <- internal$parameters$n_explain
  gaussian.cov_mat <- internal$parameters$gaussian.cov_mat
  gaussian.mu <- internal$parameters$gaussian.mu
  n_features <- internal$parameters$n_features
  linear_model_coef <- internal$parameters$linear_model_coef
  S <- internal$objects$S
  X <- internal$objects$X
  X_perm <- internal$objects$X_perm

  max_id_combinations <- X[,.N]

  # For consistency
  defaults <- mget(c("gaussian.mu", "gaussian.cov_mat"))
  internal <- insert_defaults(internal, defaults)

  x_train <- internal$data$x_train
  feature_specs <- internal$objects$feature_specs

  # Checking if factor features are present
  if (any(feature_specs$classes == "factor")) {
    factor_features <- names(which(feature_specs$classes == "factor"))
    factor_approaches <- get_factor_approaches()
    stop(paste0(
      "The following feature(s) are factor(s): ", factor_features, ".\n",
      "approach = 'linear_gaussian' does not support factor features.\n",
      "Please change approach to one of ", paste0(factor_approaches, collapse = ", "), "."
    ))
  }

  # If gaussian.mu is not provided directly in internal list, use mean of training data
  if (is.null(gaussian.mu)) {
    gaussian.mu <- get_mu_vec(x_train)
  }

  # If gaussian.cov_mat is not provided directly in internal list, use sample covariance of training data
  if (is.null(gaussian.cov_mat)) {
    gaussian.cov_mat <- get_cov_mat(x_train)
  }

  # Computing the US and QS objects




  # To get rid of the smallest and largest subset
  S <- S[-c(1,max_id_combinations),]
  Sbar <- 1-S


  # Now using the formulas form True to the model or true to the data paper: https://arxiv.org/pdf/2006.16234.pdf
  # Consider whether doing this with sparse matrices is faster
  PS_full <- diag(n_features)
  PS <- apply(S,FUN = function(x)PS_full[which(x==1),,drop = FALSE],MARGIN = 1)
  PSbar <- apply(1-S,FUN = function(x)PS_full[which(x==1),,drop = FALSE],MARGIN = 1)

  # TODO: Should do this in rcpp for speed up later
  US_list <- QS_list <- QSbar_list <- list()
  for (i in seq_len(nrow(S))){
    US_list[[i]] <- t(PSbar[[i]])%*%PSbar[[i]]%*%gaussian.cov_mat%*%t(PS[[i]])%*%solve(PS[[i]]%*%gaussian.cov_mat%*%t(PS[[i]]))
    QS_list[[i]] <- t(PS[[i]])%*%PS[[i]]
    QSbar_list[[i]] <- t(PSbar[[i]])%*%PSbar[[i]] # Make this every time as well since it is fast to do, even though it could be extracted from QS_list if it is always present. Change this later
  }

  ### Computing the Tmu and Tx objects
  # Loop over each feature and create Tmu and Tx lists
  # Need to do some mapping from the permutations to the SRbar, SRbar+i, SRbar+i, SRbar+i subsets, probably similar to
  # That done in compute_shapley_permutation()

  Tmu_list <- Tx_list <- list()
  for(j in seq_len(n_features)){
    for(i in seq(n_permutations_used)){

      #### JUST SOME COPIED CODE BELOW. NEED TO CONTINUE FROM HERE, figuring out how to map the permutations to the subsets

      # Find id combinations that are permuted
      these_id_combs <- c(1,X_perm[permute_id==i,id_combination],max_id_combination)

      # Find the feature to map the contributions to
      mapping_mat <- apply(S[these_id_combs,],FUN=diff,MARGIN=2)
      contributes_to <- apply(mapping_mat,FUN=function(x) which(x==1),MARGIN=1)
      reorder_vec <- order(contributes_to)

  }

  return(internal)
}



#' @rdname prepare_data
#' @export
prepare_data.linear_gaussian <- function(internal, index_features = NULL, ...) {
  x_train <- internal$data$x_train
  x_explain <- internal$data$x_explain
  n_explain <- internal$parameters$n_explain
  gaussian.cov_mat <- internal$parameters$gaussian.cov_mat
  gaussian.mu <- internal$parameters$gaussian.mu
  n_features <- internal$parameters$n_features
  linear_model_coef <- internal$parameters$linear_model_coef
  S <- internal$objects$S


  X <- internal$objects$X

  x_explain0 <- as.matrix(x_explain)
  dt_l <- list()


  #TODO: Move this test to somewhere else.
  #  if (!is.null(index_features)) {
  #    stop("approach = 'linear_gaussian' can only be applied with n_batches = 1.")
  #  }


  Tmu_list <- Tx_list <- list()
  for (i in seq_len(nrow(S))){

  }

  # OK, I just stop here now, as I realize I should do this in 2 different ways:
  # 1. new explain function which uses permutations, then computes the Tmu and Tx above in a new function (not prepare_data)
  #    to then compute the Shapley values with formula (9) in the true to the model or data paper
  # 2. Create a lingauss-approach which computes the v(S) with the formula in the appendix of our paper, since to compare with
  #    kernelSHAP, I will be needing that v(S) formula directly
  # 3. I can then compare the different approaches (the 2 permutation approaches with different samplign schemes using 1, and with kernelshap using 2.)


  for (i in seq_len(n_explain)) {
    l <- lapply(
      X = features,
      FUN = sample_gaussian,
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
