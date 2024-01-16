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

  max_id_combination <- X[,.N]
  n_permutations_used <- X_perm[,max(permute_id,na.rm=TRUE)]

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

  # Counting the number of repetitions of each row in S
  id_combination_reps <- X_perm[,.N,by=id_combination]
  id_combination_reps[c(1,.N),N:=n_permutations_used]

  # Computing the US and QS objects

  # Now using the formulas form True to the model or true to the data paper: https://arxiv.org/pdf/2006.16234.pdf
  # Consider whether doing this with sparse matrices is faster
  PS_full <- diag(n_features)
  PS <- apply(S,FUN = function(x)PS_full[which(x==1),,drop = FALSE],MARGIN = 1)
  PSbar <- apply(1-S,FUN = function(x)PS_full[which(x==1),,drop = FALSE],MARGIN = 1)

  # TODO: Should do this in rcpp for speed up later
  US_list <- QS_list <- QSbar_list <- list()
  US_list[[1]] <- US_list[[nrow(S)]] <- QS_list[[1]] <- matrix(0,nrow = n_features,ncol = n_features)
  QS_list[[nrow(S)]] <-  diag(n_features)
  for (i in seq(2,nrow(S)-1)){
    US_list[[i]] <- t(PSbar[[i]])%*%PSbar[[i]]%*%gaussian.cov_mat%*%t(PS[[i]])%*%solve(PS[[i]]%*%gaussian.cov_mat%*%t(PS[[i]]))%*%PS[[i]]
    QS_list[[i]] <- t(PS[[i]])%*%PS[[i]]
#    QSbar_list[[i]] <- t(PSbar[[i]])%*%PSbar[[i]] # Make this every time as well since it is fast to do, even though it could be extracted from QS_list if it is always present. Change this later
  }

  ### Computing the Tmu and Tx objects
  # A rewrite of eq (9) in the true to the model or data paper: https://arxiv.org/pdf/2006.16234.pdf shows that
  # when we force paired sampling, the Tmu and Tx objects gets a simplified formula.
  # Another trick is that since the full S matrix is constructed based on the permutations,
  # any row in S that contains features j, will have a corresponding row that does not contain feature j.
  # Thus, we can easily find all the rows that contain feature j, and those that don't to then compute Q and U differences
  # without having to map the permutations to the subsets.

  perm_dt <- internal$objects$perm_dt

  Tmu_list <- Tx_list <- list()
  these_id_combinations_mat <- list()
  for(j in seq_len(n_features)){
    Tmu_list[[j]] <- Tx_list[[j]] <- matrix(0,nrow = n_features,ncol = n_features)
    these_id_combinations_mat[[j]] <- numeric(0)
    for(i in seq(n_permutations_used)){

      perm0 <- perm_dt[permute_id==i,perm][[1]]

      position <- which(perm0==j)
      if(position==1){
        this_S <- integer(0)
        this_S_plus_j <- sort(perm0[seq_len(position)])
        this_Sbar <- sort(perm0)
        this_Sbar_min_j <- sort(perm0[-seq_len(position)])
      } else {
        this_S <- sort(perm0[seq_len(position-1)])
        this_S_plus_j <- sort(perm0[seq_len(position)])
        this_Sbar <- sort(perm0[-seq_len(position-1)])
        this_Sbar_min_j <- sort(perm0[-seq_len(position)])
      }

      vec <- c(paste0(this_S,collapse = " "),
               paste0(this_S_plus_j,collapse = " "),
               paste0(this_Sbar,collapse = " "),
               paste0(this_Sbar_min_j,collapse = " "))

      merge_dt <- data.table(features_tmp=vec,id=seq_along(vec))

      comb_dt <- merge(merge_dt,X[,.(id_combination,features_tmp)],by="features_tmp",all.x=TRUE)
      data.table::setorderv(comb_dt,"id")

      these_id_combinations <- comb_dt[,id_combination]
      S_id_comb <- these_id_combinations[1]
      S_plus_j_id_comb <- these_id_combinations[2]
      Sbar_id_comb <- these_id_combinations[3]
      Sbar_min_j_id_comb <- these_id_combinations[4]

      Qdiff1 <- QS_list[[Sbar_min_j_id_comb]]-QS_list[[Sbar_id_comb]]
      Qdiff2 <- QS_list[[S_plus_j_id_comb]]-QS_list[[S_id_comb]]
      Udiff <- US_list[[S_plus_j_id_comb]]-US_list[[S_id_comb]]

      Tmu_list[[j]] <- Tmu_list[[j]] + (Qdiff1-Udiff)/ n_permutations_used
      Tx_list[[j]] <- Tx_list[[j]]+ (Qdiff2+Udiff)/ n_permutations_used

      #Tmu_list[[j]] <- Tmu_list[[j]] + (Qdiff1-Udiff)/ n_permutations_used
      #Tx_list[[j]] <- Tx_list[[j]]+ (Qdiff2-Udiff)/ n_permutations_used

      # Udiff <- Reduce("+",US_list[includes_j])-Reduce("+",US_list[!includes_j])
      #
      # Tmu_list[[j]] <- (Qdiff-Udiff) / n_permutations_used
      # Tx_list[[j]] <- (Qdiff+Udiff) / n_permutations_used

      these_id_combinations_mat[[j]] <- rbind(these_id_combinations_mat[[j]],these_id_combinations)
    }

    # includes_j <- S[,j]==1 # Find all subsets that include feature j
    # id_combination_reps[id_combination%in%which(includes_j),N]
    #
    # Qdiff <- Reduce("+",QS_list[includes_j])-Reduce("+",QS_list[!includes_j])
    # Udiff <- Reduce("+",US_list[includes_j])-Reduce("+",US_list[!includes_j])
    #
    # Tmu_list[[j]] <- (Qdiff-Udiff) / n_permutations_used
    # Tx_list[[j]] <- (Qdiff+Udiff) / n_permutations_used

  }

  internal$objects$US_list <- US_list
  internal$objects$QS_list <- QS_list
  internal$objects$Tmu_list <- Tmu_list
  internal$objects$Tx_list <- Tx_list
  internal$parameters$gaussian.mu <- gaussian.mu
  internal$parameters$gaussian.cov_mat <- gaussian.cov_mat

  return(internal)
}

SRfun <- function(permute_id0=1,feature=3,max_id_combination=X[,.N]){
  perm0 <- perm_dt[permute_id==permute_id0,perm]
  position <- which(perm[[1]]==feature)

  these_id_combinations <- c(1,X_perm[permute_id==permute_id0,id_combination],max_id_combination)
  SR_and_SRi <- these_id_combinations[c(position,position+1)]

  return(SR_and_SRi)
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
