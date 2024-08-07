library(shapr)

actual_feature_translator_func <- function(oldvec,num_feat_vec){
  newvec <- num_feat_vec[oldvec]
  return(newvec)
}

rev_actual_feature_translator_func <- function(newvec,num_feat_vec){
  oldvec <- match(newvec,num_feat_vec) # Check if this is correct
  return(oldvec)
}


feature_set_sample <- function(feature_sample_prev = NULL, m, n_combinations_sample = 200, unique_sampling = TRUE,paired_sampling = FALSE) {

  #n_combinations_sample specifies the number of NEW n_combinations that shoudl be samples (it does not account for the full and zero set).
  # if unique_sampling is TRUE, then the sampling will continue until the number of unique samples is equal to n_combinations_sample
  # if paired_sampling is TRUE, then to the total number of combinations is equal to n_combinations_sample

  # Find weights for given number of features ----------


  n_features <- seq(m - 1)
  n <- sapply(n_features, choose, n = m)
  w <- shapr:::shapley_weights(m = m, N = n, n_features) * n
  p <- w / sum(w)

  if(is.null(feature_sample_prev)){
    feature_sample_prev <- list()
    unique_samples_prev <- unique_samples_current <- 0
  } else {
    unique_samples_prev <- unique_samples_current <- length(unique(feature_sample_prev))
  }

  feature_sample_current_1 <- feature_sample_current_2 <- NULL

  if (unique_sampling) {
    unique_samples_new <- unique_samples_current-unique_samples_prev
    while (unique_samples_new < n_combinations_sample) {
      remaining_samples <- n_combinations_sample-unique_samples_new

      # Sample number of chosen features ----------
      n_features_sample <- sample(
        x = n_features,
        size = remaining_samples*ifelse(paired_sampling,0.5,1), # Sample -2 as we add zero and m samples below
        replace = TRUE,
        prob = p
      )

      # Sample specific set of features -------
      feature_sample_0 <- shapr:::sample_features_cpp(m, n_features_sample)
      if(paired_sampling){
        feature_sample_1 <- feature_sample_0
        feature_sample_2 <- lapply(feature_sample_0, function(x) seq(m)[-x])
      } else {
        feature_sample_1 <- feature_sample_0[seq_len(n_combinations_sample*0.5)]
        feature_sample_2 <- feature_sample_0[-seq_len(n_combinations_sample*0.5)]
      }

      feature_sample_current_1 <- c(feature_sample_current_1, feature_sample_1)
      feature_sample_current_2 <- c(feature_sample_current_2, feature_sample_2) # The paired copy

      unique_samples_current <- length(unique(c(feature_sample_prev,feature_sample_current_1,feature_sample_current_2)))

      unique_samples_new <- unique_samples_current-unique_samples_prev
    }
  } else {

    n_features_sample <- sample(
      x = n_features,
      size = n_combinations_sample*ifelse(paired_sampling,0.5,1), # Sample -2 as we add zero and m samples below
      replace = TRUE,
      prob = p
    )
    feature_sample_0 <- shapr:::sample_features_cpp(m, n_features_sample)
    if(paired_sampling){
      feature_sample_1 <- feature_sample_0
      feature_sample_2 <- lapply(feature_sample_0, function(x) seq(m)[-x])
    } else {
      feature_sample_1 <- feature_sample_0[seq_len(n_combinations_sample*0.5)]
      feature_sample_2 <- feature_sample_0[-seq_len(n_combinations_sample*0.5)]
    }

    feature_sample_current_1 <- c(feature_sample_current_1, feature_sample_1)
    feature_sample_current_2 <- c(feature_sample_current_2, feature_sample_2) # The paired copy

  }

  return(list(feature_sample_current_1,feature_sample_current_2))

}

# Not sure if I actually need the sample_ids stuff here.
X_from_feature_set_v3 <- function(feature_sample_all = NULL, m, weight_zero_m = 10^6,sample_ids=seq_along(feature_sample_all)) {
  # Unsure if I actually need the sample_ids stuff

  # Find weights for given number of features ----------
  n_features <- seq(m - 1)
  n <- sapply(n_features, choose, n = m)
  w <- shapr:::shapley_weights(m = m, N = n, n_features) * n
  p <- w / sum(w)

  # Add zero and m features
  feature_sample_all <- c(list(integer(0)), feature_sample_all, list(c(1:m)))
  X <- data.table(n_features = sapply(feature_sample_all, length))
  X[, n_features := as.integer(n_features)]

  # Get number of occurences and duplicated rows-------
  is_duplicate <- NULL # due to NSE notes in R CMD check
  r <- shapr:::helper_feature(m, feature_sample_all)
  X[, is_duplicate := r[["is_duplicate"]]]

  # When we sample combinations the Shapley weight is equal
  # to the frequency of the given combination
  X[, shapley_weight := r[["sample_frequence"]]]

  # Populate table and remove duplicated rows -------
  X[, S := feature_sample_all]
  X[-c(1,.N), sample_id:=sample_ids]

  if (any(X[["is_duplicate"]])) {
    X <- X[is_duplicate == FALSE]
  }
  X[, is_duplicate := NULL]
  data.table::setkeyv(X, "n_features")

  # Make feature list into character
  X[, S_char := sapply(S, paste0, collapse = "_")]

  # Aggregate weights by how many samples of a combination we observe
  X <- X[, .(
    n_features = data.table::first(n_features),
    shapley_weight = sum(shapley_weight),
    S = S[1],
    sample_id = sample_id
  ), by = S_char]


  data.table::setorder(X, n_features)

  # Add shapley weight and number of combinations
  X[c(1, .N), shapley_weight := weight_zero_m]
  X[, N := 1]
  ind <- X[, .I[data.table::between(n_features, 1, m - 1)]]
  X[ind, p := p[n_features]]
  X[ind, N := n[n_features]]

  # Adding Sbar to X
  X[,Sbar:=lapply(S, function(x) seq(m)[-x])]
  X[n_features==0,Sbar:=list(seq(m))]
  X[, Sbar_char := sapply(Sbar, paste0, collapse = "_")]

  # Set column order and key table
  data.table::setkeyv(X, "n_features")

  X[, id_combination := .I]
  X[, N := as.integer(N)]
  nms <- c("id_combination", "S","Sbar", "n_features", "N", "shapley_weight", "p","sample_id")
  data.table::setcolorder(X, nms)


  return(X)
}


# This is the shapr:::feature_not_exact function where feature_sample_all is added as an optional input.
# If that vector is set, the sampling is appended to that vector

sample_cov_estimator <- function(feature_sample_all,dt_vS,testObs_computed,n_var_est_reps = 10,n_var_est_groups = 10,m,comp_strategy = "all_combined", shapley_reweighting_strategy = "on_N",return = "sd_mat",
                                 paired_sampling = paired_sampling,
                                 feature_names = NULL){


  sample_var_dt_kshap_list <- list()
  for(j in seq_len(n_var_est_reps)){


    if(paired_sampling == TRUE){ # Assuming the second half is the paired copy of the first half of the feature samples
      halfway <- length(feature_sample_all)/2
      feature_sample_initial <- feature_sample_all[1:halfway]

      # Covert-approach by splitting the feature samples into subgroups
      feature_sample_initial_randomorder <- sample(seq_along(feature_sample_initial))
      # Split the feature samples into n_var_est_groups different subgroups of approximately equal size
      feature_sample_inital_randomsplit <- split(feature_sample_initial_randomorder, ceiling(seq(10^(-20),n_var_est_groups,length.out = length(feature_sample_initial_randomorder))))
      feature_sample_all_randomsplit <- lapply(feature_sample_inital_randomsplit,function(x)c(x,x+halfway))

    } else {
      # Covert-approach by splitting the feature samples into subgroups
      feature_sample_all_randomorder <- sample(seq_along(feature_sample_all))
      # Split the feature samples into n_var_est_groups different subgroups of approximately equal size
      feature_sample_all_randomsplit <- split(feature_sample_all_randomorder, ceiling(seq(10^(-20),n_var_est_groups,length.out = length(feature_sample_all_randomorder))))

    }



    sample_var_list <-  list()
    for (i in seq_len(n_var_est_groups)){

      these_ids <- feature_sample_all_randomsplit[[i]]
      X_tmp <- X_from_feature_set_v3(feature_sample_all[these_ids],m=m,sample_ids=these_ids)[] # sample_ids could be removed from the function -- never used

      X_tmp[,S_char:=sapply(S,function(x)paste0(x, collapse = "_"))]

      X_rw_tmp <- shapley_reweighting(X_tmp,strategy = shapley_reweighting_strategy)

      # Get weighted matrix ----------------
      X_rw_tmp[,features:=S] # Since shapr:::weight_matrix needs this name
      W_tmp <- shapr:::weight_matrix(
        X = X_rw_tmp,
        normalize_W_weights = TRUE,
        is_groupwise = FALSE
      )

      dt_vS_relevant <- dt_vS[X_tmp[,.(S_char)],, on = "S_char"] # This merges and orderes the dt_vS by the order of the features in X_tmp

      kshap <- t(W_tmp %*% as.matrix(dt_vS_relevant[, -c("S_char","Sbar_char")]))
      dt_kshap <- data.table::as.data.table(kshap)
      colnames(dt_kshap) <- c("none", feature_names)
      dt_kshap[,id:=testObs_computed]

      sample_var_list[[i]] <- copy(dt_kshap)

    }

    sample_var_dt_kshap_list[[j]] <- rbindlist(sample_var_list)
  }


  sample_cov_mat_full_list <- list()
  n_indep_samps_full <- length(feature_sample_all)
  n_indep_samps_group <- mean(sapply(feature_sample_all_randomsplit,length))

  if(comp_strategy=="all_combined"){
    sample_var_dt_kshap_all <- rbindlist(sample_var_dt_kshap_list)

    sample_cov_mat_full_list <- list()
    for(k in seq_along(testObs_computed)){
      sample_cov_mat_full_list[[k]] <- n_indep_samps_group/n_indep_samps_full*sample_var_dt_kshap_all[id==k,cov(.SD),.SDcols = -"id"]
    }


  } else {
    for(k in seq_along(testObs_computed)){
      tmp_cov_list <- list()
      for(j in seq_len(n_var_est_reps)){
        tmp_cov_list[[j]] <- sample_var_dt_kshap_list[[j]][id==testObs_computed[k],cov(.SD),.SDcols = -"id"]
      }
      mean_sample_cov <- Reduce("+",tmp_cov_list)/n_var_est_reps

      sample_cov_mat_full_list[[k]] <- n_indep_samps_group/n_indep_samps_full*mean_sample_cov

    }
  }


  if(return == "sd_mat"){
    ret <- t(sapply(sample_cov_mat_full_list,function(x) sqrt(diag(x))))
    ret_dt <- as.data.table(ret)
    ret_dt[,id:=testObs_computed]
    ret_dt[,sd_type:=paste0("sample_",comp_strategy)]
    setcolorder(ret_dt,c("sd_type","id"))
    return(ret_dt)
  } else {
    return(sample_cov_mat_full_list)
  }

}

boot_cov_estimator <- function(feature_sample_all_1,feature_sample_all_2,dt_vS,testObs_computed,n_boot_ests = 100,m,shapley_reweighting_strategy = "on_N",return = "sd_mat",
                               paired_sampling = paired_sampling,
                               feature_names = NULL,
                               seed = 123){

  set.seed(seed)

  feature_sample_all <- c(feature_sample_all_1,feature_sample_all_2)
  halfway <- length(feature_sample_all_1)

  boot_var_list <- list()

  for (i in seq_len(n_boot_ests)){

    if(paired_sampling == TRUE){ # Assuming the second half is the paired copy of the first half of the feature samples
      these_ids0 <- sample(seq_len(halfway),replace = TRUE)
      these_ids <- c(these_ids0,these_ids0+halfway)

    } else {
      these_ids <- sample(seq_len(halfway*2),replace = TRUE)
    }

    X_tmp0 <- X_from_feature_set_v3(feature_sample_all[these_ids],m=m,sample_ids=these_ids)[] # sample_ids could be removed from the function -- never used

    X_tmp <- shapley_reweighting(X_tmp0,strategy = shapley_reweighting_strategy)

    # Get weighted matrix ----------------
    X_tmp[,features:=S] # Since shapr:::weight_matrix needs this name
    W_tmp <- shapr:::weight_matrix(
      X = X_tmp,
      normalize_W_weights = TRUE,
      is_groupwise = FALSE
    )

    dt_vS_relevant <- dt_vS[X_tmp[,.(S_char)],, on = "S_char"] # This merges and orderes the dt_vS by the order of the features in X_tmp

    kshap <- t(W_tmp %*% as.matrix(dt_vS_relevant[, -c("S_char","Sbar_char")]))
    dt_kshap <- data.table::as.data.table(kshap)
    colnames(dt_kshap) <- c("none", feature_names)
    dt_kshap[,id:=testObs_computed]

    boot_var_list[[i]] <- copy(dt_kshap)

  }

  boot_var_dt_kshap <- rbindlist(boot_var_list)


  boot_cov_mat_full_list <- list()
  for(k in seq_along(testObs_computed)){
    boot_cov_mat_full_list[[k]] <- boot_var_dt_kshap[id==testObs_computed[k],cov(.SD),.SDcols = -"id"]
  }

  if(return == "sd_mat"){
    ret <- t(sapply(boot_cov_mat_full_list,function(x) sqrt(diag(x))))
    ret_dt <- as.data.table(ret)
    ret_dt[,id:=testObs_computed]
    if(paired_sampling){
      ret_dt[,sd_type:="boot_paired"]
    } else {
      ret_dt[,sd_type:="boot_unpaired"]
    }
    setcolorder(ret_dt,c("sd_type","id"))
    return(ret_dt)
  } else {
    return(boot_cov_mat_full_list)
  }
}

shapley_reweighting <- function(XX,strategy = "on_N"){

  XX2 <- copy(XX)
  m <- XX2[.N,n_features]
  XX2[,shapley_weight:=as.numeric(shapley_weight)]

  if(strategy=="on_N"){
    XX2[,shapley_weight:=mean(shapley_weight),by=N]
  } else if(strategy=="on_n_features"){
    XX2[,shapley_weight:=mean(shapley_weight),by=n_features]
  } else if(strategy=="on_all"){
    XX2[,shapley_weight := shapr:::shapley_weights(m = m, N = N, n_components = n_features, weight_zero_m=10^6)]
  } # strategy= "none" or something else do nothing

  return(XX2)
}

#Pr(∣X∣>t)=1−Pr(−t<X<t).
probfunc <- function(est,sd,shapley_threshold_val){
  prob <- 1 - (pnorm(shapley_threshold_val, mean = est, sd = sd) - pnorm(-shapley_threshold_val, mean = est, sd = sd))
  return(prob)
}




iterative_kshap_func <- function(model,
                                 x_explain,
                                 x_train,
                                 predict_model,
                                 testObs_computed, # Only a single test observation for now
                                 cutoff_feats, # Vector of feature names to be included in the estimation (features in x_explain/x_train which are not in cutoff_feats are fixed to the value in x_explain)
                                 initial_n_combinations = min(20,2^length(cutoff_feats)-2), # Number of features combinations to be used in the first iteration
                                 n_combinations_per_iter = 10, # Number of feature combination per iteration (where shapley values + variance is estimated and we test for reduction)
                                 max_n_combinations = NULL,
                                 n_boot_ests = 50, # Number of bootstrap datasets to be used to estimate the variance of the shapley values
                                 unique_sampling = TRUE, # Whether to initial_n_combinations and n_combinations_per_iter refer to the number of unique samples (TRUE) or the total number of samples (FALSE)
                                 paired_sampling = TRUE, # whether the feature combinations should be sampled in a paired way (this also effects the bootstrap estimation)
                                 shapley_reweighting_strategy = "on_N", # How to reweight the shapley values ("on_N", "on_n_features", "on_all", "none" are the different options. See the shapley_reweighting function for details)
                                 full_pred, # The full prediction value to decompose
                                 shapsum_other_features, # The sum of the shapley values of the features not in cutoff_feats (this is excluded from the decomposition onto the cutoff_feats
                                 p0, # The mean prediction not decomposed to any features
                                 shapley_threshold_val = 0.1, # Upper threshold for when to remove a feature value from further shapley computation
                                 shapley_threshold_prob = 0.1, # Required certainty that the shapley value exceeds shapley_threshold_val when cutting it off. (I.e. a feature j is removed if # Reduce if P(\phi_j > shapley_threshold_val) < shapley_threshold_prob) )
                                 approach ="ctree", # Approach used to estimate the shapley values
                                 n_samples = 1000, # Number of samples used in the Gaussian method
                                 gaussian.mu = NULL, # The mean of the Gaussian distribution used in the Gaussian method
                                 gaussian.cov_mat = NULL, # The covariance matrix of the Gaussian distribution used in the Gaussian method
                                 ctree.mincriterion = 0.95, # The minimum criterion for the ctree method
                                 ctree.minsplit = 20, # The minimum split for the ctree method
                                 ctree.minbucket = 7, # The minimum bucket for the ctree method
                                 ctree.sample = TRUE, # Whether to sample in the ctree method
                                 all_trees = NULL){ # list of trees to reuse in the ctree method. This feature

  if (!is.null(max_n_combinations)){
    max_n_combinations = min(max_n_combinations, 2^nrow(x_train[, ..cutoff_feats]))
  }

  ### SOME SETUP ###

  m <- length(cutoff_feats)#max_cutoff_features-0

  feature_names <- names(x_train)

  excluded_feature_cols <- setdiff(names(x_train),cutoff_feats)

  # We ignore a feature from further computations once Pr(|\phi_j| > shapley_threshold_val) < shapley_threshold_prob

  feature_sample_all <- feature_sample_prev_1 <- feature_sample_prev_2 <- NULL
  keep_list <- list()
  set.seed(123)
  iter <- 1
  converged = FALSE
  shap_it_excluded_features <- 0

  #remaining_cutoff_feats <- cutoff_feats[-seq_len(m)]

  num_feat_vec <- seq(m)

  S_mapper_list <- list()
  S_mapper_list[[1]] <- data.table(feature_numbers = seq(m),
                                   feature_names = cutoff_feats[seq(m)])

  current_excluded_feature_cols <- excluded_feature_cols

  current_x_train_red <- x_train[,..cutoff_feats]
  current_x_explain_red <- x_explain[,..cutoff_feats]

  if(!is.null(all_trees)){
    S_all_trees <- lapply(all_trees,"[[","given_ind")
    feat_names_all_trees <- sapply(S_all_trees, function(x)paste0(cutoff_feats[x],collapse="_"))
  }

  fixed_kshap_est_dt <- NULL
  kshap_est_dt_list <- kshap_sd_dt_list <- kshap_prob_dt_list <- kshap_est_sd_prob_dt_list <- list()


  while (converged == FALSE){

    ##### UNLESS ESTIMATION IS DONE ####

    ## Setup for current iteration

    current_feature_names <- S_mapper_list[[iter]]$feature_names
    current_m <- S_mapper_list[[iter]][,.N]

    current_unique_feature_samples <- length(unique(feature_sample_all))+2
    remaining_unique_feature_samples <- 2^current_m -current_unique_feature_samples


    if(unique_sampling == TRUE && remaining_unique_feature_samples < n_combinations_per_iter){
      n_combinations_per_iter <- remaining_unique_feature_samples
      converged = TRUE
    }
    if (!is.null(max_n_combinations) && (initial_n_combinations + n_combinations_per_iter * (iter-1)) >= max_n_combinations){
      converged = TRUE
    }

    #### Sampling of new feature combinations (this is done with 2 lists which are paried duplicates if paired_sampling == TRUE) ####

    if(remaining_unique_feature_samples>0){
      feature_sample_new_list <- feature_set_sample(feature_sample_prev = feature_sample_all, m = current_m,n_combinations_sample = ifelse(iter==1,initial_n_combinations-2,n_combinations_per_iter),
                                                    unique_sampling = unique_sampling, paired_sampling = paired_sampling)

      feature_sample_new_1 <- feature_sample_new_list[[1]]
      feature_sample_new_2 <- feature_sample_new_list[[2]]

      feature_sample_1 <- c(feature_sample_prev_1,feature_sample_new_1)
      feature_sample_2 <- c(feature_sample_prev_2,feature_sample_new_2)

      feature_sample_all <- c(feature_sample_1,feature_sample_2)

    }


    ### Transforming sampled features to the X data.table used in shapr
    X0 <- X_from_feature_set_v3(feature_sample_all,m=current_m,sample_ids=seq_along(feature_sample_all))[]

    # Reweighting shapley values #
    if(shapley_reweighting_strategy!="none"){
      X <- shapley_reweighting(X0, strategy = shapley_reweighting_strategy) # This probably needs adjustment.
    } else {
      X <- X0
    }

    if(converged==TRUE){
      X <- shapley_reweighting(X0, strategy = "on_all") # To get the weights corresponding to exact computation (always used when we have all feature combinations)
    }

    # Only Needed for gaussian method
     S <- feature_matrix_cpp(
       features = X[["S"]],
       m = current_m
     )




    # Get weighted matrix ----------------

    X[,features:=S] # Since shapr:::weight_matrix needs this name


    # Weight matrix used in the shapley value computation
    W <- shapr:::weight_matrix(
      X = X,
      normalize_W_weights = TRUE,
      is_groupwise = FALSE
    )



    #### Just doing a basic variant with the feature I got here for now #####
    current_x_train_red <- x_train[,..current_feature_names]
    current_x_explain_red <- x_explain[,..current_feature_names]

    if(iter==1){
      dt_vS <- X[c(1,.N),.(S_char,Sbar_char)]
    }

    # (Re)set the full and zero prediction
    # This is only has an effect on iter=1, and when pred_to_decompose has changed (on feature removal)
    for(i in seq_along(testObs_computed)){
      this = paste0("p_hat_",i)
      dt_vS[c(1,.N),(this):=c(p0[1]+shap_it_excluded_features,full_pred[i])]
    }


    # Extract which feature combinations are new
    new_combinations <- X[,.(S_char,Sbar_char)][!(X[,S_char] %in% dt_vS[,S_char])]

    # Estimates v(S) for the new combinations
    vS_feature_list <- list()
    if(nrow(new_combinations)>0){
      x_explain_red_here <- current_x_explain_red[testObs_computed,]
      x_excluded_here <- x_explain[testObs_computed,..current_excluded_feature_cols]

      if(approach=="gaussian"){

        S_char_here <- new_combinations[,S_char]
        these <- X[,which(S_char %in% S_char_here)]
        new_combinations[, id_combination := these]

        S_here <- S[these,]

        mu <- gaussian.mu[feature_names %in% current_feature_names]
        cov_mat <- gaussian.cov_mat[feature_names %in% current_feature_names,feature_names %in% current_feature_names]
        n_features <- length(current_x_train_red)
        x_explain_mat <- as.matrix(x_explain_red_here)
        n_combinations_now <- nrow(S_here)

        # Generate the MC samples from N(0, 1)
        MC_samples_mat <- matrix(rnorm(n_samples * n_features), nrow = n_samples, ncol = n_features)

        # Use Cpp to convert the MC samples to N(mu_{Sbar|S}, Sigma_{Sbar|S}) for all coalitions and explicands.
        # The object `dt` is a 3D array of dimension (n_samples, n_explain * n_coalitions, n_features).
        dt <- prepare_data_gaussian_cpp(
          MC_samples_mat = MC_samples_mat,
          x_explain_mat = x_explain_mat,
          S = S_here,
          mu = mu,
          cov_mat = cov_mat
        )

        # Reshape `dt` to a 2D array of dimension (n_samples * n_explain * n_coalitions, n_features).
        dim(dt) <- c(n_combinations_now * 1 * n_samples, n_features)

        # Convert to a data.table and add extra identification columns
        dt0 <- data.table::as.data.table(dt)
        data.table::setnames(dt0, current_feature_names)
        dt0[, id_combination := rep(seq_len(nrow(S_here)), each = n_samples * 1)]
        dt0[, id := rep(seq(1), each = n_samples, times = nrow(S_here))]
        dt0[, w := 1 / n_samples]
        dt0[, id_combination := these[id_combination]]
        data.table::setcolorder(dt0, c("id_combination", "id", current_feature_names))

        vS_feature_dt <- merge(dt0,new_combinations,by="id_combination")

      } else if(approach=="ctree"){
        for(i in seq_len(nrow(new_combinations))){

          S_char_here <- new_combinations[i,S_char]
          Sbar_char_here <- new_combinations[i,Sbar_char]

          S_here <- unlist(X[S_char ==S_char_here,S])

          # Created solely to extract the right tree below
          S_feat_names <- paste0(S_mapper_list[[iter]][feature_numbers %in% S_here,feature_names],collapse="_")

          if(!is.null(all_trees) && (S_feat_names %in% feat_names_all_trees)){             # In order for this to work properly, I also need to check Sbar_feat_names here

            tree <- all_trees[[which(feat_names_all_trees==S_feat_names)]]
          } else {
            # Optimally, trained trees should be added to all_trees for use in later calls to this function. BUt then I need to currect the feature numbering too, so that becomes harder
            tree <- shapr:::create_ctree(S_here, current_x_train_red, ctree.mincriterion, ctree.minsplit, ctree.minbucket)

          }

          samp_list <- list()
          for(j in seq_along(testObs_computed)){
            samp_list[[j]] <- shapr:::sample_ctree(
              tree,
              n_samples = n_samples,
              x_explain = x_explain_red_here[j,],
              x_train = current_x_train_red,
              n_features = length(current_x_train_red),
              sample = ctree.sample
            )
          }

          vS_feature_list[[i]] <- rbindlist(samp_list,idcol="id")
          #      vS_feature_list[[i]][,id:=testObs_computed]
          vS_feature_list[[i]][,S_char := S_char_here]
          vS_feature_list[[i]][,Sbar_char := Sbar_char_here]
          vS_feature_list[[i]][,id_combination := i] # This is just temporary and only used as it is needed by compute_MCint

        }


        vS_feature_dt <- rbindlist(vS_feature_list,fill=TRUE)

      }


      #### Merging the samples for the new combinations and doing monte carlo interation to get v(S)

      dt <- cbind(vS_feature_dt,x_excluded_here)
      setcolorder(dt,names(x_train))

      dt[, p_hat := predict_model(model, newdata = .SD), .SDcols = feature_names]
      dt[,w:=1/.N,by=.(S_char,id)]

      tmp_id_combination_S_char_mapping <- unique(dt[,.(id_combination,S_char,Sbar_char)])

      dt_vS00 <- shapr:::compute_MCint(dt)
      dt_vS0 <- merge(dt_vS00,tmp_id_combination_S_char_mapping,by="id_combination")
      dt_vS0[,id_combination:=NULL]

      dt_vS <- rbind(dt_vS,
                     dt_vS0)

    }

    dt_vS <- dt_vS[X[,.(S_char)],,on="S_char"] # reordering dt_vS to match the order given in X

    ### COmpute shapley values ####
    kshap <- t(W %*% as.matrix(dt_vS[, -c("S_char","Sbar_char")]))
    kshap_est_dt <- data.table::as.data.table(kshap)
    colnames(kshap_est_dt) <- c("none", current_feature_names)
    kshap_est_dt[,id:=testObs_computed]
    kshap_est_dt[,none:=p0] # Overwrite this with the original p0 value
    setcolorder(kshap_est_dt,"id")

    kshap_est_mat <- as.matrix(kshap_est_dt[,-"id"])


    if(remaining_unique_feature_samples>0){
      #### Variance computation ####


      #sample_sd_dt_seperate <- sample_cov_estimator(feature_sample_all,dt_vS=copy(dt_vS_relevant),testObs_computed,n_var_est_reps = 10,n_var_est_groups = 10,m,comp_strategy = "separate",return="sd_mat",paired_sampling = paired_sampling)
      kshap_sd_dt <- boot_cov_estimator(feature_sample_1,feature_sample_2,dt_vS=dt_vS,testObs_computed,n_boot_ests = n_boot_ests,m=current_m,
                                        shapley_reweighting_strategy = shapley_reweighting_strategy,return="sd_mat",paired_sampling = paired_sampling,
                                        feature_names = current_feature_names)

      kshap_sd_mat <- as.matrix(kshap_sd_dt[,-c("sd_type","id")])

      #### Compute probabilities ####
      kshap_prob_mat <- probfunc(kshap_est_mat,kshap_sd_mat,shapley_threshold_val = shapley_threshold_val)
      kshap_prob_dt <- data.table(kshap_est_dt[,"id"],kshap_prob_mat)

    } else {
      kshap_sd_dt <- copy(kshap_est_dt)
      kshap_sd_dt[,sd_type:="No estimation"]
      kshap_sd_dt[,none:=0]
      kshap_sd_dt[,(current_feature_names):=0]
      setcolorder(kshap_sd_dt,"sd_type")

      kshap_sd_mat <- as.matrix(kshap_sd_dt[,-c("sd_type","id")])


      kshap_prob_dt <- copy(kshap_est_dt)
      kshap_prob_dt[,none:=as.numeric(NA)]
      kshap_prob_dt[,(current_feature_names):=as.numeric(NA)]

      kshap_prob_mat <- as.matrix(kshap_sd_dt[,-c("sd_type","id")])

    }


  #### Just lots of formatting for the iterative printouts ####

    matrix1 <- format(round(kshap_est_mat,3),nsmall=2,justify = "right")
    matrix2 <- format(round(kshap_sd_mat,2),nsmall=2,justify = "right")
    matrix3 <- format(round(kshap_prob_mat,2),nsmall=2,justify = "right")


    kshap_est_sd_prob_dt <- cbind(kshap_est_dt[,"id"],as.data.table(matrix(paste(matrix1, " (", matrix2,") [", matrix3,"]", sep = ""), nrow = length(testObs_computed))))
    names(kshap_est_sd_prob_dt) <- names(kshap_est_dt)

    kshap_est_sd_prob_dt <- cbind(kshap_est_sd_prob_dt,fixed_kshap_est_dt)

    kshap_est_sd_prob_dt[,it_excluded_features:=format(round(shap_it_excluded_features,3),width=2,justify = "right")]
    kshap_est_sd_prob_dt[,other_features:=format(round(shapsum_other_features,3),width=2,justify = "right")]


    setcolorder(kshap_est_sd_prob_dt,c("id","none",cutoff_feats))

    print(iter)
    print(kshap_est_sd_prob_dt[])

    keep_list[[iter]] <- list(kshap_est_dt = copy(kshap_est_dt),
                              kshap_sd_dt = kshap_sd_dt,
                              kshap_prob_dt = kshap_prob_dt,
                              dt_vS = dt_vS,
                              X = X,
                              feature_sample_1 = feature_sample_1,
                              feature_sample_2 = feature_sample_2,
                              full_pred = full_pred,
                              p0 = p0,
                              shap_it_excluded_features = shap_it_excluded_features,
                              no_computed_combinations = nrow(new_combinations))

    kshap_est_dt_list[[iter]] <- kshap_est_dt
    kshap_sd_dt_list[[iter]] <- kshap_sd_dt
    kshap_prob_dt_list[[iter]] <- kshap_prob_dt
    kshap_est_sd_prob_dt_list[[iter]] <- kshap_est_sd_prob_dt


    #### Check for exclusion ####
    if(converged==FALSE && any(as.vector(kshap_prob_mat)<shapley_threshold_prob)){

      #### Sorry, the following is a bit of a mess and hard to understand -- will have to clean it up later. I suggest not spending time trying to understand it ####
      # What we do is simply to remove the feature with the smallest shapley value fulfills the threshold condition #
      # Then, there is a lot of bookeeping to adjust the feature numbering, and align the computed v(S) to the reduced feature space #


      exclude_feature <- which.min(as.vector(kshap_prob_mat)[-1]) # 5 # TODO: Need to adjust this when the second features is removed so it picks the right one
      exclude_feature_name <- current_feature_names[exclude_feature]

      #excluded_shap_value <- kshap_est_mat[-1][exclude_feature]



      keep <- kshap_est_dt[,..exclude_feature_name]
      # Updates the sum of the shap values for the iteratively exlcuded features
      shap_it_excluded_features <- shap_it_excluded_features+unlist(keep)

      cols <- names(keep)
      keep[,(cols):=lapply(.SD,function(x) format(round(x,3),width=2,justify = "right"))]
      keep[,(cols):=lapply(.SD,function(x) paste0(x, " [done estimating]"))]

      fixed_kshap_est_dt <- cbind(fixed_kshap_est_dt,keep)

      current_feature_names <- setdiff(current_feature_names,exclude_feature_name)
      current_excluded_feature_cols <- c(current_excluded_feature_cols,exclude_feature_name)

      # The point with the below code is to map the current S and Sbar to the updated feature numbers for
      # the next iteration
      # I also need the S_mapper to have control over this

      next_S_mapper <- S_mapper_list[[iter]][-exclude_feature]

      Xtmp <- X[,.(S,Sbar,id_combination,n_features)]

      # For S
      Xtmp[,S_next:=lapply(S,function(x) x[!(x %in% exclude_feature)])] # Removing the excluded feature
      Xtmp[,S_next:=sapply(S_next,match,next_S_mapper$feature_numbers)] # mapping to the new feature numbers
      Xtmp[, S_next_char := sapply(S_next, paste0, collapse = "_")] # Making character representation of the new S

      # For Sbar (for completeness, but maybe not needed(?))
      Xtmp[,Sbar_next:=lapply(Sbar,function(x) x[!(x %in% exclude_feature)])] # Removing the excluded feature
      Xtmp[,Sbar_next:=sapply(Sbar_next,match,next_S_mapper$feature_numbers)] # mapping to the new feature numbers
      Xtmp[, Sbar_next_char := sapply(Sbar_next, paste0, collapse = "_")] # Making character representation of the new S

      # Needed for the merging with dt_vS
      Xtmp[, S_char := sapply(S, paste0, collapse = "_")] # Making character representation of the new S
      Xtmp[, Sbar_char := sapply(Sbar, paste0, collapse = "_")] # Making character representation of the new S


      reduction_strategy = "by_S"
      if(reduction_strategy == "by_S"){
        # Uses 12|345 as replacement for 12|34, if 5 is removed
        setorder(Xtmp,id_combination)
        Xtmp[,keep:=!duplicated(S_next_char)] #
      } else if(reduction_strategy == "by_Sbar"){
        # Uses 125|34 as replacement for 12|34, if 5 is removed
        setorder(Xtmp,-id_combination)
        Xtmp[,keep:=duplicated(S_next_char)]
      } else {
        # Uses the mean of 125|34 and 12|345 as replacement for 12|34, if 5 is removed
        Xtmp[,keep:=TRUE]
      }
      setorder(Xtmp,id_combination)

      Xtmp[,id_combination_next := .GRP,by=S_next_char]

      #browser()


      # # YES; I DO NEED THIS !!!!
      # I don't think I actually need this (is overwritten later on anyway), so I comment it out
      ## Always keep the zero and full set
       id_combination_new_keepers <- Xtmp[c(1,.N),id_combination_next]
       Xtmp[id_combination_next %in% id_combination_new_keepers,keep:=FALSE]
       Xtmp[c(1,.N),keep:=TRUE]


      X_dt_vS <- merge(Xtmp,dt_vS,by=c("S_char","Sbar_char"))
      X_dt_vS <- X_dt_vS[keep==TRUE]
      X_dt_vS[,p_hat_1 := mean(p_hat_1),by=id_combination_next]

      head(X_dt_vS[,.(S,S_next,id_combination,id_combination_next,p_hat_1)],15)

      setorder(X_dt_vS,id_combination)
      dt_vS <- unique(X_dt_vS[,.(S_char = S_next_char,
                                 Sbar_char = Sbar_next_char,
                                 p_hat_1)])


#
#
#       X[,Sbar_new:=lapply(Sbar,function(x) x[!(x %in% exclude_feature)])]
#
#       X[, S_char_new := sapply(S_new, paste0, collapse = "_")]
#       X[, Sbar_char_new := sapply(Sbar_new, paste0, collapse = "_")]
#
#       reduction_strategy = "by_S"
#       if(reduction_strategy == "by_S"){
#         # Uses 12|345 as replacement for 12|34, if 5 is removed
#         setorder(X,n_features)
#         X[,keep:=!duplicated(S_char_new)] #
#       } else if(reduction_strategy == "by_Sbar"){
#         # Uses 125|34 as replacement for 12|34, if 5 is removed
#         setorder(X,-n_features)
#         X[,keep:=duplicated(S_char_new)]
#       } else {
#         # Uses the mean of 125|34 and 12|345 as replacement for 12|34, if 5 is removed
#         X[,keep:=TRUE]
#       }
#       setorder(X,id_combination)
#
#       X[,id_combination_new := .GRP,by=S_char_new]
#
#       # Always keep the zero and full set
#       id_combination_new_keepers <- X[c(1,.N),id_combination_new]
#       X[id_combination_new %in% id_combination_new_keepers,keep:=FALSE]
#       X[c(1,.N),keep:=TRUE]
#
#       S_mapper <- X[,.(S_char,S_char_new,keep)]
#
#       X_dt_vS <- merge(X,dt_vS,by=c("S_char","Sbar_char"))
#       X_dt_vS <- X_dt_vS[keep==TRUE]
#       X_dt_vS[,p_hat_1 := mean(p_hat_1),by=id_combination_new]
#
#       ### Modify which features we work with
#
#       num_feat_vec <- num_feat_vec[-exclude_feature]
#
#
#       # Converting to the new format for the next iteration
#       X_dt_vS[,S_new_newformat:=sapply(S_new,match,num_feat_vec)]
#       X_dt_vS[,Sbar_new_newformat:=sapply(Sbar_new,match,num_feat_vec)]
#       X_dt_vS[, S_char_new_newformat := sapply(S_new_newformat, paste0, collapse = "_")]
#       X_dt_vS[, Sbar_char_new_newformat := sapply(Sbar_new_newformat, paste0, collapse = "_")]
#
#       setorder(X_dt_vS,id_combination)
#       dt_vS <- unique(X_dt_vS[,.(S_char = S_char_new_newformat,
#                                  Sbar_char = Sbar_char_new_newformat,
#                                  p_hat_1)])
#
#
#       # These needs to be placed here, after the X_dt_vS stuff, I think, as they might need the old one...
#
      feature_samples_to_delete <- Xtmp[keep==FALSE,S_char]
      feature_sample_1_next <- feature_sample_1[!(sapply(feature_sample_1,paste0,collapse="_") %in% feature_samples_to_delete)]
      feature_sample_1_next <- lapply(feature_sample_1_next,function(x) x[!(x %in% exclude_feature)]) # Removing the excluded feature
      feature_sample_1_next <- lapply(feature_sample_1_next,match,next_S_mapper$feature_numbers) # mapping to the new feature numbers
      feature_sample_1_next <- feature_sample_1_next[!(sapply(feature_sample_1_next,length) %in% c(0,length(current_feature_names)))] # Remove full and empty set

      feature_sample_2_next <- feature_sample_2[!(sapply(feature_sample_2,paste0,collapse="_") %in% feature_samples_to_delete)]
      feature_sample_2_next <- lapply(feature_sample_2_next,function(x) x[!(x %in% exclude_feature)]) # Removing the excluded feature
      feature_sample_2_next <- lapply(feature_sample_2_next,match,next_S_mapper$feature_numbers) # mapping to the new feature numbers
      feature_sample_2_next <- feature_sample_2_next[!(sapply(feature_sample_2_next,length) %in% c(0,length(current_feature_names)))] # Remove full and empty set

      feature_sample_all_next <- c(feature_sample_1_next,feature_sample_2_next)

      feature_sample_all_paired_next <- lapply(feature_sample_all_next, function(x) seq_along(current_feature_names)[-x])

      feature_sample_all_comb_next <- list()
      feature_sample_all_comb_next[seq(1,2*length(feature_sample_all_next),by=2)] <- feature_sample_all_next
      feature_sample_all_comb_next[seq(2,2*length(feature_sample_all_next),by=2)] <- feature_sample_all_paired_next

      feature_sample_all_comb_next_dt <- data.table(features=feature_sample_all_comb_next)
      feature_sample_all_comb_next_dt[,features_char := sapply(features,paste0, collapse = "_")]
      feature_sample_all_comb_next_dt[,dups:=duplicated(features_char)]
      feature_sample_all_comb_next_dt <- feature_sample_all_comb_next_dt[dups==FALSE]

      feature_sample_1 <- feature_sample_all_comb_next_dt[seq(1,.N,by=2),features]
      feature_sample_2 <- feature_sample_all_comb_next_dt[seq(2,.N,by=2),features]

      feature_sample_all <- c(feature_sample_1,feature_sample_2)
#      feature_sample_1_next <- feature_sample_1[sapply(feature_sample_1,function(x) !(exclude_feature %in% x))] # exclude samples that contain the excluded feature
#      feature_sample_1_next <- lapply(feature_sample_1_next,match,next_S_mapper$feature_numbers) # mapping to the new feature numbers

#      feature_sample_2_next <- feature_sample_2[sapply(feature_sample_2,function(x) !(exclude_feature %in% x))] # exclude samples that contain the excluded feature
#      feature_sample_2_next <- lapply(feature_sample_2_next,match,next_S_mapper$feature_numbers) # mapping to the new feature numbers



#
#
#
#       feature_sample_1_0 <- lapply(feature_sample_1,function(x) x[!(x %in% exclude_feature)]) # Removing the excluded feature
#       feature_sample_1_0 <- lapply(feature_sample_1_0,match,next_S_mapper$feature_numbers) # mapping to the new feature numbers
#       feature_sample_1_0_char <- sapply(feature_sample_1_0,paste0, collapse = "_") # Making character representation of the new S
#       which(feature_sample_1_0_char %in% Xtmp[keep==TRUE,S_next_char])
#
#       feature_sample_2_0 <- lapply(feature_sample_2,function(x) x[!(x %in% exclude_feature)]) # Removing the excluded feature
#       feature_sample_2_0 <- lapply(feature_sample_2_0,match,next_S_mapper$feature_numbers) # mapping to the new feature numbers
#       feature_sample_2_0_char <- sapply(feature_sample_2_0,paste0, collapse = "_") # Making character representation of the new S
#       which(feature_sample_2_0_char %in% Xtmp[keep==TRUE,S_next_char])
#
#
#       # Mofifying the feature_samples as well.
#       feature_sample_1_char_dt <- data.table(S =feature_sample_1)
#       feature_sample_1_char_dt[,S_next:=lapply(S,function(x) x[!(x %in% exclude_feature)])] # Removing the excluded feature
#       feature_sample_1_char_dt[,S_next:=sapply(S_next,match,next_S_mapper$feature_numbers)] # mapping to the new feature numbers
#       feature_sample_1_char_dt[, S_next_char := sapply(S_next, paste0, collapse = "_")] # Making character representation of the new S
#
#
#
#
#       feature_sample_1_char_dt <- data.table(S_char=sapply(feature_sample_1,paste0, collapse = "_"))
#       feature_sample_1_char_dt <- S_mapper[feature_sample_1_char_dt,on="S_char"] #merge(feature_sample_1_char_dt,S_mapper,by="S_char")
#       feature_sample_1_char_dt[,S_new:=sapply(strsplit(S_char_new, "_"),as.numeric)]
#       feature_sample_1_char_dt[,rowno:=.I]
#       delrows_1 <- feature_sample_1_char_dt[keep==FALSE,rowno]
#
#       feature_sample_2_char_dt <- data.table(S_char=sapply(feature_sample_2,paste0, collapse = "_"))
#       feature_sample_2_char_dt <- S_mapper[feature_sample_2_char_dt,on="S_char"] #merge(feature_sample_2_char_dt,S_mapper,by="S_char")
#       feature_sample_2_char_dt[,S_new:=sapply(strsplit(S_char_new, "_"),as.numeric)]
#       feature_sample_2_char_dt[,rowno:=.I]
#       delrows_2 <- feature_sample_2_char_dt[keep==FALSE,rowno]
#
#
#       feature_sample_1_0 <- feature_sample_1_char_dt[keep==TRUE,S_new]
#       feature_sample_1 <- lapply(feature_sample_1_0,match,num_feat_vec)
#
#
#       feature_sample_2_0 <- feature_sample_2_char_dt[keep==TRUE,S_new]
#       feature_sample_2 <- lapply(feature_sample_2_0,match,num_feat_vec)
#
#       # Here I need to redistribute the feature_sample_1 and feature_sample_2 such that they are paired copies of each other
#
#       # Very brute for method for ensuring that all feature_sample_1 and 2 are paried copies of each other (which is needed) for the bootstrappign procedure in the next iteration
#
#       feature_sample_all <- c(feature_sample_1,feature_sample_2)
#
#
#       feature_sample_all_paired <- lapply(feature_sample_all, function(x) seq_along(current_feature_names)[-x])
#
#       feature_sample_all_comb <- list()
#       feature_sample_all_comb[seq(1,2*length(feature_sample_all),by=2)] <- feature_sample_all
#       feature_sample_all_comb[seq(2,2*length(feature_sample_all),by=2)] <- feature_sample_all_paired
#
#       feature_sample_all_comb_dt <- data.table(features=feature_sample_all_comb)
#       feature_sample_all_comb_dt[,features_char := sapply(features,paste0, collapse = "_")]
#       feature_sample_all_comb_dt[,dups:=duplicated(features_char)]
#       feature_sample_all_comb_dt <- feature_sample_all_comb_dt[dups==FALSE]
#
#       feature_sample_1 <- feature_sample_all_comb_dt[seq(1,.N,by=2),features]
#       feature_sample_2 <- feature_sample_all_comb_dt[seq(2,.N,by=2),features]

      # TODO:
      # 1. Modify m, the sampling function and the X_dt-creator in order to not sample the excluded feature
      # 2. Figure out how to reweight the samples already obtained as they are probably now having an incorrect weight
      # 3. Figure out how to deal with feature_sample_1 and feature_sample_2 now longer being paired copies of each other (if I am using by_S or by_Sbar as reduction strategy)
      # I then probably have to exclude the samples which are the paired copies of those who are excluded. I should not exclude them from dt_vS though.
      # in any case, for now I will use the reduction strategy = "by_both" since that only deletes those getting into teh zero or full pred, and thus keeps the symmetry.



      # Also, need to modify m and possibly the sampling function and X-dt creator


    }




    iter <- iter+1
    feature_sample_prev_1 <- feature_sample_1
    feature_sample_prev_2 <- feature_sample_2


    S_mapper_list[[iter]] <- data.table(feature_numbers = seq_along(current_feature_names),
                                        feature_names = current_feature_names)


  }


  kshap_it_est_dt <- rbindlist(kshap_est_dt_list,fill=TRUE)
  kshap_final0 <- copy(kshap_it_est_dt[,-1])
  setnafill(kshap_final0,"locf")
  kshap_final <- kshap_final0[.N,] # final estimate
  kshap_final[,other_features := shapsum_other_features]


  return(list(kshap_final = kshap_final, # The final estimates
              kshap_it_est_dt = kshap_it_est_dt, # The iterative estimates (NA for those who are removed)
              kshap_est_dt_list = kshap_est_dt_list, # The estimates at each iteration
              kshap_sd_dt_list = kshap_sd_dt_list, # The standard deviations at each iteration
              kshap_prob_dt_list = kshap_prob_dt_list, # The probabilities at each iteration
              kshap_est_sd_prob_dt_list = kshap_est_sd_prob_dt_list, # The estimates, standard deviations and probabilities at each iteration
              keep_list = keep_list) # List of information we keep from each iteration
  )

}


