#source("/nr/project/stat/BigInsight//Projects//Explanations//EffektivShapley//helpFunctions.R")

#library(treeshap)
library(xgboost)
library(data.table)

# Install the github version of the shapr pacakge
#remotes::install_github("NorskRegnesentral/shapr")

library(shapr)

#library(progressr)
#progressr::handlers(global = TRUE) # To get progress updates

#library(future)
#future::plan(multisession, workers = 10) # for paralellization (on both linux and windows)


plotFig <- 0

datafolder <- "/nr/project/stat/BigInsight/Projects/Explanations/EffektivShapley/NHANES-data/"
#datafolder <- "M:/BigInsight/Projects/Explanations/EffektivShapley/NHANES-data/"

x_explain <- fread(file.path(datafolder,"newdata/Xtest_imp.csv"))
y_test    <- fread(file.path(datafolder,"newdata/ytest.csv"))$V1
x_train   <- fread(file.path(datafolder,"newdata/Xtrain_imp.csv"))
y_train   <- fread(file.path(datafolder,"newdata/ytrain.csv"))$V1

names(x_train)[52:55]   <- c("urine_albumin_is_gt_30","urine_albumin_is_gt_100","urine_albumin_is_get_300", "urine_albumin_is_gt_1000")
names(x_explain)[52:55] <- c("urine_albumin_is_gt_30","urine_albumin_is_gt_100","urine_albumin_is_get_300", "urine_albumin_is_gt_1000")

model <- xgboost::xgb.load(file.path(datafolder,"newdata/xgb_model_imp.json"))

preds <- predict(model,as.matrix(x_explain),outputmargin = TRUE)
preds_train <- predict(model,as.matrix(x_train),outputmargin = TRUE)

pred_mod_xgb <- function(model,newdata){
  xgboost:::predict.xgb.Booster(model,as.matrix(newdata),outputmargin = TRUE)
}

#preds <- log(predict(model,as.matrix(x_explain)))

#ind   <- rev(order(preds))[1:2]
#ind   <- rev(order(preds))[9:10]
#x_explain <- x_explain[ind,]


treeShaps=predict(model,as.matrix(x_explain),predcontrib = TRUE)

prediction_zero <- treeShaps[1,"BIAS"]
feature_names <- colnames(x_train)


#### testing here

# TODO:
# 1. Check that the order of the features is correct such that the correct id combinations are used in the shapley value
# computations later on.
# 2. Control the computed shapley values with what you get with the standard approach in th nhanes-shapr-reduced setting e.g. all but 2, 3 or 4 features
# to zero. should be about the same if the procedure works.


n_features <- ncol(x_train)
abscorMat <- abs(cor(x_train))
max_cutoff_features <- 6
max_cutoff_remaining_imp <- 0.10
p0 <- treeShaps[1,"BIAS"]
S_replacement_for_remaining_cutoff_feats <- 1 # 1 for conditioning on these, 0 for marginalizing them out



fix_zero_and_full_prediction <- TRUE

# For a specific testObs:

testObs <- 24
testObs <- 5

org_imp <- abs(treeShaps[testObs,-(n_features+1)]) # absolute value of treeshap values
norm_org_imp <- org_imp/sum(org_imp)
cor_imp <- as.vector(org_imp%*%abscorMat)
names(cor_imp) <- names(x_train)
norm_cor_imp <- cor_imp/sum(cor_imp)
plot(norm_cor_imp,type="l",ylim=c(0,0.25))
lines(org_imp/sum(org_imp),col="red")

sorted_norm_cor_imp <- sort(norm_cor_imp,decreasing = TRUE)
cumsum_sorted_norm_cor_imp <- cumsum(sorted_norm_cor_imp)


cutoff0 <- which(cumsum_sorted_norm_cor_imp<=1-max_cutoff_remaining_imp)
cutoff <- ifelse(length(cutoff0)>=max_cutoff_features,cutoff0[max_cutoff_features],cutoff0[length(cutoff)])
cutoff_imp <- sorted_norm_cor_imp[1:cutoff]
cutoff_feats <- names(cutoff_imp)

excluded_feature_cols <- setdiff(names(x_train),cutoff_feats)

x_train_red <- x_train[,..cutoff_feats]
x_explain_red <- x_explain[,..cutoff_feats]

ctree.mincriterion = 0.95
ctree.minsplit = 20
ctree.minbucket = 7
ctree.sample = TRUE
n_samples <- 1000

#
# X_full <- shapr:::feature_combinations(
#   m = max_comp_features,
#   exact = TRUE,
#   n_combinations = NULL,
#   weight_zero_m = 10^6,
#   group_num = NULL
# )
#
# ## Get feature matrix ---------
# S_full <- feature_matrix_cpp(
#   features = X_full[["features"]],
#   m = max_comp_features
# )
#
# S_full <- cbind(S_full,matrix(S_replacement_for_remaining_cutoff_feats,ncol=max_cutoff_features-max_comp_features,nrow=nrow(S_full)))
#
# S_full_dt <- as.data.table(S_full)
# S_full_dt[,id_combination_full:=.I]
# S_full_dt[,computed_in_loop:=as.numeric(NA)]
#
# S_full_dt[.N,computed_in_loop:=0] # We don't need to compute the full model
# S_full_dt[.N, p_hat_1:=preds[testObs]] # Already computed

# Not sure if this should be included here or not, as the smallest model now condiitonal on some features
# so is not really the same as the zero model
# I think it is best to model this every time, yes


#S_full_dt[1,computed_in_loop:=0] # We don't need to compute the zero model
#S_full_dt[1, p_hat_1:=prediction_zero]



#### TODO:


# Start with some samples of the largest model
# Compute variance of this model
# Cut out the ones that with a certain probability is within an epsilon from zero, to reduce
# the number of features
# Remmeber to always decompose what is left after extracting the shapley values for the features not included int the procedure

actual_feature_translator_func <- function(oldvec,num_feat_vec){
  newvec <- num_feat_vec[oldvec]
  return(newvec)
}

rev_actual_feature_translator_func <- function(newvec,num_feat_vec){
  oldvec <- match(newvec,num_feat_vec) # Check if this is correct
  return(oldvec)
}


feature_set_sample <- function(feature_sample_prev = NULL, num_feat_vec, n_combinations_sample = 200, unique_sampling = TRUE,paired_sampling = FALSE) {

  #n_combinations_sample specifies the number of NEW n_combinations that shoudl be samples (it does not account for the full and zero set).
  # if unique_sampling is TRUE, then the sampling will continue until the number of unique samples is equal to n_combinations_sample
  # if paired_sampling is TRUE, then to the total number of combinations is equal to n_combinations_sample

  # Find weights for given number of features ----------
  m <- length(num_feat_vec)


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
      feature_sample_00 <- shapr:::sample_features_cpp(m, n_features_sample)
      feature_sample_0 <- lapply(feature_sample_00,actual_feature_translator_func,num_feat_vec = num_feat_vec)
      if(paired_sampling){
        feature_sample_1 <- feature_sample_0
        feature_sample_2 <- lapply(lapply(feature_sample_00, function(x) seq(m)[-x]),actual_feature_translator_func,num_feat_vec = num_feat_vec)
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
    feature_sample_00 <- shapr:::sample_features_cpp(m, n_features_sample)
    feature_sample_0 <- lapply(feature_sample_00,actual_feature_translator_func,num_feat_vec = num_feat_vec)
    if(paired_sampling){
      feature_sample_1 <- feature_sample_0
      feature_sample_2 <- lapply(lapply(feature_sample_00, function(x) seq(m)[-x]),actual_feature_translator_func,num_feat_vec = num_feat_vec)
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
                                 paired_sampling = paired_sampling){


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
      colnames(dt_kshap) <- c("none", cutoff_feats)
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
                               paired_sampling = paired_sampling){


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
    colnames(dt_kshap) <- c("none", cutoff_feats)
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


treeShaps_dt <- as.data.table(treeShaps)
# Assume we are sampling say 20 coalitions at a time,

# Here we try start out sampling based on fewer features, but we add the full cutoff features afterwords.
# And run kernelshap with the full set of features

# TRUE


# TODO:
# 1. use exact shapley value to check whether it matters whether you change the prediction or the phi0 as long
# as you adjust the other one, i.e. that what is distributed is the difference between the two
# Yes, I have done this and it is only the difference between the full pred phi0 which is distributed in the features.
# when doing exact kernelshap
# 1.5. Allow multiple testObs to be evaluated with this approach, assuming the same set of features are used for all
# DONE

# THIS IS NEXT ##### 2. set a threshold for what when to discard a value. I think that when maxval is below 0.10 or 0.15, we can discard it
# One issue might be that when this is used, the shapley value of the excluded ones are biased towards zero,
# since that is the point at which they are being cut off. think about whether we can adjust for this.
# In any case, I should then remove the features from the kernelshap computation, and subtract the sum of their
# values from the full prediction value
# Further, I should have differnet options for reusing the v(S) from that full model.
# Assume first that we remove one feature at a time:
#   #a) reuse those who condition on the removed feature (i.e. S=1)
#   #b) reuse those who does not condition on the remove feature (i.e. S=0)
#   #c) take the mean of those in a and b
# When we iteratively remove more features, it might be wise to use some kind of weighted mean of all those
# who are removed.

#n_var_est_groups <- 5
#n_var_est_reps <- 10
#redistribute_shapley_weight <- TRUE


testObs_computed <- 5
converged = FALSE
m <- max_cutoff_features-0
initial_n_combinations <- 20
n_combinations_per_iter <- 10
n_boot_ests <- 50
unique_sampling <- TRUE
iter <- 1
paired_sampling <- TRUE
shapley_reweighting_strategy = "on_N"



full_pred <- rowSums(treeShaps_dt[testObs_computed,])
pred_to_decompose <- rowSums(treeShaps_dt[testObs_computed,..cutoff_feats])
pred_not_to_decompose <- rowSums(treeShaps_dt[testObs_computed,..excluded_feature_cols])
org_p0 <- treeShaps_dt[testObs_computed,BIAS][1]
p0 <- org_p0 + pred_not_to_decompose

all.equal(p0+pred_to_decompose,full_pred)


# We ignore a feature from further computations once Pr(|\phi_j| > shapley_threshold_val) < shapley_threshold_prob
avg_contrib <- (pred_to_decompose-p0)/max_cutoff_features

shapley_threshold_val <- 0.5*abs(avg_contrib)
shapley_threshold_prob <- 0.2

feature_sample_all <- feature_sample_prev_1 <- feature_sample_prev_2 <- NULL
keep_list <- list()
set.seed(123)
iter <- 1

num_feat_vec <- seq_len(m)
#remaining_cutoff_feats <- cutoff_feats[-seq_len(m)]

S_mapper_list <- list()
S_mapper_list[[1]] <- data.table(feature_numbers = seq(m),
                                 feature_names = cutoff_feats[seq(m)])

while (converged == FALSE){

  # Setup for current iteration

  current_feature_names <- S_mapper_list[[iter]]$feature_names

  current_unique_feature_samples <- length(unique(feature_sample_all))
  remaining_unique_feature_samples <- 2^length(num_feat_vec)-2 -current_unique_feature_samples


  if(unique_sampling == TRUE && remaining_unique_feature_samples < n_combinations_per_iter){
    n_combinations_per_iter <- remaining_unique_feature_samples
    converged = TRUE
  }


  feature_sample_new_list <- feature_set_sample(feature_sample_prev = feature_sample_all, num_feat_vec = num_feat_vec,n_combinations_sample = ifelse(iter==1,initial_n_combinations-2,n_combinations_per_iter),
                                                unique_sampling = unique_sampling, paired_sampling = paired_sampling)

  feature_sample_new_1 <- feature_sample_new_list[[1]]
  feature_sample_new_2 <- feature_sample_new_list[[2]]

  feature_sample_1 <- c(feature_sample_prev_1,feature_sample_new_1)
  feature_sample_2 <- c(feature_sample_prev_2,feature_sample_new_2)

  feature_sample_all <- c(feature_sample_1,feature_sample_2)


  X0 <- X_from_feature_set_v3(feature_sample_all,m=length(num_feat_vec),sample_ids=seq_along(feature_sample_all))[]

  if(shapley_reweighting_strategy!="none"){
    X <- shapley_reweighting(X0, strategy = shapley_reweighting_strategy) # This probably needs adjustment.
  } else {
    X <- X0
  }

  # No longer needed
  # ## Get feature matrix ---------
  # S <- feature_matrix_cpp(
  #   features = X[["S"]],
  #   m = length(num_feat_vec)
  # )




  # Get weighted matrix ----------------

  #X[,features:=S] # Since shapr:::weight_matrix needs this name
  X[,features:= lapply(S,rev_actual_feature_translator_func,num_feat_vec = num_feat_vec)]

  W <- shapr:::weight_matrix(
    X = X,
    normalize_W_weights = TRUE,
    is_groupwise = FALSE
  )

  ### CONTINUE HERE #####


  #### Just doing a basic variant with the feature I got here for now #####
  x_train_red <- x_train[,..cutoff_feats]

  if(iter==1){
    dt_vS <- X[c(1,.N),.(S_char,Sbar_char)]
    for(i in seq_along(testObs_computed)){
      this = paste0("p_hat_",i)
      dt_vS[,(this):=c(p0[1],pred_to_decompose[i])]
    }

  }


  new_combinations <- X[,.(S_char,Sbar_char)][!(X[,S_char] %in% dt_vS[,S_char])]

  vS_feature_list <- list()
  if(nrow(new_combinations)>0){
    x_explain_red_here <- x_explain_red[testObs_computed,]
    x_excluded_here <- x_explain[testObs_computed,..excluded_feature_cols]

    for(i in seq_len(nrow(new_combinations))){
      S_char_here <- new_combinations[i,S_char]
      Sbar_char_here <- new_combinations[i,Sbar_char]

      S_here <- unlist(X[S_char ==S_char_here,S])

      tree <- shapr:::create_ctree(S_here, x_train_red, ctree.mincriterion, ctree.minsplit, ctree.minbucket)

      samp_list <- list()
      for(j in seq_along(testObs_computed)){
        samp_list[[j]] <- shapr:::sample_ctree(
          tree,
          n_samples = n_samples,
          x_explain = x_explain_red_here[j,],
          x_train = x_train_red,
          n_features = length(x_train_red),
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

    dt <- cbind(vS_feature_dt,x_excluded_here)
    setcolorder(dt,names(x_train))

    dt[, p_hat := pred_mod_xgb(model, newdata = .SD), .SDcols = feature_names]
    dt[,w:=1/.N,by=.(S_char,id)]

    tmp_id_combination_S_char_mapping <- unique(dt[,.(id_combination,S_char,Sbar_char)])

    dt_vS00 <- shapr:::compute_MCint(dt)
    dt_vS0 <- merge(dt_vS00,tmp_id_combination_S_char_mapping,by="id_combination")
    dt_vS0[,id_combination:=NULL]

    dt_vS <- rbind(dt_vS,
                   dt_vS0)
    dt_vS <- dt_vS[X[,.(S_char)],,on="S_char"] # reordering dt_vS to match the order given in X

  }

  ### COmpute shapley values ####
  kshap <- t(W %*% as.matrix(dt_vS[, -c("S_char","Sbar_char")]))
  kshap_est_dt <- data.table::as.data.table(kshap)
  colnames(kshap_est_dt) <- c("none", cutoff_feats)
  kshap_est_dt[,id:=testObs_computed]
  setcolorder(kshap_est_dt,"id")

  #sample_sd_dt_seperate <- sample_cov_estimator(feature_sample_all,dt_vS=copy(dt_vS_relevant),testObs_computed,n_var_est_reps = 10,n_var_est_groups = 10,m,comp_strategy = "separate",return="sd_mat",paired_sampling = paired_sampling)
  kshap_sd_dt <- boot_cov_estimator(feature_sample_1,feature_sample_2,dt_vS=dt_vS,testObs_computed,n_boot_ests = n_boot_ests,m,shapley_reweighting_strategy = shapley_reweighting_strategy,return="sd_mat",paired_sampling = paired_sampling)

  kshap_est_mat <- as.matrix(kshap_est_dt[,-"id"])
  kshap_sd_mat <- as.matrix(kshap_sd_dt[,-c("sd_type","id")])

  #### Compute probabilities ####
  kshap_prob_mat <- probfunc(kshap_est_mat,kshap_sd_mat,shapley_threshold_val = shapley_threshold_val)
  kshap_prob_dt <- data.table(kshap_est_dt[,"id"],kshap_prob_mat)

  keep_list[[iter]] <- list(kshap_est_dt = kshap_est_dt,
                            kshap_sd_dt = kshap_sd_dt,
                            kshap_prob_dt = kshap_prob_dt,
                            dt_vS = dt_vS,
                            X = X,
                            feature_sample_1 = feature_sample_1,
                            feature_sample_2 = feature_sample_2)


  matrix1 <- format(round(kshap_est_mat,3),width=2,justify = "right")
  matrix2 <- format(round(kshap_sd_mat,2),width=2,justify = "right")
  matrix3 <- format(round(kshap_prob_mat,2),width=2,justify = "right")


  kshap_est_sd_prob_dt <- cbind(kshap_est_dt[,"id"],as.data.table(matrix(paste(matrix1, " (", matrix2,") [", matrix3,"]", sep = ""), nrow = length(testObs_computed))))
  names(kshap_est_sd_prob_dt) <- names(kshap_est_dt)

  kshap_est_sd_prob_dt[,other_features:=format(round(pred_not_to_decompose,3),width=2,justify = "right")]

  print(iter)
  print(kshap_est_sd_prob_dt[])

  # Check for exclusion
  if(any(as.vector(kshap_prob_mat)<shapley_threshold_prob)){
    exclude_feature <- 5#which.min(as.vector(kshap_prob_mat)[-1]) # 5 # TODO: Need to adjust this when the second features is removed so it picks the right one
    exclude_feature_name <- current_feature_names[exclude_feature]

    current_feature_names <- setdiff(current_feature_names,exclude_feature_name)


    X[,S_new:=lapply(S,function(x) x[!(x %in% exclude_feature)])]
    X[,Sbar_new:=lapply(Sbar,function(x) x[!(x %in% exclude_feature)])]

    X[, S_char_new := sapply(S_new, paste0, collapse = "_")]
    X[, Sbar_char_new := sapply(Sbar_new, paste0, collapse = "_")]

    reduction_strategy = "by_S"
    if(reduction_strategy == "by_S"){
      # Uses 12|345 as replacement for 12|34, if 5 is removed
      setorder(X,n_features)
      X[,keep:=!duplicated(S_char_new)] #
    } else if(reduction_strategy == "by_Sbar"){
      # Uses 125|34 as replacement for 12|34, if 5 is removed
      setorder(X,-n_features)
      X[,keep:=duplicated(S_char_new)]
    } else {
      # Uses the mean of 125|34 and 12|345 as replacement for 12|34, if 5 is removed
      X[,keep:=TRUE]
    }
    setorder(X,id_combination)

    X[,id_combination_new := .GRP,by=S_char_new]

    # Always keep the zero and full set
    id_combination_new_keepers <- X[c(1,.N),id_combination_new]
    X[id_combination_new %in% id_combination_new_keepers,keep:=FALSE]
    X[c(1,.N),keep:=TRUE]

    S_mapper <- X[,.(S_char,S_char_new,keep)]

    X_dt_vS <- merge(X,dt_vS,by=c("S_char","Sbar_char"))
    X_dt_vS <- X_dt_vS[keep==TRUE]
    X_dt_vS[,p_hat_1 := mean(p_hat_1),by=id_combination_new]
    setorder(X_dt_vS,id_combination)
    dt_vS <- unique(X_dt_vS[,.(S_char = S_char_new,
                               Sbar_char = Sbar_char_new,
                               p_hat_1)])


    ### Modify which features we work with

    these_features <- cutoff_feats[-exclude_feature]
    num_feat_vec <- num_feat_vec[-exclude_feature]


    # Mofifying the feature_samples as well.

    feature_sample_1_char_dt <- data.table(S_char=sapply(feature_sample_1,paste0, collapse = "_"))
    feature_sample_1_char_dt <- merge(feature_sample_1_char_dt,S_mapper,by="S_char")
    feature_sample_1_char_dt[,S_new:=sapply(strsplit(S_char_new, "_"),as.numeric)]
    feature_sample_1_0 <- feature_sample_1_char_dt[keep==TRUE,S_new]
    feature_sample_1 <- lapply(feature_sample_1_0,match,num_feat_vec)


    feature_sample_2_char_dt <- data.table(S_char=sapply(feature_sample_2,paste0, collapse = "_"))
    feature_sample_2_char_dt <- merge(feature_sample_2_char_dt,S_mapper,by="S_char")
    feature_sample_2_char_dt[,S_new:=sapply(strsplit(S_char_new, "_"),as.numeric)]
    feature_sample_2_0 <- feature_sample_2_char_dt[keep==TRUE,S_new]
    feature_sample_2 <- lapply(feature_sample_2_0,match,num_feat_vec)


    feature_sample_all <- c(feature_sample_1,feature_sample_2)





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


  S_mapper_list[[iter]] <- data.table(feature_number = seq_along(current_feature_names),
                                      feature_name = current_feature_names)


}

