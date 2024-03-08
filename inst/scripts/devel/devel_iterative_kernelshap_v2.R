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
datafolder <- "M:/BigInsight/Projects/Explanations/EffektivShapley/NHANES-data/"

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
max_cutoff_features <- 5
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


feature_set_sample <- function(feature_sample_all = NULL, m, n_combinations = 200, unique_sampling = TRUE) {
  # Find weights for given number of features ----------
  n_features <- seq(m - 1)
  n <- sapply(n_features, choose, n = m)
  w <- shapr:::shapley_weights(m = m, N = n, n_features) * n
  p <- w / sum(w)

  if(is.null(feature_sample_all)){
    feature_sample_all <- list()
    unique_samples <- 0
  } else {
    unique_samples <- length(unique(feature_sample_all))
    n_combinations <- n_combinations + unique_samples + 2
  }


  if (unique_sampling) {
    while (unique_samples < n_combinations - 2) {
      # Sample number of chosen features ----------
      n_features_sample <- sample(
        x = n_features,
        size = n_combinations - unique_samples - 2, # Sample -2 as we add zero and m samples below
        replace = TRUE,
        prob = p
      )

      # Sample specific set of features -------
      feature_sample <- shapr:::sample_features_cpp(m, n_features_sample)
      feature_sample_all <- c(feature_sample_all, feature_sample)
      unique_samples <- length(unique(feature_sample_all))
    }
  } else {
    n_features_sample <- sample(
      x = n_features,
      size = n_combinations - 2, # Sample -2 as we add zero and m samples below
      replace = TRUE,
      prob = p
    )
    feature_sample <- shapr:::sample_features_cpp(m, n_features_sample)
    feature_sample_all <- c(feature_sample_all, feature_sample)
  }

  return(feature_sample_all)

}


# This is the shapr:::feature_not_exact function where feature_sample_all is added as an optional input.
# If that vector is set, the sampling is appended to that vector
X_from_feature_set_v2 <- function(feature_sample_all = NULL, m, weight_zero_m = 10^6,sample_ids) {
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
  X[, features := feature_sample_all]
  X[-c(1,.N), sample_id:=sample_ids]

  if (any(X[["is_duplicate"]])) {
    X <- X[is_duplicate == FALSE]
  }
  X[, is_duplicate := NULL]
  data.table::setkeyv(X, "n_features")

  # Make feature list into character
  X[, features_tmp := sapply(features, paste, collapse = " ")]

  # Aggregate weights by how many samples of a combination we observe
  X <- X[, .(
    n_features = data.table::first(n_features),
    shapley_weight = sum(shapley_weight),
    features = features[1],
    sample_id = sample_id
  ), features_tmp]

  X[, features_tmp := NULL]

  data.table::setorder(X, n_features)

  # Add shapley weight and number of combinations
  X[c(1, .N), shapley_weight := weight_zero_m]
  X[, N := 1]
  ind <- X[, .I[data.table::between(n_features, 1, m - 1)]]
  X[ind, p := p[n_features]]
  X[ind, N := n[n_features]]

  # Set column order and key table
  data.table::setkeyv(X, "n_features")

  X[, id_combination := .I]
  X[, N := as.integer(N)]
  nms <- c("id_combination", "features", "n_features", "N", "shapley_weight", "p")
  data.table::setcolorder(X, nms)


  return(X)
}

X_from_feature_set <- function(feature_sample_all = NULL, m, weight_zero_m = 10^6,remove_duplicates = TRUE,order_X = TRUE) {
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
  X[, features := feature_sample_all]
  X[-c(1,.N), orgorder:=.I]
  X[1, orgorder:=0]
  X[.N, orgorder:=100000000L] # Just a large number such that it is always last

  if (remove_duplicates == TRUE && any(X[["is_duplicate"]])) {
    X <- X[is_duplicate == FALSE]
  }
  #  X[, is_duplicate := NULL]
  data.table::setkeyv(X, "n_features")

  # Make feature list into character
  X[, features_tmp := sapply(features, paste, collapse = " ")]

  # Aggregate weights by how many samples of a combination we observe
  X <- X[, .(
    n_features = data.table::first(n_features),
    shapley_weight = sum(shapley_weight),
    features = features[1],
    orgorder = orgorder
  ), features_tmp]

  X[, features_tmp := NULL]

  data.table::setorder(X, n_features)

  # Add shapley weight and number of combinations
  X[c(1, .N), shapley_weight := weight_zero_m]
  X[, N := 1]
  ind <- X[, .I[data.table::between(n_features, 1, m - 1)]]
  X[ind, p := p[n_features]]
  X[ind, N := n[n_features]]

  # Set column order and key table
  if(order_X==FALSE){
    data.table::setorderv(X, "orgorder")
  } else {
    data.table::setkeyv(X, "n_features")
  }

  X[, id_combination := .I]
  X[, N := as.integer(N)]
  nms <- c("id_combination", "features", "n_features", "N", "shapley_weight", "p")
  data.table::setcolorder(X, nms)

  X[, orgorder := NULL]

  return(X)
}


treeShaps_dt <- as.data.table(treeShaps)
# Assume we are sampling say 20 coalitions at a time,

# Here we try start out sampling based on fewer features, but we add the full cutoff features afterwords.
# And run kernelshap with the full set of features

testObs_computed <- 1:5
converged = FALSE
m <- max_cutoff_features-0
initial_n_combinations <- 10
n_combinations_per_iter <- 10
unique_sampling <- FALSE
n_var_est_groups <- 5
n_boot_ests <- 100
n_var_est_reps <- 10
feature_sample_all <- NULL
iter <- 1
redistribute_shapley_weight <- TRUE


full_pred <- rowSums(treeShaps_dt[testObs_computed,])
pred_to_decompose <- rowSums(treeShaps_dt[testObs_computed,..cutoff_feats])
pred_not_to_decompose <- rowSums(treeShaps_dt[testObs_computed,..excluded_feature_cols])
org_p0 <- treeShaps_dt[testObs_computed,BIAS][1]
p0 <- org_p0 + pred_not_to_decompose

all.equal(p0+pred_to_decompose,full_pred)
# TRUE

# TODO:
# 1. use exact shapley value to check whether it matters whether you change the prediction or the phi0 as long
# as you adjust the other one, i.e. that what is distributed is the difference between the two
# Yes, I have done this and it is only the difference between the full pred phi0 which is distributed in the features.
# when doing exact kernelshap
# 1.5. Allow multiple testObs to be evaluated with this approach, assuming the same set of features are used for all


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
# 3.. Implement paired sampling, and use that also in the sampling
# 4. run tests to see if there is an effect in normalizing the shapley_weights, both only within n_features,
# but then also in total based on the relative weights of different number of features in the coalitions.
# as all feautres are sampled this should give close to zero sampling uncertainty, and exact kernelshap values



keep_list <- list()
set.seed(123)
while (converged == FALSE){

  these_features <- cutoff_feats[seq_len(m)]
  remaining_cutoff_feats<- cutoff_feats[-seq_len(m)]

  current_unique_feature_samples <- length(unique(feature_sample_all))
  remianing_unique_feature_samples <- 2^m-2 -current_unique_feature_samples


  if(unique_sampling == TRUE && remianing_unique_feature_samples < n_combinations_per_iter){
    n_combinations_per_iter <- remianing_unique_feature_samples
    converged = TRUE
  }

  # TODO: Expend the setup to allow proper combination of any paired samples using the improved version of the feature_not_exact function
  # where we keep the original samples and perhaps manually append the reverse of the samples
  if(iter ==1){
    feature_sample_all0 <- feature_set_sample(feature_sample_all = NULL, m=m,n_combinations = initial_n_combinations,unique_sampling = unique_sampling)
    if(length(remaining_cutoff_feats)>0){
      feature_sample_all <- lapply(feature_sample_all0, function(x) c(x,m+seq_along(remaining_cutoff_feats)))
    } else {
      feature_sample_all <- feature_sample_all0
    }

    X <- X_from_feature_set_v2(feature_sample_all,m=max_cutoff_features,sample_ids=seq_along(feature_sample_all))[]
    X[,id_combination_org := id_combination]

  } else {
    feature_sample_all0 <- feature_set_sample(feature_sample_all = feature_sample_all0, m=m,n_combinations = n_combinations_per_iter,unique_sampling = unique_sampling)
    if(length(remaining_cutoff_feats)>0){
      feature_sample_all <- lapply(feature_sample_all0, function(x) c(x,m+seq_along(remaining_cutoff_feats)))
    } else {
      feature_sample_all <- feature_sample_all0
    }

    if(redistribute_shapley_weight){
      X[,shapley_weight:=as.numeric(shapley_weight)]
      X[,shapley_weight:=mean(shapley_weight),by=n_features]
    }

    prev_n_id_combinations <- X[,.N]

    X <- X_from_feature_set_v2(feature_sample_all,m=max_cutoff_features,sample_ids=seq_along(feature_sample_all))[]
    X[-c(1,.N),id_combination_org:=id_combinations_for_feature_sample_all[sample_id]]  #### SOMETHING WRONG HERE!!!!!
    X[1,id_combination_org:=1]
    X[.N,id_combination_org:=full_pred_id_combination]
    X[is.na(id_combination_org),id_combination_org:=prev_n_id_combinations+.I]
    setorder(X,id_combination_org)

  }

  char_feature_sample_all <- sapply(feature_sample_all,function(x)paste0(x, collapse = "_"))
  char_X_features <- sapply(X$features,function(x)paste0(x, collapse = "_"))

  id_combinations_for_feature_sample_all <- match(char_feature_sample_all,char_X_features)

  comb_dt <- data.table(features = char_feature_sample_all,id_comb_org = id_combinations_for_feature_sample_all)

  setorder(X,id_combination)

  ## Get feature matrix ---------
  S <- feature_matrix_cpp(
    features = X[["features"]],
    m = max_cutoff_features
  )


  # Get weighted matrix ----------------
  W <- shapr:::weight_matrix(
    X = X,
    normalize_W_weights = TRUE,
    is_groupwise = FALSE
  )

  #### Just doing a basic variant with the feature I got here for now #####
  x_train_red <- x_train[,..cutoff_feats]

  if(iter==1){
    full_pred_id_combination <- X[,max(id_combination)]
    dt_vS <- data.table(id_combination=c(1,full_pred_id_combination))
    for(i in seq_along(testObs_computed)){
      this = paste0("p_hat_",i)
      dt_vS[,(this):=c(p0[1],pred_to_decompose[i])]
    }

  }

  new_combinations <- X[,id_combination_org][!(X[,id_combination_org] %in% dt_vS[,id_combination])]

  vS_feature_list <- list()
  if(length(new_combinations)>0){
    for(i in new_combinations){
      features_here <- unlist(X[id_combination_org ==i,features])

      tree <- shapr:::create_ctree(features_here, x_train_red, ctree.mincriterion, ctree.minsplit, ctree.minbucket)

      x_explain_red_here <- x_explain_red[testObs_computed,]


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
      vS_feature_list[[i]][,id_combination:=i]
    }

    vS_feature_dt <- rbindlist(vS_feature_list,fill=TRUE)

    x_excluded_here <- x_explain[testObs_computed,..excluded_feature_cols]

    dt <- cbind(vS_feature_dt,x_excluded_here)
    setcolorder(dt,names(x_train))

    dt[, p_hat_1 := pred_mod_xgb(model, newdata = .SD), .SDcols = feature_names]
    dt[,w:=1/.N,by=.(id_combination,id)]
    dt[,p_hat:=p_hat_1]

    dt_vS0 <- shapr:::compute_MCint(dt)

    dt_vS <- rbind(dt_vS,
                   dt_vS0)
    setorder(dt_vS,id_combination)

  }

  ### COmpute shapley values ####
  kshap <- t(W %*% as.matrix(dt_vS[, -"id_combination"]))
  dt_kshap <- data.table::as.data.table(kshap)
  colnames(dt_kshap) <- c("none", cutoff_feats)
  dt_kshap[,id:=testObs_computed]



  #### FOR estimating the variance of the Shapley values

  #
  set.seed(123)

  sample_cov_list  <- list()
  for(k in seq_along(testObs_computed)){
      sample_cov_list[[k]] <- list()
  }


  for(j in seq_len(n_var_est_reps)){


    # Covert-approach by splitting the feature samples into subgroups
    feature_sample_all_randomorder <- sample(seq_along(feature_sample_all))
    # Split the feature samples into n_var_est_groups different subgroups of approximately equal size
    feature_sample_all_randomsplit <- split(feature_sample_all_randomorder, ceiling(seq(10^(-20),n_var_est_groups,length.out = length(feature_sample_all_randomorder))))



    sample_var_list <-  list(X =list(),
                             S = list(),
                             W = list(),
                             dt_kshap = list())
    for (i in seq_len(n_var_est_groups)){

      these_ids <- feature_sample_all_randomsplit[[i]]
      X_tmp <- X_from_feature_set_v2(feature_sample_all[these_ids],m=max_cutoff_features,sample_ids = these_ids)[]
      X_tmp[-c(1,.N),id_combination_org:=id_combinations_for_feature_sample_all[sample_id]]
      X_tmp[1,id_combination_org:=1]
      X_tmp[.N,id_combination_org:=full_pred_id_combination]

      if(redistribute_shapley_weight){
        X_tmp[,shapley_weight:=as.numeric(shapley_weight)]

        X_tmp[,shapley_weight:=mean(shapley_weight),by=n_features]
      }


      sample_var_list$X[[i]] <- copy(X_tmp)

      sample_var_list$S[[i]] <- feature_matrix_cpp(
        features = X_tmp[["features"]],
        m = max_cutoff_features
      )

      # Get weighted matrix ----------------
      suppressWarnings(W_tmp <- shapr:::weight_matrix(
        X = X_tmp,
        normalize_W_weights = TRUE,
        is_groupwise = FALSE
      ))

      sample_var_list$W[[i]] <- copy(W_tmp) # If I ever need this...

      ### COmpute shapley values ####
      kshap <- t(W_tmp %*% as.matrix(dt_vS[X_tmp[,id_combination_org], -"id_combination"]))
      dt_kshap <- data.table::as.data.table(kshap)
      colnames(dt_kshap) <- c("none", cutoff_feats)
      dt_kshap[,id:=testObs_computed]

      sample_var_list$dt_kshap[[i]] <- copy(dt_kshap)

    }

    sample_var_dt_kshap <- rbindlist(sample_var_list$dt_kshap)

    for(k in seq_along(testObs_computed)){
      sample_cov_list[[k]][[j]] <- sample_var_dt_kshap[id==k,cov(.SD),.SDcols = -"id"]
    }
  }

  sample_cov_mat_full_list <- list()
  for(k in seq_along(testObs_computed)){
    mean_sample_cov <- Reduce("+",sample_cov_list[[k]])/n_var_est_reps
    n_indep_samps_full <- length(feature_sample_all)
    n_indep_samps_group <- mean(sapply(feature_sample_all_randomsplit,length))

    sample_cov_mat_full_list[[k]] <- n_indep_samps_group/n_indep_samps_full*mean_sample_cov
  }



  ### bootstrap estimation of the variance

  ### Now do the same with the bootstrap approach


  set.seed(123)
  boot_var_list <-  list(X =list(),
                         S = list(),
                         W = list(),
                         dt_kshap = list())

  for (i in seq_len(n_boot_ests)){

    these_ids <- sample(seq_along(feature_sample_all),replace = TRUE)

    X_tmp <- X_from_feature_set_v2(feature_sample_all[these_ids],m=max_cutoff_features,sample_ids = these_ids)[]
    X_tmp[-c(1,.N),id_combination_org:=id_combinations_for_feature_sample_all[sample_id]]
    X_tmp[1,id_combination_org:=1]
    X_tmp[.N,id_combination_org:=full_pred_id_combination]

    if(redistribute_shapley_weight){
      X_tmp[,shapley_weight:=as.numeric(shapley_weight)]

      X_tmp[,shapley_weight:=mean(shapley_weight),by=n_features]
    }


    boot_var_list$X[[i]] <- copy(X_tmp)

    boot_var_list$S[[i]] <- feature_matrix_cpp(
      features = X_tmp[["features"]],
      m = max_cutoff_features
    )

    # Get weighted matrix ----------------
    suppressWarnings(W_tmp <- shapr:::weight_matrix(
      X = X_tmp,
      normalize_W_weights = TRUE,
      is_groupwise = FALSE
    )
    )

    boot_var_list$W[[i]] <- copy(W_tmp) # If I ever need this...

    ### Compute shapley values ####
    kshap <- t(W_tmp %*% as.matrix(dt_vS[X_tmp[,id_combination_org], -"id_combination"]))
    dt_kshap <- data.table::as.data.table(kshap)
    colnames(dt_kshap) <- c("none", cutoff_feats)
    dt_kshap[,id:=testObs_computed]

    boot_var_list$dt_kshap[[i]] <- copy(dt_kshap)


  }

  boot_var_dt_kshap <- rbindlist(boot_var_list$dt_kshap)


  boot_cov_mat_full_list <- list()
  for(k in seq_along(testObs_computed)){
    boot_cov_mat_full_list[[k]] <- boot_var_dt_kshap[id==k,cov(.SD),.SDcols = -"id"]
  }







  keep_list[[iter]] <- list(dt_kshap = dt_kshap,
                            dt_vS = dt_vS,
                            X = X,
                            feature_sample_all = feature_sample_all,
                            sample_cov_mat_full_list = sample_cov_mat_full_list,
                            boot_cov_mat_full_list = boot_cov_mat_full_list)

  boot_sd <- t(sapply(boot_cov_mat_full_list,function(x){sqrt(diag(as.matrix(x)))}))
  samp_sd <- t(sapply(sample_cov_mat_full_list,function(x){sqrt(diag(as.matrix(x)))}))


  matrix1 <- format(as.matrix(round(dt_kshap[,-"id"],3)),width=2,justify = "right")
  matrix2 <- format(round(boot_sd,2),width=2,justify = "right")
  matrix3 <- format(round(samp_sd,2),width=2,justify = "right")

  shapley_dt_with_sd <- cbind(dt_kshap[,"id"],as.data.table(matrix(paste(matrix1, " (", matrix2,",", matrix3, ")", sep = ""), nrow = length(testObs_computed))))
  names(shapley_dt_with_sd) <- names(dt_kshap)

  shapley_dt_with_sd[,other_features:=format(round(pred_not_to_decompose,3),width=2,justify = "right")]

  vals <- as.matrix(abs(dt_kshap[,-"id"]))
  max_vals <- vals+2*boot_sd
  rank_order <- cbind(1,1+t(apply(max_vals[,-1],1,order,decreasing = TRUE)))



  print(iter)
  print(shapley_dt_with_sd)

  iter <- iter+1

}



#### working verison with a single m only# ###

converged = FALSE
m <- max_cutoff_features
initial_n_combinations <- 50
n_combinations_per_iter <- 10
n_var_est_groups <- 10
n_boot_ests <- 100
n_var_est_reps <- 10
feature_sample_all <- NULL
iter <- 1

full_pred <- sum(treeShaps_dt[testObs,])
pred_to_decompose <- sum(treeShaps_dt[testObs,..cutoff_feats])
pred_not_to_decompose <- sum(treeShaps_dt[testObs,..excluded_feature_cols])
org_p0 <- treeShaps_dt[testObs,BIAS]
p0 <- org_p0 + pred_not_to_decompose

all.equal(p0+pred_to_decompose,full_pred)
# TRUE

keep_list <- list()
set.seed(123)
while (converged == FALSE){

  these_features <- cutoff_feats[seq_len(m)]
  remaining_cutoff_feats<- cutoff_feats[-seq_len(m)]

  current_unique_feature_samples <- length(unique(feature_sample_all))
  remianing_unique_feature_samples <- 2^m-2 -current_unique_feature_samples

  if(remianing_unique_feature_samples < n_combinations_per_iter){
    n_combinations_per_iter <- remianing_unique_feature_samples
    converged = TRUE
  }

  # TODO: Expend the setup to allow proper combination of any paired samples using the improved version of the feature_not_exact function
  # where we keep the original samples and perhaps manually append the reverse of the samples
  if(iter ==1){
    feature_sample_all <- feature_set_sample(feature_sample_all = feature_sample_all, m=m,n_combinations = initial_n_combinations,unique_sampling = TRUE)
    X <- X_from_feature_set_v2(feature_sample_all,m=m,sample_ids=seq_along(feature_sample_all))[]
    X[,id_combination_org := id_combination]

  } else {
    feature_sample_all <- feature_set_sample(feature_sample_all = feature_sample_all, m=m,n_combinations = n_combinations_per_iter,unique_sampling = TRUE)

    prev_n_id_combinations <- X[,.N]

    X <- X_from_feature_set_v2(feature_sample_all,m=m,sample_ids=seq_along(feature_sample_all))[]
    X[-c(1,.N),id_combination_org:=id_combinations_for_feature_sample_all[sample_id]]
    X[1,id_combination_org:=1]
    X[.N,id_combination_org:=full_pred_id_combination]
    X[is.na(id_combination_org),id_combination_org:=prev_n_id_combinations+.I]
    setorder(X,id_combination_org)

  }


  char_feature_sample_all <- sapply(feature_sample_all,function(x)paste0(x, collapse = "_"))
  char_X_features <- sapply(X$features,function(x)paste0(x, collapse = "_"))

  id_combinations_for_feature_sample_all <- match(char_feature_sample_all,char_X_features)

  setorder(X,id_combination)

  ## Get feature matrix ---------
  S <- feature_matrix_cpp(
    features = X[["features"]],
    m = m
  )


  # Get weighted matrix ----------------
  W <- shapr:::weight_matrix(
    X = X,
    normalize_W_weights = TRUE,
    is_groupwise = FALSE
  )

  #### Just doing a basic variant with the feature I got here for now #####
  x_train_red <- x_train[,..these_features]

  if(iter==1){
    full_pred_id_combination <- X[,max(id_combination)]
    dt_vS <- data.table(id_combination=c(1,full_pred_id_combination),
                        p_hat_1 = c(p0,pred_to_decompose))

  }
  new_combinations <- X[,id_combination_org][!(X[,id_combination_org] %in% dt_vS[,id_combination])]

  vS_feature_list <- list()
  for(i in new_combinations){
    features_here <- unlist(X[id_combination_org ==i,features])

    tree <- shapr:::create_ctree(features_here, x_train_red, ctree.mincriterion, ctree.minsplit, ctree.minbucket)

    x_explain_red_here <- x_explain_red[testObs,]

    vS_feature_list[[i]] <- shapr:::sample_ctree(
      tree,
      n_samples = n_samples,
      x_explain = x_explain_red_here,
      x_train = x_train_red,
      n_features = length(x_train_red),
      sample = ctree.sample
    )
    vS_feature_list[[i]][,testObs:=testObs]
    vS_feature_list[[i]][,id_combination:=i]

  }

  vS_feature_dt <- rbindlist(vS_feature_list,fill=TRUE)

  x_excluded_here <- x_explain[testObs,..excluded_feature_cols]

  dt <- cbind(vS_feature_dt,x_excluded_here)
  setcolorder(dt,names(x_train))

  dt[, p_hat_1 := pred_mod_xgb(model, newdata = .SD), .SDcols = feature_names]
  dt[,w:=1/.N,by=id_combination]
  dt[,id:=1]
  dt[,p_hat:=p_hat_1]

  dt_vS0 <- shapr:::compute_MCint(dt)

  dt_vS <- rbind(dt_vS,
                 dt_vS0)
  setorder(dt_vS,id_combination)

  ### COmpute shapley values ####
  kshap <- t(W %*% as.matrix(dt_vS[, -"id_combination"]))
  dt_kshap <- data.table::as.data.table(kshap)
  colnames(dt_kshap) <- c("none", these_features)



  #### FOR estimating the variance of the Shapley values

  #
  set.seed(123)

  sample_cov_list <- list()
  for(j in seq_len(n_var_est_reps)){


    # Covert-approach by splitting the feature samples into subgroups
    feature_sample_all_randomorder <- sample(seq_along(feature_sample_all))
    # Split the feature samples into n_var_est_groups different subgroups of approximately equal size
    feature_sample_all_randomsplit <- split(feature_sample_all_randomorder, ceiling(seq(10^(-20),n_var_est_groups,length.out = length(feature_sample_all_randomorder))))



    sample_var_list <-  list(X =list(),
                             S = list(),
                             W = list(),
                             dt_kshap = list())
    for (i in seq_len(n_var_est_groups)){

      these_ids <- feature_sample_all_randomsplit[[i]]
      X_tmp <- X_from_feature_set_v2(feature_sample_all[these_ids],m=m,sample_ids = these_ids)[]
      X_tmp[-c(1,.N),id_combination_org:=id_combinations_for_feature_sample_all[sample_id]]
      X_tmp[1,id_combination_org:=1]
      X_tmp[.N,id_combination_org:=full_pred_id_combination]


      sample_var_list$X[[i]] <- copy(X_tmp)

      sample_var_list$S[[i]] <- feature_matrix_cpp(
        features = X_tmp[["features"]],
        m = m
      )

      # Get weighted matrix ----------------
      suppressWarnings(W_tmp <- shapr:::weight_matrix(
        X = X_tmp,
        normalize_W_weights = TRUE,
        is_groupwise = FALSE
      ))

      sample_var_list$W[[i]] <- copy(W_tmp) # If I ever need this...

      ### COmpute shapley values ####
      kshap <- t(W_tmp %*% as.matrix(dt_vS[X_tmp[,id_combination_org], -"id_combination"]))
      dt_kshap <- data.table::as.data.table(kshap)
      colnames(dt_kshap) <- c("none", these_features)

      sample_var_list$dt_kshap[[i]] <- copy(dt_kshap)

    }

    sample_var_dt_kshap <- rbindlist(sample_var_list$dt_kshap)
    sample_cov_list[[j]] <- cov(sample_var_dt_kshap)
  }

  mean_sample_cov <- Reduce("+",sample_cov_list)/n_var_est_reps

  n_indep_samps_full <- length(feature_sample_all)
  n_indep_samps_group <- mean(sapply(feature_sample_all_randomsplit,length))

  sample_cov_mat_full <- n_indep_samps_group/n_indep_samps_full*mean_sample_cov

  ### bootstrap estimation of the variance

  ### Now do the same with the bootstrap approach


  set.seed(123)
  boot_var_list <-  list(X =list(),
                           S = list(),
                           W = list(),
                           dt_kshap = list())
  for (i in seq_len(n_boot_ests)){

    these_ids <- sample(seq_along(feature_sample_all),replace = TRUE)

    X_tmp <- X_from_feature_set_v2(feature_sample_all[these_ids],m=m,sample_ids = these_ids)[]
    X_tmp[-c(1,.N),id_combination_org:=id_combinations_for_feature_sample_all[sample_id]]
    X_tmp[1,id_combination_org:=1]
    X_tmp[.N,id_combination_org:=full_pred_id_combination]

    boot_var_list$X[[i]] <- copy(X_tmp)

    boot_var_list$S[[i]] <- feature_matrix_cpp(
      features = X_tmp[["features"]],
      m = m
    )

    # Get weighted matrix ----------------
    suppressWarnings(W_tmp <- shapr:::weight_matrix(
      X = X_tmp,
      normalize_W_weights = TRUE,
      is_groupwise = FALSE
    )
    )

    boot_var_list$W[[i]] <- copy(W_tmp) # If I ever need this...

    ### Compute shapley values ####
    kshap <- t(W_tmp %*% as.matrix(dt_vS[X_tmp[,id_combination_org], -"id_combination"]))
    dt_kshap <- data.table::as.data.table(kshap)
    colnames(dt_kshap) <- c("none", these_features)

    boot_var_list$dt_kshap[[i]] <- copy(dt_kshap)


  }

  boot_var_dt_kshap <- rbindlist(boot_var_list$dt_kshap)

  boot_cov_mat_full <- cov(boot_var_dt_kshap)

  keep_list[[iter]] <- list(dt_kshap = dt_kshap,
                            dt_vS = dt_vS,
                            X = X,
                            feature_sample_all = feature_sample_all,
                            sample_cov_mat_full = sample_cov_mat_full,
                            boot_cov_mat_full = boot_cov_mat_full)

  matrix1 <- format(as.matrix(round(dt_kshap,3)),width=2,justify = "right")
  matrix2 <- format(round(as.matrix(sqrt(diag(boot_cov_mat_full))),2),width=2,justify = "right")
  matrix3 <- format(round(as.matrix(sqrt(diag(sample_cov_mat_full))),2),width=2,justify = "right")

  shapley_dt_with_sd <- as.data.table(matrix(paste(matrix1, " (", matrix2,",", matrix3, ")", sep = ""), nrow = 1))
  names(shapley_dt_with_sd) <- names(dt_kshap)

  vals <- abs(unlist(dt_kshap))
  max_vals <- vals+2*sqrt(diag(boot_cov_mat_full))
  rank_order <- c(1,1+order(-vals[-1]))

  iter <- iter+1


  print(iter)
  print(shapley_dt_with_sd)

}

age_boot_vec <- age_sample_vec <- n_samples_vec <- NULL
for(i in seq_along(keep_list)){
  n_samples_vec[i] <- length(keep_list[[i]]$feature_sample_all)
  age_sample_vec[i] <- sqrt(diag(keep_list[[i]]$sample_cov_mat_full))[9]
  age_boot_vec[i] <- sqrt(diag(keep_list[[i]]$boot_cov_mat_full))[9]
}

plot(n_samples_vec,age_sample_vec,type="l")
lines(n_samples_vec,age_boot_vec,col=2)
lines(n_samples_vec,tail(age_boot_vec,1)*max(sqrt(n_samples_vec))/sqrt(n_samples_vec),col=3)

#saveRDS(keep_list, "keep_list.rds")


keep_list = readRDS("keep_list.rds")








feature_id_names <- names(S_full_dt)[seq_along(cutoff_feats)]


dt_kshap_list <- dt_vS_list <- S_extended_list <- S_full_dt_list <- list()
j=4
for(j in 2:max_comp_features){
  these_features <- cutoff_feats[seq_len(j)]
  remaining_cutoff_feats<- cutoff_feats[-seq_len(j)]

  X <- shapr:::feature_combinations(
    m = j,
    exact = TRUE,
    n_combinations = NULL,
    weight_zero_m = 10^6,
    group_num = NULL
  )

  ## Get feature matrix ---------
  S <- feature_matrix_cpp(
    features = X[["features"]],
    m = j
  )


  # Get weighted matrix ----------------
  W <- shapr:::weight_matrix(
    X = X,
    normalize_W_weights = TRUE,
    is_groupwise = FALSE
  )

  # Here I should check what I have computed earlier in the loop...


  S_extended <- cbind(S,matrix(S_replacement_for_remaining_cutoff_feats,ncol=length(remaining_cutoff_feats),nrow=nrow(S)))

  S_extended_dt <- as.data.table(S_extended)
  S_extended_dt[,id_combination:=.I]








  S_merged <- copy(S_full_dt)


  S_merged <- rbind(S_merged,S_extended_dt,fill=TRUE)
  update_cols <- c("id_combination_full","computed_in_loop","p_hat_1")
  S_merged[,(update_cols):=nafill(.SD,type="locf"),by=feature_id_names,.SDcols=update_cols]

  this_S_dt <- S_merged[!is.na(id_combination)]


  S_extended_comp <- this_S_dt[is.na(computed_in_loop),]


  ################


  samps_list <- list()
  for(i in 1:(nrow(S_extended_comp))){
    feat_vec <- unlist(S_extended_comp[i,..feature_id_names])

    features_here <- which(feat_vec==1)
    tree <- shapr:::create_ctree(features_here, x_train_red, ctree.mincriterion, ctree.minsplit, ctree.minbucket)

    x_explain_red_here <- x_explain_red[testObs,]

    samps_list[[i]] <- shapr:::sample_ctree(
      tree,
      n_samples = n_samples,
      x_explain = x_explain_red_here,
      x_train = x_train_red,
      n_features = length(x_train_red),
      sample = ctree.sample
    )
    samps_list[[i]][,testObs:=testObs]
    id_combination_here0 <- S_extended_comp[i,id_combination]
    samps_list[[i]][,id_combination:=id_combination_here0]

  }

  samps_dt <- rbindlist(samps_list,fill=TRUE)


  x_excluded_here <- x_explain[testObs,..excluded_feature_cols]

  dt <- cbind(samps_dt,x_excluded_here)
  setcolorder(dt,names(x_train))

  dt[, p_hat_1 := pred_mod_xgb(model, newdata = .SD), .SDcols = feature_names]
  dt[,w:=1/.N,by=id_combination]
  dt[,id:=1]
  dt[,p_hat:=p_hat_1]

  dt_vS0 <- shapr:::compute_MCint(dt)

  dt_vS <- rbind(dt_vS0,this_S_dt[!is.na(computed_in_loop),.(id_combination,p_hat_1)])
  setorder(dt_vS,id_combination)

  if(fix_zero_and_full_prediction){

    dat_for_pred_zero <- cbind(x_train_red,x_excluded_here)
    setcolorder(dat_for_pred_zero,names(x_train))

    pred_zero_here <- mean(pred_mod_xgb(model,dat_for_pred_zero))
    print(pred_zero_here)

    pred_full_here <- preds[testObs]
    dt_vS[1,p_hat_1:=pred_zero_here]
    dt_vS[.N,p_hat_1:=pred_full_here]
  }



  ### COmpute shapley values ####
  kshap <- t(W %*% as.matrix(dt_vS[, -"id_combination"]))
  dt_kshap <- data.table::as.data.table(kshap)
  colnames(dt_kshap) <- c("none", these_features)


  ### Updating result lists
  dt_kshap_list[[j]] <- copy(dt_kshap)
  dt_vS_list[[j]] <- copy(dt_vS)
  S_merged[dt_vS,on="id_combination"]

  S_full_dt0 <- merge(S_merged,dt_vS,by="id_combination",all = TRUE)
  S_full_dt0[,p_hat_1:=rowSums(.SD,na.rm=T),.SDcols=c("p_hat_1.x","p_hat_1.y")]
  S_full_dt0[p_hat_1==0,p_hat_1:=NA]
  S_full_dt0[is.na(computed_in_loop) & !is.na(p_hat_1),computed_in_loop:=j]
  update_cols2 <- c("id_combination_full","computed_in_loop","p_hat_1")
  S_full_dt0[,(update_cols2):=nafill(.SD,type="nocb"),by=id_combination_full,.SDcols=update_cols2]


  S_full_dt <- S_full_dt0[is.na(id_combination),.SD,.SDcols = c(feature_id_names,"id_combination_full","computed_in_loop","p_hat_1")]

  S_full_dt_list[[j]] <- copy(S_full_dt) # Just to control that things are working as they should

  print(j)
  print(dt_kshap)
  }


### testing ends

with_1_fixed_dt_kshap_list <- copy(dt_kshap_list)
with_1_fixed_dt_vS_list <- copy(dt_vS_list)

with_1_fixed_S_full_dt <- copy(S_full_dt)


with_0_fixed_dt_kshap_list <- copy(dt_kshap_list)
with_0_fixed_dt_vS_list <- copy(dt_vS_list)

with_0_fixed_S_full_dt <- copy(S_full_dt)



with_1_dt_kshap_list <- copy(dt_kshap_list)
with_1_dt_vS_list <- copy(dt_vS_list)

with_1_S_full_dt <- copy(S_full_dt)




with_0_dt_kshap_list <- copy(dt_kshap_list)
with_0_dt_vS_list <- copy(dt_vS_list)

with_0_S_full_dt <- copy(S_full_dt)



with_1_dt_kshap_list <- copy(dt_kshap_list)
with_1_dt_vS_list <- copy(dt_vS_list)

with_1_S_full_dt <- copy(S_full_dt)



with_0_fixed_dt_kshap_list
with_1_fixed_dt_kshap_list
with_0_dt_kshap_list
with_1_dt_kshap_list














nSmall <- 0
numVarArray <- array(0,dim(treeShaps)[1])
testObs_vec <- c(5,12,27)#,24)

treeShaps
#for(testObs in 3281:dim(treeShaps)[1])
for(testObs in testObs_vec)
{
  #testObs <- 2
  numVar <- length(treeShaps[testObs,])-1
  ind <- rev(order(abs(treeShaps[testObs,-80])))
  toExplain <- sum(treeShaps[testObs,-80])
  treeShapSort <- treeShaps[testObs,ind]

  for(i in 1:numVar)
    if(abs(cumsum(as.numeric(treeShapSort))[i]-toExplain)/abs(toExplain) < 0.05)
    {
      nVarRed <- i
      break
    }
  if(plotFig)
  {
    plot.ts(cumsum(as.numeric(treeShapSort)),ylab="Sum av N foerste verdier", xlab="N")
    abline(h=toExplain,lty=2)
    abline(v=nVarRed,lty=2)
  }
  #if(nVarRed > 10)
  # nVarRed <- 6


  corMat <- cor(x_train)
  indFull <- ind[1:nVarRed]
  for(i in 1:nVarRed) indFull <- c(indFull,as.numeric(which(abs(corMat[ind[i],])>0.5)))

  indexTab  <- unique(indFull)
  nVarRed   <- length(indexTab)
  indep.ind <- which(is.na(match(1:numVar,indexTab)))
  avPred <- treeShaps[testObs,"BIAS"]
  numVarArray[testObs] <- nVarRed


  shapr_tab=reduced_Kshap(model = model,
                          x_train = x_train,
                          x_explain = x_explain[testObs,],
                          prediction_zero = avPred,
                          indexTab = indexTab,
                          n_combinations = NULL) # set n_combinations to a positive integer to use the sampling approach


  #  fwrite(shapr_tab,file="inst/scripts/devel/MJ_Kshap_new.csv",append=TRUE)

  print(testObs)
}


