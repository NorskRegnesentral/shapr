
library(xgboost)
library(data.table)

# Install the github version of the shapr pacakge
#remotes::install_github("NorskRegnesentral/shapr")

library(shapr)

library(progressr)
progressr::handlers(global = TRUE) # To get progress updates

library(future)
#future::plan(multisession, workers = 4) # for paralellization (on both linux and windows)
future::plan(sequential)


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

treeShaps_dt <- as.data.table(treeShaps)

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
max_cutoff_features <- 10
max_cutoff_remaining_imp <- 0.10
p0 <- treeShaps[1,"BIAS"]
S_replacement_for_remaining_cutoff_feats <- 1 # 1 for conditioning on these, 0 for marginalizing them out



fix_zero_and_full_prediction <- TRUE

# For a specific testObs:

testObs <- 24
testObs <- 5

org_imp <- abs(treeShaps[testObs,-(n_features+1)]) # absolute value of treeshap values
norm_org_imp <- org_imp/sum(org_imp)
cor_imp <- as.vector(org_imp%*%abscorMat^2)
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
x_explain_excluded <<- x_explain[,..excluded_feature_cols]


ctree.mincriterion = 0.95
ctree.minsplit = 20
ctree.minbucket = 7
ctree.sample = TRUE
n_samples <- 1000


library(shapr)



model_shapr <- model
class(model_shapr) <- "blabla"

predict_model_shapr <<- function(model_shapr,newdata){

  newdata_excluded <- unlist(x_explain_excluded[testObs,])

  newdata_excluded <- matrix(rep(newdata_excluded,each=dim(newdata)[1]),nrow=dim(newdata)[1])
  colnames(newdata_excluded) <- colnames(x_explain_excluded)
  newdata_full <- as.data.table(cbind(newdata_excluded,as.matrix(newdata)))
  setcolorder(newdata_full,names(x_train))

  class(model_shapr) = "xgb.Booster"

  xgboost:::predict.xgb.Booster(model_shapr,as.matrix(newdata_full),outputmargin =TRUE)
}

aa=predict_model_shapr(model_shapr,x_explain_red[testObs,])
bb = predict(model,as.matrix(x_explain[testObs,]),outputmargin = TRUE)
aa
bb



expl <- shapr::explain(model = model_shapr,
                       x_explain= x_explain_red[testObs,],
                       x_train = x_train_red,
                       approach = "ctree",
                       prediction_zero = p0,
                       n_batches = 1, # In order to get the all the  all_trees with the set to global <<- hack we introduced
                       predict_model = predict_model_shapr)

seq_along(all_trees)
W_full <- expl$internal$objects$W
### OK, so now we have the trees we need


testObs_computed <- 1:10


full_pred <- rowSums(treeShaps_dt[testObs_computed,])
pred_to_decompose <- rowSums(treeShaps_dt[testObs_computed,..cutoff_feats])
pred_not_to_decompose <- rowSums(treeShaps_dt[testObs_computed,..excluded_feature_cols])
org_p0 <- treeShaps_dt[testObs_computed,BIAS][1]
p0 <- org_p0 + pred_not_to_decompose

pred_mod_xgb <- function(model,newdata){
  xgboost:::predict.xgb.Booster(model,as.matrix(newdata),outputmargin = TRUE)
}



index_features <- expl$internal$objects$X[approach=="ctree",id_combination]
samp_list <- list()
for(j in seq_along(testObs_computed)){

  l <- lapply(
    X = all_trees,
    FUN = shapr:::sample_ctree,
    n_samples = n_samples,
    x_explain = x_explain_red[testObs_computed[j], , drop = FALSE],
    x_train = x_train_red,
    n_features = ncol(x_train_red),
    sample = ctree.sample
  )

  samp_list[[j]] <- data.table::rbindlist(l, idcol = "id_combination")
  samp_list[[j]][, w := 1 / n_samples]
  samp_list[[j]][, id := testObs_computed[j]]
  if (!is.null(index_features)) samp_list[[j]][, id_combination := index_features[id_combination]]

}


vS_feature_dt <- rbindlist(samp_list)

x_excluded_here <- x_explain[testObs_computed,..excluded_feature_cols]

dt <- cbind(vS_feature_dt,x_excluded_here)
setcolorder(dt,names(x_train))



dt[, p_hat_1 := pred_mod_xgb(model, newdata = .SD), .SDcols = feature_names]
dt[,w:=1/.N,by=.(id_combination,id)]
dt[,p_hat:=p_hat_1]

full_pred_id_combination <- expl$internal$objects$X[,max(id_combination)]
dt_vS <- data.table(id_combination=c(1,full_pred_id_combination))
for(i in seq_along(testObs_computed)){
  this = paste0("p_hat_",i)
  dt_vS[,(this):=c(p0[1],pred_to_decompose[i])]
}


dt_vS0 <- shapr:::compute_MCint(dt)

dt_vS <- rbind(dt_vS,
               dt_vS0)
setorder(dt_vS,id_combination)

id_combination_features_char_mapping <- expl$internal$objects$X[,.(id_combination,features_char=sapply(features,function(x)paste0(x, collapse = "_")))]

dt_vS <- merge(dt_vS,id_combination_features_char_mapping,by="id_combination")

kshap <- t(W_full %*% as.matrix(dt_vS[, -c("id_combination","features_char")]))
dt_kshap_full <- data.table::as.data.table(kshap)
colnames(dt_kshap_full) <- c("none", cutoff_feats)
dt_kshap_full[,id:=testObs_computed]
setcolorder(dt_kshap_full,"id")


#### funcs from devel_iterative_kernelshap_v2

# feature_sample_all is always NULL here, since that is what we do in the simulation setting here
feature_set_sample <- function(feature_sample_all = NULL, m, n_combinations = 200, unique_sampling = TRUE,paired_sampling = FALSE) {
  if(paired_sampling == TRUE & unique_sampling == TRUE){
    stop("unique_sampling cannot be TRUE for paired_sampling in the simulation setting. Should implement it when we don't do simulations though.")
  }
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
      size = (n_combinations - 2)*ifelse(paired_sampling,0.5,1), # Sample -2 as we add zero and m samples below
      replace = TRUE,
      prob = p
    )
    feature_sample <- shapr:::sample_features_cpp(m, n_features_sample)
    if(paired_sampling){
      feature_sample_paired <- lapply(feature_sample, function(x) seq(m)[-x])
      feature_sample <- c(feature_sample,feature_sample_paired) # Beware that the ordering here only works properly if we sample all at the same time, not with feature_sample_all set to something else as input
    }

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
      X_tmp <- X_from_feature_set_v2(feature_sample_all[these_ids],m=m,sample_ids=these_ids)[] # sample_ids could be removed from the function -- never used

      X_tmp[,features_char:=sapply(features,function(x)paste0(x, collapse = "_"))]

      X_rw_tmp <- shapley_reweighting(X_tmp,strategy = shapley_reweighting_strategy)

      # Get weighted matrix ----------------
      W_tmp <- shapr:::weight_matrix(
        X = X_rw_tmp,
        normalize_W_weights = TRUE,
        is_groupwise = FALSE
      )

      dt_vS_relevant <- merge(dt_vS, X_rw_tmp[,.(sorter=id_combination,features_char)], by = "features_char")
      setorder(dt_vS_relevant,sorter)
      dt_vS_relevant[,sorter:=NULL]

      kshap <- t(W_tmp %*% as.matrix(dt_vS_relevant[, -c("id_combination","features_char")]))
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
        tmp_cov_list[[j]] <- sample_var_dt_kshap_list[[j]][id==k,cov(.SD),.SDcols = -"id"]
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
boot_cov_estimator <- function(feature_sample_all,dt_vS,testObs_computed,n_boot_ests = 100,m,shapley_reweighting_strategy = "on_N",return = "sd_mat",
                               paired_sampling = paired_sampling){

  boot_var_list <- list()

  for (i in seq_len(n_boot_ests)){

    if(paired_sampling == TRUE){ # Assuming the second half is the paired copy of the first half of the feature samples
      halfway <- length(feature_sample_all)/2
      feature_sample_initial <- feature_sample_all[1:halfway]

      these_ids0 <- sample(seq_along(feature_sample_initial),replace = TRUE)
      these_ids <- c(these_ids0,these_ids0+halfway)

    } else {
      these_ids <- sample(seq_along(feature_sample_all),replace = TRUE)
    }


    X_tmp <- X_from_feature_set_v2(feature_sample_all[these_ids],m=m,sample_ids=these_ids)[] # sample_ids could be removed from the function -- never used

    X_tmp[,features_char:=sapply(features,function(x)paste0(x, collapse = "_"))]

    X_rw_tmp <- shapley_reweighting(X_tmp,strategy = shapley_reweighting_strategy)

    # Get weighted matrix ----------------
    W_tmp <- shapr:::weight_matrix(
      X = X_rw_tmp,
      normalize_W_weights = TRUE,
      is_groupwise = FALSE
    )

    dt_vS_relevant <- merge(dt_vS, X_rw_tmp[,.(sorter=id_combination,features_char)], by = "features_char")
    setorder(dt_vS_relevant,sorter)
    dt_vS_relevant[,sorter:=NULL]


    kshap <- t(W_tmp %*% as.matrix(dt_vS_relevant[, -c("id_combination","features_char")]))
    dt_kshap <- data.table::as.data.table(kshap)
    colnames(dt_kshap) <- c("none", cutoff_feats)
    dt_kshap[,id:=testObs_computed]

    boot_var_list[[i]] <- copy(dt_kshap)


  }

  boot_var_dt_kshap <- rbindlist(boot_var_list)


  boot_cov_mat_full_list <- list()
  for(k in seq_along(testObs_computed)){
    boot_cov_mat_full_list[[k]] <- boot_var_dt_kshap[id==k,cov(.SD),.SDcols = -"id"]
  }

  if(return == "sd_mat"){
    ret <- t(sapply(boot_cov_mat_full_list,function(x) sqrt(diag(x))))
    ret_dt <- as.data.table(ret)
    ret_dt[,id:=testObs_computed]
    if(paired_sampling){
      ret_dt[,sd_type:="boot_unpaired"]
    } else {
      ret_dt[,sd_type:="boot_paired"]
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


#### NEXT:



# do regular feature_sampling like in iterative_kernelshap_v2, compute the W and so on,
# then add the features_char to that X_tmp as well.
# then insetad, of doing the vS estimation etc, use the mapping above to reduce the full dt_vS
# to the one we need for that specific sample, and then use that in the kernelshap computations
# in the end


n_combinations_vec <- c(50,100,200,400,800,1600,3200,6400)[1:5]
res_list <- list()
paired_sampling_vec <- c(TRUE,FALSE)[1]
unique_sampling <- FALSE
m <- max_cutoff_features
estimation_reps <- 100
shapley_reweighting_strategy_vec <- c("none","on_N","on_n_features","on_all")

set.seed(123)

dt_kshap_list <- dt_sdkshap_list <- list()


for(aa in seq_along(n_combinations_vec)){

  n_combinations <- n_combinations_vec[aa]

  for (bb in seq_along(paired_sampling_vec)){

    paired_sampling <- paired_sampling_vec[bb]

    for(i in seq_len(estimation_reps)){

      feature_sample_all <- feature_set_sample(feature_sample_all = NULL, m=m,n_combinations = n_combinations,unique_sampling = unique_sampling,paired_sampling = paired_sampling)

      X <- X_from_feature_set_v2(feature_sample_all,m=m,sample_ids=seq_along(feature_sample_all))[]
      X[,features_char:=sapply(features,function(x)paste0(x, collapse = "_"))]

      tmp <- list(kshap=list(),sdkshap=list())
      for(j in seq_along(shapley_reweighting_strategy_vec)){
        shapley_reweighting_strategy <- shapley_reweighting_strategy_vec[j]

        X_rw <- shapley_reweighting(X,strategy = shapley_reweighting_strategy)

        # Get weighted matrix ----------------
        W <- shapr:::weight_matrix(
          X = X_rw,
          normalize_W_weights = TRUE,
          is_groupwise = FALSE
        )

        dt_vS_relevant <- merge(dt_vS, X_rw[,.(sorter=id_combination,features_char)], by = "features_char")
        setorder(dt_vS_relevant,sorter)
        dt_vS_relevant[,sorter:=NULL]

        kshap <- t(W %*% as.matrix(dt_vS_relevant[, -c("id_combination","features_char")]))
        dt_kshap <- data.table::as.data.table(kshap)
        colnames(dt_kshap) <- c("none", cutoff_feats)
        dt_kshap[,id:=testObs_computed]
        dt_kshap[,est_rep:=i]
        dt_kshap[,reweighting_strategy:=shapley_reweighting_strategy]
        setcolorder(dt_kshap,c("id","est_rep","reweighting_strategy"))
        tmp$kshap[[j]] <- copy(dt_kshap)


        #sample_sd_dt_combined <- sample_cov_estimator(feature_sample_all,dt_vS=copy(dt_vS_relevant),testObs_computed,n_var_est_reps = 10,n_var_est_groups = 10,m,comp_strategy = "all_combined",return="sd_mat")
        sample_sd_dt_seperate <- sample_cov_estimator(feature_sample_all,dt_vS=copy(dt_vS_relevant),testObs_computed,n_var_est_reps = 10,n_var_est_groups = 10,m,comp_strategy = "separate",return="sd_mat",paired_sampling = paired_sampling)
        boot_sd_dt_paired <- boot_cov_estimator(feature_sample_all,dt_vS=dt_vS_relevant,testObs_computed,n_boot_ests = 100,m,shapley_reweighting_strategy = shapley_reweighting_strategy,return="sd_mat",paired_sampling = paired_sampling)
        boot_sd_dt_unpaired <- boot_cov_estimator(feature_sample_all,dt_vS=dt_vS_relevant,testObs_computed,n_boot_ests = 100,m,shapley_reweighting_strategy = shapley_reweighting_strategy,return="sd_mat",paired_sampling = FALSE)


        sd_dt <- rbind(sample_sd_dt_seperate,
                       boot_sd_dt_paired,
                       boot_sd_dt_unpaired)

        #sd_dt <- rbind(sample_sd_dt_combined,
        #               sample_sd_dt_seperate,
        #               boot_sd_dt)

        sd_dt[,est_rep:=i]
        sd_dt[,reweighting_strategy:=shapley_reweighting_strategy]
        setcolorder(sd_dt,c("sd_type","id","est_rep","reweighting_strategy"))

        # save results
        tmp$sdkshap[[j]] <- copy(sd_dt)

      }

      dt_kshap_list[[i]] <- rbindlist(tmp$kshap)
      dt_sdkshap_list[[i]] <- rbindlist(tmp$sdkshap)

      print(c(aa,i))
    }

    dt_kshap_sim <- rbindlist(dt_kshap_list)
    dt_sdkshap_sim <- rbindlist(dt_sdkshap_list)

    dt_kshap_sim[,n_comb:=n_combinations]
    dt_sdkshap_sim[,n_comb:=n_combinations]

    dt_kshap_sim[,uses_paired_sampling:=paired_sampling]
    dt_sdkshap_sim[,uses_paired_sampling:=paired_sampling]

  }

  res_list[[aa]] <- list(dt_kshap_full = dt_kshap_full,
                         dt_kshap_sim = dt_kshap_sim,
                         dt_sdkshap_sim  = dt_sdkshap_sim)


  saveRDS(res_list,file=paste0("res_list_10_features_",aa,"_with_paired_sampling.rds"))


}


dt_kshap_sim_all <- dt_sdkshap_sim_all <- NULL
dt_kshap_full <- res_list[[1]]$dt_kshap_full

for(aa in seq_along(n_combinations_vec)){
  dt_kshap_sim_all <- rbind(dt_kshap_sim_all,res_list[[aa]]$dt_kshap_sim)
  dt_sdkshap_sim_all <- rbind(dt_sdkshap_sim_all,res_list[[aa]]$dt_sdkshap_sim)

}




resres_list <- list()
for(aa in seq_along(n_combinations_vec)){

  dt_kshap_sim <- copy(res_list[[aa]]$dt_kshap_sim)
  dt_sdkshap_sim <- copy(res_list[[aa]]$dt_sdkshap_sim)


  ### RMSE on the actual kernelshap estimates
  RMSE_dt_list <- list()
  for(i in seq_along(dt_kshap_full$id)){
    RMSE_tmp <- list()
    for(j in seq_along(shapley_reweighting_strategy_vec)){
      truth <- unlist(dt_kshap_full[id==i,-c("id","none")])
      RMSE_tmp[[j]] <- as.data.table(matrix(sqrt(colMeans((t(t(dt_kshap_sim[id==i & reweighting_strategy==shapley_reweighting_strategy_vec[j],..cutoff_feats])-truth))^2)),nrow=1))
      names(RMSE_tmp[[j]]) <- cutoff_feats
      RMSE_tmp[[j]][,reweighting_strategy:=shapley_reweighting_strategy_vec[j]]
    }
    RMSE_dt_list[[i]] <- rbindlist(RMSE_tmp)
    RMSE_dt_list[[i]][,id:=i]
  }
  RMSE_dt <- rbindlist(RMSE_dt_list)

  MAE_dt_list <- list()
  for(i in seq_along(dt_kshap_full$id)){
    MAE_tmp <- list()
    for(j in seq_along(shapley_reweighting_strategy_vec)){
      truth <- unlist(dt_kshap_full[id==i,-c("id","none")])
      MAE_tmp[[j]] <- as.data.table(matrix((colMeans(abs(t(t(dt_kshap_sim[id==i & reweighting_strategy==shapley_reweighting_strategy_vec[j],..cutoff_feats])-truth)))),nrow=1))
      names(MAE_tmp[[j]]) <- cutoff_feats
      MAE_tmp[[j]][,reweighting_strategy:=shapley_reweighting_strategy_vec[j]]
    }
    MAE_dt_list[[i]] <- rbindlist(MAE_tmp)
    MAE_dt_list[[i]][,id:=i]
  }
  MAE_dt <- rbindlist(MAE_dt_list)



  # Bias of the actual kernelshap estimates
  bias_dt_list <- list()
  for(i in seq_along(dt_kshap_full$id)){
    bias_tmp <- list()
    for(j in seq_along(shapley_reweighting_strategy_vec)){
      truth <- unlist(dt_kshap_full[id==i,-c("id","none")])
      bias_tmp[[j]] <- as.data.table(matrix(colMeans(dt_kshap_sim[id==i & reweighting_strategy==shapley_reweighting_strategy_vec[j],..cutoff_feats])-truth,nrow=1))
      names(bias_tmp[[j]]) <- cutoff_feats
      bias_tmp[[j]][,reweighting_strategy:=shapley_reweighting_strategy_vec[j]]
    }
    bias_dt_list[[i]] <- rbindlist(bias_tmp)
    bias_dt_list[[i]][,id:=i]
  }
  bias_dt <- rbindlist(bias_dt_list)

  bias_dt[,lapply(.SD,mean),by=reweighting_strategy,.SDcols=cutoff_feats]

  # sd of the actual kernelshap estimates
  sd_dt_list <- list()
  for(i in seq_along(dt_kshap_full$id)){
    sd_tmp <- list()
    for(j in seq_along(shapley_reweighting_strategy_vec)){
      truth <- dt_kshap_sim[id==i & reweighting_strategy==shapley_reweighting_strategy_vec[j],..cutoff_feats][,lapply(.SD,sd)]
      sd_tmp[[j]] <- truth
      sd_tmp[[j]][,reweighting_strategy:=shapley_reweighting_strategy_vec[j]]
    }
    sd_dt_list[[i]] <- rbindlist(sd_tmp)
    sd_dt_list[[i]][,id:=i]
  }
  sd_dt <- rbindlist(sd_dt_list)

  sd_dt[,lapply(.SD,mean),by=reweighting_strategy,.SDcols=cutoff_feats]

  ## Comparing the estimated sd with the true sd

  sd_types <- unique(dt_sdkshap_sim$sd_type)
  sd_est_RMSE_dt_list <- list()
  for(i in seq_along(dt_kshap_full$id)){
    sd_est_RMSE_tmp0 <- list()
    for(j in seq_along(shapley_reweighting_strategy_vec)){
      truth <- unlist(sd_dt[id==i & reweighting_strategy==shapley_reweighting_strategy_vec[j],..cutoff_feats])
      sd_est_RMSE_tmp <- list()
      for(k in seq_along(sd_types)){
        est <- as.matrix(dt_sdkshap_sim[id==i & reweighting_strategy==shapley_reweighting_strategy_vec[j] & sd_type==sd_types[k],..cutoff_feats])
        sd_est_RMSE_tmp[[k]] <- as.data.table(matrix(sqrt(colMeans(t((t(est)-truth)^2))),nrow=1))
        names(sd_est_RMSE_tmp[[k]]) <- cutoff_feats
        sd_est_RMSE_tmp[[k]][,reweighting_strategy:=shapley_reweighting_strategy_vec[j]]
        sd_est_RMSE_tmp[[k]][,sd_type:=sd_types[k]]
      }
      sd_est_RMSE_tmp0[[j]] <- rbindlist(sd_est_RMSE_tmp)
    }
    sd_est_RMSE_dt_list[[i]] <- rbindlist(sd_est_RMSE_tmp0)
    sd_est_RMSE_dt_list[[i]][,id:=i]
  }
  sd_est_RMSE_dt <- rbindlist(sd_est_RMSE_dt_list)

  ## Comparing the estimated sd with the true sd for MAE

  sd_types <- unique(dt_sdkshap_sim$sd_type)
  sd_est_MAE_dt_list <- list()
  for(i in seq_along(dt_kshap_full$id)){
    sd_est_MAE_tmp0 <- list()
    for(j in seq_along(shapley_reweighting_strategy_vec)){
      truth <- unlist(sd_dt[id==i & reweighting_strategy==shapley_reweighting_strategy_vec[j],..cutoff_feats])
      sd_est_MAE_tmp <- list()
      for(k in seq_along(sd_types)){
        est <- as.matrix(dt_sdkshap_sim[id==i & reweighting_strategy==shapley_reweighting_strategy_vec[j] & sd_type==sd_types[k],..cutoff_feats])
        sd_est_MAE_tmp[[k]] <- as.data.table(matrix((colMeans(t(abs(t(est)-truth)))),nrow=1))
        names(sd_est_MAE_tmp[[k]]) <- cutoff_feats
        sd_est_MAE_tmp[[k]][,reweighting_strategy:=shapley_reweighting_strategy_vec[j]]
        sd_est_MAE_tmp[[k]][,sd_type:=sd_types[k]]
      }
      sd_est_MAE_tmp0[[j]] <- rbindlist(sd_est_MAE_tmp)
    }
    sd_est_MAE_dt_list[[i]] <- rbindlist(sd_est_MAE_tmp0)
    sd_est_MAE_dt_list[[i]][,id:=i]
  }
  sd_est_MAE_dt <- rbindlist(sd_est_MAE_dt_list)

  sd_est_MAE_dt[,lapply(.SD,mean),by=.(reweighting_strategy,sd_type),.SDcols=cutoff_feats]

  #### Comparing the coverage as well ####

  CI_levels <- c(0.5,0.9,0.95,0.99)

  sd_types <- unique(dt_sdkshap_sim$sd_type)
  coverage_dt_list <- list()
  for(i in seq_along(dt_kshap_full$id)){
    truth <- unlist(dt_kshap_full[id==i,..cutoff_feats])
    coverage_tmp0 <- list()
    for(j in seq_along(shapley_reweighting_strategy_vec)){
      vals <- dt_kshap_sim[id==i & reweighting_strategy==shapley_reweighting_strategy_vec[j],..cutoff_feats]
      coverage_tmp00 <- list()
      for(k in seq_along(sd_types)){
        sds <-  dt_sdkshap_sim[id==i & reweighting_strategy==shapley_reweighting_strategy_vec[j] &  sd_type==sd_types[k],..cutoff_feats]
        coverage_tmp000 <- list()
        for(kk in seq_along(CI_levels)){


          lower <- vals+qnorm((1-CI_levels[kk])/2)*sds
          upper <- vals+qnorm(1-(1-CI_levels[kk])/2)*sds

          truth_over_lower <- t(t(as.matrix(lower))-truth)<0
          truth_under_upper <- t(t(as.matrix(upper))-truth)>0

          truth_between_lower_n_upper <- truth_over_lower & truth_under_upper

          coverage_tmp000[[kk]] <- as.data.table(matrix(colMeans(truth_between_lower_n_upper),nrow=1))
          names(coverage_tmp000[[kk]]) <- cutoff_feats
          coverage_tmp000[[kk]][,reweighting_strategy:=shapley_reweighting_strategy_vec[j]]
          coverage_tmp000[[kk]][,sd_type:=sd_types[k]]
          coverage_tmp000[[kk]][,CI_level:=CI_levels[kk]]

        }
        coverage_tmp00[[k]] <- rbindlist(coverage_tmp000)
      }
      coverage_tmp0[[j]] <- rbindlist(coverage_tmp00)
    }
    coverage_dt_list[[i]] <- rbindlist(coverage_tmp0)
    coverage_dt_list[[i]][,id:=i]
  }
  coverage_dt <- rbindlist(coverage_dt_list)

  coverage_dt[,lapply(.SD,mean),by=.(reweighting_strategy,sd_type,CI_level),.SDcols=cutoff_feats]

  RMSE_dt[,n_comb:=n_combinations_vec[aa]]
  MAE_dt[,n_comb:=n_combinations_vec[aa]]
  bias_dt[,n_comb:=n_combinations_vec[aa]]
  sd_dt[,n_comb:=n_combinations_vec[aa]]
  sd_est_RMSE_dt[,n_comb:=n_combinations_vec[aa]]
  sd_est_MAE_dt[,n_comb:=n_combinations_vec[aa]]
  coverage_dt[,n_comb:=n_combinations_vec[aa]]

  resres_list$RMSE[[aa]] <- copy(RMSE_dt)
  resres_list$MAE[[aa]] <- copy(MAE_dt)
  resres_list$bias[[aa]] <- copy(bias_dt)
  resres_list$sd[[aa]] <- copy(sd_dt)
  resres_list$sd_est_RMSE[[aa]] <- copy(sd_est_RMSE_dt)
  resres_list$sd_est_MAE[[aa]] <- copy(sd_est_MAE_dt)
  resres_list$coverage[[aa]] <- copy(coverage_dt)


}


RMSE <- rbindlist(resres_list$RMSE)
MAE <- rbindlist(resres_list$MAE)
bias <- rbindlist(resres_list$bias)
sd <- rbindlist(resres_list$sd)
sd_est_RMSE <- rbindlist(resres_list$sd_est_RMSE)
sd_est_MAE <- rbindlist(resres_list$sd_est_MAE)
coverage <- rbindlist(resres_list$coverage)

aaap=MAE[n_comb==100,lapply(.SD,mean),by=.(n_comb,reweighting_strategy),.SDcols=cutoff_feats][,rowMeans(abs(.SD)),.SDcols=cutoff_feats,by=.(n_comb,reweighting_strategy)]
bbbp=sd_est_MAE[n_comb==100&sd_type=="boot_paired",lapply(.SD,mean),by=reweighting_strategy,.SDcols=cutoff_feats][,rowMeans(abs(.SD)),.SDcols=cutoff_feats,by=reweighting_strategy]
bbbp2=sd_est_MAE[n_comb==100&sd_type=="boot_unpaired",lapply(.SD,mean),by=reweighting_strategy,.SDcols=cutoff_feats][,rowMeans(abs(.SD)),.SDcols=cutoff_feats,by=reweighting_strategy]
bbbp3=sd_est_MAE[n_comb==100&sd_type=="sample_separate",lapply(.SD,mean),by=reweighting_strategy,.SDcols=cutoff_feats][,rowMeans(abs(.SD)),.SDcols=cutoff_feats,by=reweighting_strategy]


aaa=MAE[n_comb==100,lapply(.SD,mean),by=.(n_comb,reweighting_strategy),.SDcols=cutoff_feats][,rowMeans(abs(.SD)),.SDcols=cutoff_feats,by=.(n_comb,reweighting_strategy)]
bbb=sd_est_MAE[n_comb==100&sd_type=="boot",lapply(.SD,mean),by=reweighting_strategy,.SDcols=cutoff_feats][,rowMeans(abs(.SD)),.SDcols=cutoff_feats,by=reweighting_strategy]


MAE[lapply(.SD,mean),by=.(n_comb,reweighting_strategy),.SDcols=cutoff_feats][,rowMeans(abs(.SD)),.SDcols=cutoff_feats,by=.(n_comb,reweighting_strategy)]
RMSE[lapply(.SD,mean),by=.(n_comb,reweighting_strategy),.SDcols=cutoff_feats][,rowMeans(abs(.SD)),.SDcols=cutoff_feats,by=.(n_comb,reweighting_strategy)]
coverage[sd_type=="boot",lapply(.SD,mean),by=.(CI_level,reweighting_strategy),.SDcols=cutoff_feats][,rowMeans(abs(.SD)),.SDcols=cutoff_feats,by=.(CI_level,reweighting_strategy)]
coverage[sd_type=="boot" & reweighting_strategy=="on_N",lapply(.SD,mean),by=.(CI_level,n_comb),.SDcols=cutoff_feats][,rowMeans(abs(.SD)),.SDcols=cutoff_feats,by=.(CI_level,n_comb)]
coverage[sd_type=="boot" & CI_level==0.95,lapply(.SD,mean),by=.(reweighting_strategy,n_comb),.SDcols=cutoff_feats][,rowMeans(abs(.SD)),.SDcols=cutoff_feats,by=.(reweighting_strategy,n_comb)]
coverage[sd_type=="sample_separate" & CI_level==0.95,lapply(.SD,mean),by=.(reweighting_strategy,n_comb),.SDcols=cutoff_feats][,rowMeans(abs(.SD)),.SDcols=cutoff_feats,by=.(reweighting_strategy,n_comb)]

coverage[sd_type=="boot",lapply(.SD,mean),by=.(CI_level,n_comb,reweighting_strategy),.SDcols=cutoff_feats][,rowMeans(abs(.SD)),.SDcols=cutoff_feats,by=.(CI_level,n_comb,reweighting_strategy)]


sd_est_MAE[sd_type=="sample_separate",lapply(.SD,mean),by=reweighting_strategy,.SDcols=cutoff_feats][,rowMeans(abs(.SD)),.SDcols=cutoff_feats,by=reweighting_strategy]
sd_est_RMSE[sd_type=="sample_separate",lapply(.SD,mean),by=reweighting_strategy,.SDcols=cutoff_feats][,rowMeans(abs(.SD)),.SDcols=cutoff_feats,by=reweighting_strategy]


