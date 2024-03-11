

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


testObs_computed <- 1:10
n_combinations_vec <- c(50,100,200,400,800,1600,3200,6400)
paired_sampling_vec <- c(TRUE,FALSE)[1]
unique_sampling <- FALSE
m <- max_cutoff_features
estimation_reps <- 100
shapley_reweighting_strategy_vec <- c("none","on_N","on_n_features","on_all")


res_list_paired <- readRDS("res_list_10_features_8_with_paired_sampling.rds")
res_list_unpaired <- readRDS("res_list_10_features_8.rds")

dt_kshap_full <- res_list_paired[[1]]$dt_kshap_full

res_list <- res_list_paired

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


RMSE_paired <- rbindlist(resres_list$RMSE)
MAE_paired <- rbindlist(resres_list$MAE)
bias_paired <- rbindlist(resres_list$bias)
sd_paired <- rbindlist(resres_list$sd)
sd_est_RMSE_paired <- rbindlist(resres_list$sd_est_RMSE)
sd_est_MAE_paired <- rbindlist(resres_list$sd_est_MAE)
coverage_paired <- rbindlist(resres_list$coverage)


res_list <- res_list_unpaired

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


RMSE_unpaired <- rbindlist(resres_list$RMSE)
MAE_unpaired <- rbindlist(resres_list$MAE)
bias_unpaired <- rbindlist(resres_list$bias)
sd_unpaired <- rbindlist(resres_list$sd)
sd_est_RMSE_unpaired <- rbindlist(resres_list$sd_est_RMSE)
sd_est_MAE_unpaired <- rbindlist(resres_list$sd_est_MAE)
coverage_unpaired <- rbindlist(resres_list$coverage)


MAE_dt <- rbind(MAE_paired[,paired:=TRUE],MAE_unpaired[,paired:=FALSE])
RMSE_dt <- rbind(RMSE_paired[,paired:=TRUE],RMSE_unpaired[,paired:=FALSE])
bias_dt <- rbind(bias_paired[,paired:=TRUE],bias_unpaired[,paired:=FALSE])
sd_dt <- rbind(sd_paired[,paired:=TRUE],sd_unpaired[,paired:=FALSE])
sd_est_RMSE_dt <- rbind(sd_est_RMSE_paired[,paired:=TRUE],sd_est_RMSE_unpaired[,paired:=FALSE])
sd_est_MAE_dt <- rbind(sd_est_MAE_paired[,paired:=TRUE],sd_est_MAE_unpaired[,paired:=FALSE])
coverage_dt <- rbind(coverage_paired[,paired:=TRUE],coverage_unpaired[,paired:=FALSE])

library(ggplot2)


plot_MAE <- MAE_dt[,.(MAE=mean(colMeans(.SD))),by=.(reweighting_strategy,paired,n_comb),.SDcols=cutoff_feats]

ggplot(plot_MAE,aes(x=n_comb,y=MAE,color=reweighting_strategy,linetype=paired))+
  geom_line(linewidth=1)+
#  facet_wrap(~paired,scales="free")+
  theme_minimal()+
  # log scale on x axis
  scale_y_log10()+
  labs(title="MAE",x="n_combinations",y="Mean absolute error")



plot_bias <- bias_dt[,.(bias=mean(colMeans(.SD))),by=.(reweighting_strategy,paired,n_comb),.SDcols=cutoff_feats]

ggplot(plot_bias,aes(x=n_comb,y=abs(bias),color=reweighting_strategy,linetype=paired))+
  geom_line(linewidth=1)+
  #  facet_wrap(~paired,scales="free")+
  theme_minimal()+
  # log scale on x axis
  scale_y_log10()+
  labs(title="abs(bias)",x="n_combinations",y="Mean absolute error")

plot_sd <- sd_dt[,.(sd=mean(colMeans(.SD))),by=.(reweighting_strategy,paired,n_comb),.SDcols=cutoff_feats]

ggplot(plot_sd,aes(x=n_comb,y=sd,color=reweighting_strategy,linetype=paired))+
  geom_line(linewidth=1)+
  #  facet_wrap(~paired,scales="free")+
  theme_minimal()+
  # log scale on x axis
  scale_y_log10()+
  labs(title="sd",x="n_combinations",y="Mean absolute error")


#### Now looking at the methods for estimating this variance

plot_sd_est_MAE <- sd_est_MAE_dt[,.(sd_est_MAE=mean(colMeans(.SD))),by=.(reweighting_strategy,sd_type,paired,n_comb),.SDcols=cutoff_feats]

ggplot(plot_sd_est_MAE[reweighting_strategy=="on_n_features"],aes(x=n_comb,y=sd_est_MAE,color=sd_type,linetype=paired))+
  geom_line(linewidth=1)+
  #  facet_wrap(~paired,scales="free")+
  theme_minimal()+
  # log scale on x axis
  scale_y_log10()+
  labs(title="MAE of sd estimates",x="n_combinations",y="Mean absolute error")

# And finally coverage

plot_coverage <- coverage_dt[,.(coverage=mean(colMeans(.SD))),by=.(reweighting_strategy,sd_type,CI_level,paired,n_comb),.SDcols=cutoff_feats]
plot_coverage[,coverage_error:=coverage-CI_level]

ggplot(plot_coverage[CI_level ==0.95 &reweighting_strategy=="on_n_features"],aes(x=n_comb,y=coverage,color=sd_type,linetype=paired))+
  geom_line(linewidth=1)+
#    facet_wrap(~CI_level,scales="free")+
  theme_minimal()+
  # log scale on x a,xis
  labs(title="Coverage",x="n_combinations",y="Mean absolute error")





