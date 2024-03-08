
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
x_explain_excluded <- x_explain[,..excluded_feature_cols]


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

full_pred <- sum(treeShaps_dt[testObs,])
pred_to_decompose <- sum(treeShaps_dt[testObs,..cutoff_feats])
pred_not_to_decompose <- sum(treeShaps_dt[testObs,..excluded_feature_cols])
org_p0 <- treeShaps_dt[testObs,BIAS]
p0 <- org_p0 + pred_not_to_decompose


expl <- shapr::explain(model = model_shapr,
                       x_explain= x_explain_red[testObs,],
                       x_train = x_train_red,
                       approach = "ctree",
                       prediction_zero = p0,
                       predict_model = predict_model_shapr)

expl
#none    age systolic_blood_pressure pulse_pressure cholesterol sex_isFemale alkaline_phosphatase hematocrit
#<num>  <num>                   <num>          <num>       <num>        <num>                <num>      <num>
#  1: -0.08753 -0.838                 -0.3277        -0.1736     0.04863      -0.1826              -0.3247   -0.03286
#sedimentation_rate hemoglobin    bmi
#<num>      <num>  <num>
#  1:           -0.09941    -0.1349 -0.103

shapley_weight_dt <- expl$internal$objects$X[,.(shapley_weight_sum=sum(shapley_weight),
                                                    shapley_weight_mean=mean(shapley_weight)),by=n_features]


no_tests <- 100
n_combinations_vec <- c(15,50,100,200,400,600,800,1000)#[c(1,2,4)]

res_list <- list(dt_std=list(),dt_nfeat=list(),dt_n=list(),dt_N=list())


dt_std <- list()
dt_nfeat <- list()
dt_N <- list()
dt_all <- list()
RMSElist <- list()

expl_list <- list()
for(j in seq_along(n_combinations_vec)){
  expl_list[[j]] <- list()
  res_list$dt_std[[j]] <- list()
  res_list$dt_nfeat[[j]] <- list()
  res_list$dt_N[[j]] <- list()
  res_list$dt_all[[j]] <- list()
  for(i in seq_len(no_tests)){

    expl_list[[j]][[i]] <- shapr::explain(model = model_shapr,
                                          x_explain= x_explain_red[testObs,],
                                          x_train = x_train_red,
                                          approach = "ctree",
                                          n_combinations =n_combinations_vec[j],
                                          seed = i,
                                          prediction_zero = p0,
                                          predict_model = predict_model_shapr)

    dt_vS <- copy(expl_list[[j]][[i]]$internal$output$dt_vS)
    X_redistr_per_nfeat <- copy(expl_list[[j]][[i]]$internal$objects$X)
    X_redistr_per_N <- copy(expl_list[[j]][[i]]$internal$objects$X)
    X_redistr_all <- copy(expl_list[[j]][[i]]$internal$objects$X)


    X_redistr_per_nfeat[,shapley_weight:=as.numeric(shapley_weight)]
    X_redistr_per_nfeat[,shapley_weight:=mean(shapley_weight),by=n_features]

    X_redistr_per_N[,shapley_weight:=as.numeric(shapley_weight)]
    X_redistr_per_N[,shapley_weight:=mean(shapley_weight),by=N]

    X_redistr_all[,shapley_weight:=as.numeric(shapley_weight)]
    X_redistr_all <- merge(X_redistr_all,shapley_weight_dt,by="n_features")
    X_redistr_all[,shapley_weight:=shapley_weight_mean]

#    plot(expl_list[[j]][[i]]$internal$objects$X[-c(1,.N),shapley_weight/sum(shapley_weight)],type="l",ylim=c(0,0.1))
#    lines(X_redistr_per_nfeat[-c(1,.N),shapley_weight/sum(shapley_weight)],col=2)
#    lines(X_redistr_per_N[-c(1,.N),shapley_weight/sum(shapley_weight)],col=3)
#    lines(X_redistr_all[-c(1,.N),shapley_weight/sum(shapley_weight)],col=4)
#    plot(expl$internal$objects$X[-c(1,.N),shapley_weight/sum(shapley_weight)],col=4,type="l")

    # Get weighted matrix ----------------
    W_redistr_per_nfeat <- shapr:::weight_matrix(
      X = X_redistr_per_nfeat,
      normalize_W_weights = TRUE,
      is_groupwise = FALSE
    )

    # Get weighted matrix ----------------
    W_redistr_per_N <- shapr:::weight_matrix(
      X = X_redistr_per_N,
      normalize_W_weights = TRUE,
      is_groupwise = FALSE
    )

    # Get weighted matrix ----------------
    W_redistr_all <- shapr:::weight_matrix(
      X = X_redistr_all,
      normalize_W_weights = TRUE,
      is_groupwise = FALSE
    )


    kshap_nfeat <- t(W_redistr_per_nfeat %*% as.matrix(dt_vS[, -"id_combination"]))
    dt_kshap_nfeat <- data.table::as.data.table(kshap_nfeat)
    colnames(dt_kshap_nfeat) <- c("none", cutoff_feats)
    dt_kshap_nfeat

    kshap_N <- t(W_redistr_per_N %*% as.matrix(dt_vS[, -"id_combination"]))
    dt_kshap_N <- data.table::as.data.table(kshap_N)
    colnames(dt_kshap_N) <- c("none", cutoff_feats)
    dt_kshap_N

    kshap_all <- t(W_redistr_all %*% as.matrix(dt_vS[, -"id_combination"]))
    dt_kshap_all <- data.table::as.data.table(kshap_all)
    colnames(dt_kshap_all) <- c("none", cutoff_feats)
    dt_kshap_all

    res_list$dt_std[[j]][[i]] <- expl_list[[j]][[i]]$shapley_values
    res_list$dt_nfeat[[j]][[i]] <- dt_kshap_nfeat
    res_list$dt_N[[j]][[i]] <- dt_kshap_N
    res_list$dt_all[[j]][[i]] <- dt_kshap_all



    print(c(i,j))
  }

  dt_std[[j]] <- rbindlist(res_list$dt_std[[j]])
  dt_nfeat[[j]] <- rbindlist(res_list$dt_nfeat[[j]])
  dt_N[[j]] <- rbindlist(res_list$dt_N[[j]])
  dt_all[[j]] <- rbindlist(res_list$dt_all[[j]])

  for(i in seq_len(no_tests)){
    dt_std[[j]][i,] <- dt_std[[j]][i,]-expl$shapley_values
    dt_nfeat[[j]][i,] <- dt_nfeat[[j]][i,]-expl$shapley_values
    dt_N[[j]][i,] <- dt_N[[j]][i,]-expl$shapley_values
    dt_all[[j]][i,] <- dt_all[[j]][i,]-expl$shapley_values
  }

  plot(sqrt(colMeans(dt_std[[j]]^2)),type="l",ylim=c(0,0.3))
  lines(sqrt(colMeans(dt_nfeat[[j]]^2)),col=2)
  lines(sqrt(colMeans(dt_N[[j]]^2)),col=3)
  lines(sqrt(colMeans(dt_all[[j]]^2)),col=4)

  RMSElist[[j]] <- (c(sqrt(mean(unlist(dt_std[[j]]^2))),
                     sqrt(mean(unlist(dt_nfeat[[j]]^2))),
                     sqrt(mean(unlist(dt_N[[j]]^2))),
                     sqrt(mean(unlist(dt_all[[j]]^2)))))

  print(RMSElist)

}

####


for(j in seq_along(n_combinations_vec)){
  dt_std[[j]] <- rbindlist(res_list$dt_std[[j]])
  dt_nfeat[[j]] <- rbindlist(res_list$dt_nfeat[[j]])
  dt_N[[j]] <- rbindlist(res_list$dt_N[[j]])
  dt_all[[j]] <- rbindlist(res_list$dt_all[[j]])

  for(i in seq_len(no_tests)){
    dt_std[[j]][i,] <- dt_std[[j]][i,]-expl$shapley_values
    dt_nfeat[[j]][i,] <- dt_nfeat[[j]][i,]-expl$shapley_values
    dt_N[[j]][i,] <- dt_N[[j]][i,]-expl$shapley_values
    dt_all[[j]][i,] <- dt_all[[j]][i,]-expl$shapley_values
  }

  plot(sqrt(colMeans(dt_std[[j]]^2)),type="l",ylim=c(0,0.3))
  lines(sqrt(colMeans(dt_nfeat[[j]]^2)),col=2)
  lines(sqrt(colMeans(dt_N[[j]]^2)),col=3)
  lines(sqrt(colMeans(dt_all[[j]]^2)),col=4)

  print(c(sqrt(mean(unlist(dt_std[[j]]^2))),
  sqrt(mean(unlist(dt_nfeat[[j]]^2))),
  sqrt(mean(unlist(dt_N[[j]]^2))),
  sqrt(mean(unlist(dt_all[[j]]^2)))))

  print(c((mean(unlist(dt_std[[j]]))),
          (mean(unlist(dt_nfeat[[j]]))),
          (mean(unlist(dt_N[[j]]))),
          (mean(unlist(dt_all[[j]])))))


  Sys.sleep(2)
}


##########

(expl$shapley_values)

expl_testing <- copy(expl)



dt_vS <- expl_testing$internal$output$dt_vS
W <- expl_testing$internal$objects$W


dt_vS[1,p_hat1_1:=p0+1]
dt_vS[.N,p_hat1_1:=full_pred+1]



kshap <- t(W %*% as.matrix(dt_vS[, -"id_combination"]))
dt_kshap <- data.table::as.data.table(kshap)
colnames(dt_kshap) <- c("none", cutoff_feats)
dt_kshap

### Yes, adding the same number to both p0 and full_pred does not change the shapley values of the features


dt_vS[1,p_hat1_1:=p0-1]

kshap <- t(W %*% as.matrix(dt_vS[, -"id_combination"]))
dt_kshap <- data.table::as.data.table(kshap)
colnames(dt_kshap) <- c("none", cutoff_feats)
dt_kshap

dt_vS[1,p_hat1_1:=p0-5]

kshap <- t(W %*% as.matrix(dt_vS[, -"id_combination"]))
dt_kshap <- data.table::as.data.table(kshap)
colnames(dt_kshap) <- c("none", cutoff_feats)
dt_kshap

dt_vS[1,p_hat1_1:=p0-10]

kshap <- t(W %*% as.matrix(dt_vS[, -"id_combination"]))
dt_kshap <- data.table::as.data.table(kshap)
colnames(dt_kshap) <- c("none", cutoff_feats)
dt_kshap

# Yes, adding to p0 adds to the features add/nofeatures to each of the features

dt_vS[.N,p_hat1_1:=full_pred-5]

kshap <- t(W %*% as.matrix(dt_vS[, -"id_combination"]))
dt_kshap <- data.table::as.data.table(kshap)
colnames(dt_kshap) <- c("none", cutoff_feats)
dt_kshap

dt_vS[.N,p_hat1_1:=full_pred-10]

kshap <- t(W %*% as.matrix(dt_vS[, -"id_combination"]))
dt_kshap <- data.table::as.data.table(kshap)
colnames(dt_kshap) <- c("none", cutoff_feats)
dt_kshap

# Yes, adding to full pred also adds to the feature add/nfeatures to each of the features
