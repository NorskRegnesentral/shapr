source("/nr/project/stat/BigInsight//Projects//Explanations//EffektivShapley//helpFunctions.R")

#library(treeshap)
library(xgboost)
library(data.table)

# Install the github version of the shapr pacakge
#remotes::install_github("NorskRegnesentral/shapr")

library(shapr)

library(progressr)
progressr::handlers(global = TRUE) # To get progress updates

library(future)
future::plan(multisession, workers = 10) # for paralellization (on both linux and windows)


plotFig <- 0

datafolder <- "/nr/project/stat/BigInsight/Projects/Explanations/EffektivShapley/NHANES-data/"

x_explain <- fread(file.path(datafolder,"newdata/Xtest_imp.csv"))
y_test    <- fread(file.path(datafolder,"newdata/ytest.csv"))$V1
x_train   <- fread(file.path(datafolder,"newdata/Xtrain_imp.csv"))
y_train   <- fread(file.path(datafolder,"newdata/ytrain.csv"))$V1

names(x_train)[52:55]   <- c("urine_albumin_is_gt_30","urine_albumin_is_gt_100","urine_albumin_is_get_300", "urine_albumin_is_gt_1000")
names(x_explain)[52:55] <- c("urine_albumin_is_gt_30","urine_albumin_is_gt_100","urine_albumin_is_get_300", "urine_albumin_is_gt_1000")

model <- xgboost::xgb.load(file.path(datafolder,"newdata/xgb_model_imp.json"))

preds <- predict(model,as.matrix(x_explain),outputmargin = TRUE)
treeShaps=predict(model,as.matrix(x_explain),predcontrib = TRUE)


feature_names <- names(x_train)
prediction_zero <- treeShaps[1,"BIAS"]

testObs_vec <- 1:100#c(5,12,27)
max_nvar <- 6

# Get the weighted matrix for each feature length
W_list <- SS_list <- list()
for(i in 2:max_nvar){
  X <- shapr:::feature_combinations(
    m = i,
    exact = TRUE,
    n_combinations = NULL,
    weight_zero_m = 10^6,
    group_num = NULL
  )

  ## Get feature matrix ---------
  SS_list[[i]] <- feature_matrix_cpp(
    features = X[["features"]],
    m = i
  )


  # Get weighted matrix ----------------
  W_list[[i]] <- shapr:::weight_matrix(
    X = X,
    normalize_W_weights = TRUE,
    is_groupwise = FALSE
  )

}


indexTab_list <- S_list <- list()
i0=0
for(testObs in testObs_vec)
{
  i0 <- i0+1
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


  ### MJ ####

  indexTab_list[[i0]] <- indexTab

  if(nVarRed<=max_nvar){

    S_tmp <- SS_list[[length(indexTab)]]
    colnames(S_tmp) <- feature_names[indexTab]

    S_list[[i0]] <- as.data.table(S_tmp[-1,])
  }
  print(i0)
}


S_full <- rbindlist(S_list,fill = TRUE,idcol="testObs")
#setnafill(S_full,cols=names(S_full),fill=-1)
feat_cols_S <- names(S_full)[-1]
S_full[,id_combination:=.GRP,by=feat_cols_S]
S_full[,row_id:=.I]
setkey(S_full,id_combination)
S_unique <- unique(S_full,by = "id_combination",cols=feat_cols_S)


x_train_red <- x_train[,..feat_cols_S]
x_explain_red <- x_explain[,..feat_cols_S]

ctree.mincriterion = 0.95
ctree.minsplit = 20
ctree.minbucket = 7
ctree.sample = TRUE
n_samples <- 1000


samps_list <- list()
i0 <- 0
for(i in 1:nrow(S_unique)){
  feat_vec0 <- unlist(S_unique[id_combination==i,-1])
  feat_vec <- feat_vec0[!is.na(feat_vec0)]
  keepcols <- names(feat_vec)

  features_here <- which(feat_vec==1)
  x_train_here <- x_train_red[,..keepcols]
  tree <- shapr:::create_ctree(features_here, x_train_here, ctree.mincriterion, ctree.minsplit, ctree.minbucket)
  print(i)

  these_testObs <- S_full[id_combination==i,testObs]

  x_explain_here <- x_explain_red[,..keepcols]


  for (j in seq_along(these_testObs)) {
    i0 <- i0 +1

    x_explain_here <- x_explain_red[these_testObs[j],..keepcols]

    samps_list[[i0]] <- shapr:::sample_ctree(
      tree,
      n_samples = n_samples,
      x_explain = x_explain_here,
      x_train = x_train_here,
      n_features = length(x_train_here),
      sample = ctree.sample
    )

    samps_list[[i0]][,testObs:=these_testObs[j]]
    samps_list[[i0]][,id_combination:=i]


    }


}

samps_dt <- rbindlist(samps_list,fill=TRUE)

dt_vS_list <- list()
zero_pred_dt <- data.table(id_combination=1,p_hat_1=prediction_zero)



dt_kshap_list <- list()
for(j in seq_along(testObs_vec)){

  if(length(indexTab_list[[j]])<=max_nvar){

    these_feature_names <- feature_names[indexTab_list[[j]]]
    samp_cols <- c("id_combination",these_feature_names)
    x_exp_cols <- feature_names[-indexTab_list[[j]]]

    aa <- samps_dt[testObs==j,..samp_cols]

    bb <- x_explain[j,..x_exp_cols]

    dt <- cbind(aa,bb)

    dt[, p_hat := predict_model(model, newdata = .SD), .SDcols = feature_names]
    dt[,w:=1/.N,by=id_combination]
    dt[,id:=1]

    dt_vS_list[[j]] <- shapr:::compute_MCint(dt)

    dt_vS_list[[j]] <- rbind(zero_pred_dt,dt_vS_list[[j]])


    ### COmpute shapley values ####
    this_W <- W_list[[length(indexTab_list[[j]])]]

    kshap <- t(this_W %*% as.matrix(dt_vS_list[[j]][, -"id_combination"]))
    dt_kshap <- data.table::as.data.table(kshap)
    colnames(dt_kshap) <- c("none", these_feature_names)


    print(j)
  dt_kshap_list[[j]] <- copy(dt_kshap)
  }
}


dt_kshap_all <- rbindlist(dt_kshap_list,fill=TRUE,idcol="testObs")



