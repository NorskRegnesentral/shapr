#source("/nr/project/stat/BigInsight//Projects//Explanations//EffektivShapley//helpFunctions.R")

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

reduced_Kshap <- function(model,x_train,x_explain,prediction_zero,indexTab,...){
  model_shapr <- model
  class(model_shapr) <- "blabla"

  tmp_data <- as.matrix(x_explain)

  predict_model_shapr <<- function(model_shapr,newdata){

    newdata_full <- matrix(rep(tmp_data,each=dim(newdata)[1]),nrow=dim(newdata)[1])
    newdata_full[,indexTab] <- as.matrix(newdata)
    class(model_shapr) = "xgb.Booster"

    xgboost:::predict.xgb.Booster(model_shapr,newdata_full,outputmargin =TRUE)
  }

  #aa=predict_model_shapr(model_shapr,x_train[,.SD,.SDcols = indexTab])

  #tmp_data <- as.matrix(x_train[,colMeans(.SD)])
  #bb=predict_model_shapr(model_shapr,x_train[,.SD,.SDcols = indexTab])


  expl <- shapr::explain(model = model_shapr,
                         x_train = x_train[,.SD,.SDcols = indexTab],
                         x_explain = x_explain[,.SD,.SDcols = indexTab],
                         approach = "ctree",
                         prediction_zero = avPred,
                         predict_model = predict_model_shapr,
                         ...)

  keep <- copy(expl$shapley_values)
  keep[,testid:=testObs]
  keep[,nVar:=length(indexTab)]

  ret <- x_explain[,]*0
  delcols <- names(keep)[names(keep)%in% names(ret)]
  ret[,(delcols):=NULL]
  ret[,testid:=testObs]
  shapr_tab <- merge(ret,keep,by="testid",all.y=TRUE)
  setcolorder(shapr_tab,c("testid","nVar","none",names(x_train)))

  return(shapr_tab)
}


#### testing here

# TODO:
# 1. Check that the order of the features is correct such that the correct id combinations are used in the shapley value
# computations later on.
# 2. Control the computed shapley values with what you get with the standard approach in th nhanes-shapr-reduced setting e.g. all but 2, 3 or 4 features
# to zero. should be about the same if the procedure works.


n_features <- ncol(x_train)
abscorMat <- abs(cor(x_train))
max_comp_features <- 8
max_cutoff_features <- 40
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


X_full <- shapr:::feature_combinations(
  m = max_comp_features,
  exact = TRUE,
  n_combinations = NULL,
  weight_zero_m = 10^6,
  group_num = NULL
)

## Get feature matrix ---------
S_full <- feature_matrix_cpp(
  features = X_full[["features"]],
  m = max_comp_features
)

S_full <- cbind(S_full,matrix(S_replacement_for_remaining_cutoff_feats,ncol=max_cutoff_features-max_comp_features,nrow=nrow(S_full)))

S_full_dt <- as.data.table(S_full)
S_full_dt[,id_combination_full:=.I]
S_full_dt[,computed_in_loop:=as.numeric(NA)]

S_full_dt[.N,computed_in_loop:=0] # We don't need to compute the full model
S_full_dt[.N, p_hat_1:=preds[testObs]] # Already computed

# Not sure if this should be included here or not, as the smallest model now condiitonal on some features
# so is not really the same as the zero model
# I think it is best to model this every time, yes


#S_full_dt[1,computed_in_loop:=0] # We don't need to compute the zero model
#S_full_dt[1, p_hat_1:=prediction_zero]



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


