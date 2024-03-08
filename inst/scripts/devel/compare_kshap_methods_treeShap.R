source("/nr/project/stat/BigInsight//Projects//Explanations//EffektivShapley//helpFunctions.R")

#library(treeshap)
library(xgboost)
library(data.table)
library(shapr)


datafolder <- "/nr/project/stat/BigInsight/Projects/Explanations/EffektivShapley/NHANES-data/"

x_explain <- fread(file.path(datafolder,"newdata/Xtest_imp.csv"))
y_test    <- fread(file.path(datafolder,"newdata/ytest.csv"))$V1
x_train   <- fread(file.path(datafolder,"newdata/Xtrain_imp.csv"))
y_train   <- fread(file.path(datafolder,"newdata/ytrain.csv"))$V1

names(x_train)[52:55]   <- c("urine_albumin_is_gt_30","urine_albumin_is_gt_100","urine_albumin_is_get_300", "urine_albumin_is_gt_1000")
names(x_explain)[52:55] <- c("urine_albumin_is_gt_30","urine_albumin_is_gt_100","urine_albumin_is_get_300", "urine_albumin_is_gt_1000")

model <- xgboost::xgb.load(file.path(datafolder,"newdata/xgb_model_imp.json"))

preds <- predict(model,as.matrix(x_explain),outputmargin = TRUE)

testObs <- c(5,12,27)

treeShaps=predict(model,as.matrix(x_explain),predcontrib = TRUE)

Kjersti_Kshap <- fread("inst/scripts/devel/Kjersti_shapley_NHANES.csv")

MJ_Kshap <- fread("inst/scripts/devel/MJ_Kshap_new.csv")

treeShaps[testObs,]
varval <- seq_len(ncol(Kjersti_Kshap)-1)

par(mfrow=c(3,1))
for(i in 1:3){
  plot(varval,Kjersti_Kshap[i,-1],type="l",ylim=c(-1.8,0.5))#range(treeShaps[testObs[i],-80]))
  points(varval,Kjersti_Kshap[i,-1])
  lines(varval,MJ_Kshap[i,-(1:3)],col=2)
  points(varval,MJ_Kshap[i,-(1:3)],col=2)
  lines(varval,treeShaps[testObs[i],-80],col=3)
  points(varval,treeShaps[testObs[i],-80],col=3)
}


### Would be interesting to do a timing conparison here, where I manually run
### say ctree with only the features that is needed, and then add the rest of the features (with little influence)
### and time that as well for a comparison
