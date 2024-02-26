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
#preds <- log(predict(model,as.matrix(x_explain)))

#ind   <- rev(order(preds))[1:2]
#ind   <- rev(order(preds))[9:10]
#x_explain <- x_explain[ind,]


treeShaps=predict(model,as.matrix(x_explain),predcontrib = TRUE)


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


