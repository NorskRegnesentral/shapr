source("/nr/project/stat/BigInsight//Projects//Explanations//EffektivShapley//helpFunctions.R")
library(doParallel)

cores=detectCores()
#cl <- makeCluster(cores[1]-1) #not to overload your computer
cl <- makeCluster(18)
registerDoParallel(cl)

if(!require("treeshap")){
  install.packages("treeshap")
  library(treeshap)
}

########################################################################################################################################################
#
# NHANES
#
#########################################################################################################################################################
library(xgboost)
library(data.table)


plotFig <- 0

datafolder <- "/nr/project/stat/BigInsight/Projects/Explanations/EffektivShapley/NHANES-data/"

x_explain <- fread(file.path(datafolder,"newdata/Xtest_imp.csv"))
y_test    <- fread(file.path(datafolder,"newdata/ytest.csv"))$V1
x_train   <- fread(file.path(datafolder,"newdata/Xtrain_imp.csv"))
y_train   <- fread(file.path(datafolder,"newdata/ytrain.csv"))$V1

names(x_train)[52:55]   <- c("urine_albumin_is_gt_30","urine_albumin_is_gt_100","urine_albumin_is_get_300", "urine_albumin_is_gt_1000")
names(x_explain)[52:55] <- c("urine_albumin_is_gt_30","urine_albumin_is_gt_100","urine_albumin_is_get_300", "urine_albumin_is_gt_1000")

model <- xgboost::xgb.load(file.path(datafolder,"newdata/xgb_model_imp.json"))

preds <- log(predict(model,as.matrix(x_explain)))
#ind   <- rev(order(preds))[1:2]
#ind   <- rev(order(preds))[9:10]
#x_explain <- x_explain[ind,]

treeShaps=predict(model,as.matrix(x_explain),predcontrib = TRUE)
#rowSums(treeShaps)

nSmall <- 0
numVarArray <- array(0,dim(treeShaps)[1])
#for(testObs in 3281:dim(treeShaps)[1])
for(testObs in 1:dim(treeShaps)[1])
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
  for(i in 1:nVarRed)
    indFull <- c(indFull,as.numeric(which(abs(corMat[ind[i],])>0.5)))

  indexTab  <- unique(indFull)
  nVarRed   <- length(indexTab)
  indep.ind <- which(is.na(match(1:numVar,indexTab)))
  avPred <- treeShaps[testObs,"BIAS"]
  numVarArray[testObs] <- nVarRed

  if(nVarRed < 10 && nVarRed > 1)
  {
    nSmall <- nSmall + 1
    print(nSmall)
    start_time <- Sys.time()
    kernelShapsRed <- computeKernelSHAP(nVarRed, numVar, indexTab, indep.ind, x_train, x_explain, model, testObs, avPred, method="ctree")
    end_time <- Sys.time()
    #print(end_time-start_time)
    kernelShapNew <- array(0,79)
    kernelShapNew[indexTab] <- kernelShapsRed[-1]
    if(nSmall==1)
      write(kernelShapNew,file="/nr/user/kjersti/SHAPLEY_NHANES.csv",append=FALSE, sep=";", ncolumns=79)
    else
      write(kernelShapNew,file="/nr/user/kjersti/SHAPLEY_NHANES.csv",append=TRUE, sep=";", ncolumns=79)

    if(plotFig)
    {
      rbind(treeShaps[testObs,indexTab],kernelShapsRed[-1])
      plot.ts(cbind(treeShaps[testObs,-80],kernelShapNew),plot.type="single")
      lines(kernelShapNew,col=3)
    }
  }
}
###########################################################################################################################################################################
computeKernelSHAP <- function(nVarRed, numVaR, indexTab, indep.ind, trainData, testData, model, indTest, avPred, method="Gauss")
{
  tmp       <- makeXmatrix(m=nVarRed, eksakt=TRUE,nRows=100)
  weightMat <- tmp$weightMat
  xMat      <- tmp$xMat
  xMat0     <- xMat

  res <- foreach(i = 1:(dim(xMat)[1]-2), .packages=c("xgboost", "party"), .export=c("testFuncC", "computeNonlinearNew")) %dopar% testFuncC(i, indexTab, indep.ind, xMat0, model, testData, indTest, nSamples=10000,trainData)
  vGauss <- as.numeric(res)

  vGauss2 <- array(0,dim(xMat0)[1])
  vGauss2[1] <- avPred
  vGauss2[2:(dim(xMat0)[1]-1)] <- vGauss[1:(dim(xMat0)[1]-2)]
  vGauss2[dim(xMat0)[1]] <- log(predict(model,as.matrix(testData)))[indTest]
  kernelShapGauss <- weightMat%*%vGauss2

  kernelShapGauss
}

##########################################################################################################################################################################################
testFuncC <- function(i, indexTab, indep.ind, xMat0, model, Xtest, indTest, nSamples,trainData)
{
  xMat <- xMat0[-1,-1]
  given.ind <- indexTab[which(xMat[i,]==1)]
  dep.ind   <- c(indexTab[which(xMat[i,]==0)],indep.ind)

  nVar <- length(c(dep.ind,given.ind))

  sim1      <- matrix(0,ncol=nVar,nrow=nSamples)

  names <- names(trainData)

  #Determine v(i,k) for this i and test observation indTest
  x0 <- as.numeric(Xtest[indTest,])
  x1 <- x0[given.ind]

  set.seed(200)
  #Fit ctree
  y <- paste(names[dep.ind],collapse = "+")
  x <- paste(names[given.ind],collapse = "+")
  fma <- as.formula(paste(y,x,sep=" ~ "))
  ctreeMod <- ctree(fma,data=trainData)

  # Simulate
  fit.nodes <- party::where(object = ctreeMod)
  # newdata must be data.frame + have the same colnames as x
  xp <- Xtest[indTest,..given.ind]
  pred.nodes <- party::where(object = ctreeMod, newdata = xp)
  rowno <- seq_len(nrow(trainData))
  newrowno <- sample(rowno[fit.nodes == pred.nodes], nSamples, replace = TRUE)
  sim1[,dep.ind] <- as.matrix(trainData[newrowno,..dep.ind])
  vCtree         <- computeNonlinearNew(sim1[,dep.ind],x1,dep.ind,given.ind,model,names)
  return(vCtree)
}
####################################################################################################################################################################################################
computeNonlinearNew <- function(simData,condVals,dep.ind,given.ind,model,names)
{
  if(length(dep.ind)==1)
    nSamples <- length(simData)
  else
    nSamples <- dim(simData)[1]
  yMat <- matrix(0,nrow=nSamples,ncol=length(c(dep.ind,given.ind)))
  yMat[1:nSamples,dep.ind]    <- simData
  yMat[1:nSamples, given.ind] <- rep(condVals, each = nSamples)
  trainData <- as.data.frame(yMat)
  colnames(trainData)=names
  predVals  <- log(predict(model,as.matrix(trainData)))
  rm(yMat)
  rm(trainData)
  mean(predVals)
}
