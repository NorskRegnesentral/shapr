rm(list=ls())

library(MASS)
library(ranger)
library(shapr)
library(mvtnorm)
library(data.table)

nTrain <- 1000
nTest  <- 100

train6 <-  read.table("/nr/project_stat/BFFGB18/LIME/lime/R/train6.csv",sep=";",header=TRUE)
test6 <-   read.table("/nr/project_stat/BFFGB18/LIME/lime/R/test6.csv",sep=";",header=TRUE)

train6[,"default"] <- as.factor(train6[,"default"])
#2:6
model6 <- ranger(default ~ ., data = train6[,c(1,2,21:25,26:29)], num.trees = 500, num.threads = 6,
                 verbose = TRUE,
                 probability = TRUE,
                 importance = "impurity",
                 mtry = sqrt(28))
pred.rf <- predict(model6, data = test6)

corMat <- cor(train6[,c(2,21:25,26:29)])
set.seed(100)
simData <- mvrnorm(n = nTrain+nTest, mu=rep(0,10), Sigma=corMat)


pred <-  predict(model6, data = simData)$predictions[,2]
default <- array(0,dim(simData)[1])
default[which(pred <= 0.45)] <- 0
default[which(pred > 0.45)]  <- sample(c(0,1), replace=TRUE, size=length(which(pred>0.45)))
default[which(pred > 0.8)]   <- 1

simTrain <- as.data.frame(cbind(default[1:nTrain],simData[1:nTrain,]))
colnames(simTrain)[1] <- "default"
simTrain[,1] <- as.factor(simTrain[,1])

modFull <- ranger(default ~ ., data = simTrain, num.trees = 200, num.threads = 6,
                  verbose = TRUE,
                  probability = TRUE,
                  importance = "impurity",
                  mtry = sqrt(10))
trainData <- simTrain[,-1]
indices <- 1:10
newData2 <- as.data.frame(cbind(default[(nTrain+1):(nTrain+nTest)],simData[(nTrain+1):(nTrain+nTest),indices]))
colnames(newData2)[1] <- "default"
pred.rf <- predict(modFull, data = newData2)
testData <- newData2[,-1]

##################################################################################################################
meanDefaultProb <- length(simTrain[which(simTrain[,1]==1),1])/nTrain
#nSamples <- 1000

#KernelShap with conditional Gaussian TRUE
#set.seed(200)
#mu     <- rep(0,10)
#covMat <- corMat
#kernelShapValsGaussTrue <- kernelShap.Gauss(m=10,trainData=trainData, testData=testData, nSamples=nSamples,meanDefaultProb=meanDefaultProb,eksakt=TRUE,mu=mu,covMat=covMat,model=modFull,logit=FALSE)


##################### MJ

### Redefining stuff

m <- ncol(trainData)
Xtrain <- trainData
Xtest <- testData
model <- modFull
pred_zero <- meanDefaultProb
w_threshold = 1 # For a fairer comparison, all models use the same number of samples (n_threshold)
n_threshold = 10^3
mu <- rep(0,m)
Sigma <- corMat
#### Pre computation before kernel shap --------------------------------
l <- prepare_kernelShap(
    m = m,
    Xtrain = Xtrain,
    Xtest = Xtest,
    exact = TRUE,
    nrows = 1e4,
    distance_metric = "Mahalanobis_scaled",
    gaussian_sample = T)


Shapley.Gauss = compute_kernelShap(model = model,
                                   l,
                                   sigma = 0, # Ignored when gaussian_sample==T
                                   w_threshold = w_threshold,
                                   n_threshold = n_threshold,
                                   verbose = FALSE,
                                   gaussian_sample = TRUE,
                                   pred_zero=pred_zero,
                                   kernel_metric = "Gaussian",
                                   mu = mu,
                                   Sigma = Sigma)

Shapley.Gauss$other_objects$DT.mat <- matrix(Shapley.Gauss$other_objects$DT$k,ncol=2^m,nrow=nrow(Xtest),byrow = T)


#### HERE we do the various ML stuff

dat = cbind(Xtrain,pred=pred_vector(model,Xtrain))
#colnames(dat) <- c(paste0("X",1:m),"pred")
submodel.list <- list()

#k.mat.ML <- matrix(NA,ncol=2^m,nrow=nrow(Xtest))

DT.ML.list <- list()
DT.ML.list[[1]] <- data.table(wcomb=1,k=pred_zero,id=1:nrow(Xtest))
DT.ML.list[[2^m]] <- data.table(wcomb=2^m,k=pred_vector(model,Xtest),id=1:nrow(Xtest))

for (i in 2:(2^m-1)){
    formula <- paste0("pred~",paste0(names(dat)[which(l$S[i,]==1)],collapse="+"))

    submodel.list[[i]] = ranger::ranger(
        formula = as.formula(formula),
        data = dat,
        num.trees = 50,
        num.threads = 12,
        verbose = TRUE,
        importance = "impurity")

    DT.ML.list[[i]] <- data.table(wcomb=i,k=pred_vector(model = submodel.list[[i]],data = Xtest),id=1:nrow(Xtest))
    print(i/(2^m-1))
}

DT.ML <- rbindlist(DT.ML.list)
setkey(DT.ML,"id")

DT.ML.mat <- matrix(DT.ML$k,ncol=2^m,nrow=nrow(Xtest),byrow = T)

Kshap.ML <- matrix(0, nrow = nrow(l$Xtest), ncol = nrow(l$W))
for (i in l$Xtest[, .I]) {
    Kshap.ML[i, ] = l$W %*% DT.ML[id == i, k]
}

head(Kshap.ML)


setnames(Shapley.Gauss$other_objects$DT,"k","k.Gauss")
setnames(DT.ML,"k","k.ML")


DT.merged <- merge(Shapley.Gauss$other_objects$DT,DT.ML,by=c("wcomb","id"))

lX.new <- copy(l$X)
setnames(lX.new,"ID","wcomb")
lX.new[,weight:=NULL]
DT.merged <- merge(DT.merged,lX.new,by = "wcomb")

DT.merged[,abserror.ML:= abs(k.Gauss-k.ML)]

DT.merged[,mean(abserror.ML),by="wcomb"]
DT.merged[,mean(abserror.ML),by="nfeatures"]

#colMeans(abs(Shapley.Gauss$other_objects$DT.mat-DT.ML.mat))




########### Running a comparison with the empirical version with sigma = 0.1 everywhere #####

l.2 <- prepare_kernelShap(
    m = m,
    Xtrain = Xtrain,
    Xtest = Xtest,
    exact = TRUE,
    nrows = 1e4,
    distance_metric = "Mahalanobis_scaled",
    gaussian_sample = F)


Shapley.sigma.01 = compute_kernelShap(model = model,
                                      l.2,
                                      sigma = 0.1, # Ignored when gaussian_sample==T
                                      w_threshold = w_threshold,
                                      n_threshold = n_threshold,
                                      verbose = FALSE,
                                      gaussian_sample = F,
                                      pred_zero=pred_zero,
                                      kernel_metric = "Gaussian",
                                      mu = mu,
                                      Sigma = Sigma)

setnames(Shapley.sigma.01$other_objects$DT,"k","k.emp")
DT.merged <- merge(DT.merged,Shapley.sigma.01$other_objects$DT,by=c("wcomb","id"))

DT.merged[,abserror.emp:= abs(k.Gauss-k.emp)]

DT.merged[,.(error.ML=mean(abserror.ML),error.emp=mean(abserror.emp)),by="wcomb"]
DT.merged[,.(error.ML=mean(abserror.ML),error.emp=mean(abserror.emp)),by="nfeatures"]

#### runing independece

Shapley.indep = compute_kernelShap(model = model,
                                   l.2,
                                   sigma = 0.1, # Ignored when gaussian_sample==T
                                   w_threshold = w_threshold,
                                   n_threshold = n_threshold,
                                   verbose = FALSE,
                                   gaussian_sample = F,
                                   pred_zero=pred_zero,
                                   kernel_metric = "independence",
                                   mu = mu,
                                   Sigma = Sigma)


setnames(Shapley.indep$other_objects$DT,"k","k.indep")
DT.merged <- merge(DT.merged,Shapley.indep$other_objects$DT,by=c("wcomb","id"))

DT.merged[,abserror.indep:= abs(k.Gauss-k.indep)]



#### HERE we do the various gam stuff


library(h2o)
h2o.init(nthreads = 20)

dat.dt <- as.data.table(dat)
dat.dt[,const := 1]

Xtest.dt <- as.data.table(Xtest)
Xtest.dt[,const := 1]
Xtest.dt[,pred:=pred_vector(model,Xtest)]

dat.h2o <- as.h2o(dat.dt)
Xtest.h2o <- as.h2o(Xtest.dt)



DT.nnet.list <- list()
DT.nnet.list[[1]] <- data.table(wcomb=1,k=pred_zero,id=1:nrow(Xtest))
DT.nnet.list[[2^m]] <- data.table(wcomb=2^m,k=pred_vector(model,Xtest),id=1:nrow(Xtest))

for (i in 2:(2^m-1)){
    these.x <- c(names(dat)[which(l$S[i,]==1)],"const")

    submodel <- h2o::h2o.deeplearning(x=these.x,
                                      y="pred",
                                      training_frame = dat.h2o,
                                      #                                      validation_frame = XYtest.h2o, # For initial testing just now
                                      nfolds = 5,
                                      activation = "MaxoutWithDropout",
                                      hidden = c(100,100),
                                      seed = 1234,
                                      input_dropout_ratio = 0.2,
                                      hidden_dropout_ratio = c(0.1,0.1),
                                      l2 = 0.00001,
                                      distribution = "gaussian",
                                      ignore_const_cols = F)

    DT.nnet.list[[i]] <- data.table(wcomb=i,k=as.vector(predict(object = submodel,newdata = Xtest.h2o)),id=1:nrow(Xtest))
    print(i/(2^m-1))
}

DT.nnet <- rbindlist(DT.nnet.list)
setkey(DT.nnet,"id")

DT.nnet.mat <- matrix(DT.nnet$k,ncol=2^m,nrow=nrow(Xtest),byrow = T)

Kshap.nnet <- matrix(0, nrow = nrow(l$Xtest), ncol = nrow(l$W))
for (i in l$Xtest[, .I]) {
    Kshap.nnet[i, ] = l$W %*% DT.nnet[id == i, k]
}

head(Kshap.nnet)


setnames(DT.nnet,"k","k.nnet")

DT.merged <- merge(DT.merged,DT.nnet,by=c("wcomb","id"))

DT.merged[,abserror.nnet:= abs(k.Gauss-k.nnet)]

DT.merged[,.(error.ML=mean(abserror.ML),error.emp=mean(abserror.emp),error.nnet=mean(abserror.nnet),error.indep=mean(abserror.indep)),by="wcomb"]
DT.merged[,.(error.ML=mean(abserror.ML),error.emp=mean(abserror.emp),error.nnet=mean(abserror.nnet),error.indep=mean(abserror.indep)),by="nfeatures"]


######


colMeans(abs(Shapley.Gauss$Kshap-Kshap.nnet))
colMeans(abs(Shapley.Gauss$Kshap-Kshap.ML))
colMeans(abs(Shapley.Gauss$Kshap-Shapley.sigma.01$Kshap))

