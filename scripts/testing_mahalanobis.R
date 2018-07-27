
### Camilla's 15-dimensional example with corrected Mahalanobis distance and

rm(list = ls())

library(shapr)
library(data.table)
library(mvtnorm)
library(condMVNorm)

library(clusterGeneration)
library(ranger)
library(hmeasure)

######################################################
# First: create dataset with M=15 features and a binary reponse.
M = 15
set.seed(123)
mu.15 = rep(0,M)
corMat.15 = cov2cor(genPositiveDefMat(M,'eigen')$Sigma) # A correlation matrix
nTrain.15 <- 10000
nTest.15  <- 10
simData.15 <- mvrnorm(n = nTrain.15+nTest.15, mu=mu.15, Sigma=corMat.15)
# Create nonlinear response:
if(M==15){
    resp.15 <- 0.5*rowSums(simData.15[,1:3]) + 1*rowSums(I(simData.15[,4:8]< -0.2)) +2*I(simData.15[,9]>0.3)*I(simData.15[,10]>0.3)+rowSums(simData.15[,11:15]^2)
    response.15 = resp.15 > 7.5
}
if (M==5){
    resp.15 <- 0.5*rowSums(simData.15[,1:3]) + 1*rowSums(I(simData.15[,4:5]< -0.2))
    response.15 = resp.15 > 0.8
}

simData.15 = as.data.frame(cbind(response.15,simData.15))
colnames(simData.15) = c('y',paste('X',1:M,sep=''))
trainData.15 = simData.15[1:nTrain.15,]
testData.15 = simData.15[(nTrain.15+1) :(nTrain.15+nTest.15),]

# Fit model
model.15 <- ranger(y ~ ., data = trainData.15, num.trees = 200, num.threads = 6,
                   verbose = TRUE,probability = T,	importance = "impurity",mtry = sqrt(28))
# Use on test data. AUC=0.97
pred.15 <- predict(model.15, data = testData.15) # Use model on test data
results.15 <- HMeasure(testData.15[,1],pred.15$predictions[,2],threshold=0.15)
summary(results.15,show.all=TRUE)

Xtrain=trainData.15[,-1] # Why remove the first column? It is not the response.
Xtest=testData.15[,-1] # Why remove the first column? It is not the response.


m <- ncol(Xtrain)
#### Pre computation before kernel shap --------------------------------
l <- prepare_kernelShap(
    m = m,
    Xtrain = Xtrain,
    Xtest = Xtest,
    exact = TRUE,
    nrows = 1e2,
    scale = T,
    distance_metric = "Mahalanobis_scaled")


# Settings
model = model.15
w_threshold = 1 # For a fairer comparison, all models use the same number of samples (n_threshold)
n_threshold = 10^2
verbose = FALSE
gaussian_sample = FALSE
pred_zero = mean(response.15)
kernel_metric = "Gaussian_old"

#### Computing the various Shapley approximations --------


sigma = .3

Shapley.approx.test = compute_kernelShap(model = model,
                                         l = l,
                                         sigma = sigma,
                                         w_threshold = w_threshold,
                                         n_threshold = n_threshold,
                                         verbose = verbose,
                                         gaussian_sample = gaussian_sample,
                                         pred_zero=pred_zero,
                                         kernel_metric = kernel_metric)

Shapley.gaussian = compute_kernelShap(model = model,
                                      l = l,
                                      sigma = sigma,
                                      w_threshold = w_threshold,
                                      n_threshold = n_threshold,
                                      verbose = verbose,
                                      gaussian_sample = TRUE,
                                      pred_zero=pred_zero,
                                      kernel_metric = kernel_metric)

DT.approx <- rbindlist(Shapley.approx.test$other_object$ll)
DT.gaussian <- rbindlist(Shapley.gaussian$other_object$ll)
DT <- merge(DT.approx,DT.gaussian,by=c("wcomb","id"),all=T)
setnames(DT,"k.x","p.approx")
setnames(DT,"k.y","p.gaussian")

helper <- copy(l$X)
helper[,wcomb:=ID]
helper[,ID:= NULL]
helper[,features:= NULL]
helper[,N:= NULL]
helper[,weight:= NULL]

DT <- merge(DT,helper,by="wcomb")
DT[,diff:=p.approx-p.gaussian]
DT[,absdiff:=abs(diff)]
DT[,mean(absdiff)] # Mean
DT.summary <- DT[,.(mean=mean(absdiff),sd=sd(absdiff)),by=nfeatures] # Summary per nfeatures

plot(DT.summary$nfeatures,DT.summary$mean,ylim=c(0,max(DT.summary$mean+DT.summary$sd)))
arrows(DT.summary$nfeatures,DT.summary$mean-DT.summary$sd,
       DT.summary$nfeatures,DT.summary$mean+DT.summary$sd,code=3,angle=90,length=0.05)

