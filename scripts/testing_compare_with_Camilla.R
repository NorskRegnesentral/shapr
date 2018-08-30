rm(list = ls())

library(shapr)
library(data.table)
library(mvtnorm)
library(condMVNorm)

library(clusterGeneration)
library(ranger)
library(hmeasure)
library(Matrix)

######################################################
M =
set.seed(123)
mu.15 = rep(0,M)
corMat.15 = cov2cor(genPositiveDefMat(M,'eigen')$Sigma) # A correlation matrix
nTrain.15 <- 200
nTest.15  <- 5
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

trainData.15=trainData.15[,-1]
testData.15=testData.15[,-1]

#################################################################################################

# Investigate the distance to the closest points

# Create the X-matrix of all possible coalations.
xMat.15 <- NULL
for (i in 0:M)
{
    # compute all possible combinations of i features
    coalitions <- combn(M, i) # is ix(no of ways to draw i indices)
    tmpMat <- matrix(0,ncol=M,nrow=ncol(coalitions))
    for(j in 1:ncol(coalitions))
    {
        tmpMat[j,coalitions[,j]] <- 1
    }
    xMat.15 <- rbind(xMat.15,tmpMat)
}
#Legger til col med enere (interceptet)
xMat.15 <- cbind(rep(1,2^M),xMat.15) # The 2^Mx(M+1) X-matrix


nSamples.15 <- 10000
sigma.15  <- 0.1
mu.hat.15  <- apply(trainData.15,2,mean)
covMat.hat.15 <- var(trainData.15)
probTab.15   <- matrix(0,ncol=4,nrow=2^M) # 2^M x 4
weightTab.15 <- matrix(0,ncol=10,nrow=2^M) # 2^M x 10

#Maa skalere data som skal benyttes i vektingen(Skalerer mhp sd til treningsdataene)
sdTrainData.15 <- apply(trainData.15,2,sd)
trainDataS.15 <- trainData.15
for(i in 1:dim(trainDataS.15)[2])
    trainDataS.15[,i] <- trainData.15[,i]/sdTrainData.15[i]
testDataS.15 <- testData.15
for(i in 1:dim(testDataS.15)[2])
    testDataS.15[,i] <- testData.15[,i]/sdTrainData.15[i]

# For test sample k: find Mahalanobis distance to training samples, for different combinations of included variables
k <- 2
for(i in 2:(2^M-1)) # From row 2 to 2^M-1 (skip when there are only zeros, or when all variables is included)
{
    print(i)
    inds  <- which(xMat.15[i,-1]==1) # Ikke ta med intercept-ledd
    dep.inds <- which(xMat.15[i,-1]==0)
    # Find Mahalanobis distance to all other individuals wrt these variables
    distK <- mahalanobis(as.matrix(trainDataS.15[,inds]),as.vector(unlist(testDataS.15[k,inds])), var(trainDataS.15[,inds]))/length(inds)
    indW <- order(distK) # Order distances
    weightTab.15[i,] <- distK[indW[1:10]] # Only include the 10 closest observations
}
nVar.15 <- apply(xMat.15[2:(2^M-1),-1],1,sum) # The number of included variables in each combination in the xMat
# For each possible combination of variables in S for the given size, the distance to the closest point in the training data set is shown
# Note: we do not include row 2^M, as this is where |S| = 10 and there would only be one single closest point.
par(mfrow=c(1,1))
plot(nVar.15,weightTab.15[2:(2^M-1),1],xlab='|S|, number of included features',ylab='Normalized Mahalanobis distance',main=paste('Distances to 10 closest points in training data, for test sample k=10,\n',nTrain.15,' training samples')) # Plot distance as a function of included variables

# Plot all 10 closest points
matplot(nVar.15,weightTab.15[2:(2^M-1),1:10],pch=1,xlab='|S|, number of included features',ylab='Normalized Mahalanobis distance',main=paste('Distances to closest points in training data, for test sample k=10,\n',nTrain.15,' training samples'),col=1:M) # Plot distance as a function of included variables
legend(1,0.22,sapply(1:M,FUN=function(s) paste(s, 'th closest')),y.intersp=0.5,bty='n',cex=1,pch=1,col=1:M)


#########################################################################################

# Compare different ways of estimating the coalitions/conditional expectations v(S) = E[f(X)|X_S]


for(i in 2:(2^M-1))
{
    print(i)
    inds     <- which(xMat.15[i,-1]==1) # Do not include intercept term
    dep.inds <- which(xMat.15[i,-1]==0)

    distK <- mahalanobis(as.matrix(trainDataS.15[,inds]),as.vector(unlist(testDataS.15[k,inds])), var(trainDataS.15[,inds]))/length(inds)
    obsWeights <- sqrt(exp(-distK/(2*sigma.15^2)))
    indW <- rev(order(obsWeights))
    weightTab.15[i,] <- obsWeights[indW[1:10]]

    nSamples.15 <- 10000
    yMat <- trainData.15[sample(dim(trainData.15)[1],nSamples.15,replace=TRUE,prob=obsWeights),]
    empDataOrig <- yMat[,dep.inds]

    yMat <- trainData.15[indW[1:100],]
    empData <- yMat[,dep.inds]

    ret <- condMVN(X.given = as.numeric(testData.15[k,inds]), mean = mu.hat.15, sigma = covMat.hat.15, dependent.ind = dep.inds, given.ind = inds)
    sigma = as.matrix(nearPD(ret$condVar, corr = FALSE, keepDiag = FALSE)$mat)
    normData <- rmvnorm(nSamples.15, mean = ret$condMean, sigma = sigma)

    yMat <- as.data.frame(matrix(0,nrow=nSamples.15,ncol=dim(testData.15)[2]))
    yMat[1:nSamples.15,dep.inds] <- normData
    yMat[1:nSamples.15,inds]     <- testData.15[k,inds]
    colnames(yMat) = colnames(testData.15)
    probTab.15[i,1] <-  mean(predict(model.15,yMat)$predictions[,2])

    nSamples.15 <- dim(as.matrix(empDataOrig))[1]
    yMat <- as.data.frame(matrix(0,nrow=nSamples.15,ncol=dim(testData.15)[2]))
    yMat[1:nSamples.15,dep.inds] <- empDataOrig
    yMat[1:nSamples.15,inds]     <- testData.15[k,inds]
    colnames(yMat) = colnames(testData.15)
    probTab.15[i,2] <- mean(predict(model.15,yMat)$predictions[,2])

    nSamples.15 <- dim(as.matrix(empData))[1]
    yMat <- as.data.frame(matrix(0,nrow=nSamples.15,ncol=dim(testData.15)[2]))
    yMat[1:nSamples.15,dep.inds] <- empData
    yMat[1:nSamples.15,inds]     <- testData.15[k,inds]
    colnames(yMat) = colnames(testData.15)
    probTab.15[i,3] <- mean(predict(model.15,yMat)$predictions[,2])

    w <- obsWeights[indW[1:100]]/sum(obsWeights[indW[1:100]])
    probTab.15[i,4] <- weighted.mean(predict(model.15,yMat)$predictions[,2],w)

}




##################################

m <- ncol(trainData.15)
#### Pre computation before kernel shap --------------------------------
l <- prepare_kernelShap(
    m = m,
    Xtrain = trainData.15,
    Xtest = testData.15,
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


sigma = .1

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

distK <- as.vector(mahalanobis(as.matrix(trainDataS.15[,inds]),as.vector(unlist(testDataS.15[k,inds])), var(trainDataS.15[,inds])))/length(inds)^2
distK-l$D[,2,31]
obsWeights <- sqrt(exp(-distK/(2*sigma.15^2)))
Shapley.approx.test$other_objects$W_kernel[,2,31]-obsWeights

Shapley.approx.test$other_objects$ll[[2]]$k
Shapley.gaussian$other_objects$ll[[2]]$k

probTab.15



bb=gen_Mahlanobis_dist_cpp(X$features,head(as.matrix(Xtrain)),as.matrix(Xtest[4,]),mcov=cov(Xtrain),T)
mahalanobis(head(as.matrix(Xtrain))[,c(1,3)],as.matrix(Xtest)[4,c(1,3)],cov(Xtrain)[c(1,3),c(1,3)])/2^2



