
### Camilla's 15-dimensional example with corrected Mahalanobis distance and

rm(list = ls())

library(shapr)
library(data.table)
library(mvtnorm)
library(condMVNorm)

library(clusterGeneration)
library(ranger)
library(hmeasure)

# Translated matlab function from here: https://stats.stackexchange.com/questions/124538/how-to-generate-a-large-full-rank-random-correlation-matrix-with-some-strong-cor
vineBeta <- function(d, betaparam){

    P = diag(d)*0
    S = diag(d)

    for (k in 1:d-1){
        for (i in (k+1):d){
            P[k,i] = rbeta(n = 1,shape1 = betaparam,shape2 = betaparam)
            P[k,i] = (P[k,i]-0.5)*2;     #%// linearly shifting to [-1, 1]
            p = P[k,i]
            for (l in (k-1):1) {  # converting partial correlation to raw correlation
                p = p * sqrt((1-P[l,i]^2)%*%(1-P[l,k]^2)) + P[l,i]%*%P[l,k]
            }
            S[k,i] = p
            S[i,k] = p
        }
    }
    permutation = sample(1:d)
    S = S[permutation, permutation]
    return(S)
}


######################################################
# First: create dataset with M=15 features and a binary reponse.
M = 10
set.seed(123)
mu.15 = rep(0,M)
#corMat.15 = cov2cor(genPositiveDefMat(M,'eigen')$Sigma*.2) # A correlation matrix
#corMat.15 = cov2cor(genPositiveDefMat(M,'onion',eta=0.0001)$Sigma) # A correlation matrix
#corMat.15 = rcorrmatrix(M,0.0000000000015)
corMat.15 <- vineBeta(M,1) # Generating high-correlation matrix

nTrain.15 <- 10000
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
if (M==10){
    resp.15 <- 0.5*rowSums(simData.15[,1:3]) + 1*rowSums(I(simData.15[,4:8]< -0.2)) +2*I(simData.15[,9]>0.3)*I(simData.15[,10]>0.3)
    response.15 = resp.15 > 2
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
    nrows = 1e5,
    scale = T,
    distance_metric = "Mahalanobis_scaled")


# Settings
model = model.15
w_threshold = 1 # For a fairer comparison, all models use the same number of samples (n_threshold)
n_threshold_gaussian <- 1000 # i.e. how many conditional samples we use in the Gaussian approach (the truth)
verbose = FALSE
gaussian_sample = FALSE
pred_zero = mean(response.15)
kernel_metric = "Gaussian"

#### Computing the various Shapley approximations --------


sigma.vec <- 100#c(0.1,0.3,0.5,0.7,1,2,100)
n_threshold.vec = 10^3#10^2*c(0.1,0.5,1,2,5,10)

DT.summary.list <- list()
listnum <- 0
for (j in 1:length(n_threshold.vec)){
    for (i in 1:length(sigma.vec)){

        sigma = sigma.vec[i]
        n_threshold = n_threshold.vec[j]
        listnum <- listnum + 1


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
                                              n_threshold = n_threshold_gaussian,
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
        print(sigma)
        print(n_threshold)
        print(DT[,mean(absdiff)]) # Mean
        DT.summary <- DT[,.(mean=mean(absdiff),sd=sd(absdiff)),by=nfeatures] # Summary per nfeatures
        DT.summary[,sigma:=sigma]
        DT.summary[,K:=n_threshold]

        DT.summary.list[[listnum]] <- copy(DT.summary)

        plot(DT.summary$nfeatures,DT.summary$mean,ylim=c(0,max(DT.summary$mean+DT.summary$sd)))
        arrows(DT.summary$nfeatures,DT.summary$mean-DT.summary$sd,
               DT.summary$nfeatures,DT.summary$mean+DT.summary$sd,code=3,angle=90,length=0.05)

        print(listnum)
    }
}

DT.summary.all <- rbindlist(DT.summary.list)
DT.summary.all[,mean(mean),by=c("sigma","K")]

rbPal <- colorRampPalette(c('grey','red'))
col.vec <- rbPal(7)[as.numeric(cut(1:7,breaks = 7))]


n_feat <- max(DT.summary.all$nfeatures)
plot(0,0,type = "n",xlim=c(0,n_feat),ylim=c(0,max(DT.summary.all$mean)))
for (i in 1:listnum){
    K <- DT.summary.list[[i]]$K[1]
    sigma <- DT.summary.list[[i]]$sigma[1]

    lines(0:n_feat,DT.summary.list[[i]]$mean,type="l",lwd=log(K),col=col.vec[which(sigma.vec==sigma)])
}



