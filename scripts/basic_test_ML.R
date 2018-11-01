
rm(list=ls())

library(MASS)
library(ranger)
library(shapr)
library(mvtnorm)
library(data.table)
source("scripts/AICc_helper_functions.R")

ntrain <- 1000
ntrain.ML <- 1000
ntest <- 1000

mu <- -4:5
Sigma <- matrix(0,10,10)
Sigma[1:5,1:5]   <- 0.9
Sigma[6:10,6:10] <- 0.5
Sigma[1:5,6:10]  <- 0.2
Sigma[6:10,1:5]  <- 0.2
diag(Sigma)      <- 1

a <- mu[2:10]+1

mu1.given.rest <- as.vector(mu[1]+Sigma[1,2:10,drop=FALSE]%*%solve(Sigma[2:10,2:10])%*%(a-mu[2:10]))
sd1.given.rest <- as.vector(Sigma[1,2:10,drop=FALSE]%*%solve(Sigma[2:10,2:10])%*%Sigma[2:10,1,drop=FALSE])

Xtrain <- as.data.table(rmvnorm(ntrain,mean = mu,sigma = Sigma))
#Xtest <- as.data.table(rmvnorm(ntest,mean = mu,sigma = Sigma))

Xtest <- data.table(rbind(t(mu)+1,t(mu)+1))

formula <- paste0("V1~",paste0(colnames(Xtrain)[-1],collapse="+"))

submodel = ranger::ranger(
    formula = as.formula(formula),
    data = Xtrain,
    num.trees = 1000,
    num.threads = 3,
    verbose = TRUE,
    importance = "impurity")

predict(submodel,data = Xtest)$predictions



#### Pre computation before kernel shap --------------------------------
l <- prepare_kernelShap(
    m = ncol(Xtrain),
    Xtrain = Xtrain,
    Xtest = Xtest,
    exact = TRUE,
    nrows = 1e4
)

S <- l$S[c(2,2),,drop=F]
S = l$S[c(1023,1023),]




#### Empirical approach


x.star <- Xtest[1,]

S.cols <- paste0("V",which(as.logical(S[1,])))
Sbar.cols <- paste0("V",which(as.logical(1-S[1,])))

Xtrain.S <- subset(Xtrain,select=S.cols)
Xtrain.Sbar <- subset(Xtrain,select=Sbar.cols)
x.star.S <- subset(x.star,select=S.cols)

X.pred <- cbind(Xtrain.Sbar,x.star.S)
X.nms <- colnames(Xtrain)
setcolorder(X.pred,X.nms)

pred <- X.pred$V1

nlm.obj <- nlminb(start = 1,objective = AICc.func,y = pred,X = as.matrix(Xtrain.S),kernel="Mahalanobis",scale_var=T,lower = 0,control=list(eval.max=20,trace=1))

sigma=nlm.obj$par

n_threshold <- 100
w_threshold <- 1
p <- ncol(Xtrain)
nms <- colnames(Xtest)

W_kernel <- exp((-0.5*l$D)/sigma^2)
W_kernel = W_kernel[,1,c(1023,1023)]



DTp <- impute_data(
#    W_kernel = W_kernel[,1,][,c(2,2),drop=F],
    W_kernel =  W_kernel,
    S = S,
    Xtrain = as.matrix(Xtrain),
    Xtest = as.matrix(Xtest),
    w_threshold = w_threshold,
    n_threshold = n_threshold)

#DTp[wcomb==1023,sum(V1*w)/sum(w)]
#DTp[wcomb==1023,sum(w)]

#DTp[wcomb==1023,hist(V1)]

mu1.given.rest

DTp[wcomb==1,sum(V1*w)/sum(w)]
#DTp[wcomb==2,sum(V1*w)/sum(w)]


predict(submodel,data = Xtest)$predictions

