

#### PAPER EXPERIMENT FRAMEWORK ####
#### Use the current setup for all experiements in the paper to ease reproducablity etc.

#### Example 3 ####
# Linear model with highly dependent Gaussian features

rm(list = ls())

library(shapr)
library(data.table)
library(mvtnorm)
library(condMVNorm)

source("paper_scripts/paper_helper_funcs.R")

mu.list = list(c(0,0,0))
Sigma.list <- list(matrix(c(1,0.7,0.7,
                            0.7,1,0.7,
                            0.7,0.7,1),ncol=3))
Sigma.list <- list(diag(3))
pi.G <- 1

sd = 0.1

nTrain <- 500
nTest <- 200


#### Defining the true distribution of the variables and the model------

samp_variables <- function(n,pi.G,mu.list,Sigma.list){

    X <- joint.samp.func(n = n,
                         pi.G,
                         mu.list,
                         Sigma.list)
    return(X)
}

samp_model <- function(n,X,sd){
    y <- X[,1] + X[,2] + X[,3] + rnorm(n = n,mean=0,sd=sd)
}




#### Sampling train and test data ---------

set.seed(123)
XYtrain <- data.table(samp_variables(n = nTrain,
                                     pi.G = pi.G,
                                     mu.list = mu.list,
                                     Sigma.list = Sigma.list))
XYtrain[,y:=samp_model(.N,.SD,sd=sd)]
Xtrain <- copy(XYtrain)
Xtrain[,y:=NULL]

XYtest <- data.table(samp_variables(n = nTest,
                                     pi.G = pi.G,
                                     mu.list = mu.list,
                                     Sigma.list = Sigma.list))
XYtest[,y:=samp_model(.N,.SD,sd=sd)]
Xtest <- copy(XYtest)
Xtest[,y:=NULL]

#### Fitting the model ----------

model = lm(y~.,data=XYtrain)

pred_zero = XYtrain[, mean(y)]
m = ncol(Xtrain)


#### Pre computation before kernel shap --------------------------------
l <- prepare_kernelShap(
    m = m,
    Xtrain = Xtrain,
    Xtest = Xtest,
    exact = TRUE,
    nrows = 1e4,
    distance_metric = "Euclidean"
)

#### Finding which h is actually the best


w_threshold = 1 # For a fairer comparison, all models use the same number of samples (n_threshold)
n_threshold = 10^3


Shapley.Gauss = compute_kernelShap(model = model,
                                   l,
                                   sigma = 1, # Ignored when Gaussian==T
                                   w_threshold = w_threshold,
                                   n_threshold = n_threshold,
                                   verbose = FALSE,
                                   gaussian_sample = TRUE,
                                   pred_zero=pred_zero,
                                   kernel_metric = "Gaussian")

h.val <- seq(0.05,1,0.05)
Shapley.approx.test <- list()
for (i in 1:length(h.val)){

    Shapley.approx.test[[i]] = compute_kernelShap(model = model,
                                             l,
                                             sigma = h.val[i],
                                             w_threshold = w_threshold,
                                             n_threshold = n_threshold,
                                             verbose = FALSE,
                                             gaussian_sample = FALSE,
                                             pred_zero=pred_zero,
                                             kernel_metric = "Gaussian")
    print(i)

}

DT.gaussian <- rbindlist(Shapley.Gauss$other_object$ll)
setnames(DT.gaussian,"k","p.gaussian")

DT.list <- list()
for (i in 1:length(h.val)){

    DT.approx <- rbindlist(Shapley.approx.test[[i]]$other_object$ll)
    setnames(DT.approx,"k","p.approx")
    DT.approx[,h:=h.val[i]]

    DT.list[[i]] <- merge(DT.approx,DT.gaussian,by=c("wcomb","id"),all=T)
}
DT <- rbindlist(DT.list)



helper <- copy(l$X)
helper[,wcomb:=ID]
helper[,ID:= NULL]
helper[,features:= NULL]
helper[,N:= NULL]
helper[,weight:= NULL]

DT <- merge(DT,helper,by="wcomb")
DT[,diff:=p.approx-p.gaussian]
DT[,absdiff:=abs(diff)]

print(DT[,mean(absdiff)]) # Mean
DT.summary <- DT[,.(mean=mean(absdiff),sd=sd(absdiff)),by=.(nfeatures,h)] # Summary per nfeatures
DT.summary[nfeatures %in% c(1,2),]
DT.summary[,mean(mean),by=h]

#### Optimum is h=0.20 for nfeatures=1, and h=0.35 for nfeatures=2 with correlation 0.7.




############# DOING THE AICc-stuff ####

source("scripts/AICc_helper_functions.R")

# Computing for the first test observation only

no.testobs <- 5

h.optim.mat <- matrix(NA,nrow=nrow(l$S),ncol=no.testobs)

for (j in 1:no.testobs){
    x.star <- Xtest[j,]
    for (i in 2:(nrow(l$S)-1)){ # Finding optimal h for each submodel, except the zero and full model
        S <- l$S[i,]

        S.cols <- paste0("X",which(as.logical(S)))
        Sbar.cols <- paste0("X",which(as.logical(1-S)))

        Xtrain.Sbar <- subset(Xtrain,select=Sbar.cols)
        x.star.S <- subset(x.star,select=S.cols)

        pred <- cbind(Xtrain.Sbar,x.star.S)

        X.nms <- colnames(Xtrain)
        setcolorder(pred,X.nms)

        pred[, p_hat := pred_vector(model = model, data = .SD), .SDcols = X.nms]

        # h.val <- seq(0.02,1,0.04)
        # h.val <- seq(0.001,0.02,0.001)
        #
        # AICc.vec <- rep(NA,length(h.val))
        # for (j in 1:length(h.val)){
        #     AICc.vec[j] <- AICc.func(h.vec = h.val[j],y = pred$p_hat,X = as.matrix(subset(pred,select = X.nms)))
        #     print(c(h.val[j],AICc.vec[j]))
        # }

        nlm.obj <- nlminb(start = 0.1,objective = AICc.func,y = pred$p_hat,X = as.matrix(subset(pred,select = X.nms)),lower = 0,control=list(eval.max=20,trace=1))
        h.optim.mat[i,j] <- nlm.obj$par
        # May also use mlrMBO here, something like this maybe: https://mlr-org.github.io/Stepwise-Bayesian-Optimization-with-mlrMBO/, just not stepwise. See other tutorial.
        #    exp(-l$D[,1,i]/2*h)
        print(c(i,j))
    }
}


h.optim.mat


#### Computing the various Shapley approximations --------

w_threshold = 1 # For a fairer comparison, all models use the same number of samples (n_threshold)
n_threshold = 10^3

Shapley.approx = list()

Shapley.approx$sigma.0.005 = compute_kernelShap(model = model,
                                             l,
                                             sigma = 0.01,
                                             w_threshold = w_threshold,
                                             n_threshold = n_threshold,
                                             verbose = FALSE,
                                             gaussian_sample = FALSE,
                                             pred_zero=pred_zero)


Shapley.approx$sigma.01 = compute_kernelShap(model = model,
                                             l,
                                             sigma = 0.1,
                                             w_threshold = w_threshold,
                                             n_threshold = n_threshold,
                                             verbose = FALSE,
                                             gaussian_sample = FALSE,
                                             pred_zero=pred_zero)

Shapley.approx$sigma.03 = compute_kernelShap(model = model,
                                             l,
                                             sigma = 0.3,
                                             w_threshold = w_threshold,
                                             n_threshold = n_threshold,
                                             verbose = FALSE,
                                             gaussian_sample = FALSE,
                                             pred_zero=pred_zero)

Shapley.approx$indep = compute_kernelShap(model = model,
                                          l,
                                          sigma = 0.1, # sigma==0 gives the special case of independence (NOTE: NOT the same as setting sigma= 10^10)
                                          w_threshold = w_threshold,
                                          n_threshold = n_threshold,
                                          verbose = FALSE,
                                          gaussian_sample = FALSE,
                                          pred_zero=pred_zero,
                                          kernel_metric = "independence")

Shapley.approx$Gauss = compute_kernelShap(model = model,
                                          l,
                                          sigma = 0, # Ignored when Gaussian==T
                                          w_threshold = w_threshold,
                                          n_threshold = n_threshold,
                                          verbose = FALSE,
                                          gaussian_sample = TRUE,
                                          pred_zero=pred_zero)

Shapley.true = Shapley_true(model = model,
                            Xtrain = Xtrain,
                            Xtest = Xtest,
                            pi.G = pi.G,
                            mu.list = mu.list,
                            Sigma.list = Sigma.list,
                            int.samp=200,
                            l,
                            pred_zero = pred_zero)

#### Comparing the true and approximate values -------------

# Mean absolute errors per variable (to see if the performance differ between variables)
(absmeans.sigma.01 = colMeans(abs(Shapley.true[,-1]-Shapley.approx$sigma.01$Kshap[,-1])))
(absmeans.sigma.03 = colMeans(abs(Shapley.true[,-1]-Shapley.approx$sigma.03$Kshap[,-1])))
(absmeans.indep = colMeans(abs(Shapley.true[,-1]-Shapley.approx$indep$Kshap[,-1])))
(absmeans.Gauss = colMeans(abs(Shapley.true[,-1]-Shapley.approx$Gauss$Kshap[,-1])))

# Mean of the absolute errors over all variables
res_to_paper <- c(S_KS=mean(absmeans.indep),G_KS = mean(absmeans.Gauss),E_KS_0.1=mean(absmeans.sigma.01),E_KS_0.3=mean(absmeans.sigma.03))
res_to_paper
#S_KS       G_KS   E_KS_0.1   E_KS_0.3
#0.32338379 0.01657350 0.02612238 0.03026733

# Insert ranking based measures etc. here as well.



DT.approx <- rbindlist(Shapley.approx$sigma.0.005$other_object$ll)
DT.gaussian <- rbindlist(Shapley.approx$Gauss$other_object$ll)
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
DT.summary[,sigma:=0.1]
DT.summary[,K:=n_threshold]

DT.summary.list[[listnum]] <- copy(DT.summary)



