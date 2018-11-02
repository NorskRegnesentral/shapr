

#### PAPER EXPERIMENT FRAMEWORK ####
#### Use the current setup for all experiements in the paper to ease reproducablity etc.

#### Example 1 ####
# Linear model with independent Gaussian features

rm(list = ls())

library(shapr)
library(data.table)
library(mvtnorm)
library(condMVNorm)

source("paper_scripts/paper_helper_funcs.R")

mu.list = list(c(0,0,0))
Sigma.list <- list(matrix(c(1,0,0,
                            0,1,0,
                            0,0,1),ncol=3))
pi.G <- 1

sd = 0.1

nTrain <- 2000
nTest <- 1000


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
    distance_metric = "Mahalanobis_scaled"
)

w_threshold = 1 # For a fairer comparison, all models use the same number of samples (n_threshold)
n_threshold = 10^2

Shapley.approx = list()

Shapley.approx$sigma.01 = compute_kernelShap(model = model,
                                             l,
                                             sigma = 0.1,
                                             w_threshold = w_threshold,
                                             n_threshold = n_threshold,
                                             verbose = FALSE,
                                             cond_approach = "empirical",
                                             pred_zero=pred_zero,
                                             kernel_metric = "Gaussian")

Shapley.approx$sigma.03 = compute_kernelShap(model = model,
                                             l,
                                             sigma = 0.3,
                                             w_threshold = w_threshold,
                                             n_threshold = n_threshold,
                                             verbose = FALSE,
                                             cond_approach = "empirical",
                                             pred_zero=pred_zero,
                                             kernel_metric = "Gaussian")


Shapley.approx$indep = compute_kernelShap(model = model,
                                          l,
                                          sigma = 0, # sigma==0 gives the special case of independence (NOTE: NOT the same as setting sigma= 10^10)
                                          w_threshold = w_threshold,
                                          n_threshold = n_threshold,
                                          verbose = FALSE,
                                          cond_approach = "empirical",
                                          pred_zero=pred_zero,
                                          kernel_metric = "independence")


Shapley.approx$Gauss = compute_kernelShap(model = model,
                                          l,
                                          sigma = 0, # Ignored when Gaussian==T
                                          w_threshold = w_threshold,
                                          n_threshold = n_threshold,
                                          verbose = FALSE,
                                          cond_approach = "Gaussian",
                                          pred_zero=pred_zero,
                                          kernel_metric = "Gaussian")

Shapley.approx$copula = compute_kernelShap(model = model,
                                           l,
                                           sigma = 0, # Ignored when Gaussian==T
                                           w_threshold = w_threshold,
                                           n_threshold = n_threshold,
                                           verbose = FALSE,
                                           cond_approach = "copula",
                                           pred_zero=pred_zero,
                                           kernel_metric = "Gaussian")


Shapley.true = Shapley_true(model = model,
                            Xtrain = Xtrain,
                            Xtest = Xtest,
                            pi.G = pi.G,
                            mu.list = mu.list,
                            Sigma.list = Sigma.list,
                            int.samp=200,
                            l,
                            pred_zero = pred_zero)

source("scripts/AICc_helper_functions.R")

# Computing for the first test observation only

no.testobs <- 1

# Same optimum is found for every test observation, so no need to do this for more than one test observation.

h.optim.mat <- matrix(NA,nrow=nrow(l$S),ncol=no.testobs)

### If we use Mahalanobis, we might be able to adjust for different correlation between the variables, such that we at least need to do it only once
### per conditioning size (or a few and taking the mean of them) If we are also able to generalize the distance measure to take into account how the optimal bandwidth changes from one size to another,
### we may also get away doing it just once (or a few differnet sets which we take the mean of)

S_scale_dist = T # Scaling the Mahalanbois ditstance


for (j in 1:no.testobs){
    x.star <- Xtest[j,]
    for (i in 2:(nrow(l$S)-1)){ # Finding optimal h for each submodel, except the zero and full model
        S <- l$S[i,]


        S.cols <- paste0("X",which(as.logical(S)))
        Sbar.cols <- paste0("X",which(as.logical(1-S)))

        Xtrain.S <- subset(Xtrain,select=S.cols)
        Xtrain.Sbar <- subset(Xtrain,select=Sbar.cols)
        x.star.S <- subset(x.star,select=S.cols)

        X.pred <- cbind(Xtrain.Sbar,x.star.S)
        X.nms <- colnames(Xtrain)
        setcolorder(X.pred,X.nms)

        pred <- pred_vector(model=model,data=X.pred)

        nlm.obj <- nlminb(start = 0.1,objective = AICc.func,y = pred,X = as.matrix(Xtrain.S),kernel="Mahalanobis",scale_var=T,S_scale_dist = S_scale_dist,lower = 0,control=list(eval.max=20,trace=1))
        h.optim.mat[i,j] <- nlm.obj$par
        # May also use mlrMBO here, something like this maybe: https://mlr-org.github.io/Stepwise-Bayesian-Optimization-with-mlrMBO/, just not stepwise. See other tutorial.
        #    exp(-l$D[,1,i]/2*h)
        print(c(i,j))
    }
}

h.optim.mat
#> h.optim.mat
#[,1]
#[1,]        NA
#[2,] 0.1182843
#[3,] 0.1444610
#[4,] 0.1373879
#[5,] 0.2398393
#[6,] 0.2430718
#[7,] 0.2354757
#[8,]        NA


Shapley.approx$sigma.AICc = compute_kernelShap(model = model,
                                               l,
                                               sigma = mean(h.optim.mat,na.rm=T),
                                               w_threshold = w_threshold,
                                               n_threshold = n_threshold,
                                               verbose = FALSE,
                                               cond_approach = "empirical",
                                               pred_zero=pred_zero,
                                               kernel_metric = "Gaussian")




#### Comparing the true and approximate values -------------

# Mean absolute errors per variable (to see if the performance differ between variables)
(absmeans.sigma.01 = colMeans(abs(Shapley.true$exactShap[,-1]-Shapley.approx$sigma.01$Kshap[,-1])))
(absmeans.sigma.03 = colMeans(abs(Shapley.true$exactShap[,-1]-Shapley.approx$sigma.03$Kshap[,-1])))
(absmeans.indep = colMeans(abs(Shapley.true$exactShap[,-1]-Shapley.approx$indep$Kshap[,-1])))
(absmeans.Gauss = colMeans(abs(Shapley.true$exactShap[,-1]-Shapley.approx$Gauss$Kshap[,-1])))
(absmeans.copula = colMeans(abs(Shapley.true$exactShap[,-1]-Shapley.approx$copula$Kshap[,-1])))
(absmeans.sigma.AICc = colMeans(abs(Shapley.true$exactShap[,-1]-Shapley.approx$sigma.AICc$Kshap[,-1])))


# Mean of the absolute errors over all variables
res_to_paper <- data.frame(S_KS=mean(absmeans.indep),G_KS = mean(absmeans.Gauss),E_KS_0.1=mean(absmeans.sigma.01),E_KS_0.3=mean(absmeans.sigma.03),E_KS_AICc=mean(absmeans.sigma.AICc),TreeSHAP = NA,C_KS=mean(absmeans.copula))
spec <- data.frame(gx = "Linear", fx="Linear",px="Gaussian",rho=0)
res_to_paper <- cbind(spec,res_to_paper)
#S_KS       G_KS   E_KS_0.1   E_KS_0.3
#0.01789263 0.01903016 0.03861126 0.02175289



























##################







model <- model
l <- l
verbose = FALSE
cond_approach = "copula"
pred_zero = pred_zero
kernel_metric = "Gaussian"
mu = NULL
Sigma = NULL
sigma = 0.1



p <- ncol(Xtrain)

Gauss_samp <- lapply(
    X = feature_list,
    FUN = samp_Gauss_func,
    n_threshold = n_threshold,
    mu = mu,
    Sigma = Sigma,
    p = p,
    Xtest = Xtest
)


nSim <- 52
X <- as.matrix(Xtrain)
dep.ind <- 2:3
given.ind <- 1
x00 <- as.matrix(Xtest)[1,]

Xtrain_Gauss_trans <- apply(X = Xtran0,MARGIN = 2,FUN=Gauss_trans_func)
Xtest_Gauss_trans <- apply(X = rbind(Xtest0,Xtran0),MARGIN = 2,FUN=Gauss_trans_func_seperate,n_y = nrow(Xtest0))



nSim <- length(data)
y1.f = splinefun((0:(nSim-1))/nSim, sort(data), method = "monoH.FC")




empDist <- function(data)
{
    n <- length(data)
    cumdist <- seq(1/n, 1, 1/n)
    cumdist[n] <- 1 - (1/(2 * n))
    unif <- cumdist[rank(data)]
    unif
}

newQuantile <- function(data,quant)
{
    n <- length(data)
    z <- order(data)
    ind <- round(quant*n,0)
    ind[which(ind==0)] <- 1

    y <- data[z[ind]]
    y
}
############# MJ CODE ############3

Gauss_trans_func <- function(x){
    u <- rank(x)/(length(x)+1)
    z <- qnorm(u)
    return(z)
}

inv_Gauss_trans_func <- function(z,x,type=7){
    u <- pnorm(z)
    xNew <- quantile(x,u,type=type)
    return(xNew)
    }


Xtrain_Gauss_trans <- apply(X = Xtrain,MARGIN = 2,FUN=Gauss_trans_func)



samp_copula_func <- function(given_ind,n_threshold,Xtrain_Gauss_trans,Xtest){

}
#################







simulateCondDistGaussianEmpirical <- function(nSim,X, dep.ind,given.ind,x00)
{
    XX <- X
    for(i in 1:dim(X)[2])
        XX[,i] <- qnorm(empDist(X[,i]))

    x0 <- x00
    for(i in 1:dim(X)[2])
    {
        ind <- which(abs(X[,i]-x00[i])==min(abs(X[,i]-x00[i])))
        x0[i] <- XX[ind,i]
    }
    muNorm0 <- apply(XX,2,mean)
    covMat0 <- var(XX)

    normData01 <- simulateCondDistGaussianNew(nSim=nSim, muNorm0,covMat0,dep.ind,given.ind,x0)
    if(length(dep.ind)==1)
        simDataN <- newQuantile(X[,dep.ind],pnorm(normData01))
    else
    {
        simDataN <- normData01
        for(i in 1:dim(simDataN)[2])
            simDataN[,i] <- newQuantile(X[,dep.ind[i]],pnorm(normData01[,i]))
    }
    simDataN
}













#### Computing the various Shapley approximations --------

w_threshold = 1 # For a fairer comparison, all models use the same number of samples (n_threshold)
n_threshold = 10^3

Shapley.approx = list()

Shapley.approx$sigma.01 = compute_kernelShap(model = model,
                                             l,
                                             sigma = 0.1,
                                             w_threshold = w_threshold,
                                             n_threshold = n_threshold,
                                             verbose = FALSE,
                                             gaussian_sample = FALSE,
                                             pred_zero=pred_zero,
                                             kernel_metric = "Gaussian")

Shapley.approx$sigma.03 = compute_kernelShap(model = model,
                                             l,
                                             sigma = 0.3,
                                             w_threshold = w_threshold,
                                             n_threshold = n_threshold,
                                             verbose = FALSE,
                                             gaussian_sample = FALSE,
                                             pred_zero=pred_zero,
                                             kernel_metric = "Gaussian")


Shapley.approx$indep = compute_kernelShap(model = model,
                                          l,
                                          sigma = 0, # sigma==0 gives the special case of independence (NOTE: NOT the same as setting sigma= 10^10)
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
                                          pred_zero=pred_zero,
                                          kernel_metric = "Gaussian")


Shapley.true = Shapley_true(model = model,
                            Xtrain = Xtrain,
                            Xtest = Xtest,
                            pi.G = pi.G,
                            mu.list = mu.list,
                            Sigma.list = Sigma.list,
                            int.samp=200,
                            l,
                            pred_zero = pred_zero)

#### Running AICc to optimize the sigma in the empirical version ####

source("scripts/AICc_helper_functions.R")

# Computing for the first test observation only

no.testobs <- 1

# Same optimum is found for every test observation, so no need to do this for more than one test observation.

h.optim.mat <- matrix(NA,nrow=nrow(l$S),ncol=no.testobs)

### If we use Mahalanobis, we might be able to adjust for different correlation between the variables, such that we at least need to do it only once
### per conditioning size (or a few and taking the mean of them) If we are also able to generalize the distance measure to take into account how the optimal bandwidth changes from one size to another,
### we may also get away doing it just once (or a few differnet sets which we take the mean of)

S_scale_dist = T # Scaling the Mahalanbois ditstance


for (j in 1:no.testobs){
    x.star <- Xtest[j,]
    for (i in 2:(nrow(l$S)-1)){ # Finding optimal h for each submodel, except the zero and full model
        S <- l$S[i,]


        S.cols <- paste0("X",which(as.logical(S)))
        Sbar.cols <- paste0("X",which(as.logical(1-S)))

        Xtrain.S <- subset(Xtrain,select=S.cols)
        Xtrain.Sbar <- subset(Xtrain,select=Sbar.cols)
        x.star.S <- subset(x.star,select=S.cols)

        X.pred <- cbind(Xtrain.Sbar,x.star.S)
        X.nms <- colnames(Xtrain)
        setcolorder(X.pred,X.nms)

        pred <- pred_vector(model=model,data=X.pred)

        nlm.obj <- nlminb(start = 0.1,objective = AICc.func,y = pred,X = as.matrix(Xtrain.S),kernel="Mahalanobis",scale_var=T,S_scale_dist = S_scale_dist,lower = 0,control=list(eval.max=20,trace=1))
        h.optim.mat[i,j] <- nlm.obj$par
        # May also use mlrMBO here, something like this maybe: https://mlr-org.github.io/Stepwise-Bayesian-Optimization-with-mlrMBO/, just not stepwise. See other tutorial.
        #    exp(-l$D[,1,i]/2*h)
        print(c(i,j))
    }
}

h.optim.mat
#> h.optim.mat
#[,1]
#[1,]        NA
#[2,] 0.1182843
#[3,] 0.1444610
#[4,] 0.1373879
#[5,] 0.2398393
#[6,] 0.2430718
#[7,] 0.2354757
#[8,]        NA


Shapley.approx$sigma.AICc = compute_kernelShap(model = model,
                                               l,
                                               sigma = mean(h.optim.mat,na.rm=T),
                                               w_threshold = w_threshold,
                                               n_threshold = n_threshold,
                                               verbose = FALSE,
                                               gaussian_sample = FALSE,
                                               pred_zero=pred_zero,
                                               kernel_metric = "Gaussian")




#### Comparing the true and approximate values -------------

# Mean absolute errors per variable (to see if the performance differ between variables)
(absmeans.sigma.01 = colMeans(abs(Shapley.true$exactShap[,-1]-Shapley.approx$sigma.01$Kshap[,-1])))
(absmeans.sigma.03 = colMeans(abs(Shapley.true$exactShap[,-1]-Shapley.approx$sigma.03$Kshap[,-1])))
(absmeans.indep = colMeans(abs(Shapley.true$exactShap[,-1]-Shapley.approx$indep$Kshap[,-1])))
(absmeans.Gauss = colMeans(abs(Shapley.true$exactShap[,-1]-Shapley.approx$Gauss$Kshap[,-1])))
(absmeans.sigma.AICc = colMeans(abs(Shapley.true$exactShap[,-1]-Shapley.approx$sigma.AICc$Kshap[,-1])))


# Mean of the absolute errors over all variables
res_to_paper <- data.frame(S_KS=mean(absmeans.indep),G_KS = mean(absmeans.Gauss),E_KS_0.1=mean(absmeans.sigma.01),E_KS_0.3=mean(absmeans.sigma.03),E_KS_AICc=mean(absmeans.sigma.AICc),TreeSHAP = NA)
spec <- data.frame(gx = "Linear", fx="Linear",px="Gaussian",rho=0)
res_to_paper <- cbind(spec,res_to_paper)
#S_KS       G_KS   E_KS_0.1   E_KS_0.3
#0.01789263 0.01903016 0.03861126 0.02175289

# Insert ranking based measures etc. here as well.

