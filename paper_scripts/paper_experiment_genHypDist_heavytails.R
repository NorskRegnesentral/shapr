

#### PAPER EXPERIMENT FRAMEWORK ####

#### Example ####
# Sampling variables from a genealized hyperbolic distribution WITH HEAVY TAILS, with response being a mixture of linear and piecewise constant effects

rm(list = ls())

library(shapr)
library(data.table)
library(mvtnorm)
library(condMVNorm)
library(xgboost)

library(condMVNorm)
library(ghyp)
library(Matrix)


source("paper_scripts/paper_helper_funcs.R")

#### Parameters for the generlaized hyperbolic distribution ####

lambda <- 1
mu     <- c(1:3,1:3,1:3,3)
Sigma <- diag(c(1:3,1:3,1:3,3))
beta <- c(rep(1,5),rep(0.5,5))
omega <- 0.5
sd <- 0.1

nTrain <- 2000
#nTrain <- 100
nTest <- 50
nTest <- 10



#### Defining the true distribution of the variables and the model------

samp_variables_genHyp <- function(n,Sigma,beta,omega,lambda,mu){

    X <- simulateGenHyperbolic(nSim=n,
                               Sigma=Sigma,
                               beta = beta,
                               omega = omega,
                               lambda = lambda,
                               mu = mu)

    return(X)
}

samp_model <- function(n,X,sd){
    y <- 0.5*X[,2]  +  (X[,1]<0)*1 + (X[,3]<2) + (X[,4]>4)*1 + (X[,5]<6)*1 + (X[,6]<0)*1 + (X[,7]>-2)*(X[,8]<4)*1+ X[,9] + X[,10] + rnorm(n, mean=0, sd = sd)

    return(y)
    }




#### Sampling train and test data ---------

set.seed(123)
XYtrain <- data.table(samp_variables_genHyp(n = nTrain,
                                            Sigma=Sigma,
                                            beta = beta,
                                            omega = omega,
                                            lambda = lambda,
                                            mu = mu))

XYtrain[,y:=samp_model(.N,.SD,sd=sd)]
Xtrain <- copy(XYtrain)
Xtrain[,y:=NULL]

XYtest <- data.table(samp_variables_genHyp(n = nTest,
                                           Sigma=Sigma,
                                           beta = beta,
                                           omega = omega,
                                           lambda = lambda,
                                           mu = mu))
XYtest[,y:=samp_model(.N,.SD,sd=sd)]
Xtest <- copy(XYtest)
Xtest[,y:=NULL]

#### Fitting the model ----------

xgb.train <- xgb.DMatrix(data = as.matrix(Xtrain),
                         label = XYtrain$y)
xgb.test <- xgb.DMatrix(data = as.matrix(Xtest),
                         label = XYtest$y)

params <- list(eta =  0.3,
               objective = "reg:linear",
               eval_metric = "rmse",
               tree_method="hist") # gpu_hist

model <- xgb.train(data = xgb.train,
                     params = params,
                     nrounds = 50,
                     print_every_n = 10,
                     ntread = 3,
                     watchlist = list(train=xgb.train,test=xgb.test))

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

########################################


feature_list <- l$X$features
n_threshold_true <- 10^4
p <- ncol(Xtest)
ll <- list()

for (i in l$Xtest[, .I]) { # This may be parallelized when the prediction function is not parallelized.
    print(sprintf("%d out of %d", i, l$Xtest[, .N]))


    genHyp_samp <- lapply(
        X = feature_list,
        FUN = simulateCondDistHyperbolic_new,
        n_threshold = n_threshold_true,
        Sigma = Sigma,
        lambda = lambda,
        omega = omega,
        beta = beta,
        mu = mu,
        p = p,
        Xtest = as.matrix(l$Xtest)[i, , drop = FALSE])


    DTp <- rbindlist(genHyp_samp, idcol = "wcomb")
    DTp[, w := 1 / n_threshold]
    DTp[wcomb %in% c(1, 2 ^ p), w := 1] # Adjust weights for zero and full model

    nms <- colnames(Xtest)

    DTp[!(wcomb %in% c(1, 2 ^ p)), p_hat := pred_vector(model = model, data = .SD), .SDcols = nms]
    if(nrow(Xtest)==1){
        DTp[wcomb == 2 ^ p, p_hat := pred_vector(model = model, data = as.data.frame(rbind(Xtest,Xtest)))[1]] # Just a hack for a single prediction
    } else {
        DTp[wcomb == 2 ^ p, p_hat := pred_vector(model = model, data = as.data.frame(Xtest))]
    }
    DTp[wcomb == 1, p_hat := pred_zero]


    DTres <- DTp[, .(k = sum((p_hat * w) / sum(w))), wcomb]
    setkey(DTres, wcomb)
    ll[[i]] <- DTres
    ll[[i]][, id := i]
}

DT <- rbindlist(ll)

Kshap <- matrix(0, nrow = nrow(l$Xtest), ncol = nrow(l$W))
for (i in l$Xtest[, .I]) {
    Kshap[i, ] = l$W %*% DT[id == i, k]
}

Shapley.true = list(exactShap = Kshap, other_objects = list(ll = ll, DT = DT))



#######################################

# #### Running AICc to optimize the sigma in the empirical version ####
#
# source("scripts/AICc_helper_functions.R")
#
# # Computing for the first test observation only
#
# no.testobs <- 1
#
# # Same optimum is found for every test observation, so no need to do this for more than one test observation.
#
# h.optim.mat <- matrix(NA,nrow=nrow(l$S),ncol=no.testobs)
#
# ### If we use Mahalanobis, we might be able to adjust for different correlation between the variables, such that we at least need to do it only once
# ### per conditioning size (or a few and taking the mean of them) If we are also able to generalize the distance measure to take into account how the optimal bandwidth changes from one size to another,
# ### we may also get away doing it just once (or a few differnet sets which we take the mean of)
#
# S_scale_dist = T # Scaling the Mahalanbois ditstance
#
#
# for (j in 1:no.testobs){
#     x.star <- Xtest[j,]
#     for (i in 2:(nrow(l$S)-1)){ # Finding optimal h for each submodel, except the zero and full model
#         S <- l$S[i,]
#
#
#         S.cols <- paste0("X",which(as.logical(S)))
#         Sbar.cols <- paste0("X",which(as.logical(1-S)))
#
#         Xtrain.S <- subset(Xtrain,select=S.cols)
#         Xtrain.Sbar <- subset(Xtrain,select=Sbar.cols)
#         x.star.S <- subset(x.star,select=S.cols)
#
#         X.pred <- cbind(Xtrain.Sbar,x.star.S)
#         X.nms <- colnames(Xtrain)
#         setcolorder(X.pred,X.nms)
#
#         pred <- pred_vector(model=model,data=X.pred)
#
#         nlm.obj <- nlminb(start = 0.1,objective = AICc.func,y = pred,X = as.matrix(Xtrain.S),kernel="Mahalanobis",scale_var=T,S_scale_dist = S_scale_dist,lower = 0,control=list(eval.max=20,trace=1))
#         h.optim.mat[i,j] <- nlm.obj$par
#         # May also use mlrMBO here, something like this maybe: https://mlr-org.github.io/Stepwise-Bayesian-Optimization-with-mlrMBO/, just not stepwise. See other tutorial.
#         #    exp(-l$D[,1,i]/2*h)
#         print(c(i,j))
#     }
# }
#
# h.optim.mat
# #> h.optim.mat
# #[,1]
# #[1,]        NA
# #[2,] 0.1182843
# #[3,] 0.1444610
# #[4,] 0.1373879
# #[5,] 0.2398393
# #[6,] 0.2430718
# #[7,] 0.2354757
# #[8,]        NA
#
#
# Shapley.approx$sigma.AICc = compute_kernelShap(model = model,
#                                                l,
#                                                sigma = mean(h.optim.mat,na.rm=T),
#                                                w_threshold = w_threshold,
#                                                n_threshold = n_threshold,
#                                                verbose = FALSE,
#                                                cond_approach = "empirical",
#                                                pred_zero=pred_zero,
#                                                kernel_metric = "Gaussian")


Shapley.tree = predict(model,xgb.test,predcontrib=T)
Shapley.tree = Shapley.tree[,c(m+1,1:m)]


#### Running AICc to optimize the sigma in the empirical version ####

source("scripts/AICc_helper_functions.R")

# Computing for the first test observation only

no.testobs <- 2

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


        S.cols <- paste0("V",which(as.logical(S)))
        Sbar.cols <- paste0("V",which(as.logical(1-S)))

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


AICc.func.2 <- function(h.vec,y,XX,negative = FALSE,kernel = "Euclidean",scale_var = T,S_scale_dist = F){
    n <- length(y)
    q <- ncol(XX)

    if (length(h.vec)==1){
        h.vec <- rep(h.vec,q)
    }

    H <- H.func(h.vec = h.vec,X = XX,kernel = kernel, scale_var = scale_var, S_scale_dist = S_scale_dist)

    sigma.hat.sq <- sigma.hat.sq.func(y=y,
                                      H = H)

    tr.H <- sum(diag(H))
    correction.term <- (1+tr.H/n)/(1-(tr.H+2)/n)

    AICc <- log(sigma.hat.sq) + correction.term
    if(negative){
        AICc <- -AICc
    }
    return(AICc)
}


h.vec <- seq(0.02,0.6,by=0.02)

h.optim.mat.2 <- matrix(NA,nrow=nrow(l$S),ncol=no.testobs)

for (j in 1:no.testobs){
    x.star <- Xtest[j,]
    for (i in 2:(nrow(l$S)-1)){ # Finding optimal h for each submodel, except the zero and full model

        S <- l$S[i,]


        S.cols <- paste0("V",which(as.logical(S)))
        Sbar.cols <- paste0("V",which(as.logical(1-S)))

        Xtrain.S <- subset(Xtrain,select=S.cols)
        Xtrain.Sbar <- subset(Xtrain,select=Sbar.cols)
        x.star.S <- subset(x.star,select=S.cols)

        X.pred <- cbind(Xtrain.Sbar,x.star.S)
        X.nms <- colnames(Xtrain)
        setcolorder(X.pred,X.nms)

        pred <- pred_vector(model=model,data=X.pred)

        val <- sapply(X = h.vec,FUN = AICc.func.2,y=pred,XX=as.matrix(Xtrain.S),kernel="Mahalanobis",scale_var=T,S_scale_dist = S_scale_dist)
        h.optim.mat.2[i,j] <- h.vec[which.min(val)]

        # May also use mlrMBO here, something like this maybe: https://mlr-org.github.io/Stepwise-Bayesian-Optimization-with-mlrMBO/, just not stepwise. See other tutorial.
        #    exp(-l$D[,1,i]/2*h)
        print(c(i,j))
    }
}


val <- sapply(X = h.vec,FUN = AICc.func.2,y=pred,XX=as.matrix(Xtrain.S),kernel="Mahalanobis",scale_var=T,S_scale_dist = S_scale_dist)

h.vec[which.min(val)]


h.optim.mat
# > head(h.optim.mat,10)
# [,1]
# [1,]         NA
# [2,] 0.06837773
# [3,] 0.01318516
# [4,] 0.12330996
# [5,] 0.02945112
# [6,] 0.01029743
# [7,] 0.12852126
# [8,]         NA
# [9,]         NA
# [10,]         NA




lX <- copy(l$X)
lX[,h.optim:=h.optim.mat[,1]]
lX[,mean(h.optim),by=nfeatures]
lX[nfeatures==1,h.optim]

Shapley.approx$sigmagrid <- list()

sigmaval <- seq(0.02,0.5,0.01)

for (i in 1:length(sigmaval)){
    Shapley.approx$sigmagrid[[i]] = compute_kernelShap(model = model,
                                                  l,
                                                  sigma = sigmaval[i],
                                                  w_threshold = w_threshold,
                                                  n_threshold = n_threshold,
                                                  verbose = FALSE,
                                                  cond_approach = "empirical",
                                                  pred_zero=pred_zero,
                                                  kernel_metric = "Gaussian")
}

which.optimal.sigmagrid <- rep(1,nrow(l$S))
for (i in 2:(nrow(l$S)-1)){
    which.optimal.sigmagrid[i] <- which.min((sigmaval-rowMeans(h.optim.mat)[i])^2)
}


DT.AICc <- Shapley.approx$sigmagrid[[1]]$other_objects$DT

for (i in 2:(nrow(l$S)-1)){
    this <- Shapley.approx$sigmagrid[[which.optimal.sigmagrid[i]]]$other_objects$DT[wcomb==i,k]

    DT.AICc[wcomb==i,k:=this]

    print(i)
}

Kshap.AICc <- matrix(0, nrow = nrow(l$Xtest), ncol = nrow(l$W))
for (i in l$Xtest[, .I]) {
    Kshap.AICc[i, ] = l$W %*% DT.AICc[id == i, k]
}

Shapley.approx$sigma.AICc = list(Kshap = Kshap.AICc, other_objects = list( DT = DT.AICc))





#### Comparing the true and approximate values -------------



# Mean absolute errors per variable (to see if the performance differ between variables)
(absmeans.sigma.01 = colMeans(abs(Shapley.true$exactShap[,-1]-Shapley.approx$sigma.01$Kshap[,-1])))
(absmeans.sigma.03 = colMeans(abs(Shapley.true$exactShap[,-1]-Shapley.approx$sigma.03$Kshap[,-1])))
(absmeans.indep = colMeans(abs(Shapley.true$exactShap[,-1]-Shapley.approx$indep$Kshap[,-1])))
(absmeans.Gauss = colMeans(abs(Shapley.true$exactShap[,-1]-Shapley.approx$Gauss$Kshap[,-1])))
(absmeans.sigma.AICc = colMeans(abs(Shapley.true$exactShap[,-1]-Shapley.approx$sigma.AICc$Kshap[,-1])))
#
(absmeans.treeSHAP = colMeans(abs(Shapley.true$exactShap[,-1]-Shapley.tree[,-1]))) # Additional one


# Mean of the absolute errors over all variables
# Mean of the absolute errors over all variables
res_to_paper <- data.frame(S_KS=mean(absmeans.indep),G_KS = mean(absmeans.Gauss),E_KS_0.1=mean(absmeans.sigma.01),E_KS_0.3=mean(absmeans.sigma.03),E_AICc = mean(absmeans.sigma.AICc),TreeSHAP = mean(absmeans.treeSHAP))
spec <- data.frame(gx="Piecewise constant+linear",fx="XGBoost",px="genHyp_heavytails",rho=NA)
res_to_paper <- cbind(spec,res_to_paper)













#### Results!!!

#gx      fx                px rho      S_KS      G_KS  E_KS_0.1  E_KS_0.3  TreeSHAP
#1 Piecewise constant+linear XGBoost genHyp_heavytails  NA 0.8272501 0.7067385 0.6899325 0.7527633 0.8125419

#[1] 0.7716888 0.7275275 0.7262588 0.5864149 0.7219045 0.6428779 0.6711904 0.5005127 0.7915150 0.7594348
#> (absmeans.sigma.03 = colMeans(abs(Shapley.true$exactShap[,-1]-Shapley.approx$sigma.03$Kshap[,-1])))
#[1] 0.7904345 0.9389035 0.6441909 0.4816689 0.7254520 0.7237760 0.6950459 0.6212739 0.9289945 0.9778925
#> (absmeans.indep = colMeans(abs(Shapley.true$exactShap[,-1]-Shapley.approx$indep$Kshap[,-1])))
#[1] 0.8230117 1.3734182 0.5684398 0.3429761 0.7221522 0.5320960 0.6725391 0.5995844 1.3480320 1.2902518
#> (absmeans.Gauss = colMeans(abs(Shapley.true$exactShap[,-1]-Shapley.approx$Gauss$Kshap[,-1])))
#[1] 0.8202128 0.7748885 0.8590873 0.7573373 0.6648654 0.6154649 0.7467502 0.6109138 0.5212647 0.6966001
#> #(absmeans.sigma.AICc = colMeans(abs(Shapley.true$exactShap[,-1]-Shapley.approx$sigma.AICc$Kshap[,-1])))
#    >
#    > (absmeans.treeSHAP = colMeans(abs(Shapley.true$exactShap[,-1]-Shapley.tree[,-1]))) # Additional one
#V1        V2        V3        V4        V5        V6        V7        V8        V9       V10
#0.7443706 1.2210754 0.5381274 0.3296334 0.6923652 0.5370582 0.6683634 0.6146682 1.1855305 1.5942265



# Insert ranking based measures etc. here as well.

