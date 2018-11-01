

#### PAPER EXPERIMENT FRAMEWORK ####

#### Example ####
# Sampling variables from a genealized hyperbolic distribution, with response being a mixture of linear and piecewise constant effects

#rm(list = ls())

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
Sigma <- matrix(0,10,10)
Sigma[1:5,1:5]   <- 0.9
Sigma[6:10,6:10] <- 0.5
Sigma[1:5,6:10]  <- 0.2
Sigma[6:10,1:5]  <- 0.2
diag(Sigma)      <- 1
beta <- rep(0,10)
omega <- 100
sd <- 0.1

nTrain <- 2000
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

Shapley.true = list(exactShap = Kshap, other_objects = list(ll = ll, DT = DT,W_kernel=W_kernel))



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


#### Comparing the true and approximate values -------------



# Mean absolute errors per variable (to see if the performance differ between variables)
(absmeans.sigma.01 = colMeans(abs(Shapley.true$exactShap[,-1]-Shapley.approx$sigma.01$Kshap[,-1])))
(absmeans.sigma.03 = colMeans(abs(Shapley.true$exactShap[,-1]-Shapley.approx$sigma.03$Kshap[,-1])))
(absmeans.indep = colMeans(abs(Shapley.true$exactShap[,-1]-Shapley.approx$indep$Kshap[,-1])))
(absmeans.Gauss = colMeans(abs(Shapley.true$exactShap[,-1]-Shapley.approx$Gauss$Kshap[,-1])))
#(absmeans.sigma.AICc = colMeans(abs(Shapley.true$exactShap[,-1]-Shapley.approx$sigma.AICc$Kshap[,-1])))

(absmeans.treeSHAP = colMeans(abs(Shapley.true$exactShap[,-1]-Shapley.tree[,-1]))) # Additional one


# Mean of the absolute errors over all variables
# Mean of the absolute errors over all variables
res_to_paper <- data.frame(S_KS=mean(absmeans.indep),G_KS = mean(absmeans.Gauss),E_KS_0.1=mean(absmeans.sigma.01),E_KS_0.3=mean(absmeans.sigma.03),TreeSHAP = mean(absmeans.treeSHAP))
spec <- data.frame(gx="Piecewise constant+linear",fx="XGBoost",px="genHyp_simple",rho=NA)
res_to_paper <- cbind(spec,res_to_paper)


#### Results!!!

#gx      fx            px rho      S_KS      G_KS  E_KS_0.1  E_KS_0.3  TreeSHAP
#1 Piecewise constant+linear XGBoost genHyp_simple  NA 0.2410484 0.1154103 0.1469652 0.1781528 0.2333389#

#> (absmeans.sigma.01 = colMeans(abs(Shapley.true$exactShap[,-1]-Shapley.approx$sigma.01$Kshap[,-1])))
#[1] 0.1492870 0.1199400 0.1555138 0.1355401 0.1467319 0.1437373 0.1455014 0.1081758 0.1899633 0.1752616
#> (absmeans.sigma.03 = colMeans(abs(Shapley.true$exactShap[,-1]-Shapley.approx$sigma.03$Kshap[,-1])))
#[1] 0.15635382 0.20352307 0.17570933 0.09856262 0.11108611 0.17876933 0.16681691 0.11993681 0.25312315 0.31764714
#> (absmeans.indep = colMeans(abs(Shapley.true$exactShap[,-1]-Shapley.approx$indep$Kshap[,-1])))
#[1] 0.1822272 0.2716045 0.2146963 0.1041776 0.1178034 0.2472463 0.2276233 0.2003818 0.3611129 0.4836105
#> (absmeans.Gauss = colMeans(abs(Shapley.true$exactShap[,-1]-Shapley.approx$Gauss$Kshap[,-1])))
#[1] 0.1127418 0.1108990 0.1162368 0.1105771 0.1176708 0.1213112 0.1170605 0.1117043 0.1201605 0.1157415
#> (absmeans.treeSHAP = colMeans(abs(Shapley.true$exactShap[,-1]-Shapley.tree[,-1]))) # Additional one
#V1        V2        V3        V4        V5        V6        V7        V8        V9       V10
#0.1801251 0.2549447 0.1885090 0.1066122 0.1178764 0.2491502 0.2267156 0.1981335 0.3855008 0.4258214



# Insert ranking based measures etc. here as well.

