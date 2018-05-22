

#### PAPER EXPERIMENT FRAMEWORK ####
#### Use the current setup for all experiements in the paper to ease reproducablity etc.

#### Example 1 ####
# XGBoost with Gaussian mixture distributed features.

rm(list = ls())

library(shapr)
library(data.table)
library(mvtnorm)
library(condMVNorm)
library(xgboost)

source("paper_scripts/paper_helper_funcs.R")

mu.list = list(c(0,0,0),c(10,-5,10))
Sigma.list <- list(matrix(c(1,0.7,0.7,
                            0.7,1,0.7,
                            0.7,0.7,1),ncol=3),
                   matrix(c(1,0.7,0.7,
                            0.7,1,0.7,
                            0.7,0.7,1),ncol=3))
pi.G <- c(0.5,0.5)

sd = 0.1

nTrain <- 10000
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
    y <- 0.5*X[,2]  +  (X[,1]<0)*1 + (X[,2]<2) + (X[,2]>4)*1 + (X[,3]<10)*1 + (X[,3]<0)*1 + (X[,1]>-5)*(X[,2]<4)*1+ rnorm(n = n,mean=0,sd=sd)
    return(y)
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
    nrows = 1e4
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
                                          sigma = 0, # sigma==0 gives the special case of independence (NOTE: NOT the same as setting sigma= 10^10)
                                          w_threshold = w_threshold,
                                          n_threshold = n_threshold,
                                          verbose = FALSE,
                                          gaussian_sample = FALSE,
                                          pred_zero=pred_zero)

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

Shapley.tree = predict(model,xgb.test,predcontrib=T)
Shapley.tree = Shapley.tree[,c(m+1,1:m)]

#### Comparing the true and approximate values -------------

# Mean absolute errors per variable (to see if the performance differ between variables)
(absmeans.sigma.01 = colMeans(abs(Shapley.true[,-1]-Shapley.approx$sigma.01$Kshap[,-1])))
(absmeans.sigma.03 = colMeans(abs(Shapley.true[,-1]-Shapley.approx$sigma.03$Kshap[,-1])))
(absmeans.indep = colMeans(abs(Shapley.true[,-1]-Shapley.approx$indep$Kshap[,-1])))
(absmeans.Gauss = colMeans(abs(Shapley.true[,-1]-Shapley.approx$Gauss$Kshap[,-1])))
(absmeans.treeSHAP = colMeans(abs(Shapley.true[,-1]-Shapley.tree[,-1])))

# Mean of the absolute errors over all variables
res_to_paper <- c(S_KS=mean(absmeans.indep),G_KS = mean(absmeans.Gauss),E_KS_0.1=mean(absmeans.sigma.01),E_KS_0.3=mean(absmeans.sigma.03),TreeSHAP = mean(absmeans.treeSHAP))
res_to_paper
#S_KS       G_KS   E_KS_0.1   E_KS_0.3   TreeSHAP
#0.34820946 0.43440090 0.01641733 0.01956438 0.53103073

# > head(Shapley.true)
# [,1]       [,2]       [,3]        [,4]
# [1,] 1.967222  1.0977997  0.6804683  0.01954934
# [2,] 1.967222  1.0481261  0.6882733  1.06880830
# [3,] 1.967216 -0.7057892 -0.1752125 -0.69093237
# [4,] 1.967221  0.4420589  0.7468005  0.40357911
# [5,] 1.967222  1.1982615  0.8750538  1.24919635
# [6,] 1.967216 -0.6842513 -0.8658663 -1.17223433
# > head(Shapley.approx$sigma.01$Kshap)
# [,1]       [,2]       [,3]        [,4]
# [1,] 1.967222  1.0685953  0.6944469  0.03477513
# [2,] 1.967222  1.0398454  0.7107238  1.05463846
# [3,] 1.967216 -0.7101268 -0.1907009 -0.67110649
# [4,] 1.967221  0.4335967  0.7707739  0.38806796
# [5,] 1.967222  1.1458725  0.9400747  1.23656453
# [6,] 1.967216 -0.6789063 -0.8825749 -1.16087085
# > head(Shapley.approx$sigma.03$Kshap)
# [,1]       [,2]       [,3]        [,4]
# [1,] 1.967222  1.0814289  0.6870695  0.02931905
# [2,] 1.967222  1.0424219  0.7066151  1.05617066
# [3,] 1.967216 -0.7076935 -0.1375420 -0.72669864
# [4,] 1.967221  0.4147620  0.7960784  0.38159818
# [5,] 1.967222  1.1722584  0.9032127  1.24704063
# [6,] 1.967216 -0.6788523 -0.8795635 -1.16393621
# > head(Shapley.approx$indep$Kshap)
# [,1]       [,2]        [,3]       [,4]
# [1,] 1.967219  1.4356721  0.49482024 -0.1326743
# [2,] 1.967219  1.5733779  0.70123652  0.5305938
# [3,] 1.967219 -1.0435808  0.02296052 -0.5513144
# [4,] 1.967219  0.7119209  1.07177356 -0.1912554
# [5,] 1.967218  1.6745170  1.03857816  0.6094169
# [6,] 1.967219 -1.2644230 -0.89442034 -0.5635091
# > head(Shapley.approx$Gauss$Kshap)
# [,1]        [,2]       [,3]        [,4]
# [1,] 1.967222  1.17794001  0.5019815  0.11789619
# [2,] 1.967222  1.15240997  0.3973654  1.25543236
# [3,] 1.967217 -1.11820372  1.2369343 -1.69066419
# [4,] 1.967221  0.08669377  1.5172152 -0.01147065
# [5,] 1.967222  1.25738639  0.7392101  1.32591460
# [6,] 1.967217 -0.85652476 -0.2567578 -1.60906796
# > head(Shapley.tree)
# BIAS         X1         X2         X3
# [1,] 1.967214  1.7632607  0.3793693 -0.3448085
# [2,] 1.967214  1.9746418  0.5461401  0.2844310
# [3,] 1.967214 -1.4163880  0.2028517 -0.3583940
# [4,] 1.967214  0.9595515  0.8032548 -0.1703618
# [5,] 1.967214  2.0893619  0.9166681  0.3164874
# [6,] 1.967214 -1.6430440 -0.7250342 -0.3542672
>


# Insert ranking based measures etc. here as well.

