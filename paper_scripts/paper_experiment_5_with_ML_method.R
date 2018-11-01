

#### PAPER EXPERIMENT FRAMEWORK ####
#### Use the current setup for all experiements in the paper to ease reproducablity etc.

#### Example 5 ####
# Linear  model with Gaussian mixture distributed features.

#rm(list = ls())

library(shapr)
library(data.table)
library(mvtnorm)
library(condMVNorm)

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

nTrain <- 5000
nTest <- 1000


#### Defining the true distribution of the variables and the model------

samp_variables <- function(n,pi.G,mu.list,Sigma.list){

    X <- joint.samp.func(n = n,
                         pi.G,
                         mu.list,
                         Sigma.list)
    return(X)
}

# Old version
#samp_model <- function(n,X,sd){
#    y <- 0.5*X[,1] + 0.5*X[,2] + abs(X[,3]) + rnorm(n = n,mean=0,sd=sd)
#}
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

#### Computing the various Shapley approximations --------

w_threshold = 1 # For a fairer comparison, all models use the same number of samples (n_threshold)
n_threshold = 10^3

############# DOING THE ML-thing for fitting submodels

dat = cbind(Xtrain,pred=pred_vector(model,Xtrain))
submodel.list <- list()

#k.mat.ML <- matrix(NA,ncol=2^m,nrow=nrow(Xtest))

DT.ML.list <- list()
DT.ML.list[[1]] <- data.table(wcomb=1,k=pred_zero,id=1:nrow(Xtest))
DT.ML.list[[2^m]] <- data.table(wcomb=2^m,k=pred_vector(model,Xtest),id=1:nrow(Xtest))

for (i in 2:(2^m-1)){
    formula <- paste0("pred~",paste0("X",which(l$S[i,]==1),collapse="+"))

    submodel = ranger::ranger(
        formula = as.formula(formula),
        data = dat,
        num.trees = 500,
        num.threads = 3,
        verbose = TRUE,
        importance = "impurity")

    DT.ML.list[[i]] <- data.table(wcomb=i,k=pred_vector(model = submodel,data = Xtest),id=1:nrow(Xtest))
}

DT.ML <- rbindlist(DT.ML.list)
setkey(DT.ML,"id")

DT.ML.mat <- matrix(DT.ML$k,ncol=2^m,nrow=nrow(Xtest),byrow = T)

Kshap.ML <- matrix(0, nrow = nrow(l$Xtest), ncol = nrow(l$W))
for (i in l$Xtest[, .I]) {
    Kshap.ML[i, ] = l$W %*% DT.ML[id == i, k]
}

head(Kshap.ML)

DT.gam.list <- list()
DT.gam.list[[1]] <- data.table(wcomb=1,k=pred_zero,id=1:nrow(Xtest))
DT.gam.list[[2^m]] <- data.table(wcomb=2^m,k=pred_vector(model,Xtest),id=1:nrow(Xtest))

for (i in 2:(2^m-1)){
    formula <- paste0("pred~",paste0("s(X",which(l$S[i,]==1),")",collapse="+"))

    submodel = mgcv::gam(
        formula = as.formula(formula),
        data = dat)
    DT.gam.list[[i]] <- data.table(wcomb=i,k=predict(object = submodel,newdata = Xtest),id=1:nrow(Xtest))
}

DT.gam <- rbindlist(DT.gam.list)
setkey(DT.gam,"id")

DT.gam.mat <- matrix(DT.gam$k,ncol=2^m,nrow=nrow(Xtest),byrow = T)

Kshap.gam <- matrix(0, nrow = nrow(l$Xtest), ncol = nrow(l$W))
for (i in l$Xtest[, .I]) {
    Kshap.gam[i, ] = l$W %*% DT.gam[id == i, k]
}

head(Kshap.gam)



##############3

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
                                               sigma = mean(h.optim.mat,na.rm=T), # approx 0.05
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
(absmeans.ML = colMeans(abs(Shapley.true$exactShap[,-1]-Kshap.ML[,-1])))
(absmeans.gam = colMeans(abs(Shapley.true$exactShap[,-1]-Kshap.gam[,-1])))


# Mean of the absolute errors over all variables
res_to_paper <- data.frame(S_KS=mean(absmeans.indep),G_KS = mean(absmeans.Gauss),E_KS_0.1=mean(absmeans.sigma.01),E_KS_0.3=mean(absmeans.sigma.03),E_KS_AICc=mean(absmeans.sigma.AICc),TreeSHAP = NA,ML_KS=mean(absmeans.ML),gam_KS=mean(absmeans.gam))
spec <- data.frame(gx="Linear",fx="Linear",px="Gaussian mix",rho=NA)
res_to_paper <- cbind(spec,res_to_paper)
#gx     fx           px rho     S_KS      G_KS  E_KS_0.1  E_KS_0.3  E_KS_AICc TreeSHAP     ML_KS    gam_KS
#1 Linear Linear Gaussian mix  NA 3.297714 0.7440238 0.1130524 0.2420481 0.08655289       NA 0.2791263 0.1146308


for (i in 1:length(Shapley.approx)){
    Shapley.approx[[i]]$other_objects$DT.mat <- matrix(Shapley.approx[[i]]$other_objects$DT$k,ncol=2^m,nrow=nrow(Xtest),byrow = T)
}


absmeans.cond.mat <- matrix(NA,ncol=2^m,nrow=7)

# Mean absolute errors per variable (to see if the performance differ between variables), for the conditional expectation
(absmeans.cond.mat[1,] = colMeans(abs(Shapley.true$trueValue.mat-Shapley.approx$sigma.01$other_objects$DT.mat)))
(absmeans.cond.mat[2,] = colMeans(abs(Shapley.true$trueValue.mat-Shapley.approx$sigma.03$other_objects$DT.mat)))
(absmeans.cond.mat[3,] = colMeans(abs(Shapley.true$trueValue.mat-Shapley.approx$indep$other_objects$DT.mat)))
(absmeans.cond.mat[4,] = colMeans(abs(Shapley.true$trueValue.mat-Shapley.approx$Gauss$other_objects$DT.mat)))
(absmeans.cond.mat[5,] = colMeans(abs(Shapley.true$trueValue.mat-Shapley.approx$sigma.AICc$other_objects$DT.mat)))
(absmeans.cond.mat[6,] = colMeans(abs(Shapley.true$trueValue.mat-DT.ML.mat)))
(absmeans.cond.mat[7,] = colMeans(abs(Shapley.true$trueValue.mat-DT.gam.mat)))

rownames(absmeans.cond.mat) = c(names(Shapley.approx),"ML","gam")

absmeans.cond.mat
#                  [,1]       [,2]      [,3]      [,4]       [,5]      [,6]       [,7]         [,8]
#sigma.01   0.00000e+00 0.21028313 0.1088653 0.2465614 0.08076987 0.2543496 0.06937719 1.662147e-16
#sigma.03   0.00000e+00 0.75631564 0.5797273 0.7827274 0.11285397 0.5095590 0.11567820 0.000000e+00
#indep      2.04281e-17 2.51544031 9.8397326 2.5354900 4.98955068 2.5028390 5.00344155 4.107825e-18
#Gauss      0.00000e+00 0.68679101 3.8487512 0.7072234 0.33387058 0.9505765 0.32074224 0.000000e+00
#sigma.AICc 0.00000e+00 0.07652348 0.1442559 0.1209491 0.09527161 0.1667475 0.08009056 2.355065e-16
#ML         0.00000e+00 0.54606405 0.7013736 0.5565618 0.23391613 0.2279948 0.23194355 0.000000e+00
#gam        0.00000e+00 0.05554351 0.4764523 0.05751773 0.06031072 0.06201966 0.04346594 0.000000e+00


#S_KS       G_KS   E_KS_0.1   E_KS_0.3
#1.70075624 0.74500897 0.02322865 0.02424549

# Insert ranking based measures etc. here as well.

