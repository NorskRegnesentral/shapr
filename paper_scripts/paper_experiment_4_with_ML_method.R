

#### PAPER EXPERIMENT FRAMEWORK ####
#### Use the current setup for all experiements in the paper to ease reproducablity etc.

#### Example 6 ####
# Random Forest with Gaussian mixture distributed features.

#rm(list = ls())

library(shapr)
library(data.table)
library(mvtnorm)
library(condMVNorm)

source("paper_scripts/paper_helper_funcs.R")

mu.list = list(c(0,0,0))
Sigma.list <- list(matrix(c(1,0.7,0.7,
                            0.7,1,0.7,
                            0.7,0.7,1),ncol=3))
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

model = ranger::ranger(
    formula = y~.,
    data = XYtrain,
    num.trees = 50,
    num.threads = 3,
    verbose = TRUE,
    importance = "impurity",
    mtry=2)



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

# for a specific submodel
#plot(seq(-3,3,0.01),pred_vector(model=submodel,data=cbind(X1=seq(-3,3,0.01))),type='l')

### Trying a gam version


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

### Trying a neural net version

# initializing the h2o pacakges

library(h2o)
h2o.init(nthreads = 12)


dat[,const := 1]

XYtest[,const := 1]
XYtest[,pred:=pred_vector(model,Xtest)]

dat.h2o <- as.h2o(dat)
XYtest.h2o <- as.h2o(XYtest)



DT.nnet.list <- list()
DT.nnet.list[[1]] <- data.table(wcomb=1,k=pred_zero,id=1:nrow(Xtest))
DT.nnet.list[[2^m]] <- data.table(wcomb=2^m,k=pred_vector(model,Xtest),id=1:nrow(Xtest))

for (i in 2:(2^m-1)){
    these.x <- c(paste0("X",which(l$S[i,]==1)),"const")

    submodel <- h2o::h2o.deeplearning(x=these.x,
                                      y="pred",
                                      training_frame = dat.h2o,
#                                      validation_frame = XYtest.h2o, # For initial testing just now
                                      nfolds = 5,
                                      activation = "MaxoutWithDropout",
                                      hidden = c(100,100,100),
                                      seed = 1234,
                                      input_dropout_ratio = 0.2,
                                      hidden_dropout_ratio = c(0.1,0.1,0.1),
                                      l2 = 0.00001,
                                      distribution = "gaussian",
                                      ignore_const_cols = F)

    DT.nnet.list[[i]] <- data.table(wcomb=i,k=as.vector(predict(object = submodel,newdata = XYtest.h2o)),id=1:nrow(Xtest))
}

DT.nnet <- rbindlist(DT.nnet.list)
setkey(DT.nnet,"id")

DT.nnet.mat <- matrix(DT.nnet$k,ncol=2^m,nrow=nrow(Xtest),byrow = T)

Kshap.nnet <- matrix(0, nrow = nrow(l$Xtest), ncol = nrow(l$W))
for (i in l$Xtest[, .I]) {
    Kshap.nnet[i, ] = l$W %*% DT.nnet[id == i, k]
}

head(Kshap.nnet)


plot(seq(-3,3,0.01),as.vector(predict(object=submodel,newdata=as.h2o(data.frame(X1=seq(-3,3,0.01),const=1)))),type='l')


h <- 0.05
Xtest0 <- cbind(seq(-3,3,by=h),seq(-3,3,by=h),seq(-3,3,by=h))
colnames(Xtest0) <- colnames(Xtest)
X.grid <- Xtest0
trueValues.mat = matrix(NA,ncol=2^3,nrow=nrow(Xtest0))

for (i in (2:4)){
    given.inds = which(l$S[i,]==1)
    integrate.inds = which(l$S[i,]==0)
    if(length(integrate.inds)==1){
        trueValues.mat[,i]=integrator.1D.func(h = h,
                                              integrate.inds=integrate.inds,
                                              given.inds=given.inds,
                                              Xtest= as.matrix(Xtest0),
                                              model = model,
                                              X.grid = X.grid,
                                              pi.G = pi.G,
                                              mu.list = mu.list,
                                              Sigma.list = Sigma.list)
    } else {
        trueValues.mat[,i]=integrator.2D.func(h = h,
                                              integrate.inds=integrate.inds,
                                              given.inds=given.inds,
                                              Xtest= as.matrix(Xtest0),
                                              model = model,
                                              X.grid = X.grid,
                                              pi.G = pi.G,
                                              mu.list = mu.list,
                                              Sigma.list = Sigma.list)

    }
}


plot(Xtest0[,1],trueValues.mat[,2],type="l",ylim=c(2,6),col=2)
points(dat$X1,dat$pred)
lines(seq(-3,3,0.01),as.vector(predict(object=submodel,newdata=as.h2o(data.frame(X1=seq(-3,3,0.01),const=1)))),type='l',col=3)


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
                                               sigma = mean(h.optim.mat,na.rm=T), # 0.29
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
(absmeans.nnet = colMeans(abs(Shapley.true$exactShap[,-1]-Kshap.nnet[,-1])))


# Mean of the absolute errors over all variables
res_to_paper <- data.frame(S_KS=mean(absmeans.indep),G_KS = mean(absmeans.Gauss),E_KS_0.1=mean(absmeans.sigma.01),E_KS_0.3=mean(absmeans.sigma.03),E_KS_AICc=mean(absmeans.sigma.AICc),TreeSHAP = NA,ML_KS=mean(absmeans.ML),gam_KS=mean(absmeans.gam),nnet_KS=mean(absmeans.nnet))
spec <- data.frame(gx="Piecewise constant",fx="Random Forest",px="Gaussian",rho=0.7)
res_to_paper <- cbind(spec,res_to_paper)


for (i in 1:length(Shapley.approx)){
    Shapley.approx[[i]]$other_objects$DT.mat <- matrix(Shapley.approx[[i]]$other_objects$DT$k,ncol=2^m,nrow=nrow(Xtest),byrow = T)
}


absmeans.cond.mat <- matrix(NA,ncol=2^m,nrow=8)

# Mean absolute errors per variable (to see if the performance differ between variables), for the conditional expectation
(absmeans.cond.mat[1,] = colMeans(abs(Shapley.true$trueValue.mat-Shapley.approx$sigma.01$other_objects$DT.mat)))
(absmeans.cond.mat[2,] = colMeans(abs(Shapley.true$trueValue.mat-Shapley.approx$sigma.03$other_objects$DT.mat)))
(absmeans.cond.mat[3,] = colMeans(abs(Shapley.true$trueValue.mat-Shapley.approx$indep$other_objects$DT.mat)))
(absmeans.cond.mat[4,] = colMeans(abs(Shapley.true$trueValue.mat-Shapley.approx$Gauss$other_objects$DT.mat)))
(absmeans.cond.mat[5,] = colMeans(abs(Shapley.true$trueValue.mat-Shapley.approx$sigma.AICc$other_objects$DT.mat)))
(absmeans.cond.mat[6,] = colMeans(abs(Shapley.true$trueValue.mat-DT.ML.mat)))
(absmeans.cond.mat[7,] = colMeans(abs(Shapley.true$trueValue.mat-DT.gam.mat)))
(absmeans.cond.mat[8,] = colMeans(abs(Shapley.true$trueValue.mat-DT.nnet.mat)))

rownames(absmeans.cond.mat) = c(names(Shapley.approx),"ML","gam","nnet")

absmeans.cond.mat

#
#> absmeans.cond.mat
#[,1]       [,2]       [,3]       [,4]       [,5]       [,6]       [,7]         [,8]
#sigma.01   0.000000e+00 0.03949819 0.02392552 0.03630293 0.05613896 0.05245519 0.04911206 9.103829e-17
#sigma.03   0.000000e+00 0.02811434 0.04331281 0.02869783 0.05078068 0.05209246 0.03228684 8.881784e-19
#indep      2.842171e-17 0.03491153 0.46805332 0.02971503 0.25971182 0.25551300 0.26868009 2.664535e-18
#Gauss      0.000000e+00 0.01460872 0.01645234 0.02041756 0.01280246 0.01281789 0.01391688 0.000000e+00
#sigma.AICc 0.000000e+00 0.02854151 0.04119780 0.02880875 0.04886857 0.05008996 0.03051651 2.664535e-18
#ML         0.000000e+00 0.21770283 0.28490698 0.21209498 0.11120344 0.09088366 0.10365850 0.000000e+00
#gam        0.000000e+00 0.10021393 0.03205522 0.09633286 0.11139548 0.14540447 0.10809066 0.000000e+00

#> res_to_paper
#gx            fx       px rho      S_KS       G_KS   E_KS_0.1   E_KS_0.3  E_KS_AICc TreeSHAP     ML_KS     gam_KS
#1 Piecewise constant Random Forest Gaussian 0.7 0.2116667 0.00931992 0.03029357 0.03227759 0.03110385       NA 0.1114895 0.07112975

### With more training data -- essentially same results
# [,1]       [,2]       [,3]       [,4]       [,5]       [,6]       [,7]         [,8]
# sigma.01   4.440892e-16 0.02330711 0.02865502 0.02811145 0.03612720 0.04040346 0.02888787 4.130030e-17
# sigma.03   4.440892e-16 0.02231683 0.03769237 0.02679122 0.04527795 0.05244066 0.03095744 3.552714e-18
# indep      1.501022e-16 0.03336392 0.47565241 0.03259703 0.26831992 0.26129677 0.26497268 5.284662e-17
# Gauss      0.000000e+00 0.01643731 0.02263342 0.01883695 0.01547321 0.01442671 0.01161813 0.000000e+00
# sigma.AICc 4.440892e-16 0.02182958 0.03517798 0.02612188 0.04117766 0.04775078 0.02664812 3.108624e-18
# ML         0.000000e+00 0.21678349 0.31337607 0.22172979 0.11398401 0.09609041 0.10854116 0.000000e+00
# gam        0.000000e+00 0.10016614 0.04027871 0.10074545 0.11662945 0.15183974 0.11748291 0.000000e+00
#
#gx            fx       px rho      S_KS        G_KS   E_KS_0.1   E_KS_0.3  E_KS_AICc TreeSHAP     ML_KS     gam_KS
#1 Piecewise constant Random Forest Gaussian 0.7 0.2146501 0.009525011 0.02178125 0.02957226 0.02693061       NA 0.1163987 0.07585307


