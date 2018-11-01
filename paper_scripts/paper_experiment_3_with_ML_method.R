

#### PAPER EXPERIMENT FRAMEWORK ####
#### Use the current setup for all experiements in the paper to ease reproducablity etc.

#### Example 3 ####
# Linear model with highly dependent Gaussian features

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
    y <- -X[,1]^2 + 0.5*X[,2]^2 + 0.5*X[,3]^3 - 0.1*X[,3]^2 + rnorm(n = n,mean=0,sd=sd)
}

samp_model <- function(n,X,sd){
    y <- sin(2*X[,1]) + 0.0*X[,2] + 0.5*(X[,3]>0) - 0.5*(X[,3]<0) + rnorm(n = n,mean=0,sd=sd)
}

samp_model <- function(n,X,sd){
    y <- sin(2*X[,1]) + 0.0*X[,2] + 1*(X[,3]>0) - 1*(X[,3]<0) +(rbinom(n=n,1,prob=0.5)-0.5)*4 + rnorm(n = n,mean=0,sd=sd)
}

samp_model <- function(n,X,sd){
    y <- sin(2*X[,1]) + 0.0*X[,2] + 1*(X[,3]>0)*(X[,2]) - 1*(X[,3]<0)*X[,2] +(rbinom(n=n,1,prob=0.5)-0.5)*4 + rnorm(n = n,mean=0,sd=sd)
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

#model = lm(y~.,data=XYtrain)
form <- as.formula(paste0("y~",paste0("s(",colnames(XYtrain)[1:3],")",collapse="+")))
model = mgcv::gam(form,data=XYtrain)

model = mgcv::gam(y~s(X1,X2,X3),data=XYtrain,gamma = 0.1)

#pairs(XYtrain,pch=".")


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

########### ML STUFF ¤¤¤¤¤¤¤¤¤¤¤
# CONTINUE FIXING STUFF BELOW HERE!!!! ###########

############# DOING THE ML-thing for fitting submodels

dat = cbind(Xtrain,pred=pred_vector(model,Xtrain))
submodel.list.ML <- list()

pairs(dat)

### testing

set.seed(123)
XYtrain.ML <- data.table(samp_variables(n = 10000,
                                     pi.G = pi.G,
                                     mu.list = mu.list,
                                     Sigma.list = Sigma.list))

#XYtrain.ML <- as.data.table(expand.grid(X1=seq(-3,3,0.1),
#                                        X2=seq(-3,3,0.1),
#                                        X3=seq(-3,3,0.1)))

XYtrain.ML[,y:=samp_model(.N,.SD,sd=sd)]

dat.ML = cbind(XYtrain.ML,pred=pred_vector(model,XYtrain.ML))

#k.mat.ML <- matrix(NA,ncol=2^m,nrow=nrow(Xtest))

#DT.ML.list <- list()
#DT.ML.list[[1]] <- data.table(wcomb=1,k=pred_zero,id=1:nrow(Xtest))
#DT.ML.list[[2^m]] <- data.table(wcomb=2^m,k=pred_vector(model,Xtest),id=1:nrow(Xtest))

for (i in 2:(2^m-1)){
    formula <- paste0("pred~",paste0("X",which(l$S[i,]==1),collapse="+"))

    submodel.list.ML[[i]] = ranger::ranger(
        formula = as.formula(formula),
        data = dat.ML,
        num.trees = 1000,
        num.threads = 20,
        min.node.size = 10,
        replace = F,
        sample.fraction = 0.2,
        splitrule = "extratrees",
        num.random.splits = 3,
        verbose = TRUE,
        importance = "none")

    # DT.ML.list[[i]] <- data.table(wcomb=i,k=pred_vector(model = submodel.list.ML[[i]],data = Xtest),id=1:nrow(Xtest))
}

submodel.list.ML.local <- list()

for (i in 2:(2^m-1)){
#    formula <- paste0("pred~",paste0("X",which(l$S[i,]==1),collapse="+"))

    submodel.list.ML.local[[i]] = grf::local_linear_forest(
        X = as.data.frame(dat.ML)[,which(l$S[i,]==1),drop=FALSE],
        Y = dat.ML$pred,
        sample.fraction = 0.2,
        mtry = 1,
        min.node.size = 10,
        num.trees = 500,
        num.threads = 20,
        tune.parameters = F)

    # DT.ML.list[[i]] <- data.table(wcomb=i,k=pred_vector(model = submodel.list.ML[[i]],data = Xtest),id=1:nrow(Xtest))
}





# DT.ML <- rbindlist(DT.ML.list)
# setkey(DT.ML,"id")
#
# DT.ML.mat <- matrix(DT.ML$k,ncol=2^m,nrow=nrow(Xtest),byrow = T)
#
# Kshap.ML <- matrix(0, nrow = nrow(l$Xtest), ncol = nrow(l$W))
# for (i in l$Xtest[, .I]) {
#     Kshap.ML[i, ] = l$W %*% DT.ML[id == i, k]
# }
#
# head(Kshap.ML)

# for a specific submodel
#plot(seq(-3,3,0.01),pred_vector(model=submodel,data=cbind(X1=seq(-3,3,0.01))),type='l')

### Trying a gam version
submodel.list.gam <- list()

#DT.gam.list <- list()
#DT.gam.list[[1]] <- data.table(wcomb=1,k=pred_zero,id=1:nrow(Xtest))
#DT.gam.list[[2^m]] <- data.table(wcomb=2^m,k=pred_vector(model,Xtest),id=1:nrow(Xtest))

for (i in 2:(2^m-1)){
    formula <- paste0("pred~",paste0("s(X",which(l$S[i,]==1),")",collapse="+"))

    submodel.list.gam[[i]] = mgcv::gam(
        formula = as.formula(formula),
        data = dat)
    #DT.gam.list[[i]] <- data.table(wcomb=i,k=predict(object = submodel.list.gam[[i]],newdata = Xtest),id=1:nrow(Xtest))
}

# DT.gam <- rbindlist(DT.gam.list)
# setkey(DT.gam,"id")
#
# DT.gam.mat <- matrix(DT.gam$k,ncol=2^m,nrow=nrow(Xtest),byrow = T)
#
# Kshap.gam <- matrix(0, nrow = nrow(l$Xtest), ncol = nrow(l$W))
# for (i in l$Xtest[, .I]) {
#     Kshap.gam[i, ] = l$W %*% DT.gam[id == i, k]
# }
#
# head(Kshap.gam)

### Trying a neural net version

# initializing the h2o pacakges

library(h2o)
h2o.init(nthreads = 12)


dat[,const := 1]

#XYtest[,const := 1]
#XYtest[,pred:=pred_vector(model,Xtest)]

dat.h2o <- as.h2o(dat)
#XYtest.h2o <- as.h2o(XYtest)

submodel.list.nnet <- list()

#DT.nnet.list <- list()
#DT.nnet.list[[1]] <- data.table(wcomb=1,k=pred_zero,id=1:nrow(Xtest))
#DT.nnet.list[[2^m]] <- data.table(wcomb=2^m,k=pred_vector(model,Xtest),id=1:nrow(Xtest))

for (i in 2:(2^m-1)){
    these.x <- c(paste0("X",which(l$S[i,]==1)),"const")

    submodel.list.nnet[[i]] <- h2o::h2o.deeplearning(x=these.x,
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

    #DT.nnet.list[[i]] <- data.table(wcomb=i,k=as.vector(predict(object = submodel.list.nnet[[i]],newdata = XYtest.h2o)),id=1:nrow(Xtest))
}

#DT.nnet <- rbindlist(DT.nnet.list)
#setkey(DT.nnet,"id")

#DT.nnet.mat <- matrix(DT.nnet$k,ncol=2^m,nrow=nrow(Xtest),byrow = T)

#Kshap.nnet <- matrix(0, nrow = nrow(l$Xtest), ncol = nrow(l$W))
#for (i in l$Xtest[, .I]) {
#    Kshap.nnet[i, ] = l$W %*% DT.nnet[id == i, k]
#}

#head(Kshap.nnet)


#plot(seq(-3,3,0.01),as.vector(predict(object=submodel,newdata=as.h2o(data.frame(X1=seq(-3,3,0.01),const=1)))),type='l')
a <- -3
b <- 3
h <- (b-a)/50
X.grid <-seq(a+h/2, b-h/2, by=h) %x%t(rep(1,p))
#X.grid <-expand.grid(as.data.frame(X.grid0))


#h <- 0.2
Xtest0 <- cbind(seq(-2,2,by=0.1),seq(-2,2,by=0.1),seq(-2,2,by=0.1))
colnames(Xtest0) <- colnames(Xtest)
#X.grid <- expand.grid(as.data.frame(Xtest0))
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



Xtest0.h20 <- as.h2o(cbind(Xtest0,const=1))
XYtrain.df <- as.data.frame(dat.ML)

par(mfrow=c(2,2))
for (i in 2:4){
    plot(Xtest0[,1],trueValues.mat[,i],type="l",ylim=c(-5,5))
    points(XYtrain.df[,i-1],XYtrain.df$pred,pch=".")

    pred.ML <- pred_vector(model = submodel.list.ML[[i]],data = Xtest0)
    pred.gam <- predict(object = submodel.list.gam[[i]],newdata = as.data.frame(Xtest0))
#    pred.nnet <- as.vector(predict(object = submodel.list.nnet[[i]],newdata = Xtest0.h20))
    pred.ML.local <- predict(submodel.list.ML.local[[i]],newdata = Xtest0)$predictions
    lines(Xtest0[,1],pred.ML,col=2)
    lines(Xtest0[,1],pred.gam,col=3)
#    lines(Xtest0[,1],pred.nnet,col=4)
    lines(Xtest0[,1],pred.ML.local,col=6,lwd=2)
}



plot(Xtest0[,1],trueValues.mat[,2],type="l",ylim=c(2,6),col=2)
points(dat$X1,dat$pred)
lines(seq(-3,3,0.01),as.vector(predict(object=submodel,newdata=as.h2o(data.frame(X1=seq(-3,3,0.01),const=1)))),type='l',col=3)


##############3






############ OLD

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
spec <- data.frame(gx="Linear",fx="Linear",px="Gaussian",rho=0.7)
res_to_paper <- cbind(spec,res_to_paper)
#S_KS       G_KS   E_KS_0.1   E_KS_0.3
#0.32338379 0.01657350 0.02612238 0.03026733

# Insert ranking based measures etc. here as well.

# Mahalanbis standard
#S_KS       G_KS   E_KS_0.1   E_KS_0.3  E_KS_AICc
#0.32359089 0.01658123 0.03651685 0.02677809 0.02578491

# Mahalanbis scaled, but not when optimizing AICc
#S_KS       G_KS   E_KS_0.1   E_KS_0.3  E_KS_AICc
#0.32359089 0.01658123 0.02962848 0.02861052 0.02377968

# Mahalanobis scaled, also when optimizing.
#S_KS       G_KS   E_KS_0.1   E_KS_0.3  E_KS_AICc
#0.32359089 0.01658123 0.02962848 0.02861052 0.02437970

