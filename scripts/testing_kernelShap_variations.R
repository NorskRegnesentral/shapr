#### TESTING DIFFERNET VERSIONS OF THE KERNELSHAP METHOD #####

rm(list=ls())

library(shapr)
library(data.table)
library(mvtnorm)
library(condMVNorm)
library(stringi)

library(xgboost)
library(GIGrvg)
library(ghyp)

####################### ONLY TOUCH THINGS IN THIS SECTION ################################
X_dim <- 3 # use 3,6,9,12,15
source.local <- ifelse(exists("source.local"),source.local,FALSE)

nTrain <- 2000
nTest <- 10
w_threshold = 1 # For a fairer comparison, all models use the same number of samples (n_threshold)
n_threshold = 10^3 # Number of samples used in the Monte Carlo integration

pi.G <- 1
sd_noise = 0.1
rho <- ifelse(exists("rho"),rho,0.5) # Do not edit
mu.list = list(rep(0,X_dim))
mat <- matrix(rho,ncol=X_dim,nrow=X_dim)
diag(mat) <- 1
Sigma.list <- list(mat)

#### Defining the true distribution of the variables and the model

samp_variables <- function(n,pi.G,mu.list,Sigma.list){

    X <- joint.samp.func(n = n,
                         pi.G,
                         mu.list,
                         Sigma.list)
    return(X)
}

samp_model <- function(n,X,sd_noise,X_dim){
    y <- stepwiseConstant_fun1(X[,1])*1 + stepwiseConstant_fun2(X[,2])*1 + stepwiseConstant_fun3(X[,3])*1  + rnorm(n = n,mean=0,sd=sd_noise)
        if(X_dim>=6){
        y <- y + stepwiseConstant_fun1(X[,4])*1 + stepwiseConstant_fun2(X[,5])*1 + stepwiseConstant_fun3(X[,6])*1
        }
    if(X_dim>=9){
        y <- y + stepwiseConstant_fun1(X[,7])*1 + stepwiseConstant_fun2(X[,8])*1 + stepwiseConstant_fun3(X[,9])*1
    }
    if(X_dim>=12){
        y <- y + stepwiseConstant_fun1(X[,10])*1 + stepwiseConstant_fun2(X[,11])*1 + stepwiseConstant_fun3(X[,12])*1
    }
    if(X_dim>=15){
        y <- y + stepwiseConstant_fun1(X[,13])*1 + stepwiseConstant_fun2(X[,14])*1 + stepwiseConstant_fun3(X[,15])*1
    }

    return(y)

}

fit_model_func <- function(XYtrain){
    xgb.train <- xgb.DMatrix(data = as.matrix(XYtrain[,-"y"]),
                             label = XYtrain[,y])

    params <- list(eta =  0.3,
                   objective = "reg:linear",
                   eval_metric = "rmse",
                   tree_method="hist") # gpu_hist

    model <- xgb.train(data = xgb.train,
                       params = params,
                       nrounds = 50,
                       print_every_n = 10,
                       ntread = 3)
    return(model)
}



source("paper_scripts/paper_helper_funcs.R",local = source.local) # Helper functions these experiments (mainly computing the true Shapley values)

set.seed(123)

#### Sampling train and test data ---------
XYtrain <- data.table(samp_variables(n = nTrain,
                                     pi.G = pi.G,
                                     mu.list = mu.list,
                                     Sigma.list = Sigma.list))
XYtest <- data.table(samp_variables(n = nTest,
                                    pi.G = pi.G,
                                    mu.list = mu.list,
                                    Sigma.list = Sigma.list))

XYtrain[,y:=samp_model(.N,.SD,sd_noise=sd_noise,X_dim=X_dim)]
Xtrain <- copy(XYtrain)
Xtrain[,y:=NULL]

XYtest[,y:=samp_model(.N,.SD,sd_noise=sd_noise,X_dim=X_dim)]
Xtest <- copy(XYtest)
Xtest[,y:=NULL]

pred_zero = XYtrain[, mean(y)]

#### Fitting the model ----------

model <- fit_model_func(XYtrain)

#### Pre computation before kernel shap ---------
# Creating the l object
l.exact <- prepare_kernelShap(m = X_dim,
                              Xtrain = Xtrain,
                              Xtest = Xtest,
                              exact = TRUE,
                              replace = FALSE,
                              nrows = NULL,
                              shapley_weight_inf_replacement = 10^6,
                              scale = FALSE,
                              use_shapley_weights_in_W = T,
                              normalize_W_weights = T,
                              distance_metric = "Mahalanobis_scaled",
                              compute_distances = FALSE,
                              normalize_distance_rows = TRUE)




l <- l.exact

feature_list <- l$X$features
n_threshold_true <- 10^4
p <- ncol(l$Xtest)
ll <- list()

for (i in l$Xtest[, .I]) { # This may be parallelized when the prediction function is not parallelized.
    print(sprintf("%d out of %d", i, l$Xtest[, .N]))

    Gauss_samp <- lapply(
        X = feature_list,
        FUN = samp_Gauss_func,
        n_threshold = n_threshold_true,
        mu = mu.list[[1]],
        Sigma = Sigma.list[[1]],
        p = p,
        Xtest = as.matrix(l$Xtest)[i, , drop = FALSE])


    DTp <- rbindlist(Gauss_samp, idcol = "wcomb")
    DTp[, w := 1 / n_threshold]
    DTp[wcomb %in% c(1, 2 ^ p), w := 1] # Adjust weights for zero and full model

    nms <- colnames(Xtest)

    DTp[!(wcomb %in% c(1, 2 ^ p)), p_hat := pred_vector(model = model, data = .SD), .SDcols = nms]
    if(nrow(Xtest)==1){
        DTp[wcomb == 2 ^ p, p_hat := pred_vector(model = model, data = as.data.frame(rbind(Xtest,Xtest)))[1]] # Just a hack for a single prediction
    } else {
        DTp[wcomb == 2 ^ p, p_hat := pred_vector(model = model, data = as.data.frame(as.matrix(l$Xtest)[i, , drop = FALSE]))]
    }
    DTp[wcomb == 1, p_hat := pred_zero]


    DTres <- DTp[, .(k = sum((p_hat * w) / sum(w))), wcomb]
    setkey(DTres, wcomb)
    ll[[i]] <- DTres
    ll[[i]][, id := i]
}

DT <- rbindlist(ll)

Shapley.true = list()
Shapley.true$exactShap <- matrix(NA,ncol=X_dim+1,nrow=nTest)


nrows_prop_vec <- c(0.5,1,2,5,10,50,100,500,10^3)#seq(1,20,1)
a <- 1
all.res.DT.list <- list()
for (nrows_prop in nrows_prop_vec){

    Shapley.approx <- list()

    set.seed(123)

    Shapley.approx$no_replacement_with_shapley_weights <- list()

    Shapley.approx$no_replacement_with_shapley_weights$l <- prepare_kernelShap(m = X_dim,
                                                                               Xtrain = Xtrain,
                                                                               Xtest = Xtest,
                                                                               exact = FALSE,
                                                                               replace = FALSE,
                                                                               nrows = round(nrows_prop*(2^X_dim)),
                                                                               shapley_weight_inf_replacement = 10^6,
                                                                               scale = FALSE,
                                                                               use_shapley_weights_in_W = T,
                                                                               normalize_W_weights = T,
                                                                               distance_metric = "Mahalanobis_scaled",
                                                                               compute_distances = FALSE,
                                                                               normalize_distance_rows = TRUE)
    set.seed(123)

    Shapley.approx$with_replacement_with_shapley_weights <- list()

    Shapley.approx$with_replacement_with_shapley_weights$l <- prepare_kernelShap(m = X_dim,
                                                                                 Xtrain = Xtrain,
                                                                                 Xtest = Xtest,
                                                                                 exact = FALSE,
                                                                                 replace = T,
                                                                                 nrows = round(nrows_prop*(2^X_dim)),
                                                                                 shapley_weight_inf_replacement = 10^6,
                                                                                 scale = FALSE,
                                                                                 use_shapley_weights_in_W = T,
                                                                                 normalize_W_weights = T,
                                                                                 distance_metric = "Mahalanobis_scaled",
                                                                                 compute_distances = FALSE,
                                                                                 normalize_distance_rows = TRUE)

    set.seed(123)

    Shapley.approx$with_replacement_no_shapley_weights <- list()

    Shapley.approx$with_replacement_no_shapley_weights$l <- prepare_kernelShap(m = X_dim,
                                                                               Xtrain = Xtrain,
                                                                               Xtest = Xtest,
                                                                               exact = FALSE,
                                                                               replace = T,
                                                                               nrows = round(nrows_prop*(2^X_dim)),
                                                                               shapley_weight_inf_replacement = 10^6,
                                                                               scale = FALSE,
                                                                               use_shapley_weights_in_W = F,
                                                                               normalize_W_weights = T,
                                                                               distance_metric = "Mahalanobis_scaled",
                                                                               compute_distances = FALSE,
                                                                               normalize_distance_rows = TRUE)
    # set.seed(123)
    #
    # Shapley.approx$with_replacement_with_shapley_weights_fulldim <- list()
    #
    # Shapley.approx$with_replacement_with_shapley_weights_fulldim$l <- prepare_kernelShap(m = X_dim,
    #                                                                              Xtrain = Xtrain,
    #                                                                              Xtest = Xtest,
    #                                                                              exact = FALSE,
    #                                                                              replace = T,
    #                                                                              nrows = round(nrows_prop*(2^X_dim)),
    #                                                                              shapley_weight_inf_replacement = 10^6,
    #                                                                              scale = FALSE,
    #                                                                              use_shapley_weights_in_W = T,
    #                                                                              normalize_W_weights = T,
    #                                                                              distance_metric = "Mahalanobis_scaled",
    #                                                                              compute_distances = FALSE,
    #                                                                              normalize_distance_rows = TRUE,
    #                                                                              reduce_dim = FALSE)
    #
    # set.seed(123)
    #
    # Shapley.approx$with_replacement_no_shapley_weights_fulldim <- list()
    #
    # Shapley.approx$with_replacement_no_shapley_weights_fulldim$l <- prepare_kernelShap(m = X_dim,
    #                                                                            Xtrain = Xtrain,
    #                                                                            Xtest = Xtest,
    #                                                                            exact = FALSE,
    #                                                                            replace = T,
    #                                                                            nrows = round(nrows_prop*(2^X_dim)),
    #                                                                            shapley_weight_inf_replacement = 10^6,
    #                                                                            scale = FALSE,
    #                                                                            use_shapley_weights_in_W = F,
    #                                                                            normalize_W_weights = T,
    #                                                                            distance_metric = "Mahalanobis_scaled",
    #                                                                            compute_distances = FALSE,
    #                                                                            normalize_distance_rows = TRUE,
    #                                                                            reduce_dim = FALSE)


    for (j in 1:length(Shapley.approx)){
        Shapley.approx[[j]]$Kshap <- matrix(NA,ncol=X_dim+1,nrow=nTest)
    }




    for (i in l$Xtest[, .I]) {
        Shapley.true$exactShap[i, ] = l.exact$W %*% DT[id == i, k]
        for (j in 1:length(Shapley.approx)){
            Shapley.approx[[j]]$Kshap[i,] <- Shapley.approx[[j]]$l$W %*% DT[id == i, k][Shapley.approx[[j]]$l$X$ID]
        }
    }

    #### COMPUTING RESULTS


    colSds <- function(x){
        apply(X=x,MARGIN = 2,FUN=sd)
    }


    absmeans <- matrix(NA,nrow=length(Shapley.approx), ncol=ncol(Xtrain))
    rownames(absmeans) <- names(Shapley.approx)
    colnam <- paste0("absmean_X",1:ncol(Xtrain))
    colnames(absmeans) <- colnam

    for (i in 1:length(Shapley.approx)){
        absmeans[i,] <- colMeans(abs(Shapley.true$exactShap[,-1]-Shapley.approx[[i]]$Kshap[,-1]))
    }
    absmeans <- cbind(absmeans,absmean_total=rowMeans(absmeans))

    absrelmeans <- matrix(NA,nrow=length(Shapley.approx), ncol=ncol(Xtrain))
    rownames(absrelmeans) <- names(Shapley.approx)
    colnam <- paste0("absrelmean_X",1:ncol(Xtrain))
    colnames(absrelmeans) <- colnam

    for (i in 1:length(Shapley.approx)){
        absrelmeans[i,] <- colMeans(abs((Shapley.true$exactShap[,-1]-Shapley.approx[[i]]$Kshap[,-1])/Shapley.true$exactShap[,-1]))

    }
    absrelmeans <- cbind(absrelmeans,absrelmean_total=rowMeans(absrelmeans))


    absrelmeans <- matrix(NA,nrow=length(Shapley.approx), ncol=ncol(Xtrain))
    rownames(absrelmeans) <- names(Shapley.approx)
    colnam <- paste0("absrelmean_X",1:ncol(Xtrain))
    colnames(absrelmeans) <- colnam

    for (i in 1:length(Shapley.approx)){
        absrelmeans[i,] <- colMeans(abs((Shapley.true$exactShap[,-1]-Shapley.approx[[i]]$Kshap[,-1])/Shapley.true$exactShap[,-1]))

    }
    absrelmeans <- cbind(absrelmeans,absrelmean_total=rowMeans(absrelmeans))



    abssds <- matrix(NA,nrow=length(Shapley.approx), ncol=ncol(Xtrain))
    rownames(abssds) <- names(Shapley.approx)
    colnam <- paste0("abssd_X",1:ncol(Xtrain))
    colnames(abssds) <- colnam

    abssd_total <- numeric()
    for (i in 1:length(Shapley.approx)){
        abssds[i,] <- colSds(abs(Shapley.true$exactShap[,-1]-Shapley.approx[[i]]$Kshap[,-1]))
        abssd_total[i] <- sd(abs(Shapley.true$exactShap[,-1]-Shapley.approx[[i]]$Kshap[,-1]))
    }
    abssds <- cbind(abssds,abssd_total=abssd_total)


    res <- cbind(absrelmeans,absmeans,abssds)
    res.DT <- data.table(res,keep.rownames = T)

        nrows_used <- rep(NA,length(Shapley.approx))
    for(j in 1:length(Shapley.approx)){
        nrows_used[j] <- nrow(Shapley.approx[[j]]$l$S)
    }

    res.DT[,nrows_used:= nrows_used]

    res.DT[,.(rn,absmean_total,nrows_used)]

    all.res.DT.list[[a]] <- copy(res.DT[,.(rn,absmean_total,nrows_used)])

    a <- a+1

    print(nrows_prop)

    all.res.DT <- rbindlist(all.res.DT.list,idcol = T)

    plot(all.res.DT[rn=="no_replacement_with_shapley_weights",nrows_used],all.res.DT[rn=="no_replacement_with_shapley_weights",absmean_total],type="l",xlim=c(0,2^X_dim),ylim=range(all.res.DT$absmean_total))
    lines(all.res.DT[rn=="with_replacement_with_shapley_weights",nrows_used],all.res.DT[rn=="with_replacement_with_shapley_weights",absmean_total],col=2,lty=2)
    lines(all.res.DT[rn=="with_replacement_no_shapley_weights",nrows_used],all.res.DT[rn=="with_replacement_no_shapley_weights",absmean_total],col=3)
    lines(all.res.DT[rn=="with_replacement_with_shapley_weights_fulldim",nrows_used],all.res.DT[rn=="with_replacement_with_shapley_weights_fulldim",absmean_total],col=4,lty=2)
    lines(all.res.DT[rn=="with_replacement_no_shapley_weights_fulldim",nrows_used],all.res.DT[rn=="with_replacement_no_shapley_weights_fulldim",absmean_total],col=5)

}

aaa = copy(Shapley.approx$with_replacement_no_shapley_weights$l$X)

aaa[N!=1,shapley_weight:=shapley_weight/sum(shapley_weight)]

aaa[,no:=as.numeric(no)]
aaa[N!=1,no:=as.numeric(no)/sum(no)]

aaa[N!=1,diff:=shapley_weight-no]

aaa[,.(no=mean(no),diff=mean(diff)),by=nfeatures]

Shapley.approx$no_replacement_with_shapley_weights$l$W[1:10,1:10]
Shapley.approx$with_replacement_no_shapley_weights$l$W[1:10,1:10]
