
#### Real data experiment

#rm(list = ls())

library(shapr)
library(data.table)
library(mvtnorm)
library(condMVNorm)
library(stringi)

library(xgboost)
library(GIGrvg)
library(ghyp)


experiment = "Real"
true_model <- "Unknown"
fitted_model <- "XGBoost"
variables <- "Unknown" # Gaussian, Gaussianmix, or GenHyp
notes <- "Real data experiment"
X_dim <- 28
source.local <- ifelse(exists("source.local"),source.local,FALSE)
these_run_ind = 1:20






# May adjust these for the differnet methods
this.seed <- 12345
nrows_kernelShap <- 10^4
w_threshold = 1 # For a fairer comparison, all models use the same number of samples (n_threshold)
n_threshold = 10^3 # Number of samples used in the Monte Carlo integration

X_GenHyp <- (variables=="GenHyp")
(joint_csv_filename <- paste0("all_results_experiment_",experiment,"_dim_",X_dim,"_",true_model,"_",fitted_model,"_",variables,".csv")) # May hardcode this to NULL for not saving to joint in testing circumstances
(initial_current_csv_filename <- paste0("current_results_experiment_",experiment,"_dim_",X_dim,"_",true_model,"_",fitted_model,"_",variables))


source("paper_scripts/paper_helper_funcs.R",local = source.local) # Helper functions these experiments (mainly computing the true Shapley values)

source("paper_experiments/source_specifying_seed_and_filenames.R",local = source.local) # Setting random or fixed seed and filenames.

#### Loading data ####
XYtrain <-  fread("/nr/project/stat/BFFGB18/LIME/lime/R/train6.csv")
XYtest <-   fread("/nr/project/stat/BFFGB18/LIME/lime/R/test6.csv")


dim(XYtrain)

XYtrain[,V1:=NULL]
XYtest[,V1:=NULL]

setnames(XYtrain,"default","y")
setnames(XYtest,"default","y")

# Testing, reducing the dimension of the data
XYtest <- XYtest
XYtrain <- XYtrain


nTrain <- nrow(XYtrain)
nTest <- nrow(XYtest)

Xtrain <- copy(XYtrain)
Xtest <- copy(XYtest)

Xtrain[,y:=NULL]
Xtest[,y:=NULL]

################## Fitting the model ######
FastROC <- function(y, x) {
    # y = actual
    # x = predicted
    x1 = x[y==1]
    n1 = length(x1)
    x2 = x[y==0]
    n2 = length(x2)
    r = rank(c(x1,x2))
    return((sum(r[1:n1]) - n1*(n1+1)/2) / (n1*n2))
}

# xgb.train <- xgb.DMatrix(data = as.matrix(XYtrain[,-"y"]),
#                          label = XYtrain[,y])
# xgb.test <- xgb.DMatrix(data = as.matrix(XYtest[,-"y"]),
#                          label = XYtest[,y])
#
# params <- list(eta =  0.1,
#                objective = "binary:logistic",
#                eval_metric = "auc",
#                tree_method="hist") # gpu_hist
#
# model.xgb <- xgb.train(data = xgb.train,
#                    params = params,
#                    nrounds = 500,
#                    print_every_n = 10,
#                    ntread = 5,
#                    watchlist = list(train = xgb.train,
#                                     test = xgb.test),
#                    early_stopping_rounds = 20,
#                    verbose = 1)
#
# pred.xgb = predict(object=model.xgb,newdata = xgb.test)
# (auc.xgb <- FastROC(XYtest$y,pred.xgb))
#
# library(ranger)
#
# model.rf <- ranger(formula = y~.,data = XYtrain,probability = T,num.trees=500)
# pred.rf <- predict(object=model,data=XYtest)$pred[,2]
#
# (auc.rf <- FastROC(XYtest$y,pred.rf))

xgb.train <- xgb.DMatrix(data = as.matrix(XYtrain[,-"y"]),
                         label = XYtrain[,y])
xgb.test <- xgb.DMatrix(data = as.matrix(XYtest[,-"y"]),
                        label = XYtest[,y])

params <- list(eta =  0.1,
               objective = "binary:logistic",
               eval_metric = "auc",
               tree_method="hist") # gpu_hist



model <- xgb.train(data = xgb.train,
                   params = params,
                   nrounds = 50,
                   print_every_n = 10,
                   ntread = 5,
                   watchlist = list(train = xgb.train,
                                    test = xgb.test),
                   verbose = 1)

pred_zero = mean(XYtrain$y)


#### Make a parallelized loop over parts of the

d <- 1:nrow(Xtest)
run_list <- split(d, ceiling(seq_along(d)/21))

for(run_ind in these_run_ind){ # tot 92
    set.seed(this.seed)

    current.Xtest <- copy(Xtest[run_list[[run_ind]],])
#### Pre computation before kernel shap ---------
# Creating the l object
l <- prepare_kernelShap(
    m = ncol(Xtrain),
    Xtrain = Xtrain,
    Xtest = current.Xtest,
    exact = FALSE,
    nrows = nrows_kernelShap,
    distance_metric = "Mahalanobis_scaled",
    normalize_distance_rows = TRUE,
    compute_distances_for_no_var = 0:3
)
#lapply(l,function(x){format(object.size(x),units="auto")})

#### Running various approximation approaches ####
empirical_independence_settings = list(type = "independence")

empirical_fixed_sigma.01_settings = list(type = "fixed_sigma",
                                         fixed_sigma_vec = 0.1,
                                         kernel_metric = "Gaussian")


Shapley.approx = list()

# Shapley.approx$empirical_independence = compute_kernelShap(model = model,
#                                                            l = l,
#                                                            w_threshold = w_threshold,
#                                                            n_threshold = n_threshold,
#                                                            cond_approach = "empirical",
#                                                            empirical_settings = empirical_independence_settings,
#                                                            pred_zero=pred_zero)
#
# Shapley.approx$comb_sigma.01 = compute_kernelShap(model = model,
#                                                   l = l,
#                                                   w_threshold = w_threshold,
#                                                   n_threshold = n_threshold,
#                                                   cond_approach = list(empirical = l$D_for_these_varcomb,copula =l$X[,.I][-l$D_for_these_varcomb]),
#                                                   empirical_settings = empirical_fixed_sigma.01_settings,
#                                                   pred_zero=pred_zero)

Shapley.approx$comb_Gaussian_sigma.01 = compute_kernelShap(model = model,
                                                  l = l,
                                                  w_threshold = w_threshold,
                                                  n_threshold = n_threshold,
                                                  cond_approach = list(empirical = l$D_for_these_varcomb,Gaussian =l$X[,.I][-l$D_for_these_varcomb]),
                                                  empirical_settings = empirical_fixed_sigma.01_settings,
                                                  pred_zero=pred_zero,
                                                  ensure_condcov_symmetry = T)


# Shapley.approx$Gaussian = compute_kernelShap(model = model,
#                                              l = l,
#                                              w_threshold = w_threshold,
#                                              n_threshold = n_threshold,
#                                              cond_approach = "Gaussian",
#                                              pred_zero=pred_zero,
#                                              verbose = TRUE,
#                                              ensure_condcov_symmetry = T)
#
#
#
# Shapley.approx$copula = compute_kernelShap(model = model,
#                                            l = l,
#                                            w_threshold = w_threshold,
#                                            n_threshold = n_threshold,
#                                            cond_approach = "copula",
#                                            pred_zero=pred_zero)
#
# tt <- proc.time()
# tmp= predict(model,as.matrix(current.Xtest),predcontrib=T)
# colnames(tmp) <- NULL
# Shapley.approx$treeSHAP <- list()
# Shapley.approx$treeSHAP$Kshap <- tmp[,c(ncol(current.Xtest)+1,1:ncol(current.Xtest))]
# Shapley.approx$treeSHAP$other_objects <- list()
# Shapley.approx$treeSHAP$other_objects$h_optim_DT <- NULL
# Shapley.approx$treeSHAP$other_objects$comp_time <- proc.time()-tt

print(paste0("Run_ind ",run_ind," out of ",length(run_list)," just computed!"))


#### Writing common csv-file
res.DT <- NULL

for (i in 1:length(Shapley.approx)){
    res.DT <- rbind(res.DT,cbind(names(Shapley.approx)[i],run_list[[run_ind]],Shapley.approx[[i]]$Kshap))
}

colnames(res.DT) <- c("Method","test_no",paste0("phi_",0:X_dim))
res.DT <- as.data.table(res.DT)
fwrite(x = res.DT,file = paste0("paper_experiments/res/",joint_csv_filename),append = T)
###############

save(Shapley.approx,file=paste0("paper_experiments/res/single_res/realrunind_",run_ind,"_only_Gaussian_comb__",current_RData_filename))

}
