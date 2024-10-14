

library(data.table)
library(MASS)
library(Matrix)
library(shapr)
library(future)
library(xgboost)

nVar       <- 12
nVarRed    <- 3
rhoLarge   <- 0.98
rhoSmall   <- 0.0
rhoBetween <- 0.0
corMat <- diag(nVar)
corMat[1:nVarRed,1:nVarRed]                 <- rhoLarge
corMat[(nVarRed+1):nVar,(nVarRed+1):nVar]   <- rhoSmall
corMat[1:nVarRed,(nVarRed+1):nVar]          <- rhoBetween
corMat[(nVarRed+1):nVar,1:nVarRed]          <- rhoBetween
diag(corMat) <- 1
mu <- rep(0,nVar)
indep.ind <- seq(nVarRed+1,nVar)

predFuncT <- function(X)
{
  n <- dim(X)[1]
  eps <- rnorm(n,0,0.01)
  y <- 0.5*X[,1]+0.3*X[,2]+0.4*X[,3]+0.2*X[,4]+0.6*X[,5]+0.3*X[,6]+0.2*X[,7]+0.1*X[,8]
  y <- y + eps
}

###################################################################################################
nTrain  <- 10000
nTest   <- 100
simSeed <-100
set.seed(simSeed)
library(mvtnorm)
X       <- rmvnorm(n=nTrain,mean=mu,sigma=corMat)
Xtest   <- rmvnorm(n=nTest,mean=mu,sigma=corMat)

y <- predFuncT(X)
trainData <- as.data.frame(cbind(y,X))

ytest <- predFuncT(Xtest)
testData <- as.data.frame(cbind(ytest,Xtest))

model <- lm(y~., data = trainData)

###########################################################################################################

muNorm    <- apply(trainData[,-1],2,mean)
covMat    <- var(trainData[,-1])
coef <- model$coef[-1]
indTest <- 1
x0 <- as.numeric(testData[indTest,-1])

####### MJ translating to my setup ####

Sigma <- covMat
mu <- muNorm
p0 <- mean(predict(model,trainData[,-1]))

x_train <- as.data.table(X)
names(x_train) <- paste0("V",1+1:nVar)

x_explain <- as.data.table(testData[,-1])
names(x_explain) <- paste0("V",1+1:nVar)

set.seed(123)



# These are the parameters for for interative_kshap_func
n_samples <- 10000
approach = "gaussian"

# Reduce if < 10% prob of shapval > 0.2
#shapley_threshold_dt <- data.table(val = c(rep(0.2,3),
#                                           rep(0.1,3),
#                                           rep(0.05,3)),
#                                   prob = rep(c(0.3,0.2,0.1),3))
shapley_threshold_dt <- data.table(val = rep(0.1,3),
                                   prob = c(0.2,0.1,0.05))

#shapley_threshold_dt <- data.table(val = c(0.2,0.1,0.05),
#                                   prob = rep(0.1,3))



source("inst/scripts/devel/iterative_kernelshap_sourcefuncs.R")

set.seed(123)
testObs_computed_vec <- 1:100
cutoff_feats <- paste0("V",1+1:12)
shapley_reweighting_strategy = "none"
# Using threshold: 0.1
runres_list <- runcomps_list <- list()

for(jj in seq_len(nrow(shapley_threshold_dt))){
  runres_list[[jj]] <-list()
  runcomps_list[[jj]] <- list()
  for(kk in testObs_computed_vec){
    testObs_computed <- testObs_computed_vec[kk]
    full_pred <- predict(model,x_explain)[testObs_computed]
    shapsum_other_features <- 0

    run <- iterative_kshap_func(model,x_explain,x_train,
                                testObs_computed = testObs_computed,
                                cutoff_feats = cutoff_feats,
                                initial_n_combinations = 50,
                                full_pred = full_pred,
                                shapsum_other_features = shapsum_other_features,
                                p0 = p0,
                                predict_model = predict.lm,
                                shapley_threshold_val = shapley_threshold_dt$val[jj],
                                shapley_threshold_prob = shapley_threshold_dt$prob[jj],
                                approach = approach,
                                n_samples = n_samples,
                                gaussian.mu = mu,
                                gaussian.cov_mat = Sigma,
                                shapley_reweighting_strategy = shapley_reweighting_strategy)
    runres_list[[jj]][[kk]] <- run$kshap_final
    runcomps_list[[jj]][[kk]] <- sum(sapply(run$keep_list,"[[","no_computed_combinations"))
    print(kk)
  }

}

mean_K <- sapply(runcomps_list,function(x) mean(unlist(x)))

est <- lapply(runres_list,rbindlist)

est[[1]][,K:=unlist(runcomps_list[[1]])]
est[[2]][,K:=unlist(runcomps_list[[2]])]
est[[3]][,K:=unlist(runcomps_list[[3]])]


fwrite(est[[1]],file="inst/scripts/devel/iterative_kshap_est_0.1_0.2.csv")
fwrite(est[[2]],file="inst/scripts/devel/iterative_kshap_est_0.1_0.1.csv")
fwrite(est[[3]],file="inst/scripts/devel/iterative_kshap_est_0.1_0.05.csv")


#########

# Full regular shapley call
set.seed(465132)
progressr::handlers(global = TRUE)
expl_full <- shapr::explain(model = model,
                       x_explain= x_explain[testObs_computed_vec,],
                       x_train = x_train,
                       approach = "gaussian",
                       n_samples = n_samples,
                       prediction_zero = p0,Sigma=Sigma,mu=mu)

expl_same_K_as_1 <- shapr::explain(model = model,
                       x_explain= x_explain[testObs_computed_vec,],
                       x_train = x_train,
                       approach = "gaussian",
                       n_samples = n_samples,
                       prediction_zero = p0,Sigma=Sigma,mu=mu,n_combinations = round(mean_K[1]))

expl_same_K_as_2 <- shapr::explain(model = model,
                                   x_explain= x_explain[testObs_computed_vec,],
                                   x_train = x_train,
                                   approach = "gaussian",
                                   n_samples = n_samples,
                                   prediction_zero = p0,Sigma=Sigma,mu=mu,n_combinations = round(mean_K[2]))

expl_same_K_as_3 <- shapr::explain(model = model,
                                   x_explain= x_explain[testObs_computed_vec,],
                                   x_train = x_train,
                                   approach = "gaussian",
                                   n_samples = n_samples,
                                   prediction_zero = p0,Sigma=Sigma,mu=mu,n_combinations = round(mean_K[3]))

expl_400 <- shapr::explain(model = model,
                           x_explain= x_explain[testObs_computed_vec,],
                           x_train = x_train,
                           approach = "gaussian",
                           n_samples = n_samples,
                           prediction_zero = p0,Sigma=Sigma,mu=mu,n_combinations = 400)

expl_800 <- shapr::explain(model = model,
                           x_explain= x_explain[testObs_computed_vec,],
                           x_train = x_train,
                           approach = "gaussian",
                           n_samples = n_samples,
                           prediction_zero = p0,Sigma=Sigma,mu=mu,n_combinations = 800)

expl_1600 <- shapr::explain(model = model,
                           x_explain= x_explain[testObs_computed_vec,],
                           x_train = x_train,
                           approach = "gaussian",
                           n_samples = n_samples,
                           prediction_zero = p0,Sigma=Sigma,mu=mu,n_combinations = 1600)


#### Checking performance ####


mean(colMeans(abs(expl_full$shapley_values[,-1]-expl_same_K_as_1$shapley_values[,-1])))
mean(colMeans(abs(expl_full$shapley_values[,-1]-est[[1]][,-c("none","other_features")])))

mean(colMeans(abs(expl_full$shapley_values[,-1]-expl_same_K_as_2$shapley_values[,-1])))
mean(colMeans(abs(expl_full$shapley_values[,-1]-est[[2]][,-c("none","other_features")])))

mean(colMeans(abs(expl_full$shapley_values[,-1]-expl_same_K_as_3$shapley_values[,-1])))
mean(colMeans(abs(expl_full$shapley_values[,-1]-est[[3]][,-c("none","other_features")])))

mean(colMeans(abs(expl_full$shapley_values[,-1]-expl_400$shapley_values[,-1])))
mean(colMeans(abs(expl_full$shapley_values[,-1]-expl_800$shapley_values[,-1])))
mean(colMeans(abs(expl_full$shapley_values[,-1]-expl_1600$shapley_values[,-1])))


##############








### First run proper shapr call on this

#sim_results_saving_folder = "/nr/project/stat/BigInsight/Projects/Explanations/EffektivShapley/Frida/simuleringsresultater/sim_lingauss_v2/"#"../effektiv_shapley_output/"

set.seed(465132)
inds = 1:n_explain
progressr::handlers(global = TRUE)
expl <- shapr::explain(model = model,
                       x_explain= x_explain[inds,],
                       x_train = x_train,
                       approach = "gaussian",
                       prediction_zero = p0,Sigma=Sigma,mu=mu)

fwrite(expl$shapley_values,paste0(sim_results_saving_folder,"exact_shapley_values_",shapley_threshold_val,"_",shapley_reweighting_strategy, ".csv"))




### Need to create an lm analogoue to pred_mod_xgb here


set.seed(123)



fwrite(est,paste0(sim_results_saving_folder,"iterative_shapley_values_",shapley_threshold_val,"_",shapley_reweighting_strategy, ".csv"))




truth <- expl$shapley_values

expl_approx <- matrix(0, nrow = length(inds), ncol = m+1)
expl_approx_obj_list <- list()
for (i in testObs_computed_vec){
  expl_approx_obj <- shapr::explain(model = model,
                                    x_explain= x_explain[inds[i],],
                                    x_train = x_train,
                                    approach = "gaussian",
                                    prediction_zero = p0,
                                    n_combinations = runcomps_list[[i]],
                                    Sigma=Sigma,mu=mu)
  expl_approx[i,] = unlist(expl_approx_obj$shapley_values)
  expl_approx_obj_list[[i]] <- expl_approx_obj
}
expl_approx <- as.data.table(expl_approx)
colnames(expl_approx) <- colnames(truth)
fwrite(expl_approx,paste0(sim_results_saving_folder,"approx_shapley_values_",shapley_threshold_val,"_",shapley_reweighting_strategy, ".csv"))

bias_vec <- colMeans(est-truth)
rmse_vec <- sqrt(colMeans((est-truth)^2))
mae_vec <- colMeans(abs(est-truth))

bias_vec_approx <- colMeans(expl_approx-truth)
rmse_vec_approx <- sqrt(colMeans((expl_approx-truth)^2))
mae_vec_approx <- colMeans(abs(expl_approx-truth))

save.image(paste0(sim_results_saving_folder, "iterative_kernelshap_",shapley_threshold_val,"_",shapley_reweighting_strategy, ".RData"))

hist(unlist(runcomps_list),breaks = 20)

summary(unlist(runcomps_list))


run$kshap_final
sum(unlist(run$kshap_final))
full_pred








# TODO: Må finne ut av hvorfor det ikke gir korrekt sum her...
# Hvis det er noen variabler som ble ekskludert, så må jeg legge til disse i summen for å få prediksjonen til modellen.
# for(i in 1:18){
#   print(sum(unlist(run$keep_list[[i]]$kshap_est_dt[,-1]))+run$keep_list[[i]]$shap_it_excluded_features)
# #print(run$keep_list[[i]]$shap_it_excluded_features)
# }

# run$kshap_it_est_dt



# run$kshap_final
# expl$shapley_values




# kshap_final <- copy(run$kshap_est_dt_list[,-1])
# setnafill(kshap_final,"locf")
# kshap_final[.N,] # final estimate

# sum(unlist(kshap_final[.N,]))

# sum(unlist(expl$shapley_values[testObs_computed,]))










# cutoff_feats <- paste0("VV",1:6)
# testObs_computed <- 5

# full_pred <- predict(model,x_explain)[5]
# p0 <- mean(y_train)
# pred_not_to_decompose <- sum(expl$shapley_values[5,VV7:VV9])


# run_minor <- iterative_kshap_func(model,x_explain,x_train,
#                             testObs_computed = 5,
#                             cutoff_feats = cutoff_feats,
#                             full_pred = full_pred,
#                             pred_not_to_decompose = pred_not_to_decompose,
#                             p0 = p0,
#                             predict_model = predict.lm,shapley_threshold_val = 0)


# aa=run$keep_list[[8]]$dt_vS

# bb=run_minor$keep_list[[6]]$dt_vS
# setnames(bb,"p_hat_1","p_hat_1_approx")

# cc=merge(aa,bb)
# cc[,diff:=p_hat_1-p_hat_1_approx]


# TODO:

# 1. Run example with gaussian features where the truth is known in advance in a large setting, with e.g. 12 features or so. I want the estimate
# both for the full 12 features, and for subsets where one is removed.
# 2.

# Utfordringer:
# 1. Hvordan justere vekter og samplingrutine fra subset S når man allerede har et utvalg sampler (som også er noe biased).
# 2. Bruker altså E[f(x1=x1*,x2,x3=x3*,x4)|x1=x1*] som proxy for E[f(x1=x1*,x2,x3=x3*,x4)|x1=x1*,x3=x3*],
#men hva med E[f(x1=x1*,x2,x3,x4=x4*)|x1=x1*,x4=x4*]? Burde jeg bruke den for
#E[f(x1=x1*,x2,x3=x3*,x4=x4*)|x1=x1*,x4=x4*]?
# 3. Når jeg fjerner en variabel (som har lite å si), så settes shapley-verdien til det den har per da. MEN den verdien vil trolig være noe biased fordi den fjernes første gangen den går over terskelverdiene
# jeg har satt for ekskludering.

