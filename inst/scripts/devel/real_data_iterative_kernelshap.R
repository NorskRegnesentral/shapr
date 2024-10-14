
### Upcoming generalization:

#1. Use non-linear truth (xgboost or so)
#2. Even more features

print(Sys.time())
library(data.table)
library(shapr)
library(ranger)

# Give me some credit data set
gmc <- read.table("/nr/project/stat//BigInsight//Projects//Explanations//Counterfactual_kode//Carla_datasets//GiveMeSomeCredit-training.csv",header=TRUE, sep=",")
foo <- apply(gmc,1,sum)
ind <- which(is.na(foo))
gmc <- gmc[-ind,]


nobs <- dim(gmc)[1]
ind <- sample(x=nobs, size=round(0.75*nobs))
gmcTrain <- gmc[ind,-1]
gmcTest  <- gmc[-ind,-1]
gmcTrain <- as.data.table(gmcTrain)
gmcTest <- as.data.table(gmcTest)

integer_columns <- sapply(gmcTrain, is.integer) # Identify integer columns
integer_columns = integer_columns[2:length(integer_columns)]
gmcTrain[, c("RevolvingUtilizationOfUnsecuredLines", "age",
"NumberOfTime30.59DaysPastDueNotWorse", "DebtRatio", "MonthlyIncome",
"NumberOfOpenCreditLinesAndLoans", "NumberOfTimes90DaysLate",
"NumberRealEstateLoansOrLines", "NumberOfTime60.89DaysPastDueNotWorse", "NumberOfDependents"):=
lapply(.SD, as.numeric), .SDcols =  c("RevolvingUtilizationOfUnsecuredLines", "age",
"NumberOfTime30.59DaysPastDueNotWorse", "DebtRatio", "MonthlyIncome",
"NumberOfOpenCreditLinesAndLoans", "NumberOfTimes90DaysLate",
"NumberRealEstateLoansOrLines", "NumberOfTime60.89DaysPastDueNotWorse", "NumberOfDependents")]
integer_columns <- sapply(gmcTest, is.integer) # Identify integer columns
integer_columns = integer_columns[2:length(integer_columns)]
gmcTest[, c("RevolvingUtilizationOfUnsecuredLines", "age",
"NumberOfTime30.59DaysPastDueNotWorse", "DebtRatio", "MonthlyIncome",
"NumberOfOpenCreditLinesAndLoans", "NumberOfTimes90DaysLate",
"NumberRealEstateLoansOrLines", "NumberOfTime60.89DaysPastDueNotWorse", "NumberOfDependents"):=
lapply(.SD, as.numeric), .SDcols =  c("RevolvingUtilizationOfUnsecuredLines", "age",
"NumberOfTime30.59DaysPastDueNotWorse", "DebtRatio", "MonthlyIncome",
"NumberOfOpenCreditLinesAndLoans", "NumberOfTimes90DaysLate",
"NumberRealEstateLoansOrLines", "NumberOfTime60.89DaysPastDueNotWorse", "NumberOfDependents")]

# model <- ranger(SeriousDlqin2yrs  ~ ., data = gmcTrain, num.trees = 500, num.threads = 6,
#                 verbose = TRUE,
#                 probability = FALSE,
#                 importance = "impurity",
#                 mtry = sqrt(11),
#                 seed = 3045)
library(hmeasure)
#pred.rf <- predict(model, data = gmcTest)
#results <- HMeasure(unlist(as.vector(gmcTest[,1])),pred.rf$predictions,threshold=0.15)
#results$metrics$AUC

y_train = gmcTrain$SeriousDlqin2yrs
x_train = gmcTrain[,-1]
y_explain = gmcTest$SeriousDlqin2yrs
x_explain = gmcTest[,-1]

set.seed(123)
model <- xgboost(
  data = as.matrix(x_train),
  label = y_train,
  nround = 50,
  verbose = FALSE,params = list(objective = "binary:logistic")
)
pred.xgb <- predict(model, newdata = as.matrix(x_explain))
results <- HMeasure(as.vector(y_explain),pred.xgb,threshold=0.15)
results$metrics$AUC


set.seed(123)

inds_train = sample(1:nrow(x_train), 9000)
x_train = x_train[inds_train,]
y_train = y_train[inds_train]

m = ncol(x_train)


p0 <- mean(y_train)
mu = colMeans(x_train)
Sigma = cov(x_train)

### First run proper shapr call on this

sim_results_saving_folder = "/nr/project/stat/BigInsight/Projects/Explanations/EffektivShapley/Frida/simuleringsresultater/gmc_data_v3/"#"../effektiv_shapley_output/"
kernelSHAP_reweighting_strategy = "none"

predict_model_xgb <- function(object,newdata){
  xgboost:::predict.xgb.Booster(object,as.matrix(newdata))
}


preds_explain <- predict_model_xgb(model,x_explain)
head(order(-preds_explain),50)
inds_1 <- head(order(-preds_explain),50)
set.seed(123)
inds_2 <- sample(which(preds_explain>quantile(preds_explain,0.9) & preds_explain<min(preds_explain[inds_1])),size = 50,replace = FALSE)
inds <- c(inds_1,inds_2)

set.seed(465132)
expl <- shapr::explain(model = model,
                       x_explain= x_explain[inds,],
                       x_train = x_train,
                       approach = "ctree",
                       prediction_zero = p0
)
fwrite(expl$shapley_values,paste0(sim_results_saving_folder,"exact_shapley_values_", kernelSHAP_reweighting_strategy, ".csv"))
print(Sys.time())

# These are the parameters for for iterative_kshap_func
n_samples <- 1000
approach = "ctree"

gaussian.mu <- mu
gaussian.cov_mat <- Sigma

# Reduce if < 10% prob of shapval > 0.05
shapley_threshold_val <-  0.02
shapley_threshold_prob <- 0.2

source("inst/scripts/devel/iterative_kernelshap_sourcefuncs.R")

testObs_computed_vec <- inds# seq_len(n_explain)
runres_list <- runcomps_list <- list()

cutoff_feats = colnames(x_train)

run_obj_list <- list()
for(kk in seq_along(testObs_computed_vec)){
  testObs_computed <- testObs_computed_vec[kk]
  full_pred <- predict_model_xgb(model,x_explain)[testObs_computed]
  shapsum_other_features <- 0


  run <- iterative_kshap_func(model,x_explain,x_train,
                              testObs_computed = testObs_computed,
                              cutoff_feats = cutoff_feats,
                              initial_n_coalitions = 50,
                              full_pred = full_pred,
                              shapsum_other_features = shapsum_other_features,
                              p0 = p0,
                              predict_model = predict_model_xgb,
                              shapley_threshold_val = shapley_threshold_val,
                              shapley_threshold_prob = shapley_threshold_prob,
                              approach = approach,
                              kernelSHAP_reweighting_strategy = kernelSHAP_reweighting_strategy)

  runres_list[[kk]] <- run$kshap_final
  runcomps_list[[kk]] <- sum(sapply(run$keep_list,"[[","no_computed_combinations"))
  run_obj_list[[kk]] <- run
  print(kk)
  print(Sys.time())
}

est <- rbindlist(runres_list)
est[,other_features:=NULL]
fwrite(est,paste0(sim_results_saving_folder,"iterative_shapley_values_", kernelSHAP_reweighting_strategy, ".csv"))

expl_approx <- matrix(0, nrow = length(inds), ncol = m+1)
expl_approx_obj_list <- list()
for (i in seq_along(testObs_computed_vec)){
  expl_approx_obj <- shapr::explain(model = model,
                        x_explain= x_explain[testObs_computed_vec[i],],
                        x_train = x_train,
                        approach = approach,
                        prediction_zero = p0,
                        n_coalitions = runcomps_list[[i]])
  expl_approx[i,] = unlist(expl_approx_obj$shapley_values)
  expl_approx_obj_list[[i]] <- expl_approx_obj
}
expl_approx <- as.data.table(expl_approx)
truth <- expl$shapley_values

colnames(expl_approx) <- colnames(truth)
fwrite(expl_approx,paste0(sim_results_saving_folder,"approx_shapley_values_", kernelSHAP_reweighting_strategy, ".csv"))

bias_vec <- colMeans(est-truth)
rmse_vec <- sqrt(colMeans((est-truth)^2))
mae_vec <- colMeans(abs(est-truth))

bias_vec_approx <- colMeans(expl_approx-truth)
rmse_vec_approx <- sqrt(colMeans((expl_approx-truth)^2))
mae_vec_approx <- colMeans(abs(expl_approx-truth))

save.image(paste0(sim_results_saving_folder, "iterative_kernelshap_lingauss_p12_", kernelSHAP_reweighting_strategy, ".RData"))

hist(unlist(runcomps_list),breaks = 20)

summary(unlist(runcomps_list))


run$kshap_final
sum(unlist(run$kshap_final))
full_pred

print(Sys.time())

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

