print(Sys.time())
library(data.table)
library(shapr)
library(xgboost)

dataset_name = "adult_v2_fix_n_comb"
adultTrain = read.table("/nr/project/stat//BigInsight//Projects//Explanations//Annabelle//counterfactuals//Datasets/adult_income.csv",sep=",",header=FALSE)
adultTest = read.table("/nr/project/stat//BigInsight//Projects//Explanations//Annabelle//counterfactuals//Datasets/adult_income_test2.csv",sep=",",header=FALSE)

data <- rbind(adultTrain, adultTest)

varNames <- c("age", "workclass",  "fnlwgt", "education", "education-num", "marital-status", "occupation", "relationship", "race", "sex", "capital-gain", "capital-loss", "hours-per-week", "native-country", "income")
colnames(data) <- varNames

#Response
response <- array(0,dim(data)[1])
response[which(as.character(data[,15])==" >50K")] <- 1

factor_var = c("workclass",  "marital-status", "occupation", "relationship", "race", "sex", "native-country")
num_var = c("age", "fnlwgt", "education-num", "capital-gain", "capital-loss", "hours-per-week")

numVarList <- list()

ind <- match(num_var,varNames)
for(i in ind){
  col = colnames(data)[i]
  numVarList[[col]] <- as.numeric(data[, col])
}

numVarMat <- do.call(cbind,numVarList)

factorVarList <- list()
ind <- match(factor_var,varNames)
for(i in ind){
  col = colnames(data)[i]
  factorVarList[[col]] <- as.factor(data[,col])
}

factorVarMat <- do.call(cbind,factorVarList)
data <- cbind(numVarMat,factorVarMat)
data <- cbind(data,response)

set.seed(92385)
ind <- sample(1:dim(data)[1],0.3*dim(data)[1])
adultTrain <- data[-ind,]
adultTest <- data[ind,]

params = list(eta = 0.1, max_depth = 2, lambda = 1, alpha = 1)

set.seed(584)

model <- xgboost(data = adultTrain[, -14],
                 label = adultTrain[,14],
                 nrounds = 500,
                 objective = "binary:logistic",
                 params = params,
                 verbose = 0)

library(hmeasure)
pred <- predict(model, newdata = adultTest[, -14])
results <- HMeasure(unlist(as.vector(adultTest[,14])), pred, threshold=0.15)
results$metrics$AUC


y_train = adultTrain[, 14]
x_train = adultTrain[, -14]
y_explain = adultTest[, 14]
x_explain = adultTest[,-14]

x_train = as.data.table(x_train)
x_explain = as.data.table(x_explain)

m = ncol(x_train)

p0 <- mean(y_train)

sim_results_saving_folder = paste0("/nr/project/stat/BigInsight/Projects/Explanations/EffektivShapley/Frida/simuleringsresultater/", dataset_name, "/")
shapley_reweighting_strategy = "none"

preds_explain <- predict(model, as.matrix(x_explain))
head(order(-preds_explain),50)
inds_1 <- head(order(-preds_explain),50)
inds_2 <- head(order(preds_explain),50)

# inds_1 <- head(order(-preds_explain),200)[51:200]
# inds_2 <- head(order(preds_explain),200)[51:200]
inds <- c(inds_1,inds_2)
print(paste0(min(preds_explain[inds]), ", ", max(preds_explain[inds])))

set.seed(465132)
expl <- shapr::explain(model = model,
                       x_explain= x_explain[inds,],
                       x_train = x_train,
                       approach = "ctree",
                       prediction_zero = p0,
                       n_combinations = 10000
                       )
fwrite(expl$shapley_values,paste0(sim_results_saving_folder,"exact_shapley_values_", shapley_reweighting_strategy, ".csv"))
print(Sys.time())

# These are the parameters for for iterative_kshap_func
approach = "ctree"

# Reduce if < 10% prob of shapval > 0.05
if (dataset_name == "adult_v2_fix_n_comb"){
  shapley_threshold_val <- -0.02
  max_n_combinations = 1000
} else {
  shapley_threshold_val <- 0.02
  max_n_combinations = NULL
}

shapley_threshold_prob <- 0.2

source("inst/scripts/devel/iterative_kernelshap_sourcefuncs.R")

testObs_computed_vec <- inds# seq_len(n_explain)
runres_list <- runcomps_list <- list()

cutoff_feats = colnames(x_train)

predict_model <- function(model, newdata){
  return(predict(model,as.matrix(newdata)))
}

predict_model(model,x_explain[inds[1],])

# load(paste0("/nr/project/stat/BigInsight/Projects/Explanations/EffektivShapley/Frida/simuleringsresultater/fico_data_v2/iterative_kernelshap_lingauss_p12", shapley_reweighting_strategy, ".RData"))

set.seed(1298)
run_obj_list <- list()
for(kk in seq_along(testObs_computed_vec)){
  testObs_computed <- testObs_computed_vec[kk]
  full_pred <- predict(model,as.matrix(x_explain))[testObs_computed]
  shapsum_other_features <- 0


  run <- iterative_kshap_func(model,x_explain,x_train,
                              testObs_computed = testObs_computed,
                              cutoff_feats = cutoff_feats,
                              initial_n_combinations = 50,
                              max_n_combinations = 70, #max_n_combinations,
                              full_pred = full_pred,
                              shapsum_other_features = shapsum_other_features,
                              p0 = p0,
                              predict_model = predict_model,
                              shapley_threshold_val = shapley_threshold_val,
                              shapley_threshold_prob = shapley_threshold_prob,
                              approach = approach,
                              shapley_reweighting_strategy = shapley_reweighting_strategy)

  runres_list[[kk]] <- run$kshap_final
  runcomps_list[[kk]] <- sum(sapply(run$keep_list,"[[","no_computed_combinations"))
  write.table(runcomps_list, file = paste0(sim_results_saving_folder,"runcomps_list_", shapley_reweighting_strategy, ".txt"))
  run_obj_list[[kk]] <- run
  saveRDS(run_obj_list, paste0(sim_results_saving_folder, "run_obj_list_", shapley_reweighting_strategy, ".RDS"))

  print(kk)
  print(Sys.time())
}

est <- rbindlist(runres_list)
est[,other_features:=NULL]
fwrite(est,paste0(sim_results_saving_folder,"iterative_shapley_values_", shapley_reweighting_strategy, ".csv"))


# runcomps_list = fread(paste0(sim_results_saving_folder,"runcomps_list_", shapley_reweighting_strategy, ".txt"))
# runcomp_list = as.list(runcomps_list)

set.seed(832)
expl_approx <- matrix(0, nrow = length(inds), ncol = m+1)
expl_approx_obj_list <- list()
for (i in seq_along(testObs_computed_vec)){
  expl_approx_obj <- shapr::explain(model = model,
                        x_explain= x_explain[testObs_computed_vec[i],],
                        x_train = x_train,
                        approach = approach,
                        prediction_zero = p0,
                        n_combinations = runcomps_list[[i]])
  expl_approx[i,] = unlist(expl_approx_obj$shapley_values)
  expl_approx_obj_list[[i]] <- expl_approx_obj
  print(i)
  print(Sys.time())
}
expl_approx <- as.data.table(expl_approx)
# truth <- expl$shapley_values

# colnames(expl_approx) <- colnames(truth)
fwrite(expl_approx, paste0(sim_results_saving_folder,"approx_shapley_values_", shapley_reweighting_strategy, ".csv"))

# bias_vec <- colMeans(est-truth)
# rmse_vec <- sqrt(colMeans((est-truth)^2))
# mae_vec <- colMeans(abs(est-truth))

# bias_vec_approx <- colMeans(expl_approx-truth)
# rmse_vec_approx <- sqrt(colMeans((expl_approx-truth)^2))
# mae_vec_approx <- colMeans(abs(expl_approx-truth))

# save.image(paste0(sim_results_saving_folder, "iterative_kernelshap_lingauss_p12_", shapley_reweighting_strategy, ".RData"))

# hist(unlist(runcomps_list),breaks = 20)

# summary(unlist(runcomps_list))


# run$kshap_final
# sum(unlist(run$kshap_final))
# full_pred

print(Sys.time())

