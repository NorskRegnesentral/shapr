print(Sys.time())
library(data.table)
library(shapr)
library(xgboost)

tmp2 <- read.table("/nr/project/stat///BigInsight//Projects//Explanations//MCCE//Kjersti//CARTanalysis//fico.txt",sep=",",header=TRUE)
ind1 <- tmp2[,2]==-9
ind2 <- tmp2[,10]==-9
ind3 <- tmp2[,20]==-9
ind1 <- tmp2[,2]==-9
ind <- which(ind1*ind2*ind3==1)
fico <- tmp2[-ind,]

#fico <- fico[,c(1,2,3,5,6,9,11,12,13,15,19,21,23)]

set.seed(100)
numVar <- dim(fico)[2]-1
nobs <- dim(fico)[1]
ind <- sample(x=nobs, size=round(0.75*nobs))
ficoTrain <- fico[ind,]
ficoTest  <- fico[-ind,]

y <- array(0,dim(ficoTrain)[1])
y[which(ficoTrain[,1]=="Bad")] <- 1
y[which(ficoTrain[,1]=="Good")] <- 0
ficoTrain[,1] <- y

y <- array(0,dim(ficoTest)[1])
y[which(ficoTest[,1]=="Bad")] <- 1
y[which(ficoTest[,1]=="Good")] <- 0
ficoTest[,1] <- y

params = list(eta = 0.1, max_depth = 2, lambda = 1, alpha = 1)

set.seed(584)
model <- xgboost(data = as.matrix(ficoTrain[,-1]),
                 label = ficoTrain[,1],
                 nrounds = 500,
                 objective = "binary:logistic",
                 params = params,
                 verbose = 0)


library(hmeasure)
pred <- predict(model, newdata = as.matrix(ficoTest[, -1]))
results <- HMeasure(unlist(as.vector(ficoTest[,1])), pred, threshold=0.15)
results$metrics$AUC


y_train = ficoTrain[, 1]
x_train = ficoTrain[, -1]
y_explain = ficoTest[, 1]
x_explain = ficoTest[,-1]

x_train = as.data.table(x_train)
x_explain = as.data.table(x_explain)

m = ncol(x_train)


p0 <- mean(y_train)


sim_results_saving_folder = "/nr/project/stat/BigInsight/Projects/Explanations/EffektivShapley/Frida/simuleringsresultater/fico_data_v2/"#"../effektiv_shapley_output/"
shapley_reweighting_strategy = "none"

preds_explain <- predict(model, as.matrix(x_explain))
head(order(-preds_explain),50)
inds_1 <- head(order(-preds_explain),50)
set.seed(123)
inds_2 <- sample(which(preds_explain>quantile(preds_explain,0.9) & preds_explain<min(preds_explain[inds_1])),size = 50,replace = FALSE)
inds <- c(inds_1,inds_2)
set.seed(465132)
#inds = 1:100

# expl <- shapr::explain(model = model,
#                        x_explain= x_explain[inds,],
#                        x_train = x_train,
#                        approach = "ctree",
#                        prediction_zero = p0,
#                        n_combinations = 10000
#                        )
# fwrite(expl$shapley_values,paste0(sim_results_saving_folder,"exact_shapley_values_", shapley_reweighting_strategy, ".csv"))
# print(Sys.time())

# These are the parameters for for iterative_kshap_func
approach = "ctree"

# Reduce if < 10% prob of shapval > 0.05
shapley_threshold_val <-  0.02
shapley_threshold_prob <- 0.2

source("inst/scripts/devel/iterative_kernelshap_sourcefuncs.R")

testObs_computed_vec <- inds# seq_len(n_explain)
runres_list <- runcomps_list <- list()

cutoff_feats = colnames(x_train)

predict_model <- function(model, newdata){
  return(predict(model,as.matrix(newdata)))
}

predict_model(model,x_explain[2,])

# load(paste0("/nr/project/stat/BigInsight/Projects/Explanations/EffektivShapley/Frida/simuleringsresultater/fico_data_v2/iterative_kernelshap_lingauss_p12", shapley_reweighting_strategy, ".RData"))

# set.seed(1298)
# run_obj_list <- list()
# for(kk in seq_along(testObs_computed_vec)){
#   testObs_computed <- testObs_computed_vec[kk]
#   full_pred <- predict(model,as.matrix(x_explain))[testObs_computed]
#   shapsum_other_features <- 0


#   run <- iterative_kshap_func(model,x_explain,x_train,
#                               testObs_computed = testObs_computed,
#                               cutoff_feats = cutoff_feats,
#                               initial_n_combinations = 50,
#                               full_pred = full_pred,
#                               shapsum_other_features = shapsum_other_features,
#                               p0 = p0,
#                               predict_model = predict_model,
#                               shapley_threshold_val = shapley_threshold_val,
#                               shapley_threshold_prob = shapley_threshold_prob,
#                               approach = approach,
#                               shapley_reweighting_strategy = shapley_reweighting_strategy)

#   runres_list[[kk]] <- run$kshap_final
#   runcomps_list[[kk]] <- sum(sapply(run$keep_list,"[[","no_computed_combinations"))
#   write.table(runcomps_list, file = paste0(sim_results_saving_folder,"runcomps_list_", shapley_reweighting_strategy, ".txt"))
#   run_obj_list[[kk]] <- run

#   print(kk)
#   print(Sys.time())
# }

# est <- rbindlist(runres_list)
# est[,other_features:=NULL]
# fwrite(est,paste0(sim_results_saving_folder,"iterative_shapley_values_", shapley_reweighting_strategy, ".csv"))


runcomps_list = fread(paste0(sim_results_saving_folder,"runcomps_list_", shapley_reweighting_strategy, ".txt"))
runcomp_list = as.list(runcomps_list)

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
fwrite(expl_approx,paste0(sim_results_saving_folder,"approx_shapley_values_", shapley_reweighting_strategy, ".csv"))

bias_vec <- colMeans(est-truth)
rmse_vec <- sqrt(colMeans((est-truth)^2))
mae_vec <- colMeans(abs(est-truth))

bias_vec_approx <- colMeans(expl_approx-truth)
rmse_vec_approx <- sqrt(colMeans((expl_approx-truth)^2))
mae_vec_approx <- colMeans(abs(expl_approx-truth))

save.image(paste0(sim_results_saving_folder, "iterative_kernelshap_lingauss_p12_", shapley_reweighting_strategy, ".RData"))

hist(unlist(runcomps_list),breaks = 20)

summary(unlist(runcomps_list))


run$kshap_final
sum(unlist(run$kshap_final))
full_pred

print(Sys.time())

