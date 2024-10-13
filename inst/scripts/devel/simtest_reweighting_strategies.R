### Upcoming generalization:

#1. Use non-linear truth (xgboost or so)
#2. Even more features


library(data.table)
library(MASS)
library(Matrix)
library(shapr)
library(future)
library(xgboost)

m <- 12
n_train <- 5000
n_explain <- 5
rho_1 <- 0.9
rho_2 <- 0.6
rho_3 <- 0.3
rho_4 <- 0.1
Sigma_1 <- matrix(rho_1, m/4, m/4) + diag(m/4) * (1 - rho_1)
Sigma_2 <- matrix(rho_2, m/4, m/4) + diag(m/4) * (1 - rho_2)
Sigma_3 <- matrix(rho_3, m/4, m/4) + diag(m/4) * (1 - rho_3)
Sigma_4 <- matrix(rho_4, m/4, m/4) + diag(m/4) * (1 - rho_4)

Sigma <- as.matrix(bdiag(Sigma_1, Sigma_2, Sigma_3, Sigma_4))
mu <- rep(0,m)

set.seed(123)


x_train <- as.data.table(MASS::mvrnorm(n_train,mu,Sigma))
x_explain <- as.data.table(MASS::mvrnorm(n_explain,mu,Sigma))

names(x_train) <- paste0("VV",1:m)
names(x_explain) <- paste0("VV",1:m)


beta <- rnorm(m)
alpha <- 1
y_train <- as.vector(alpha + as.matrix(x_train) %*% beta + rnorm(n_train, 0, 1))
y_explain <- alpha + as.matrix(x_explain) %*% beta + rnorm(n_explain, 0, 1)

xy_train <- cbind(y_train, x_train)

set.seed(123)

model <- lm(y_train ~ .,data = xy_train)

p0 <- mean(y_train)


### First run proper shapr call on this

shapley_reweighting_strategy = "none"

set.seed(465132)
progressr::handlers(global = TRUE)
expl <- shapr::explain(model = model,
                       x_explain= x_explain,
                       x_train = x_train,
                       approach = "gaussian",
                       n_batches=100,n_samples = 10000,
                       prediction_zero = p0,Sigma=Sigma,mu=mu)

dt_vS_map <- merge(expl$internal$iter_list[[1]]$coalition_map,expl$internal$output$dt_vS,by="id_coalition")[,-"id_coalition"]


shapley_reweighting_strategy_vec <- c("none","on_N","on_coal_size","on_all","on_all_cond")

n_coalitions_vec <- c(50,100,200,400,800,1200,1600,2000,2400,2800,3200,3600,4000)

reps <- 100

paired_shap_sampling_vec <- c(FALSE,TRUE)

res_list <- list()

for(i0 in seq_along(paired_shap_sampling_vec)){

  for(i in seq_len(reps)){

    for(ii in seq_along(n_coalitions_vec)){

      this_seed <- 1+i
      this_n_coalitions <- n_coalitions_vec[ii]
      this_paired_shap_sampling <- paired_shap_sampling_vec[i0]

      this <- shapr::explain(model = model,
                             x_explain= x_explain,
                             x_train = x_train,
                             approach = "gaussian",
                             n_samples = 10, # Never used
                             n_batches=10,
                             prediction_zero = p0,
                             Sigma=Sigma,
                             mu=mu,
                             seed = this_seed,
                             max_n_coalitions = this_n_coalitions,
                             shapley_reweighting = "none",
                             paired_shap_sampling = this_paired_shap_sampling)

      this0_X <- this$internal$objects$X


      exact_dt_vS <- merge(this$internal$iter_list[[1]]$coalition_map,dt_vS_map,by="coalitions_str")
      setorder(exact_dt_vS,id_coalition)


      for(iii in seq_along(shapley_reweighting_strategy_vec)){
        this_shapley_reweighting_strategy <- shapley_reweighting_strategy_vec[iii]

        this_X <- copy(this0_X)

        shapr:::shapley_reweighting(this_X,reweight=this_shapley_reweighting_strategy)

        this_W <- weight_matrix(
          X = this_X,
          normalize_W_weights = TRUE
        )

        shap_dt0 <- as.data.table(cbind(seq_len(n_explain),t(this_W%*%as.matrix(exact_dt_vS[,-c("coalitions_str","id_coalition")]))))
        names(shap_dt0) <- names(this$shapley_values)

        this_diff <- unlist(shap_dt0[,-c(1,2)]-expl$shapley_values[,-c(1,2)])
        this_bias <- mean(this_diff)
        this_var <- var(this_diff)
        this_MAE <- mean(abs(this_diff))
        this_RMSE <- sqrt(mean(this_diff^2))

        res_vec <- data.table(n_coalitions = this_n_coalitions,
                     paired_shap_sampling = this_paired_shap_sampling,
                     shapley_reweighting_strategy = this_shapley_reweighting_strategy,
                     seed = this_seed,
                     bias=this_bias,
                     var = this_var,
                     MAE = this_MAE,
                     RMSE = this_RMSE)

        res_list[[length(res_list)+1]] <- copy(res_vec)

      }

    }

    print(i)

  }

}


res_dt <- rbindlist(res_list)

fwrite(res_dt,file = "../../Div/extra_shapr_scripts_etc/res_dt_reweighting_sims_lingaus.csv")

resres <- res_dt[,lapply(.SD,mean),.SDcols=c("bias","var","MAE","RMSE"),by=.(paired_shap_sampling,n_coalitions,shapley_reweighting_strategy)]

library(ggplot2)

ggplot(resres[paired_shap_sampling==TRUE],aes(x=n_coalitions,y=MAE,col=shapley_reweighting_strategy,linetype= paired_shap_sampling))+
         geom_line()



#### OLD ####

### Need to create an lm analogoue to pred_mod_xgb here


set.seed(123)



# These are the parameters for for interative_kshap_func
n_samples <- 1000
approach = "gaussian"

# Reduce if < 10% prob of shapval > 0.2

source("inst/scripts/devel/iterative_kernelshap_sourcefuncs.R")

testObs_computed_vec <- inds# seq_len(n_explain)

# Using threshold: 0.1
runres_list <- runcomps_list <- list()
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
                              shapley_threshold_val = shapley_threshold_val,
                              shapley_threshold_prob = shapley_threshold_prob,
                              approach = approach,
                              n_samples = n_samples,
                              gaussian.mu = mu,
                              gaussian.cov_mat = Sigma,
                              shapley_reweighting_strategy = shapley_reweighting_strategy)
  runres_list[[kk]] <- run$kshap_final
  runcomps_list[[kk]] <- sum(sapply(run$keep_list,"[[","no_computed_combinations"))
  print(kk)
}

est <- rbindlist(runres_list)
est[,other_features:=NULL]
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







