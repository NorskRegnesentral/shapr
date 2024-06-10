
### Upcoming generalization:

#1. Use non-linear truth (xgboost or so)
#2. Even more features


library(data.table)
library(MASS)
library(Matrix)
library(shapr)

m <- 12
n_train <- 10000
n_explain <- 100
rho_1 <- 0.5
rho_2 <- 0.5
rho_3 <- 0.5
rho_4 <- 0
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

beta <- c(5:1, rep(0, m - 5))
alpha <- 1
y_train <- as.vector(alpha + as.matrix(x_train) %*% beta + rnorm(n_train, 0, 1))
y_explain <- alpha + as.matrix(x_explain) %*% beta + rnorm(n_explain, 0, 1)

xy_train <- cbind(y_train, x_train)

model <- lm(y_train ~ .,data = xy_train)

p0 <- mean(y_train)


### First run proper shapr call on this

sim_results_saving_folder = "../effektiv_shapley_output/"
shapley_reweighting_strategy = "none"

set.seed(465132)
inds = 1:n_explain
expl <- shapr::explain(model = model,
                       x_explain= x_explain[inds,],
                       x_train = x_train,
                       approach = "gaussian",
                       prediction_zero = p0,Sigma=Sigma,mu=mu)
fwrite(expl$shapley_values,paste0(sim_results_saving_folder,"exact_shapley_values", shapley_reweighting_strategy, ".csv"))


cutoff_feats <- paste0("VV",1:12)


### Need to create an lm analogoue to pred_mod_xgb here


set.seed(123)



# These are the parameters for for interative_kshap_func
n_samples <- 1000
approach = "gaussian"

gaussian.mu <- mu
gaussian.cov_mat <- Sigma

# Reduce if < 10% prob of shapval > 0.2
shapley_threshold_val <-  0.2
shapley_threshold_prob <- 0.1

source("inst/scripts/devel/iterative_kernelshap_sourcefuncs.R")

testObs_computed_vec <- inds# seq_len(n_explain)
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
                              approach = "gaussian",
                              n_samples = 1000,
                              gaussian.mu = mu,
                              gaussian.cov_mat = Sigma,
                              shapley_reweighting_strategy = shapley_reweighting_strategy)
  runres_list[[kk]] <- run$kshap_final
  runcomps_list[[kk]] <- sum(sapply(run$keep_list,"[[","no_computed_combinations"))
  print(kk)
}

est <- rbindlist(runres_list)
est[,other_features:=NULL]
fwrite(est,paste0(sim_results_saving_folder,"iterative_shapley_values", shapley_reweighting_strategy, ".csv"))

truth <- expl$shapley_values

expl_approx <- matrix(0, nrow = length(inds), ncol = m+1)
expl_approx_obj_list <- list()
for (i in testObs_computed_vec){
  expl_approx_obj <- shapr::explain(model = model,
                        x_explain= x_explain[inds[i],],
                        x_train = x_train,
                        approach = "gaussian",
                        prediction_zero = p0,Sigma=Sigma,mu=mu,
                        n_combinations = runcomps_list[[i]])
  expl_approx[i,] = unlist(expl_approx_obj$shapley_values)
  expl_approx_obj_list[[i]] <- expl_approx_obj
}
expl_approx <- as.data.table(expl_approx)
colnames(expl_approx) <- colnames(truth)
fwrite(expl_approx,paste0(sim_results_saving_folder,"approx_shapley_values", shapley_reweighting_strategy, ".csv"))

bias_vec <- colMeans(est-truth)
rmse_vec <- sqrt(colMeans((est-truth)^2))
mae_vec <- colMeans(abs(est-truth))

bias_vec_approx <- colMeans(expl_approx-truth)
rmse_vec_approx <- sqrt(colMeans((expl_approx-truth)^2))
mae_vec_approx <- colMeans(abs(expl_approx-truth))

save.image(paste0(sim_results_saving_folder, "iterative_kernelshap_lingauss_p12", shapley_reweighting_strategy, ".RData"))

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

