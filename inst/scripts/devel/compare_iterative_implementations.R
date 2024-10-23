library(data.table)
library(MASS)
library(Matrix)
# library(shapr)
devtools::load_all()
library(future)
library(xgboost)
library(ggplot2)
library(reshape2)

m <- 8
n_train <- 5000
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


g <- function(a,b){
  a*b+a*b^2+a^2*b
}

if (m == 8){
  beta <- c(0.2, -0.8, 1.0, 0.5, -0.8, rep(0, m - 5))
} else if (m == 4){
  beta <- c(0.2, -0.8, 1.0, 0.5)
}
gamma <- c(0.8,-1)
alpha <- 1
y_train <- alpha +
  as.vector(as.matrix(cos(x_train))%*%beta) +
  unlist(gamma[1]*g(x_train[,1],x_train[,2])) +
  unlist(gamma[1]*g(x_train[,3],x_train[,4])) +
  rnorm(n_train, 0, 1)
y_explain <- alpha +
  as.vector(as.matrix(cos(x_explain))%*%beta) +
  unlist(gamma[1]*g(x_explain[,1],x_explain[,2])) +
  unlist(gamma[1]*g(x_explain[,3],x_explain[,4])) +
  rnorm(n_train, 0, 1)

xy_train <- cbind(y_train, x_train)

set.seed(123)
model <- xgboost(
  data = as.matrix(x_train),
  label = y_train,
  nround = 50,
  verbose = FALSE
)

pred_train <- predict(model, as.matrix(x_train))

this_order <- order(unlist(x_train[,1]))

# plot(unlist(x_train[this_order,1]),pred_train[this_order],type="l")

prediction_zero <- mean(y_train)


### First run proper shapr call on this


set.seed(465132)
approach = "gaussian"
adaptive = TRUE
n_boot_samps = 100
adaptive_arguments = list(initial_n_coalitions = 60,
                          fixed_n_coalitions_per_iter = 10,
                          convergence_tolerance = 1e-6,
                          n_boot_samps = n_boot_samps,
                          shapley_threshold_val = 0.1,
                          shapley_threshold_prob = 0.05,
                          allow_feature_reduction = TRUE
                          )

paired_shap_sampling = TRUE
max_n_coalitions = 200
group = NULL
n_MC_samples = 1e3
n_batches = NULL
seed = 1
keep_samp_for_vS = FALSE
predict_model = NULL
get_model_specs = NULL
MSEv_uniform_comb_weights = TRUE
verbose = 0
print_shapleyres = TRUE # tmp
print_iter_info = TRUE # tmp
shapley_reweighting = "none" # tmp # "on_N" # TODO: Make "on_N" the default later on.
prev_shapr_object = NULL

res_list = list()
tot_n_list = c()
for (ii in 1:10){
  x_explain_ii = x_explain[ii, ]
  expl <- explain(
    model = model,
    x_explain= x_explain_ii,
    x_train = x_train,
    approach = approach,
    prediction_zero = prediction_zero,
    adaptive = adaptive,
    adaptive_arguments = adaptive_arguments,
    shapley_reweighting = shapley_reweighting,
    max_n_coalitions = max_n_coalitions,
    paired_shap_sampling = paired_shap_sampling,
    # seed = 15,
    # verbose = 2,
    # print_iter_info = TRUE,
    # print_shapleyres = TRUE,
  )
  res_list[[ii]] = expl
  iter_list = expl$internal$iter_list
  tot_n = n_prev = 0
  for (i in 1:length(iter_list)){
    n_org = iter_list[[i]]$n_coalitions_org
    n = iter_list[[i]]$n_coalitions
    if (is.null(n_org)) {n_org = n}
    tot_n = tot_n + n_org - n_prev
    n_prev = n
  }
  tot_n_list[ii] = tot_n
}
tot_n_list


pred = predict(model, as.matrix(x_explain))
pred - prediction_zero

for (i in 1:length(expl$internal$iter_list)){
  est = expl$internal$iter_list[[i]]$dt_shapley_est[, 3:ncol(expl$internal$iter_list[[i]]$dt_shapley_est)]
  # print(est)
  print(sum(est))
  # print(as.numeric(expl$internal$iter_list[[i]]$shap_reduction$reduced_dt_shapley_est))
  # print(prediction_zero - expl$internal$iter_list[[i]]$dt_vS[.N, p_hat1_1])
 # writeLines(" ")
}

iter_list = expl$internal$iter_list
for (iter in 1:length(iter_list)){
  print(iter)
  print(iter_list[[iter]]$shap_reduction$reduced_dt_shapley_est)
  print(iter_list[[iter]]$shap_reduction$dropped_features)
  writeLines(" ")
}

expl2 <- explain(
  model = model,
  x_explain= x_explain,
  x_train = x_train,
  approach = approach,
  prediction_zero = prediction_zero,
  adaptive = FALSE,
  # max_n_coalitions = max_n_coalitions,
  paired_shap_sampling = paired_shap_sampling,,
  exact = TRUE
)

# library(profvis)

# p = profvis(explain(
#   model = model,
#   x_explain= x_explain,
#   x_train = x_train,
#   approach = approach,
#   prediction_zero = prediction_zero,
#   adaptive = adaptive,
#   adaptive_arguments = adaptive_arguments,
#   shapley_reweighting = shapley_reweighting,
#   max_n_coalitions = 300,
# ))

# saveRDS(p, "profile.RDS")


############## Compare standard deviation estimates ##############

# adaptive_arguments$initial_n_coalitions = 100
# adaptive_arguments$fixed_n_coalitions_per_iter = 100
# max_n_coalitions = 1000

filename = paste0("frida_results/sd_ests_", adaptive_arguments$initial_n_coalitions, "_", adaptive_arguments$fixed_n_coalitions_per_iter, "_", max_n_coalitions, ".RDS")

if (file.exists(filename)){
  df_list = readRDS(filename)
  true_sd = df_list$true_sd
  mj_sd_est = df_list$mj_sd_est
  frida_sd_est = df_list$frida_sd_est
} else {

  true_sd = array(NA, dim = c(length(expl$internal$iter_list), nrow(x_explain), m))
  set.seed(432)
  seeds = sample.int(1000, n_boot_samps, replace = FALSE)
  for (i in 1:length(expl$internal$iter_list)){
    n_coals = expl$internal$iter_list[[i]]$n_coalitions
    true_sd_array <- array(0, dim = c(length(inds), m, n_boot_samps))

    for (j in 1:n_boot_samps){
      print(c(i, j))
      kshap = explain(model = model,
                      x_explain= x_explain,
                      x_train = x_train,
                      approach = approach,
                      prediction_zero = prediction_zero,
                      max_n_coalitions = n_coals,
                      adaptive_arguments = list(compute_sd = FALSE),
                      seed = seeds[j],
                      paired_shap_sampling = paired_shap_sampling
                      )

      col_inds = which(colnames(kshap$shapley_values) %in% colnames(x_explain))
      true_sd_array[,,j] = as.matrix(kshap$shapley_values[, ..col_inds])
    }

    true_sd[i, , ] <- apply(true_sd_array, c(1, 2), sd)
  }


  mj_sd_est = array(NA, dim = c(length(expl$internal$iter_list), nrow(x_explain), m))
  frida_sd_est = array(NA, dim = c(length(expl$internal$iter_list), nrow(x_explain), m))
  for (i in 1:length(expl$internal$iter_list)){
    sd_est = expl$internal$iter_list[[i]]$dt_shapley_sd
    col_inds = which(colnames(sd_est) %in% colnames(x_explain))

    mj_sd_est[i, ,] = as.matrix(sd_est[, ..col_inds])

    frida_sd_est[i, , ] = as.matrix(expl$internal$iter_list[[i]]$frida_boot_shapley_values)
  }

  ending = paste0(adaptive_arguments$initial_n_coalitions, "_", adaptive_arguments$fixed_n_coalitions_per_iter, "_", max_n_coalitions)
  saveRDS(list(true_sd = true_sd, mj_sd_est = mj_sd_est, frida_sd_est = frida_sd_est),
  paste0("frida_results/sd_ests_", ending, ".RDS"))

#   mae = function(x, y){
#     return(mean(abs(as.matrix(x) - as.matrix(y))))
#   }


#   df = data.frame(mae_frida = numeric(length(true_sd)),
#                   mae_mj = numeric(length(true_sd)),
#                   n_coals = numeric(length(true_sd))
#                   )

#   for (i in 1:length(true_sd)){
#     df$mae_frida[i] = mae(true_sd[[i]], frida_sd_est[[i]])
#     df$mae_mj[i] = mae(true_sd[[i]], mj_sd_est[[i]])
#     df$n_coals[i] = expl$internal$iter_list[[i]]$n_coalitions
#   }

#   df = melt(df, id.vars = "n_coals")

#   write.csv(df, filename)
}
obs_to_plot = 6
df = data.frame(true = numeric(length(true_sd[, obs_to_plot, ])),
                frida = numeric(length(true_sd[, obs_to_plot, ])),
                martin = numeric(length(true_sd[, obs_to_plot, ]))
                # iter_nr = numeric(length(true_sd[, obs_to_plot, ])),
                # feature_name = string(length(true_sd[, obs_to_plot, ]))
                )
for (i in 1:dim(true_sd)[1]){
  df[(1 + m*(i-1)):((i)*m), "true"] = true_sd[i, obs_to_plot, ]
  df[(1 + m*(i-1)):((i)*m), "feature_name"] = colnames(x_explain)
  df[(1 + m*(i-1)):((i)*m), "frida"] = frida_sd_est[i, obs_to_plot, ]
  df[(1 + m*(i-1)):((i)*m), "martin"] = mj_sd_est[i, obs_to_plot, ]
}

n_coals = c()
for (i in 1:length(expl$internal$iter_list)){
  n_coals = c(n_coals, expl$internal$iter_list[[i]]$n_coalitions)
}
df$n_coals = sort(rep(n_coals, dim(true_sd)[3]))

ggplot(df, aes(n_coals, y = true, color = "true")) +
  geom_line() +
  geom_line(aes(y = frida, color = "frida")) +
  geom_line(aes(y = martin, color = "martin")) +
  facet_wrap(~feature_name) +
  labs(title = "Comparison of Shapley value standard deviation estimates",
       x = "Number of coalitions",
       y = "Standard deviation")



# ggplot(df, aes(x = n_coals, y = value, color = variable)) +
#   geom_line() +
#   labs(title = "Comparison of Shapley value standard deviation estimates",
#        x = "Number of coalitions",
#        y = "Mean absolute error")

####################################################