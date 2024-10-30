library(data.table)
library(MASS)
library(Matrix)
# library(shapr)
devtools::load_all()
library(future)
library(xgboost)
library(ggplot2)
library(reshape2)

m <- 12
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

if (m >= 8){
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
adaptive_arguments = list(initial_n_coalitions = 50,
                          fixed_n_coalitions_per_iter = 10,
                          convergence_tolerance = 1e-6,
                          n_boot_samps = n_boot_samps,
                          shapley_threshold_val = 0.1,
                          shapley_threshold_prob = 0.2,
                          allow_feature_reduction = TRUE
                          )

paired_shap_sampling = FALSE
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

x_explain_all = x_explain

n_test_obs = 10

res_list = list()
tot_n_list = c()
adaptive_arguments$allow_feature_reduction = TRUE
for (ii in 1:n_test_obs){
  print(paste("Observation", ii, "of", n_test_obs))
  x_explain_ii = x_explain_all[ii, ]
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
    n_samples = 10000
  )
  res_list[[ii]] = expl
  iter_list = expl$internal$iter_list

  tot_n = iter_list[[length(iter_list)]]$total_n_coalitions
  tot_n_list = c(tot_n_list, tot_n)
}
tot_n_list
for (i in 1:10){
  print(i)
  print(res_list[[i]]$internal$objects$dropped_vars)
  writeLines(" ")
}

iter_list = expl$internal$iter_list
est =  rbindlist(lapply(iter_list, `[[`, "dt_shapley_est"), idcol = "iter")
sd_est =  rbindlist(lapply(iter_list, `[[`, "dt_shapley_sd"), idcol = "iter")
prob_est =  rbindlist(lapply(iter_list, `[[`, "prob_of_red"), idcol = "iter", fill = T)

dropped_vars = expl$internal$objects$dropped_vars
inds_est = ifelse(is.na(dropped_vars), 1, NA)[, -1]

est[, -(1:3)]*inds_est
sd_est[, -(1:3)]*inds_est
prob_est[, -(1:2)]

# pred = predict(model, as.matrix(x_explain))
# pred - prediction_zero

# for (i in 1:length(expl$internal$iter_list)){
#   est = expl$internal$iter_list[[i]]$dt_shapley_est[, 3:ncol(expl$internal$iter_list[[i]]$dt_shapley_est)]
#   print(sum(est))
# }


# tot_n_list_org = tot_n_list
# tot_n_list = c()
# for (i in 1:10){
#   n_coals = res_list[[i]]$internal$iter_list[[length(res_list[[i]]$internal$iter_list)]]$n_coalitions
#   tot_n_list = c(tot_n_list, n_coals)
# }

adaptive_arguments$allow_feature_reduction = FALSE
res_list_normal = list()
for (ii in 1:n_test_obs){
  print(paste("Observation", ii, "of", n_test_obs))
  x_explain_ii = x_explain[ii, ]
  expl2 <- explain(
    model = model,
    x_explain= x_explain_ii,
    x_train = x_train,
    approach = approach,
    prediction_zero = prediction_zero,
    adaptive = TRUE,
    adaptive_arguments = adaptive_arguments,
    max_n_coalitions = tot_n_list[ii],
    paired_shap_sampling = paired_shap_sampling,
    exact = FALSE
  )
  res_list_normal[[ii]] = expl2
}


expl_exact <- explain(
  model = model,
  x_explain= x_explain[1:n_test_obs,],
  x_train = x_train,
  approach = approach,
  prediction_zero = prediction_zero,
  adaptive = FALSE,
  # max_n_coalitions = tot_n_list[ii],
  paired_shap_sampling = paired_shap_sampling,
  exact = TRUE
)

kshap_red = data.table::rbindlist(lapply(res_list, function(x) x$shapley_values))
kshap_normal = data.table::rbindlist(lapply(res_list_normal, function(x) x$shapley_values))
exact = expl_exact$shapley_values

kshap_red - kshap_normal # Checking that they are the same if no variable reduction is done

# Compute MSE rowwise
mse_red <- colMeans((kshap_red[, -1] - exact[, -1])^2)
mse_normal <- colMeans((kshap_normal[, -1] - exact[, -1])^2)

# Create a data frame for plotting
df_mse <- data.frame(
  variable = rep(names(mse_red), 2),
  mse = c(mse_red, mse_normal),
  method = rep(c("Variable Reduction", "Normal"), each = ncol(kshap_red)-1)
  # n_coalitions = rep(tot_n_list, 2)
)

df_mse$variable <- factor(df_mse$variable, levels = names(mse_red))

# Plot using ggplot2
ggplot(df_mse, aes(x = variable, y = mse, fill = method)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(yintercept = mean(df_mse[df_mse$method != "Normal", "mse"]), linetype = "dashed", color = "#00BFC4", size = 1) +
  geom_hline(yintercept = mean(df_mse[df_mse$method == "Normal", "mse"]), linetype = "dashed", color = "#F8766D", size = 1) +
  labs(title = "MSE Comparison of Shapley Value Estimates",
       x = "Feature",
       y = "Mean Squared Error") +
  theme_minimal()


mae_normal <- colMeans(abs(kshap_normal[, -1] - exact[, -1]))
mae_red <- colMeans(abs(kshap_red[, -1] - exact[, -1]))

# Create a data frame for plotting MAE
df_mae <- data.frame(
  variable = rep(names(mae_red), 2),
  mae = c(mae_red, mae_normal),
  method = rep(c("Variable Reduction", "Normal"), each = ncol(kshap_red)-1)
  # n_coalitions = rep(tot_n_list, 2)
)

df_mae$variable <- factor(df_mae$variable, levels = names(mae_red))

# Plot MAE using ggplot2
ggplot(df_mae, aes(x = variable, y = mae, fill = method)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(yintercept = mean(df_mae[df_mae$method != "Normal", "mae"]), linetype = "dashed", color = "#00BFC4", size = 1) +
  geom_hline(yintercept = mean(df_mae[df_mae$method == "Normal", "mae"]), linetype = "dashed", color = "#F8766D", size = 1) +
  labs(title = "MAE Comparison of Shapley Value Estimates",
        x = "Feature",
        y = "Mean Absolute Error") +
  theme_minimal()
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

# filename = paste0("frida_results/sd_ests_", adaptive_arguments$initial_n_coalitions, "_", adaptive_arguments$fixed_n_coalitions_per_iter, "_", max_n_coalitions, ".RDS")
filename = "/nr/project/stat/BigInsight/Projects/Explanations/EffektivShapley/Frida/shapr_variance_check/sd_ests_60_20_200.RDS"

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
  # saveRDS(list(true_sd = true_sd, mj_sd_est = mj_sd_est, frida_sd_est = frida_sd_est),
  # paste0("frida_results/nr/project/stat/BigInsight/Projects/Explanations/EffektivShapley/Frida/shapr_variance_check//sd_ests_", ending, ".RDS"))

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
obs_to_plot = 1
for (obs in 1:10){
  obs_to_plot = obs
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

  n_coals = c(60, 80, 100, 120, 140, 160, 180, 200)
  # for (i in 1:length(expl$internal$iter_list)){
  #   n_coals = c(n_coals, expl$internal$iter_list[[i]]$n_coalitions)
  # }

  df$n_coals = sort(rep(n_coals, dim(true_sd)[3]))

  p <-  ggplot(df, aes(n_coals, y = true, color = "true")) +
    geom_line() +
    geom_line(aes(y = frida, color = "frida")) +
    geom_line(aes(y = martin, color = "martin")) +
    facet_wrap(~feature_name) +
    labs(title = "Comparison of Shapley value standard deviation estimates",
        x = "Number of coalitions",
        y = "Standard deviation")
  ggsave(paste0("/nr/project/stat/BigInsight/Projects/Explanations/EffektivShapley/Frida/shapr_variance_check//sd_est_plots_", obs, ".png"), p)
}

# ggplot(df, aes(x = n_coals, y = value, color = variable)) +
#   geom_line() +
#   labs(title = "Comparison of Shapley value standard deviation estimates",
#        x = "Number of coalitions",
#        y = "Mean absolute error")

####################################################