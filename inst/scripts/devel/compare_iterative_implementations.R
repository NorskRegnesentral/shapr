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

beta <- c(0.2, -0.8, 1.0, 0.5, -0.8, rep(0, m - 5))
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

p0 <- mean(y_train)


### First run proper shapr call on this


set.seed(465132)
inds = 1:5#1:n_explain

x_explain= x_explain[inds,]
x_train = x_train
approach = "gaussian"
prediction_zero = p0
adaptive = TRUE
n_boot_samps = 10
adaptive_arguments = list(initial_n_coalitions = 10,
                          fixed_n_coalitions_per_iter = 10,
                          convergence_tolerance = 0.00001,
                          n_boot_samps = n_boot_samps)

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
#

devtools::load_all()
# rm(list = c("predict_model"))

# debugonce(explain)
expl <- explain(
  model = model,
  x_explain= x_explain,
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

filename = paste0("sd_comparison_", adaptive_arguments$initial_n_coalitions, "_", adaptive_arguments$fixed_n_coalitions_per_iter, "_", max_n_coalitions, ".csv")

if (file.exists(filename)){
  df = read.csv(filename)
} else {

  true_sd = list()
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
                      )

      col_inds = which(colnames(kshap$shapley_values) %in% colnames(x_explain))
      true_sd_array[,,j] = as.matrix(kshap$shapley_values[, ..col_inds])
    }

    true_sd[[i]] <- apply(true_sd_array, c(1, 2), sd)
  }


  mj_sd_est = list()
  frida_sd_est = list()
  for (i in 1:length(expl$internal$iter_list)){
    sd_est = expl$internal$iter_list[[i]]$dt_shapley_sd
    col_inds = which(colnames(sd_est) %in% colnames(x_explain))

    mj_sd_est[[i]] = sd_est[, ..col_inds]

    frida_sd_est[[i]] = expl$internal$iter_list[[i]]$frida_boot_shapley_values
  }

  mae = function(x, y){
    return(mean(abs(as.matrix(x) - as.matrix(y))))
  }


  df = data.frame(mae_frida = numeric(length(true_sd)),
                  mae_mj = numeric(length(true_sd)),
                  n_coals = numeric(length(true_sd))
                  )

  for (i in 1:length(true_sd)){
    df$mae_frida[i] = mae(true_sd[[i]], frida_sd_est[[i]])
    df$mae_mj[i] = mae(true_sd[[i]], mj_sd_est[[i]])
    df$n_coals[i] = expl$internal$iter_list[[i]]$n_coalitions
  }

  df = melt(df, id.vars = "n_coals")

  write.csv(df, filename)
}

ggplot(df, aes(x = n_coals, y = value, color = variable)) +
  geom_line() +
  labs(title = "Comparison of Shapley value standard deviation estimates",
       x = "Number of coalitions",
       y = "Mean absolute error")

####################################################