
#### Script for a simulation study with mixed data (i.e. categorical + continuous) ####

# Assuming a linear model

library(shapr)
library(data.table)
library(MASS)

response_mod <- function(mod_matrix_full, beta, epsilon){
  as.vector(mod_matrix_full %*% beta) + epsilon
}

source("inst/devel_scripts/paper_simulations/mixed_data_simulation_functions.R")

methods = c("ctree", "kernelSHAP", "empirical")
N_sample_gaussian <- c(100, 1000)

## Defining variables
No_cont_var <- 2
No_cat_var <- 2
No_levels <- 3

No_tot_var <- No_cont_var + No_cat_var

Sigma_diag = 1
corr = 0.0
mu = rep(0, No_tot_var)
beta_0 <- 1
beta_cont <- c(1, -1)
beta_cat <- c(1, 0, -1,
              2, 3, -1)

beta <- c(beta_0, beta_cont,beta_cat)
No_train_obs = 100
No_test_obs = 100
cat_cutoff = c(-200, 0, 1, 200)
noise = FALSE
name = 'testing'
seed <- 123



## Basic tests #
if(length(beta) != 1 + No_cont_var + No_cat_var * No_levels){
  stop("Incorrect length for beta.")
}
if(length(mu) != No_cont_var + No_cat_var){
  stop("Incorrect length for mu.")
}
if(length(cat_cutoff) != No_levels + 1){
  stop("Incorrect length of cat_cutoff.")
}



## Preparing covariate sampling distribution
Sigma <- matrix(corr, No_tot_var, No_tot_var)
diag(Sigma) <- Sigma_diag

set.seed(seed)
x_train <- mvrnorm(n =  No_train_obs, mu = mu, Sigma = Sigma)

dt_train <- data.table(x_train[, 1:No_cont_var])
for (i in (No_cont_var + 1):No_tot_var){
  dt_train <- cbind(dt_train, cut(x_train[, i], cat_cutoff, labels = 1:No_levels))
}

names(dt_train) <- c(paste0("cont_", 1:No_cont_var, "_"), paste0("cat_", 1:No_cat_var, "_"))

if(noise == TRUE){
  dt_train[, epsilon := rnorm(No_train_obs, 0, 0.1^2)] #
} else{
  dt_train[, epsilon := 0]
}


x_test <- mvrnorm(n =  No_test_obs, mu = mu, Sigma = Sigma)

dt_test <- data.table(x_test[, 1:No_cont_var])
for (i in (No_cont_var + 1):No_tot_var){
  dt_test <- cbind(dt_test, cut(x_test[, i], cat_cutoff, labels = 1:No_levels))
}

names(dt_test) <- c(paste0("cont_", 1:No_cont_var, "_"), paste0("cat_", 1:No_cat_var, "_"))

if(noise == TRUE){
  dt_test[, epsilon := rnorm(No_test_obs, 0, 0.1^2)] #
} else{
  dt_test[, epsilon := 0]
}

dt <- rbind(dt_train, dt_test)

cont_cols <- names(dt)[grep("cont", names(dt))]
cat_cols <- names(dt)[grep("cat", names(dt))]
feat_names <- c(cont_cols, cat_cols)

## This is for KernelSHAP
dt_numeric <- copy(dt)
dt_numeric[, (cat_cols) := lapply(.SD, as.numeric), .SDcols = cat_cols]

dt[, (cat_cols) := lapply(.SD, as.factor), .SDcols = cat_cols] # It is actually already a factor

train_obs <- 1:No_train_obs
test_obs <- (No_train_obs + 1):(No_train_obs + No_test_obs)


## One-hot encoding of data
mod_matrix <- model.matrix(~.-1, data = dt[, ..feat_names], contrasts.arg = lapply(dt[, ..cat_cols], contrasts, contrasts = FALSE))

dt <- cbind(dt, data.table(mod_matrix[, -(1:No_cont_var)])) # Adding the non-continuous columns to the original dt
full_onehot_names <- colnames(mod_matrix)
reduced_onehot_names <- full_onehot_names[-grep("_1$", full_onehot_names)] # names without reference levels

## Calculate response
dt[, response := response_mod(mod_matrix_full = cbind(1, mod_matrix), beta = beta, epsilon = epsilon)]
dt_numeric[, response := dt[['response']]]

## Fit model
fmla <- as.formula(paste0("response~", paste(feat_names, collapse = "+")))
model <- lm(formula = fmla, data = dt[train_obs, ])

fmla_onehot <- as.formula(paste("response ~", paste(reduced_onehot_names, collapse = " + ")))
model_onehot <- lm(fmla_onehot, data = dt[(1:No_train_obs)])

## Initalize shapr object with trained model -- this is used for calculating true shapley
x_train <- dt[train_obs, ..feat_names]
x_test <- dt[test_obs, ..feat_names]
y_train <- dt[train_obs, .(response)]

x_train_numeric <- dt_numeric[train_obs, ..feat_names]
x_test_numeric <- dt_numeric[test_obs, ..feat_names]

x_test_onehot_full <- dt[test_obs, ..full_onehot_names]

x_test_onehot_reduced <- dt[test_obs, ..reduced_onehot_names]
x_train_onehot_reduced <- dt[train_obs, ..reduced_onehot_names]

##
explainer <- shapr(x_train, model)

if(any(grepl("empirical", methods)) | any(grepl("gaussian", methods)) | any(grepl("ctree_onehot", methods))){
  explainer_onehot <- shapr(x_train_onehot_reduced, model_onehot)
}

## Create custom function of model_type for lm - for kernelSHAP
model_type.numeric_lm <- function(x) {
}

features.numeric_lm <- function(x, cnms, feature_labels = NULL) {
  if (!is.null(feature_labels)) message_features_labels()

  nms <- tail(all.vars(x$terms), -1)
  if (!all(nms %in% cnms)) error_feature_labels()
  return(nms)
}

predict_model.numeric_lm <- function(x, newdata) {
  newdata <- as.data.table(newdata)

  cat_cols <- names(newdata)[grep("cat",names(newdata))]
  newdata[, (cat_cols) := lapply(.SD, as.factor),.SDcols = cat_cols]

  class(x) <- "lm"
  predict(x, newdata)
}

model_numeric <- model
class(model_numeric) <- "numeric_lm"
explainer_numeric <- shapr(x_train_numeric, model_numeric)
## END



## Calculate the true shapley values
S <- explainer$S

ind_cont_cols <- which(names(x_train) %in% cont_cols)
ind_cat_cols <- which(names(x_train) %in% cat_cols)

ind_cont_cols_logical <- names(x_train) %in% cont_cols
ind_cat_cols_logical <- names(x_train) %in% cat_cols


case_matrix <- NA*S

for (i in 2:(nrow(S)-1)){
  S_i <- which(as.logical(S[i,]))
  S_is_cont <-  any(S_i %in% ind_cont_cols)
  S_is_cat <-  any(S_i %in% ind_cat_cols)
  S_is_cont_and_cat <- as.logical(S_is_cont*S_is_cat)
  Sbar_i_logical <- as.logical(1-S[i,])

  if(S_is_cont_and_cat){
    case_matrix[i,which(ind_cont_cols_logical &Sbar_i_logical)] <- 5
    case_matrix[i,which(ind_cat_cols_logical &Sbar_i_logical)] <- 6
  } else {
    if(S_is_cont){
      case_matrix[i,which(ind_cont_cols_logical &Sbar_i_logical)] <- 1
      case_matrix[i,which(ind_cat_cols_logical &Sbar_i_logical)] <- 4
    }
    if(S_is_cat){
      case_matrix[i,which(ind_cont_cols_logical &Sbar_i_logical)] <- 2
      case_matrix[i,which(ind_cat_cols_logical &Sbar_i_logical)] <- 3
    }
  }
}

eps <- 10^-6 # Very low sensitivity to this value, just don't set it smaller than 10^-6 as that might give NaN's
x_test_C_lower <- copy(x_test)
x_test_C_lower[, (cont_cols):= lapply(.SD,function(x){x-eps}),.SDcols=cont_cols]
x_test_C_lower[, (cat_cols):= lapply(.SD,function(x){cat_cutoff[as.numeric(x)]}),.SDcols=cat_cols]

x_test_C_upper <- copy(x_test)
x_test_C_upper[, (cont_cols):= lapply(.SD,function(x){x+eps}),.SDcols=cont_cols]
x_test_C_upper[, (cat_cols):= lapply(.SD,function(x){cat_cutoff[as.numeric(x)+1]}),.SDcols=cat_cols]

range_x_int <- c(min(mu) - 4 * sqrt(max(diag(Sigma))), max(mu) + 4 * sqrt(max(diag(Sigma))))
No_int_eval <- 500
h <- diff(range_x_int) / No_int_eval
x_int_grid <- seq(range_x_int[1] + h / 2, range_x_int[2] - h / 2, by = h)

x_int_grid_cat <- as.numeric(cut(x_int_grid, cat_cutoff, labels = c(1:No_levels)))


## Restructuring the beta vector per feature
beta_list <- list()
for(i in 1:No_cont_var){
  beta_list[[i]] <- beta_cont[i]
}

k <- 1
for(i in (No_cont_var + 1):No_tot_var){
  beta_list[[i]] <- beta_cat[(k-1) * No_levels + (1:No_levels)]
  k <- k + 1
}

## Restructuring one_hot test data per feature
x_test_onehot_full_list <- list()
for(i in 1:No_cont_var){
  x_test_onehot_full_list[[i]] <- x_test_onehot_full[, ..i]
}

k <- 1
for(i in (No_cont_var + 1):No_tot_var){
  this_col <- No_cont_var + (k - 1) * No_levels + (1:No_levels)
  x_test_onehot_full_list[[i]] <- x_test_onehot_full[, ..this_col]
  k <- k + 1
}

phi_0_contrib <- rep(NA, No_tot_var)
for(i in 1:No_cont_var){
  phi_0_contrib[i] <- beta_list[[i]] * mu[i]
}

for(i in (No_cont_var + 1):No_tot_var){
  phi_0_contrib[i] <- t(beta_list[[i]]) %*% diff(pnorm(q = cat_cutoff, mean = mu[i], sd = sqrt(Sigma[i, i])))
}

phi0 <- beta[1] + sum(phi_0_contrib)


## Building the Vs_mat here
Vs_mat <- matrix(NA, ncol = No_test_obs, nrow = nrow(S))

Vs_mat[1, ] <-   phi0
Vs_mat[nrow(S),] <- predict(model, x_test)

# algorithm <- mvtnorm::GenzBretz() # Not exact and slower for small dimensions
algorithm <- mvtnorm::Miwa()

start <- proc.time()
mc.cores <- 16
for (i in 2:(nrow(S)-1)){
  S_i <-   which(as.logical(S[i,]))
  Sbar_i <-   which(as.logical(1-S[i,]))

  S_features <- feat_names[S_i]

  x_test_S_i <- x_test[,..S_features]

  x_test_C_lower_S_i_list <- split(x_test_C_lower[, ..S_features],f = 1:No_test_obs)
  x_test_C_upper_S_i_list <- split(x_test_C_upper[, ..S_features],f = 1:No_test_obs)

  Vs_sum_contrib_mat <- matrix(NA, ncol = No_tot_var, nrow=No_test_obs)


  prep_list_2 <- list()
  for (j in Sbar_i){

    Omega <- Sigma[c(j, S_i), c(j, S_i)]
    xi <- mu[c(j, S_i)]

    prep_list_1 <- mapply(prep_dens_x_given_S_is_C_func,
                          C_lower =x_test_C_lower_S_i_list,
                          C_upper = x_test_C_upper_S_i_list,
                          MoreArgs = list(xi = xi, Omega = Omega, algorithm = algorithm),
                          SIMPLIFY = FALSE)

    prep_list_2[[j]] <- unlist(lapply(prep_list_1, prep_dens_x_given_S_is_C_func_v2, x_vec = x_int_grid), recursive = F)
  }


  intval_list_no_x = parallel::mclapply(X = unlist(prep_list_2, recursive = F),FUN = compute_dens_x_given_S_is_C_func_v2, mc.cores = mc.cores)
  intval_mat_no_x = matrix(unlist(intval_list_no_x), ncol = No_test_obs)

  intval_array_no_x = array(unlist(intval_list_no_x), dim = c(length(x_int_grid), No_test_obs, length(Sbar_i)))

  jj <- 1
  for (j in Sbar_i){
    j_is_cont <- j %in% ind_cont_cols

    if (j_is_cont){
      ## Continuous expectation
      expectation_vec <- rep(NA, No_test_obs)
      for(k in 1:No_test_obs){
        expectation_vec[k] <- h*sum(intval_array_no_x[,k, jj] * x_int_grid)
      }
      Vs_sum_contrib_vec <- beta_list[[j]] * expectation_vec


    } else {
      # categorical expectation
      prob_mat <- matrix(NA, nrow = No_test_obs, ncol = No_levels)
      for(k in 1:No_test_obs){
        for (l in 1:No_levels){
          prob_mat[k,l] <- h * sum(intval_array_no_x[, k, jj][x_int_grid_cat == l])
        }
        prob_mat[k,] <- prob_mat[k, ] / sum(prob_mat[k, ])
      }
      Vs_sum_contrib_vec <- as.vector(beta_list[[j]] %*% t(prob_mat))

    }

    Vs_sum_contrib_mat[,j] <- Vs_sum_contrib_vec

    jj <- jj + 1
  }

  for (j in S_i){
    Vs_sum_contrib_mat[,j] <- beta_list[[j]] %*% t(x_test_onehot_full_list[[j]])
  }
  Vs_mat[i,] <- rowSums(Vs_sum_contrib_mat)
  print(i)
}
end <- proc.time()
end - start ## 215 seconds

exactShap <- matrix(NA, ncol = No_tot_var + 1, nrow = No_test_obs)
for (i in 1:No_test_obs){
  exactShap[i,] <- c(explainer$W %*% Vs_mat[,i])
}

exactShap

max(abs(rowSums(exactShap) - predict(model, x_test))) #

### Estimating the Shapley values ####

p <- mean(y_train$response) # since y_train is no longer a matrix

## to compute sum of Shapley values - only for one-hot encoded variables
beta_matcher <- as.numeric(getstr(reduced_onehot_names))[-(1:No_cont_var)]
No_features <- max(beta_matcher) + No_cont_var
phi_sum_mat <- matrix(NA, nrow = No_test_obs, ncol = No_features)


timeit <- list()

explanation_list <- list()
for(m in methods){
  if(m == 'empirical'){
    tm0 <- proc.time()
    explanation_list[[m]] <- explain(
      x_test_onehot_reduced,
      approach = m,
      explainer = explainer_onehot,
      prediction_zero = p,
      sample = FALSE,
      w_threshold = 1,
      n_samples = 1000)
    tm1 <- proc.time()

    for(i in 1:No_cont_var){
      phi_sum_mat[, i] <- explanation_list[[m]]$dt[[i + 1]]
    }

    for (i in 1:No_cat_var){
      phi_sum_mat[, i + No_cont_var] <- rowSums(subset(explanation_list[[m]]$dt, select = which(beta_matcher == i) + 1))
    }
    colnames(phi_sum_mat) <- feat_names
    explanation_list[[m]]$dt_sum <- cbind(explanation_list[[m]]$dt[, 1], phi_sum_mat)

    print(paste0("Finished estimating Shapley value with ", m, " method."), quote = FALSE, right = FALSE)
    print(tm1 - tm0)
    timeit[m] <- list((tm1 - tm0))

  } else if(m == 'gaussian'){
    for(j in N_sample_gaussian){
      tm0 <- proc.time()
      for(k in 1:10){
        explanation_list[[paste0(m, "_nsamples", j, "_trial", k)]] <- explain(
          x_test_onehot_reduced,
          approach = m,
          explainer = explainer_onehot,
          prediction_zero = p,
          sample = FALSE,
          w_threshold = 1,
          n_samples = j,
          seed = k)

        for(i in 1:No_cont_var){
          phi_sum_mat[, i] <- explanation_list[[paste0(m, "_nsamples", j, "_trial", k)]]$dt[[i + 1]]
        }

        for (i in 1:No_cat_var){
          phi_sum_mat[, i + No_cont_var] <- rowSums(subset(explanation_list[[paste0(m, "_nsamples", j, "_trial", k)]]$dt, select = which(beta_matcher == i) + 1))
        }
        colnames(phi_sum_mat) <- feat_names
        explanation_list[[paste0(m, "_nsamples", j, "_trial", k)]]$dt_sum <- cbind(explanation_list[[paste0(m, "_nsamples", j, "_trial", k)]]$dt[, 1], phi_sum_mat)
      }
      tm1 <- proc.time()

      print(paste0("Finished estimating Shapley value with ", paste0(m, "_nsamples", j), " method."), quote = FALSE, right = FALSE)
      print(tm1 - tm0)
      timeit[paste0(m, "_nsamples", j)] <- list((tm1 - tm0))
    }
  } else if(m == 'ctree_onehot'){

    tm0 <- proc.time()
    explanation_list[[m]] <- explain(
      x_test_onehot_reduced,
      approach = 'ctree',
      explainer = explainer_onehot,
      prediction_zero = p,
      sample = FALSE,
      mincriterion = 0.95)
    tm1 <- proc.time()

    for(i in 1:No_cont_var){
      phi_sum_mat[, i] <- explanation_list[[m]]$dt[[i + 1]]
    }

    for (i in 1:No_cat_var){
      phi_sum_mat[, i + No_cont_var] <- rowSums(subset(explanation_list[[m]]$dt, select = which(beta_matcher == i) + 1))
    }
    colnames(phi_sum_mat) <- feat_names
    explanation_list[[m]]$dt_sum <- cbind(explanation_list[[m]]$dt[, 1], phi_sum_mat)

    print(paste0("Finished estimating Shapley value with ", m, " method."), quote = FALSE, right = FALSE)
    print(tm1 - tm0)
    timeit[m] <- list((tm1 - tm0))

  } else if(m == 'ctree'){
    tm0 <- proc.time()
    explanation_list[[m]] <- explain(
      x_test,
      approach = m,
      explainer = explainer,
      prediction_zero = p,
      sample = FALSE,
      w_threshold = 1,
      mincriterion = 0.95)
    tm1 <- proc.time()
    print(paste0("Finished estimating Shapley value with ", m, " method."), quote = FALSE, right = FALSE)
    print(tm1 - tm0)
    timeit[m] <- list((tm1 - tm0))

  } else if(m == 'kernelSHAP'){
    tm0 <- proc.time()
    explanation_list[[m]] <- explain(
      x_test_numeric,
      approach = "empirical",
      type = "independence",
      explainer = explainer_numeric,
      prediction_zero = p,
      sample = FALSE,
      w_threshold = 1,
      mincriterion = 0.95)
    tm1 <- proc.time()
    print(paste0("Finished estimating Shapley value with ", m, " method."), quote = FALSE, right = FALSE)
    print(tm1 - tm0)
    timeit[m] <- list((tm1 - tm0))

  }
}





exactShap
explanation_list$ctree$dt
explanation_list$kernelShap$dt

MAE <- function(true_shapley, shapley_method, weights = rep(1 / nrow(true_shapley), nrow(true_shapley))){
  mean(apply(abs((true_shapley - shapley_method) * weights), 2, sum)[-1])
}


MAE(exactShap,explanation_list$ctree$dt)
MAE(exactShap,explanation_list$kernelShap$dt)



# Just brute force testing when the features are independent for dim_cont = 2, and dim_cat = 2, no_cat = 3
xj_start_mat <- mod_matrix[test_obs,]
Exj_vec <- c(0,0,diff(pnorm(q = cat_cutoff,mean = mu[1], sd=sqrt(Sigma[1,1]))),diff(pnorm(q = cat_cutoff,mean = mu[1], sd=sqrt(Sigma[1,1]))))

# True shapley values Assuming independence
tab <- t(beta[-1]*t(xj_start_mat- matrix(Exj_vec,ncol=length(Exj_vec),nrow=nrow(xj_start_mat),byrow = T)))
SHAP <- cbind(phi0,tab[,1:2],rowSums(tab[,3:5]),rowSums(tab[,6:8]))

rowSums(SHAP)
predict(model,x_test)

exactShap-SHAP # Close enough!
#              phi0     cont_1_     cont_2_
# 101 -4.971156e-07 0.002440432 0.002440431 0.0004880859 -0.005368950
# 102 -4.971156e-07 0.002440431 0.002440431 0.0004880860 -0.005368949
# 103 -4.971156e-07 0.002440431 0.002440431 0.0004880860 -0.005368949
# 104 -4.971156e-07 0.002440431 0.002440431 0.0004880859 -0.005368949
# 105 -4.971156e-07 0.002440431 0.002440431 0.0004880861 -0.005368949
# 106 -4.971156e-07 0.002440431 0.002440431 0.0004880862 -0.005368950
# 107 -4.971156e-07 0.002440431 0.002440431 0.0004880858 -0.005368949
# 108 -4.971156e-07 0.002440432 0.002440431 0.0004880859 -0.005368950
# 109 -4.971156e-07 0.002440431 0.002440432 0.0004880859 -0.005368950
# 110 -4.971156e-07 0.002440432 0.002440431 0.0004880861 -0.005368950
# 111 -4.971156e-07 0.002440432 0.002440431 0.0004880861 -0.005368950
# 112 -4.971156e-07 0.002440432 0.002440431 0.0004880859 -0.005368949
# 113 -4.971156e-07 0.002440431 0.002440431 0.0004880859 -0.005368949
# 114 -4.971156e-07 0.002440431 0.002440431 0.0004880862 -0.005368949
# 115 -4.971156e-07 0.002440431 0.002440431 0.0004880860 -0.005368949
# 116 -4.971156e-07 0.002440431 0.002440431 0.0004880863 -0.005368950
# 117 -4.971156e-07 0.002440432 0.002440431 0.0004880860 -0.005368950
# 118 -4.971156e-07 0.002440432 0.002440431 0.0004880857 -0.005368950
# 119 -4.971156e-07 0.002440432 0.002440431 0.0004880861 -0.005368950
# 120 -4.971156e-07 0.002440431 0.002440431 0.0004880859 -0.005368949

