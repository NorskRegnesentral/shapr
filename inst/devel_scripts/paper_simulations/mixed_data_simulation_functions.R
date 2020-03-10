
library(shapr)
library(data.table)
library(MASS)
library(ggplot2)

# source("inst/devel_scripts/paper_simulations/mixed_data_simulation_functions.R")
source("inst/devel_scripts/paper_simulations/calculate_true_shapley_withdatatable.R")

# Exact mixed data functions
dens_x_given_S_is_C_func <- function(x, C_lower, C_upper, xi, Omega, algorithm) {
  # Formula in equation (13) in this paper
  # https://www.jstor.org/stable/pdf/20445223.pdf?refreqid=excelsior%3A9fdbaaf0a8fe22e64418448ad4f8090b
  # letting V = x, and U correspond to the dimensions specified in C
  # C_lower is a vector of length dim with lower bounds for each dimension, C_upper similalry contains the upper bounds
  # Omega is the joint covariance matrix of x and the length of C_lower and C_upper (dim)
  # xi is the joint mean vector of x and the length of C_lower and C_upper (dim)
  # Note: x is always one dimensional

  these_U <- (1:length(C_lower))+1
  these_V <- 1

  Omega_U <- Omega[these_U,these_U,drop=F]
  Omega_V <- Omega[these_V,these_V,drop=F]
  Delta <- Omega[these_V,these_U,drop=F]

  xi_U <- xi[these_U]
  xi_V <- xi[these_V]

  C_lower <- unlist(C_lower)
  C_upper <- unlist(C_upper)

  mean_above <- as.vector(t(Delta)%*%solve(Omega_V)%*%(x-xi_V) + xi_U)
  sigma_above <- Omega_U - t(Delta)%*%solve(Omega_V)%*%Delta

  mean_below <- xi_U
  sigma_below <- Omega_U


  above <- mvtnorm::pmvnorm(lower = C_lower,upper = C_upper,
                            mean = mean_above,
                            sigma = sigma_above,
                            algorithm = algorithm)


  below <- mvtnorm::pmvnorm(lower = C_lower,upper = C_upper,
                            mean = mean_below,
                            sigma = sigma_below,
                            algorithm = algorithm)

  left <- dnorm(x,mean=xi_V,sd = sqrt(Omega_V))

  dens <- left*above/below

  return(dens)

}

# Splitting the main function in two to make it more efficient
# Here is the preparation function
prep_dens_x_given_S_is_C_func <- function(C_lower,C_upper,xi,Omega,algorithm) {

  these_U <- (1:length(C_lower))+1
  these_V <- 1

  Omega_U <- Omega[these_U,these_U,drop=F]
  Omega_V <- Omega[these_V,these_V,drop=F]
  Delta <- Omega[these_V,these_U,drop=F]

  xi_U <- xi[these_U]
  xi_V <- xi[these_V]

  C_lower <- unlist(C_lower)
  C_upper <- unlist(C_upper)

  mean_above_mult <- t(Delta)%*%solve(Omega_V)
  mean_above_add <- xi_U - t(Delta)%*%solve(Omega_V)%*%xi_V
  sigma_above <- Omega_U - t(Delta)%*%solve(Omega_V)%*%Delta

  mean_below <- xi_U
  sigma_below <- Omega_U

  below <- mvtnorm::pmvnorm(lower = C_lower,upper = C_upper,
                            mean = mean_below,
                            sigma = sigma_below,
                            algorithm = algorithm)[1]


  left_mean <- xi_V
  left_sd <- sqrt(Omega_V)

  ret <- list(C_lower = C_lower,
              C_upper = C_upper,
              mean_above_mult = mean_above_mult,
              mean_above_add = mean_above_add,
              sigma_above = sigma_above,
              below = below,
              left_mean = left_mean,
              left_sd = left_sd)

  return(ret)
}

prep_dens_x_given_S_is_C_func_v2 <- function(ret_list,x_vec) {

  mean_above_mat <- ret_list$mean_above_mul%x%t(x_vec) + as.vector(ret_list$mean_above_add)
  mean_above_list <- as.list(as.data.frame(mean_above_mat))

  left_vec <- dnorm(x_vec,mean=ret_list$left_mean,sd = ret_list$left_sd)

  ret <- mapply(listfun,mean_above = mean_above_list,left = left_vec,x_val = x_vec,
                MoreArgs = list(C_lower = ret_list$C_lower,
                                C_upper = ret_list$C_upper,
                                below = ret_list$below,
                                sigma_above = ret_list$sigma_above),
                SIMPLIFY = F)

  return(ret)
}

listfun <- function(mean_above,left,x_val,...){
  list(mean_above = mean_above,left = left,x_val = x_val,...)
}

compute_dens_x_given_S_is_C_func_v2 <- function(ret_list,algorithm = mvtnorm::Miwa()) {

   above <- mvtnorm::pmvnorm(lower = ret_list$C_lower,upper = ret_list$C_upper,
                             mean = ret_list$mean_above,
                             sigma = ret_list$sigma_above,
                             algorithm = algorithm)[1]


  dens <- ret_list$left*above/ret_list$below

  return(dens)

}

MAE <- function(true_shapley, shapley_method, weights = rep(1 / nrow(true_shapley), nrow(true_shapley))){
  mean(apply(abs((true_shapley - shapley_method) * weights), 2, sum)[-1])
}



## NEW ANNABELLE
compute_shapley_mixed_data <- function(parameters_list){

  # Assuming a linear model
  response_mod <- function(mod_matrix_full, beta, epsilon){
    as.vector(mod_matrix_full %*% beta) + epsilon
  }


  methods <- parameters_list$methods
  No_sample_gaussian <- parameters_list$No_sample_gaussian
  No_cont_var <- parameters_list$No_cont_var
  No_cat_var <- parameters_list$No_cat_var
  No_levels <- parameters_list$No_levels
  #
  No_tot_var <- No_cont_var + No_cat_var
  #
  Sigma_diag <- parameters_list$Sigma_diag
  corr <- parameters_list$corr
  mu <- rep(0, No_tot_var)
  beta_0 <- 1
  beta_cont <- c(1, -1)
  beta_cat <- c(1, 0, -1,
                2, 3, -1)
  beta <- c(beta_0, beta_cont, beta_cat)
  #
  No_train_obs <- parameters_list$No_train_obs
  No_test_obs <- parameters_list$No_test_obs
  cat_cutoff <- parameters_list$cat_cutoff
  noise = parameters_list$noise
  name = parameters_list$name
  seed <- parameters_list$seed

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

  ## Fit model - model_onehot is for the one-hot encoded methods
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

    Vs_sum_contrib_mat <- matrix(NA, ncol = No_tot_var, nrow = No_test_obs)

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

      Vs_sum_contrib_mat[, j] <- Vs_sum_contrib_vec

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
    exactShap[i,] <- c(explainer$W %*% Vs_mat[, i])
  }

  # max(abs(rowSums(exactShap) - predict(model, x_test))) #

  ## Estimating the Shapley values
  p <- mean(y_train$response)

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

  return_list <- list()
  return_list[['true_shapley']] <- exactShap
  # return_list[['joint_prob_true']] <- joint_prob_dt_list[[1]]
  return_list[['x_train-y_train']] <- cbind(x_train, y_train)
  return_list[['methods']] <- explanation_list
  return_list[['timing']] <- timeit

  return_list[['parameters']] <- parameters_list
  print("--- End ---")
  return(return_list)



}


## Start simulation study
tod_date0 <- format(Sys.Date(), "%d_%m_%y")

dim <- 4
no_categories <- 3

clock_seed_0 <- round(as.numeric(Sys.time()) * 1000)
clock_seed <- signif(clock_seed_0) - clock_seed_0
set.seed(clock_seed)
rand_string <- stringi::stri_rand_strings(1,5)
folder <- paste0(tod_date0, "_", rand_string, "_dim", dim, "_nbcat", no_categories)

dir.create(paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/cont_and_cat_data/", folder, sep = ""))

##

parameters_list <- list()

set.seed(1); beta <- round(rnorm(dim * no_categories + 1), 1)
corr <- c(0, 0.1, 0.5, 0.8, 0.9)
k <- 1
for(j in corr){
  parameters_list[[k]] <- list(methods = c("gaussian", "empirical", "ctree", "kernelSHAP"),
                          No_sample_gaussian = c(100, 1000),
                          No_cont_var = 2,
                          No_cat_var = 2,
                          No_levels = 3,
                          Sigma_diag = 1,
                          corr = j,
                          No_train_obs = 1000,
                          No_test_obs = 100,
                          cat_cutoff = c(-200, 0, 1, 200),
                          noise = FALSE,
                          name = 'testing',
                          seed = 123)
  k <- k + 1
}

all_methods <- list()
for(i in 1:length(parameters_list)){
  all_methods[[i]] <- compute_shapley_mixed_data(parameters_list[[i]])

  nm = paste(tod_date, '_rho_', parameters_list[[i]]$corr, ".rds", sep = "")

  saveRDS(all_methods, file = paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/cont_and_cat_data", folder, nm, sep = "/"))
}

## End simulation study

## save/plot results
dt_sum_list <- list()

for(j in 1:length(all_methods)){
  K <- which(grepl("gaussian", names(all_methods[[j]]$methods)))[1]
  top <-all_methods[[j]]$parameters$No_test_obs

  l = list()
  for(i in 1:10){
    l[[i]] <- cbind(all_methods[[j]]$methods[[i + K - 1]]$dt_sum, 1:top)
  }
  l_all <- rbindlist(l)
  setnames(l_all, c('V2'), c('id'))
  gaussian_mean <- l_all[, lapply(.SD, mean), by = id]
  gaussian_mean[, id := NULL]
  ##
  l = list()
  for(i in 1:10){
    l[[i]] <- cbind(all_methods[[j]]$methods[[i + K]]$dt_sum, 1:top)
  }
  l_all <- rbindlist(l)
  setnames(l_all, c('V2'), c('id'))
  gaussian_mean2 <- l_all[, lapply(.SD, mean), by = id]
  gaussian_mean2[, id := NULL]
  dt_sum_list[[j]] <- list(gaussian_nsamples100 = gaussian_mean, gaussian_nsamples1000 = gaussian_mean2)

}

for(j in 1:length(all_methods)){
  for(k in 1:length(names(all_methods[[j]]$methods))){

    if('ctree' == names(all_methods[[j]]$methods[k]) | 'kernelSHAP' == names(all_methods[[j]]$methods[k])){
      dt_sum_list[[j]][[names(all_methods[[j]]$methods[k])]] <- all_methods[[j]]$methods[[k]]$dt

    } else if(!grepl("gaussian", names(all_methods[[j]]$methods[k]))){
      dt_sum_list[[j]][[names(all_methods[[j]]$methods[k])]] <- all_methods[[j]]$methods[[k]]$dt_sum
    }
  }
}


MAE_results <- NULL
method_names <- NULL
corr <- NULL

for(i in 1:length(dt_sum_list)){
  for(m in names(dt_sum_list[[1]])){
    MAE_results <- c(MAE_results, MAE(all_methods[[i]][['true_shapley']], dt_sum_list[[i]][[m]]))
    method_names <- c(method_names, m)
    corr <- c(corr, all_methods[[i]]$parameters$corr)
  }
}


results <- data.table(MAE_results, method_names, corr)
results[, dim := dim]
results[, no_categories := no_categories]
saveRDS(results, file = paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/cont_and_cat_data/", folder, "results_dim4_nocat3",  sep = "/"))


results <- readRDS(paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/cont_and_cat_data/09_03_20_eCMjG_dim4_nbcat3", "results_dim4_nocat3", sep = "/"))

p1 <- ggplot(data = results, aes(y = MAE_results, x = as.character(corr), col = as.factor(method_names))) +
  geom_point(size = 4, stroke = 1.5) +
  theme_bw(base_size = 22) +
  xlab("correlation") +
  ylab("Mean absolute error (MAE)") +
  scale_color_discrete(name = "Method" ) +
  ggtitle("")








#### OLD FUNCTIONS ####
pmvnorm_no_checking <- function(lower, upper,mean, sigma, algorithm){

  corr <- cov2cor(sigma)

  ret = mvtnorm:::mvt(lower = lower,upper = upper,df = 0,corr = corr, delta = mean,algorithm = algorithm)

  return(ret)
}

# Here is the computation function, taking the preparation values as input
compute_dens_x_given_S_is_C_func <- function(x,ret_list) {

  mean_above <- as.vector(ret_list$mean_above_mult%*%x+ret_list$mean_above_add)


  above <- mvtnorm::pmvnorm(lower = ret_list$C_lower,upper = ret_list$C_upper,
                            mean = mean_above,
                            sigma = ret_list$sigma_above,
                            algorithm = algorithm)[1]

  left <- dnorm(x,mean=ret_list$left_mean,sd = ret_list$left_sd)

  dens <- left*above/ret_list$below

  return(dens)

}


# Vectorizing the two functions and checking that they give the same result
vec_dens_x_given_S_is_C_func <- Vectorize(dens_x_given_S_is_C_func,vectorize.args="x")

vec_compute_dens_x_given_S_is_C_func = Vectorize(compute_dens_x_given_S_is_C_func,vectorize.args = "x")

vec_compute_dens_x_given_S_is_C_func_2 = Vectorize(compute_dens_x_given_S_is_C_func)

# aa = outer(x_int_grid,prep_list_all_x_test_C, FUN = vec_compute_dens_x_given_S_is_C_func_2)

vec_compute_dens_x_given_S_is_C_func_rev <- function(ret_list,x){
  vec_compute_dens_x_given_S_is_C_func(x,ret_list)
}
