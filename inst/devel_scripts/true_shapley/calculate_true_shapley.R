
## -------------------- some functions ----------------------

sim_true_Normal <- function(mu, Sigma, N_shapley = 10000, explainer){

  if(explainer$model_type == 'regression'){
    nms <- names(explainer$model$coefficients)[-1]
  } else{
    nms <- names(explainer$model$coefficients)
  }

  sim <- mvrnorm(n = N_shapley, mu = mu, Sigma = Sigma)
  dt <- data.table(round(sim))

  joint_prob <- table(dt)  / N_shapley

  joint_prob_dt <- data.table(joint_prob)
  joint_prob_dt[, 'V1' := as.numeric(V1)][, 'V2' := as.numeric(V2)][, 'V3' := as.numeric(V3)]
  setnames(joint_prob_dt, c("V1", "V2", "V3"), nms)

  return(joint_prob_dt)
}

marg_prob <- function(joint_prob_dt, explainer){

  nms <- names(joint_prob_dt)[1:3]

  ## compute all conditional probabilities
  marg_list <- list()
  for(i in 1:nrow(explainer$S)){
    if(i == 1){
      marg_list[[i]] <- NA
    }
    if(i > 1){ ## this is the marginal distribution we're interested in i.e f(V1)
      feat <- nms[as.logical(explainer$S[i, ])]
      mat <- data.frame(matrix(NA, ncol = length(feat) + 1, nrow = nrow(unique(joint_prob_dt[, feat, with = FALSE]))))
      names(mat) <- c(feat, "prob")
      for(j in 1:nrow(unique(joint_prob_dt[, feat, with = FALSE]))){
        # print(j)

        v <- unique(joint_prob_dt[, feat, with = FALSE])[j]

        if(ncol(v) == 1){
          v <- as.numeric(v[[1]])
          mat[j, 1] <- v
          mat[j, 2] <- joint_prob_dt[get(feat) == v, .(prob = sum(N))]
        } else if (ncol(v) == 2){

          v1 <- as.numeric(v[[1]])
          v2 <- as.numeric(v[[2]])

          mat[j, 1] <- v1
          mat[j, 2] <- v2
          mat[j, 3] <-joint_prob_dt[get(feat[1]) == v1 & get(feat[2]) == v2, .(prob = sum(N))]

        } else if (ncol(v) == 3){

          v1 <- as.numeric(v[[1]])
          v2 <- as.numeric(v[[2]])
          v3 <- as.numeric(v[[3]])

          mat[j, 1] <- v1
          mat[j, 2] <- v2
          mat[j, 3] <- v3
          mat[j, 4] <- joint_prob_dt[get(feat[1]) == v1 & get(feat[2]) == v2 & get(feat[3]) == v3, .(prob = sum(N))]
        }
      }
      marg_list[[i]] <- mat
    }
  }
  return(marg_list)
}

cond_prob <- function(marg_list, joint_prob_dt, explainer){

  nms <- names(joint_prob_dt)[1:3]

  cond_list <- list()
  for(i in 1:nrow(explainer$S)){
    if(i == 1){
      cond_list[[i]] <- NA
    }
    if(i > 1){ ## this is the conditional distribution we're interested in i.e f(V2, V3 | V1) = f(V1, V2, V3) / f(V1)
      feat <- nms[as.logical(explainer$S[i, ])]
      mat <- NULL
      for(j in 1:nrow(unique(joint_prob_dt[, feat, with = FALSE]))){
        v <- unique(joint_prob_dt[, feat, with = FALSE])[j]

        if(ncol(v) == 1){
          v <- as.numeric(v[[1]])

          mat0 <- joint_prob_dt[get(feat) == v]
          mat0[,  'marg_prob' := marg_list[[i]][j, 'prob']]
          mat0[, 'cond_prob' := N / marg_prob]
          setnames(mat0, 'N', 'joint_prob') ## HERE?????
          mat0[, 'conditioned_on' := feat]

        } else if (ncol(v) == 2){
          v1 <- as.numeric(v[[1]])
          v2 <- as.numeric(v[[2]])

          mat0 <-joint_prob_dt[get(feat[1]) == v1 & get(feat[2]) == v2]
          mat0[,  'marg_prob' := marg_list[[i]][j, 'prob']]
          mat0[, 'cond_prob' := N / marg_prob]
          setnames(mat0, 'N', 'joint_prob')
          mat0[, 'conditioned_on' := paste(feat, collapse = ", ")]

        } else if (ncol(v) == 3){
          v1 <- as.numeric(v[[1]])
          v2 <- as.numeric(v[[2]])
          v3 <- as.numeric(v[[3]])

          mat0 <- joint_prob_dt[get(feat[1]) == v1 & get(feat[2]) == v2 & get(feat[3]) == v3]
          mat0[,  'marg_prob' := marg_list[[i]][j, 'prob']]
          mat0[, 'cond_prob' := N / marg_prob]
          setnames(mat0, 'N', 'joint_prob')
          mat0[, 'conditioned_on' := paste(feat, collapse = ", ")]

        }
        mat <- rbind(mat, mat0)
      }
      cond_list[[i]] <- mat
    }
  }
  return(cond_list)
}


## function to calculate conditional expectation
cond_expec <- function(x_test, cond_list, explainer, prediction_zero){

  nms <- names(cond_list[[2]])[1:3]

  cond_expec <- NULL
  for(i in 1:nrow(explainer$S)){
    if(i == 1){
      cond_expec <- c(cond_expec, prediction_zero)
    } else if(i > 1){ ## this is the conditional distribution we're interested in i.e f(V2, V3 | V1) = f(V1, V2, V3) / f(V1)
      feat <- nms[as.logical(explainer$S[i, ])]

      v <- x_test[as.logical(explainer$S[i, ])]
      if(length(v) == 1){

        mat <- cond_list[[i]][get(feat) == v][, 'predict' := predict_model(explainer$model, newdata = .SD), .SDcols = nms][, 'expected_value' := predict * cond_prob]
        cond_expec <- c(cond_expec, sum(mat$expected_value))
      } else if(length(v) == 2){

        v1 <- v[[1]]
        v2 <- v[[2]]
        mat <- cond_list[[i]][get(feat[1]) == v1 & get(feat[2]) == v2][, 'predict' := predict_model(explainer$model, newdata = .SD), .SDcols = nms][, 'expected_value' := predict * cond_prob]
        cond_expec <- c(cond_expec, sum(mat$expected_value))
      } else if(length(v) == 3){

        v1 <- v[[1]]
        v2 <- v[[2]]
        v3 <- v[[3]]
        mat <- cond_list[[i]][get(feat[1]) == v1 & get(feat[2]) == v2 & get(feat[3]) == v3][, 'predict' := predict_model(explainer$model, newdata = .SD), .SDcols = nms][, 'expected_value' := predict * cond_prob]
        cond_expec <- c(cond_expec, sum(mat$expected_value))
      }

    }
  }
  return(cond_expec)
}

true_Kshap <- function(explainer, cond_expec, x_test){

  Kshap <- matrix(0, nrow = nrow(x_test), ncol = nrow(explainer$W))
  for (i in 1:nrow(x_test)) {
    Kshap[i, ] = explainer$W %*% cond_expec[i, ]
  }
  Kshap <- data.table(Kshap)
  names(Kshap) <- c("none", "feat1", "feat2", "feat3")

  return(Kshap)
}

linear_Kshap <- function(x_test, beta, mu){
  phi <- NULL
  phi <- c(phi, beta[1] + sum(beta[2:4] * mu))
  for(i in 1:length(mu)){
    phi <- c(phi, beta[i + 1] * (x_test[[i]] - mu[i]) )
  }
  return(phi)
}

MAE <- function(shapley_true, shapley_method){
  sum(abs(shapley_true - shapley_method))
}


# library(lqmm)
simulate_data <- function(parameters_list){

  Sigma <- matrix(rep(parameters_list$corr, 9), 3, 3)
  Sigma[1, 1] <- Sigma[2, 2] <- Sigma[3, 3] <- parameters_list$Sigma_diag

  # if(!is.positive.definite(Sigma, tol=1e-8)){
  #   print("Sigma is not positive definite.")
  #   Sigma <- make.positive.definite(Sigma)
  # }


  mu <- parameters_list$mu
  beta <- parameters_list$beta
  N_data <- parameters_list$N_data
  N_shapley <- parameters_list$N_shapley
  noise <- parameters_list$noise
  response_mod <- parameters_list$response_mod
  fit_mod <- parameters_list$fit_mod
  methods <- parameters_list$methods

  ## 1. calculate training and testing data
  x <- mvrnorm(n = N_data, mu = mu, Sigma = Sigma)
  dt <- data.table(round(x))

  setnames(dt, c("feat1", "feat2", "feat3"))
  if(noise == TRUE){
    dt[, 'epsilon' := rnorm(N_data, 0, 0.1^2)] #
  } else{
    dt[, 'epsilon' := 0] # rnorm(N, 0, 0.1^2)
  }


  dt[, "response" := response_mod(feat1, feat2, feat3, epsilon, beta)]

  x_train <- as.matrix(dt[-(1:6), .(feat1, feat2, feat3)])
  x_test <- as.matrix(dt[(1:6), .(feat1, feat2, feat3)])
  y_train <- as.matrix(dt[-(1:6), .(response)])

  ## linear regression
  if(fit_mod == 'regression'){
    model <- lm(response ~ feat1 + feat2 + feat3, data = dt[-(1:6), .(feat1, feat2, feat3, response)])
  }

  ## 2. initalize shapr object with trained model
  explainer <- shapr(x_train, model)

  ## 3. calculate the true shapley value with these parameters
  joint_prob_dt <- sim_true_Normal(mu, Sigma, N_shapley = N_shapley, explainer) ## 1 min for 10 mill

  marg_list <- marg_prob(joint_prob_dt, explainer)

  cond_list <- cond_prob(marg_list, joint_prob_dt, explainer)
  # print("I arrive here")
  cond_expec_mat <- t(apply(x_test, 1, FUN = cond_expec, cond_list, explainer, prediction_zero <- mean(y_train)))

  true_shapley <- true_Kshap(explainer, cond_expec_mat, x_test)

  ## 4. calculate shapley value for linear model
  if(explainer$model_type == 'regression'){
    true_linear <- t(apply(x_test, 1, FUN = linear_Kshap, beta, mu))
  } else{
    true_linear <- NULL
  }

  ## 5. calculate approximate shapley value with different methods
  p <- mean(y_train)

  explanation_list <- list()
  for(i in 1:length(methods)){
    explanation_list[[i]] <- explain(
      x_test,
      approach = methods[i],
      explainer = explainer,
      prediction_zero = p,
      sample = FALSE)
  }

  return(list(true_shapley, true_linear, explanation_list))

}

## -----------------------------------------------------------
