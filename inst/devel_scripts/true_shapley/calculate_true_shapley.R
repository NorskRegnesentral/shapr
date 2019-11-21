
## -------------------- some functions ----------------------

sim_true_Normal <- function(mu, Sigma, beta, N_shapley = 10000, explainer, cutoff, response_mod){

  nms <- colnames(explainer$x_train)

  cutoff[, 1] <- cutoff[, 1] - 10
  cutoff[, 4] <- cutoff[, 4] + 10

  set.seed(1)
  sim <- mvrnorm(n = N_shapley, mu = mu, Sigma = Sigma)
  dt <- NULL
  if(is.matrix(cutoff)){
    for(i in 1:length(nms)){
      dt <- cbind(dt, cut(sim[, i], cutoff[i, ], labels = c(1:3), include.lowest = TRUE))
    }
  } else{
    for(i in 1:length(nms)){
      dt <- cbind(dt, cut(sim[, i], cutoff, labels = c(1:3)))
    }
  }
  dt <- data.table(dt)

  setnames(dt, c("V1", "V2", "V3"), nms)

  dt[, feat1 := as.factor(feat1)]
  dt[, feat2 := as.factor(feat2)]
  dt[, feat3 := as.factor(feat3)]

  dt_response <- cbind(dt, data.table(model.matrix(~., data = dt)))

  ## 3. Calculate response
  dt_response[, response := response_mod(feat12, feat13, feat22, feat23, feat32, feat33, beta = beta)]

  mn <- mean(dt_response$response)

  # dt <- data.table(round(sim))

  joint_prob <- table(dt)  / N_shapley

  joint_prob_dt0 <- data.table(joint_prob)
  # joint_prob_dt0[, 'feat1' := as.numeric(feat1)][, 'feat2' := as.numeric(feat2)][, 'feat3' := as.numeric(feat3)]
  # setnames(joint_prob_dt0, c("V1", "V2", "V3"), nms)

  joint_prob_dt <- joint_prob_dt0[, ..nms][, lapply(.SD, as.factor)]
  joint_prob_dt <- cbind(joint_prob_dt, joint_prob_dt0[, .(N)])

  joint_prob_dt[, p := mn]

  return(joint_prob_dt)
}

marg_prob <- function(joint_prob_dt, explainer){

  nms <- colnames(explainer$x_train)

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

        v <- unique(joint_prob_dt[, feat, with = FALSE])[j]

        if(ncol(v) == 1){
          v <- as.numeric(v[[1]])
          mat[j, 1] <- v
          mat[j, 2] <- joint_prob_dt[get(feat) == v, .(prob = sum(N))]

          if(j == nrow(unique(joint_prob_dt[, feat, with = FALSE]))){
            mat[, feat] <- as.factor(mat[, feat])
          }

        } else if (ncol(v) == 2){

          v1 <- as.numeric(v[[1]])
          v2 <- as.numeric(v[[2]])

          mat[j, 1] <- v1
          mat[j, 2] <- v2
          mat[j, 3] <-joint_prob_dt[get(feat[1]) == v1 & get(feat[2]) == v2, .(prob = sum(N))]

          if(j == nrow(unique(joint_prob_dt[, feat, with = FALSE]))){
            mat[, feat[1]] <- as.factor(mat[, feat[1]])
            mat[, feat[2]] <- as.factor(mat[, feat[2]])
          }


        } else if (ncol(v) == 3){

          v1 <- as.numeric(v[[1]])
          v2 <- as.numeric(v[[2]])
          v3 <- as.numeric(v[[3]])

          mat[j, 1] <- v1
          mat[j, 2] <- v2
          mat[j, 3] <- v3
          mat[j, 4] <- joint_prob_dt[get(feat[1]) == v1 & get(feat[2]) == v2 & get(feat[3]) == v3, .(prob = sum(N))]

          if(j == nrow(unique(joint_prob_dt[, feat, with = FALSE]))){
            mat[, feat[1]] <- as.factor(mat[, feat[1]])
            mat[, feat[2]] <- as.factor(mat[, feat[2]])
            mat[, feat[3]] <- as.factor(mat[, feat[3]])
          }

        }
      }
      mat <- cbind(mat, data.frame(p = joint_prob_dt$p[1:nrow(mat)]))
      marg_list[[i]] <- mat
    }
  }
  return(marg_list)
}

cond_prob <- function(marg_list, joint_prob_dt, explainer){

  nms <- colnames(explainer$x_train)

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
          mat0[,  marg_prob := marg_list[[i]][j, 'prob']]
          mat0[, cond_prob := N / marg_prob]
          setnames(mat0, 'N', 'joint_prob')
          mat0[, conditioned_on := feat]

        } else if (ncol(v) == 2){
          v1 <- as.numeric(v[[1]])
          v2 <- as.numeric(v[[2]])

          mat0 <-joint_prob_dt[get(feat[1]) == v1 & get(feat[2]) == v2]
          mat0[,  marg_prob := marg_list[[i]][j, 'prob']]
          mat0[, cond_prob := N / marg_prob]
          setnames(mat0, 'N', 'joint_prob')
          mat0[, conditioned_on := paste(feat, collapse = ", ")]

        } else if (ncol(v) == 3){
          v1 <- as.numeric(v[[1]])
          v2 <- as.numeric(v[[2]])
          v3 <- as.numeric(v[[3]])

          mat0 <- joint_prob_dt[get(feat[1]) == v1 & get(feat[2]) == v2 & get(feat[3]) == v3]
          mat0[,  marg_prob := marg_list[[i]][j, 'prob']]
          mat0[, cond_prob := N / marg_prob]
          setnames(mat0, 'N', 'joint_prob')
          mat0[, conditioned_on := paste(feat, collapse = ", ")]

        }
        mat <- rbind(mat, mat0)
      }
      cond_list[[i]] <- mat
    }
  }
  return(cond_list)
}


## function to calculate conditional expectation
cond_expec <- function(x_test, cond_list, explainer){ ## removed prediction_zero

  nms <- colnames(explainer$x_train)

  cond_expec <- NULL
  for(i in 1:nrow(explainer$S)){
    if(i == 1){
      cond_expec <- c(cond_expec, cond_list[[2]]$p[1])
    } else if(i > 1){ ## this is the conditional distribution we're interested in i.e f(V2, V3 | V1) = f(V1, V2, V3) / f(V1)
      feat <- nms[as.logical(explainer$S[i, ])]
      v <- x_test[as.logical(explainer$S[i, ])]
      if(length(v) == 1){

        v <- as.numeric(v[[1]])
        mat <- cond_list[[i]][get(feat) == v]
        mat[, predict := predict_model(explainer$model, newdata = .SD), .SDcols = nms]
        mat[, expected_value := predict * cond_prob]

        cond_expec <- c(cond_expec, sum(mat$expected_value))
      } else if(length(v) == 2){

        v1 <- as.numeric(v[[1]])
        v2 <- as.numeric(v[[2]])
        mat <- cond_list[[i]][get(feat[1]) == v1 & get(feat[2]) == v2]
        mat[, predict := predict_model(explainer$model, newdata = .SD), .SDcols = nms][, 'expected_value' := predict * cond_prob]
        cond_expec <- c(cond_expec, sum(mat$expected_value))
      } else if(length(v) == 3){

        v1 <- as.numeric(v[[1]])
        v2 <- as.numeric(v[[2]])
        v3 <- as.numeric(v[[3]])
        mat <- cond_list[[i]][get(feat[1]) == v1 & get(feat[2]) == v2 & get(feat[3]) == v3]
        mat[, predict := predict_model(explainer$model, newdata = .SD), .SDcols = nms][, 'expected_value' := predict * cond_prob]
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


linear_Kshap <- function(x_test_onehot, beta, dt){

  prop <- c(0, apply(dt[, .(feat12, feat13)], 2, sum) / nrow(dt), 0, apply(dt[, .(feat22, feat23)], 2, sum) / nrow(dt), 0, apply(dt[, .(feat32, feat33)], 2, sum) / nrow(dt))

  for(i in c(1, 4, 7)){
    prop[i] <- 1 - prop[i + 1] - prop[i + 2]
  }
  phi0 <- NULL
  phi0 <- c(phi0, beta[1] + sum(beta[2:10] * prop))

  x_test0 <- x_test_onehot

  # x_test1 <- cbind((1 - x_test0[1, 1]) * (1 - x_test0[1, 2]),  x_test0[1, 1:2], (1 - x_test0[1, 3]) * (1 - x_test0[1, 4]), x_test0[1, 3:4], (1 - x_test0[1, 5]) * (1 - x_test0[1, 6]), x_test0[1, 5:6])

  x_test1 <- c((1 - x_test0[1]) * (1 - x_test0[2]),  x_test0[1:2], (1 - x_test0[3]) * (1 - x_test0[4]), x_test0[3:4], (1 - x_test0[5]) * (1 - x_test0[6]), x_test0[5:6])
  for(i in 1:length(prop)){
    phi0 <- c(phi0, beta[i + 1] * (x_test1[[i]] - prop[i]) )
  }
  phi <- c(phi0[1], sum(phi0[2:4]), sum(phi0[5:7]), sum(phi0[8:10]))

  return(phi)
}

# shapley_method <- true_linear

MAE <- function(true_shapley, shapley_method){
  mean(apply(abs(true_shapley - shapley_method), 2, mean)[-1])
}


library(lqmm) ## to check if Sigma is positive definite
simulate_data <- function(parameters_list){

  ## make sure Sigma is positive definite
  Sigma <- matrix(rep(parameters_list$corr, 9), 3, 3)
  Sigma[1, 1] <- Sigma[2, 2] <- Sigma[3, 3] <- parameters_list$Sigma_diag
  if(!is.positive.definite(Sigma)) {
    print("Covariance matrix is not positive definite but will be converted.")
    Sigma <- make.positive.definite(Sigma)
    print("New Sigma matrix:")
    print(Sigma)
  }

  mu <- parameters_list$mu
  beta <- parameters_list$beta
  # N_data <- parameters_list$N_data
  N_shapley <- parameters_list$N_shapley
  noise <- parameters_list$noise
  response_mod <- parameters_list$response_mod
  fit_mod <- parameters_list$fit_mod
  methods <- parameters_list$methods
  cutoff <- parameters_list$cutoff
  N_testing <- parameters_list$N_testing
  N_training <- parameters_list$N_training

  ## 1. calculate training and testing data
  x <- mvrnorm(n = N_testing + N_training, mu = mu, Sigma = Sigma)

  dt <- NULL
  if(is.null(cutoff)){ ## to get equal proportion in eqch level
    for(i in 1:ncol(x)){
      dt <- cbind(dt, cut(x[, i], quantile(x[, i], probs = c(0, 0.33, 0.66, 1)), labels = 1:3, include.lowest = TRUE)) # without include.lowest, you get NA at the boundaries
      cutoff <- c(cutoff, quantile(x[, i], probs = c(0, 0.33, 0.66, 1)))
    }
    cutoff <- t(matrix(cutoff, ncol = 3))
  } else{
    for(i in 1:ncol(x)){
      dt <- cbind(dt, cut(x[, i], cutoff, labels=c(1:3)))
    }
  }

  dt <- data.table(dt)
  ## Sanity check:
  # table(dt[, V1])

  setnames(dt, c("feat1", "feat2", "feat3"))

  dt[, feat1 := as.factor(feat1)]
  dt[, feat2 := as.factor(feat2)]
  dt[, feat3 := as.factor(feat3)]

  if(noise == TRUE){
    dt[, epsilon := rnorm(N_data, 0, 0.1^2)] #
  } else{
    dt[, epsilon := 0]
  }

  ## 2. One hot encoding of training data
  dt <- cbind(dt, data.table(model.matrix(~., data = dt[, .(feat1, feat2, feat3)])))

  ## 3. Calculate response
  dt[, response := response_mod(feat12, feat13, feat22, feat23, feat32, feat33, epsilon, beta)]

  ## 4. Fit model
  if(fit_mod == 'regression'){
    model <- lm(response ~ feat1 + feat2 + feat3, data = dt[-(1:N_testing), .(feat1, feat2, feat3, response)])
  }

  ## 5. initalize shapr object with trained model -- this is used for calculating true shapley
  ## changed this Nov 21 --- removed as.matrix because features are categorical.
  # x_train <- as.matrix(dt[-(1:6), .(feat1, feat2, feat3)]) ## used in explainer()
  # x_test <- as.matrix(dt[(1:6), .(feat1, feat2, feat3)]) ## used in cond_expec_mat()
  # y_train <- as.matrix(dt[-(1:6), .(response)]) ## used in cond_expec_mat()

  x_train <- dt[-(1:N_testing), .(feat1, feat2, feat3)] ## used in explainer()
  x_test <- dt[(1:N_testing), .(feat1, feat2, feat3)] ## used in cond_expec_mat()
  y_train <- dt[-(1:N_testing), .(response)] ## used in cond_expec_mat()
  explainer <- shapr(x_train, model)

  ## 6. calculate the true shapley values
  joint_prob_dt <- sim_true_Normal(mu, Sigma, beta, N_shapley = N_shapley, explainer, cutoff, response_mod) ## 1 min for 10 mill
  marg_list <- marg_prob(joint_prob_dt, explainer)
  cond_list <- cond_prob(marg_list, joint_prob_dt, explainer)
  cond_expec_mat <- t(apply(x_test, 1, FUN = cond_expec, cond_list, explainer))
  true_shapley <- true_Kshap(explainer, cond_expec_mat, x_test)

  ## 7. calculate true shapley under linear model and independence assumption (only if correlation is 0)
  x_test_onehot <- dt[(1:N_testing), .(feat12, feat13, feat22, feat23, feat32, feat33)]
  if(explainer$model_type == 'regression'){
    if(parameters_list$corr == 0){
      true_linear <- t(apply(x_test_onehot, 1, FUN = linear_Kshap, beta, dt))
    } else{
      true_linear <- NULL
    }
  } else{
    true_linear <- NULL
  }

  ## 8. calculate approximate shapley value with different methods
  p <- mean(y_train$response) # since y_train is no longer a matrix

  explanation_list <- list()
  for(m in methods){
    if(m == 'empirical' | m == 'empirical_ind' |  m == 'gaussian' | m == 'ctree_onehot'){

      x_train_onehot <- as.matrix(dt[-(1:N_testing), .(feat12, feat13, feat22, feat23, feat32, feat33)])

      if(fit_mod == 'regression'){
        fmla <- as.formula(paste("response ~", paste(colnames(x_train_onehot), collapse = " + ")))
        model_onehot <- lm(fmla, data = dt[-(1:N_testing), !c("feat1", "feat2", "feat3", "epsilon")])
      }

      explainer_onehot <- shapr(x_train_onehot, model_onehot)

      if(m == 'ctree_onehot'){
        explanation_list[[m]] <- explain(
          x_test_onehot,
          approach = 'ctree',
          explainer = explainer_onehot,
          prediction_zero = p,
          sample = FALSE)
      } else if(m == 'empirical_ind'){
        explanation_list[[m]] <- explain(
          x_test_onehot,
          approach = "empirical",
          type = "independence",
          explainer = explainer_onehot,
          prediction_zero = p,
          sample = FALSE)
      } else{
        explanation_list[[m]] <- explain(
          x_test_onehot,
          approach = m,
          explainer = explainer_onehot,
          prediction_zero = p,
          sample = FALSE)
      }
      explanation_list[[m]]$dt_sum <- cbind(NULL, explanation_list[[m]]$dt[, 1])
      for(i in c(2, 4, 6)){
        explanation_list[[m]]$dt_sum <- cbind(explanation_list[[m]]$dt_sum, apply(explanation_list[[m]]$dt[, i:(i + 1)], 1, sum))
      }
      setnames(explanation_list[[m]]$dt_sum, c("none", "feat1", "feat2", "feat3"))

    } else { ## for ctree without one-hot encoding
      explanation_list[[m]] <- explain(
        x_test,
        approach = m,
        explainer = explainer,
        prediction_zero = p,
        sample = FALSE)
    }
  }

  return_list <- list()
  return_list[['true_shapley']] <- true_shapley
  return_list[['true_linear']] <- true_linear
  return_list[['methods']] <- explanation_list

  return(return_list)

}


