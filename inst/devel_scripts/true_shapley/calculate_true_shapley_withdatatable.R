#' Function to simulate Normal random variables with true mu and Sigma parameters.
#'
#' @description
#'
#' @param mu Numeric or vector indicating the true mean of the Normal or joint Normal random variables used to calculate
#' the true Shapley values.
#' @param Sigma Numeric (if mu is Numeric) or matrix with values used for covariance matrix of the random variables used to
#' calculate the true Shapley values.
#' @param beta Numeric or vector. These are the true coefficients of the response modelthat we are trying to explain with the
#' Shapley values.
#' @param N_shapley Numeric indicating how many Normal or joint Normal random variables to simulate. Default 10000.
#' @param explainer explainer object from shapr package.
#' @param cutoff vector of Numerics. This indicates where to cutoff the Normal random variables to make levels.
#' @param response_mod function. The true response model that indicates how the features relate to the response.
#' @details
#'
#' @return list First component is a data.table with the joint probability of each level. Second component is the mean of the responses of
#' the simulated Normal random variables. Third component is a Numeric vector with the proportion in each level of each variable.
#'
#' @export

sim_true_Normal <- function(mu, Sigma, beta, N_shapley = 10000, explainer, cutoff, response_mod){

  feat_names <- colnames(explainer$x_train)
  dim <- length(feat_names)
  if(!is.null(dim(cutoff))){
    cutoff[, 1] <- cutoff[, 1] - 10
    cutoff[, 4] <- cutoff[, 4] + 10
  }

  set.seed(1)
  sim <- mvrnorm(n = N_shapley, mu = mu, Sigma = Sigma)
  dt <- NULL
  if(is.matrix(cutoff)){
    for(i in 1:dim){
      dt <- cbind(dt, cut(sim[, i], cutoff[i, ], labels = c(1:dim), include.lowest = TRUE))
    }
  } else{
    for(i in 1:dim){
      dt <- cbind(dt, cut(sim[, i], cutoff, labels = c(1:dim)))
    }
  }
  dt <- data.table(dt)
  setnames(dt, names(dt), paste0("feat_", 1:dim,"_"))

  ## this is for the list being returned
  prop <- NULL
  for(i in 1:dim){
    prop <- c(prop, table(dt[, feat_names[i], with = FALSE]) / N_shapley)
  }

  ## everything below here is to get the mean of the responses i.e 'mn' -------
  dt <- dt[, lapply(.SD, as.factor)]

  # dt_response <- copy(dt) # you need this copy otherwise you affect dt when you change dt_response
  dt[, 'epsilon' := 0]
  dt <- cbind(dt, data.table(mod_matrix))

  ## 3. Calculate response
  mod_matrix <- model.matrix(~.-1, data = dt[, 1:dim],
                             contrasts.arg = lapply(dt[, 1:dim],contrasts,contrasts=FALSE))
  all_responses <- response_mod(mod_matrix_full = cbind(1,mod_matrix),
                                beta = beta,
                                epsilon = dt$epsilon)
  mn <- mean(all_responses)

  ## -------

  joint_prob <- table(dt[, ..feat_names])  / N_shapley
  joint_prob_dt0 <- data.table(joint_prob)
  joint_prob_dt <- joint_prob_dt0[, lapply(.SD, as.factor), .SDcols = feat_names]
  joint_prob_dt <- cbind(joint_prob_dt, joint_prob_dt0[, .(N)])
  setnames(joint_prob_dt,"N", "joint_prob")


  return(list(joint_prob_dt, mn, prop))
}

#' Function to calculate marginal probabilities of the cutoff jointly Normal random variables
#'
#' @description
#'
#' @param joint_prob_dt list. Calculated using the \code{sim_true_Normal} function.
#' @param explainer explainer object from shapr package.
#'
#' @return list
#'
#' @export

marg_prob <- function(joint_prob_dt, explainer){

  feat_names <- colnames(explainer$x_train)

  ## compute all marginal probabilities
  marg_list <- list()
  marg_list[[1]] <- NA
  for(i in 2:nrow(explainer$S)){
    col_names <- feat_names[as.logical(explainer$S[i, ])]
    col <- joint_prob_dt[, ..col_names]
    nb_unique_comb <- nrow(unique(col))

    mat <- joint_prob_dt[, .(marg_prob = sum(joint_prob)), by = col_names]

    marg_list[[i]] <- mat
  }
  return(marg_list)
}

#' Function to calculate conditional probabilities of the cutoff jointly Normal random variables
#'
#' @description
#'
#' @param marg_list List. Contains the marginal probabilities calculated using the \code{marg_prob} function.
#' @param joint_prob_dt List. Calculated using the \code{sim_true_Normal} function.
#' @param explainer explainer object from shapr package.
#'
#' @return list
#'
#' @export

cond_prob <- function(marg_list, joint_prob_dt, explainer){

  feat_names <- colnames(explainer$x_train)

  cond_list <- list()
  cond_list[[1]] <- NA

  for(i in 2:nrow(explainer$S)){
    col_names <- feat_names[as.logical(explainer$S[i, ])]
    col <- joint_prob_dt[, ..col_names]
    nb_unique_comb <- nrow(unique(col))

    # working on this
    mat0 <- joint_prob_dt[, .(marg_prob = sum(joint_prob)), by = col_names]
    setkeyv(mat0, col_names)
    setkeyv(joint_prob_dt, col_names)
    mat <- merge(mat0, joint_prob_dt, all.x = TRUE)
    mat[, cond_prob := joint_prob / marg_prob]
    mat[, conditioned_on := paste(col_names, collapse = ", ")]

    cond_list[[i]] <- mat

  }
  return(cond_list)
}

#' Function to extract the column number of the conditional expectation matrix as a function of
#' all the possible x_test values
#'
#' @description
#'
#' @param tbl Data.table. Consists of all possible x_test values.
#'
#' @return list of column numbers
#'
#' @export

col_fun <- function(tbl, S_dt){
  dim <- ncol(tbl)
  v <- tbl[, 1:dim]
  v_S <- data.table(ifelse(is.na(v), 0, 1))
  colnum <- S_dt[v_S, .(id), on = names(v_S)]
  return(colnum)
}

#' Function to calculate conditional expectations of the cutoff jointly Normal random variables
#'
#' @description
#'
#' @param cond_list List. Calculated using the \code{cond_prob} function.
#' @param explainer explainer object from shapr package.
#'
#' @return list
#'
#' @export

cond_expec <- function(cond_list, explainer){

  feat_names <- colnames(explainer$x_train)
  dim <- length(feat_names)

  cond_expec_list <- list()
  cond_expec_list[[1]] <- NULL

  for(i in 2:nrow(explainer$S)){
    col_names <- feat_names[as.logical(explainer$S[i, ])]
    tmp0 <- cond_list[[i]]
    tmp0[, predict := predict_model(explainer$model, newdata = .SD), .SDcols = feat_names]
    tmp0[, expected_value := predict * cond_prob]
    tmp <- tmp0[, sum(expected_value), by = col_names]
    # tmp[, conditioned_on := paste(col_names, collapse = ", ")]
    setnames(tmp, "V1", "cond_expec")
    cond_expec_list[[i]] <- tmp
  }

  cond_expec <- rbindlist(l = cond_expec_list, fill = TRUE)
  setcolorder(cond_expec, c(feat_names, "cond_expec")) # "conditioned_on"

  S_dt <- data.table(explainer$S)
  S_dt[, id := 0:(nrow(S_dt) - 1)]
  setnames(S_dt, c(feat_names, "id"))

  all_levels <- list()
  for(i in 1:dim){
    all_levels[[i]] <- as.numeric(levels(cond_expec[, get(feat_names[i])]))
  }
  mat <- do.call(CJ, all_levels)
  setnames(mat, 1:ncol(mat), feat_names)
  mat <- mat[, lapply(.SD, as.factor), .SDcol = feat_names]

  # as.numeric is just in case the factors are not 1, 2, 3
  # cond_expec0 <- cbind(cond_expec[, lapply(.SD, as.numeric), .SDcol = 1:dim], cond_expec[, -(1:dim)])

  cond_expec[, colnum := col_fun(.SD, S_dt), .SDcol = feat_names]
  # cond_expec1 <- cond_expec[,  c(feat_names, "cond_expec", "colnum"), with = FALSE]

  select_cols <- c(feat_names, "i.cond_expec", "i.colnum") # Martin's help
  tmp <- list()
  for (i in 1:nrow(cond_expec)){
    on_cols <- feat_names[!is.na(subset(cond_expec[i,], select = feat_names))]
    # OLD: tmp[[i]] <- mat[cond_expec[i, ], .(feat1, feat2, feat3, cond_expec = i.cond_expec, colnum = i.colnum), on = on_cols]
    # NEW
    tmp[[i]] <- mat[cond_expec[i, ], ..select_cols, on = on_cols]
    setnames(tmp[[i]], c("i.cond_expec","i.colnum"), c("cond_expec","colnum"))
  }
  tmp_dt <- rbindlist(tmp)

  # The fun.aggregate function here is just to get it work when I got wrong column numbers.
  final_dt <- dcast(tmp_dt, formula = paste0(paste0(feat_names, collapse = "+"), "~colnum"), value.var = "cond_expec", fun.aggregate = mean)

  return(final_dt)
}
# x_test <- c(1, 1, 2)
# 2.019000 1.323062 2.130058 2.652778 1.428016 1.933333 2.747191 2.000000 - old stuff
# 2.019000 1.323062 2.360063 3.368715 - new stuff

#' Function to extrac the correct conditional expectation vector for each x_test
#'
#' @description
#'
#' @param x_test Matrix. Consists of all the test observations. Has the same dimension as the number of joint Normal random variables calculated in \code{sim_true_Normal} function.
#' @param cond_expec_dt data.table. Calculated using the \code{cond_expec} function.
#' @param prediction_zero Numeric. Number to assigned to phi_0 in Shapley framework.
#'
#' @return list
#'
#' @export

extract_cond_expec <- function(x_test, cond_expec_dt, prediction_zero){
  on_cols <- names(x_test)
  dim <- length(on_cols)
  results <- list()
  for(i in 1:nrow(x_test)){
    results[[i]] <- cond_expec_dt[x_test[i, ],, on = on_cols]
  }
  results_dt <- rbindlist(results)[, -(1:dim)]
  results_dt <- cbind(rep(prediction_zero, nrow(results_dt)), results_dt)
  setnames(results_dt, "V1", "0")
}



#' Function to calculate the true Shapley values based on the conditional expectations calculated using \code{cond_expec}
#'
#' @description
#'
#' @param explainer explainer object from shapr package.
#' @param cond_expec list. Calculated using \code{cond_expec} function.
#' @param x_test vector of test observations. Has the same dimension as the number of joint Normal random variables calculated in \code{sim_true_Normal} function.
#'
#' @return vector of Shapley values.
#'
#' @export

true_Kshap <- function(explainer, cond_expec_mat, x_test){
  dim <- ncol(x_test)
  Kshap <- matrix(0, nrow = nrow(x_test), ncol = nrow(explainer$W))
  for (i in 1:nrow(x_test)) {
    Kshap[i, ] = explainer$W %*% t(as.matrix(cond_expec_mat[i, ]))
  }
  Kshap <- data.table(Kshap)
  setnames(Kshap, 1:(dim + 1), c("none", names(x_test)))

  return(Kshap)
}

#' Function to calculate the true Shapley values under the strict conditions that the features are independent and the response function is linear.
#'
#' @description
#'
#' @param x_test_onehot vector of Numerics. The testing observations, one-hot encoded
#' @param beta vector of Numerics. The coefficients of the linear model.
#' @param dt
#' @param prop
#'
#' @return vector of Shapley values.
#'
#' @export

linear_Kshap_old <- function(x_test_onehot, beta, dt, prop){

  # prop <- c(0, apply(dt[, .(feat12, feat13)], 2, sum) / nrow(dt), 0, apply(dt[, .(feat22, feat23)], 2, sum) / nrow(dt), 0, apply(dt[, .(feat32, feat33)], 2, sum) / nrow(dt))

  # for(i in c(1, 4, 7)){
  #   prop[i] <- 1 - prop[i + 1] - prop[i + 2]
  # }
  phi0 <- NULL
  phi0 <- c(phi0, beta[1] + sum(beta[-1] * prop))

  x_test0 <- x_test_onehot

  # x_test1 <- cbind((1 - x_test0[1, 1]) * (1 - x_test0[1, 2]),  x_test0[1, 1:2], (1 - x_test0[1, 3]) * (1 - x_test0[1, 4]), x_test0[1, 3:4], (1 - x_test0[1, 5]) * (1 - x_test0[1, 6]), x_test0[1, 5:6])

  x_test1 <- c((1 - x_test0[1]) * (1 - x_test0[2]),  x_test0[1:2], (1 - x_test0[3]) * (1 - x_test0[4]), x_test0[3:4], (1 - x_test0[5]) * (1 - x_test0[6]), x_test0[5:6])
  for(i in 1:length(prop)){
    phi0 <- c(phi0, beta[i + 1] * (x_test1[[i]] - prop[i]) )
  }
  phi <- c(phi0[1], sum(phi0[2:4]), sum(phi0[5:7]), sum(phi0[8:10]))

  return(phi)
}

#' Function to calculate the true Shapley values under the strict conditions that the features are independent and the response function is linear.
#'
#' @description
#'
#' @param x_test_onehot vector of Numerics. The testing observations, one-hot encoded
#' @param beta vector of Numerics. The coefficients of the linear model.
#' @param dt
#' @param prop
#'
#' @return vector of Shapley values.
#'
#' @export

linear_Kshap <- function(x_test_onehot_full, beta, prop, beta_matcher, no_features){

  phi0 <- beta[1] + sum(beta[-1] * prop)

  mult <- (t(x_test_onehot_full[,-1]) - matrix(prop,nrow=length(prop),ncol=nrow(x_test_onehot_full)))
  phi_raw <- t(mult*beta[-1])
  phi <- matrix(NA,nrow=nrow(phi_raw),ncol=no_features)
  for (i in 1:no_features){
    phi[,i] <- rowSums(phi_raw[,which(beta_matcher==i)-1])
  }
  phi <- cbind(phi0,phi)
  colnames(phi) <- NULL

  return(phi)
}



# shapley_method <- true_linear

#' Function to calculate the mean average error (MAE) between the true Shapley values and the estimated Shapley values
#'
#' @description
#'
#' @param true_shapley vector of Numerics. The vector of true Shapley values.
#' @param shapley_method vector of Numerics. The vector of estimated Shapley values
#'
#' @return vector of Shapley values.
#'
#' @export

MAE <- function(true_shapley, shapley_method){
  mean(apply(abs(true_shapley - shapley_method), 2, mean)[-1])
}

#' Function to simulate the data and calculate the estimated Shapley value as well as simulate the random variables to calculate the true Shapley values
#'
#' @description
#'
#' @param parameters_list list. List of all the parameters needed for simulating the data and calcualting the true and estimated Shapley values.
#'
#' @return vector of Shapley values.
#'
#' @export


simulate_data <- function(parameters_list){

  # parameters
  mu <- parameters_list$mu
  beta <- parameters_list$beta
  N_shapley <- parameters_list$N_shapley # number of var to simulate to calc true Shapley
  N_testing <- parameters_list$N_testing
  N_training <- parameters_list$N_training
  cutoff <- parameters_list$cutoff
  noise <- parameters_list$noise
  response_mod <- parameters_list$response_mod
  fit_mod <- parameters_list$fit_mod
  methods <- parameters_list$methods

  dim <- length(mu)

  # check correct input
  if(!all(is.numeric(mu))){
    stop("mu vector must contain only numerics.")
  }
  if(!all(is.numeric(beta))){
    stop("beta vector must contain only numerics.")
  }
  if(!is.numeric(N_shapley)){
    stop("N_shapley must be a numeric.")
  }
  if(!is.numeric(N_testing)){
    stop("N_testing must be a numeric.")
  }
  if(!is.numeric(N_training)){
    stop("N_training must be a numeric.")
  }
  if(!is.boolean(noise)){
    stop("noise must be a boolean.")
  }
  if(!((length(beta) - 1) %% dim == 0)){
    stop("beta variable - 1 must be divisible by length of mu parameter.")
  }
  if((length(cutoff) - 1) != dim){
    if(!is.null(cutoff)){
      stop("cutoff vector must either be length of mu plus 1 or be NULL.")
    }
  }

  ## make sure Sigma is positive definite
  Sigma <- matrix(rep(parameters_list$corr, dim^2), dim, dim)
  for(i in 1:dim){
    Sigma[i, i] <- parameters_list$Sigma_diag
  }
  if(!lqmm::is.positive.definite(Sigma)) {
    print("Covariance matrix is not positive definite but will be converted.")
    Sigma <- make.positive.definite(Sigma)
    print("New Sigma matrix:")
    print(Sigma)
  }

  ## Creating a vector matching the beta elements to each features

  no_features <- length(mu)
  no_categories <- length(cutoff)-1
  beta_matcher <- c(0,rep(1:no_features,each=no_categories))
  if(length(beta)!=length(beta_matcher)){
    stop("The length of beta is not consistent with the lengths of mu and cutoff.")
  }

  ## 1. simulate training and testing data
  tm_current <- Sys.time()
  print("Simulating training and testing data", quote = FALSE, right = FALSE)
  x <- mvrnorm(n = N_testing + N_training, mu = mu, Sigma = Sigma)

  dt <- NULL
  if(is.null(cutoff)){ ## to get equal proportion in each level
    for(i in 1:dim){
      dt <- cbind(dt, cut(x[, i], quantile(x[, i], probs = (1 / dim * seq(0, dim, by = 1))), labels = 1:dim, include.lowest = TRUE)) # without include.lowest, you get NA at the boundaries
      cutoff <- c(cutoff, quantile(x[, i], probs = (1 / dim * seq(0, dim, by = 1))))
    }
    cutoff <- matrix(cutoff, nrow = dim, byrow = TRUE)
  } else{
    for(i in 1:dim){
      dt <- cbind(dt, cut(x[, i], cutoff, labels = 1:dim))
    }
  }

  dt <- data.table(dt)
  setnames(dt, names(dt), paste0("feat_", 1:dim,"_"))
  feat_names <- names(dt[, 1:dim])

  dt <- dt[, lapply(.SD, as.factor)]

  if(noise == TRUE){
    dt[, epsilon := rnorm(N_testing + N_training, 0, 0.1^2)] #
  } else{
    dt[, epsilon := 0]
  }

  ## 2. One-hot encoding of training data
  tm_now <- Sys.time(); print(tm_now - tm_current); tm_current <- Sys.time()
  print("One-hot encoding training data", quote = FALSE, right = FALSE)
  # dt <- cbind(dt, data.table(model.matrix(~., data = dt[, .(feat1, feat2, feat3)])))
  mod_matrix <- model.matrix(~.-1, data = dt[, 1:dim],
                             contrasts.arg = lapply(dt[, 1:dim],contrasts,contrasts=FALSE))


  dt <- cbind(dt, data.table(mod_matrix))
  full_onehot_names <- colnames(mod_matrix)
  reduced_onehot_names <- full_onehot_names[-grep("_1$",full_onehot_names)] # names without reference levels


  ## 3. Calculate response
  tm_now <- Sys.time(); print(tm_now - tm_current); tm_current <- Sys.time()
  print("Calculating response of training data", quote = FALSE, right = FALSE)
  dt[, response := response_mod(mod_matrix_full = cbind(1,mod_matrix),
                                beta = beta,
                                epsilon = epsilon)]


  ## 4. Fit model
  tm_now <- Sys.time(); print(tm_now - tm_current); tm_current <- Sys.time()
  print("Fitting model of training data", quote = FALSE, right = FALSE)
  if(fit_mod == 'regression'){
    form <- as.formula(paste0("response~", paste(feat_names, collapse= "+")))
    model <- lm(formula = form, data = dt[-(1:N_testing), ])
  }

  ## 5. initalize shapr object with trained model -- this is used for calculating true shapley
  tm_now <- Sys.time(); print(tm_now - tm_current); tm_current <- Sys.time()
  print("Initializing shapr object with trained model", quote = FALSE, right = FALSE)
  x_train <- dt[-(1:N_testing), ..feat_names] ## used in explainer()
  x_test <- dt[(1:N_testing), ..feat_names] ## used in cond_expec_mat()
  y_train <- dt[-(1:N_testing), .(response)] ## used in cond_expec_mat()
  explainer <- shapr(x_train, model)

  ## 6. calculate the true shapley values
  tm_now <- Sys.time(); print(tm_now - tm_current); tm_current <- Sys.time()
  print("Simulating Normal random variables to calculate true Shapley value", quote = FALSE, right = FALSE)
  joint_prob_dt <- sim_true_Normal(mu, Sigma, beta, N_shapley = N_shapley, explainer, cutoff, response_mod)

  tm_now <- Sys.time(); print(tm_now - tm_current); tm_current <- Sys.time()
  print("Calculating marginal probability distributions", quote = FALSE, right = FALSE)
  marg_list <- marg_prob(joint_prob_dt[[1]], explainer)

  tm_now <- Sys.time(); print(tm_now - tm_current); tm_current <- Sys.time()
  print("Calculating conditional probability distributions", quote = FALSE, right = FALSE)
  cond_list <- cond_prob(marg_list, joint_prob_dt[[1]], explainer)

  tm_now <- Sys.time(); print(tm_now - tm_current); tm_current <- Sys.time()
  print("Calculating all conditional expectations", quote = FALSE, right = FALSE)
  cond_expec_dt <- cond_expec(cond_list, explainer)

  tm_now <- Sys.time(); print(tm_now - tm_current); tm_current <- Sys.time()
  print("Extracting conditional expectations", quote = FALSE, right = FALSE)
  cond_expec_mat <- extract_cond_expec(x_test, cond_expec_dt, prediction_zero = joint_prob_dt[[2]])

  tm_now <- Sys.time(); print(tm_now - tm_current); tm_current <- Sys.time()
  print("Calculating true Shapley values", quote = FALSE, right = FALSE)
  true_shapley <- true_Kshap(explainer, cond_expec_mat, x_test)

  ## 7. calculate true shapley under linear model and independence assumption (only if correlation is 0)
  tm_now <- Sys.time(); print(tm_now - tm_current); tm_current <- Sys.time()
  print("Calculating Shapley value under linear model and indepdent variables assumption", quote = FALSE, right = FALSE)


  if(explainer$model_type == 'regression'){
    if(parameters_list$corr == 0){
#      true_linear <- t(apply(x_test_onehot, 1, FUN = linear_Kshap_old, beta = beta, dt = dt, prop = joint_prob_dt[[3]]))
      x_test_onehot_full <- dt[(1:N_testing), ..full_onehot_names]
      true_linear <-linear_Kshap(x_test_onehot_full = cbind(1,x_test_onehot_full),
                                 beta = beta,
                                 prop = joint_prob_dt[[3]],
                                 beta_matcher = beta_matcher,
                                 no_features = no_features)

    } else{
      true_linear <- NULL
    }
  } else{
    true_linear <- NULL
  }

  ## 8. calculate approximate shapley value with different methods
  p <- mean(y_train$response) # since y_train is no longer a matrix

  timeit <- list()

  explanation_list <- list()
  for(m in methods){
    tm_now <- Sys.time(); print(tm_now - tm_current); tm_current <- Sys.time()
    print(paste0("Estimating Shapley value with ", m, "method"), quote = FALSE, right = FALSE)
    if(m == 'empirical' | m == 'empirical_ind' |  m == 'gaussian' | m == 'ctree_onehot'){

      x_train_onehot <- as.matrix( dt[-(1:N_testing), -c(1:(dim + 2), ncol(dt)), with = FALSE]) # bugfix

      if(fit_mod == 'regression'){
        fmla <- as.formula(paste("response ~", paste(colnames(x_train_onehot), collapse = " + ")))
        model_onehot <- lm(fmla, data = dt[(1:N_testing), -c(1:(dim + 1)), with = FALSE]) # dt[-(1:N_testing), !c("feat1", "feat2", "feat3", "epsilon")])
      }

      explainer_onehot <- shapr(x_train_onehot, model_onehot)

      if(m == 'ctree_onehot'){
        tm <- Sys.time()
        explanation_list[[m]] <- explain(
          x_test_onehot,
          approach = 'ctree',
          explainer = explainer_onehot,
          prediction_zero = p,
          sample = FALSE)
        tm2 <- Sys.time()
        timeit['ctree_onehot'] <- (tm2 - tm)
      } else if(m == 'empirical_ind'){
        tm <- Sys.time()
        explanation_list[[m]] <- explain(
          x_test_onehot,
          approach = "empirical",
          type = "independence",
          explainer = explainer_onehot,
          prediction_zero = p,
          sample = FALSE)
        tm2 <- Sys.time()
        timeit['empirical_ind'] <- (tm2 - tm)
      } else{
        tm <- Sys.time()
        explanation_list[[m]] <- explain(
          x_test_onehot,
          approach = m,
          explainer = explainer_onehot,
          prediction_zero = p,
          sample = FALSE)
        tm2 <- Sys.time()
        timeit[m] <- (tm2 - tm)
      }
      explanation_list[[m]]$dt_sum <- cbind(NULL, explanation_list[[m]]$dt[, 1])
      for(i in c(2, 4, 6)){
        explanation_list[[m]]$dt_sum <- cbind(explanation_list[[m]]$dt_sum, apply(explanation_list[[m]]$dt[, i:(i + 1)], 1, sum))
      }
      setnames(explanation_list[[m]]$dt_sum, c("none", feat_names))

    } else { ## for ctree without one-hot encoding
      tm <- Sys.time()
      explanation_list[[m]] <- explain(
        x_test,
        approach = m,
        explainer = explainer,
        prediction_zero = p,
        sample = FALSE)
      tm2 <- Sys.time()
      timeit[m] <- (tm2 - tm)
    }
  }

  return_list <- list()
  return_list[['true_shapley']] <- true_shapley
  return_list[['true_linear']] <- true_linear
  return_list[['methods']] <- explanation_list
  return_list[['timing']] <- timeit
  print("--- End ---")
  return(return_list)

}


# EXTRA STUFF

## Annabelle's method
# cond_expec0[, max_levels := pmax(get(paste0("levels", 1:dim)))] # only for Annabelle's function

# for(i in 1:dim){
#   cond_expec0[, paste0("levels", i) := nlevels(as.factor(get(paste0('feat', i))))][]
# }

# row_fun <- function(tbl){
#   dim <- (ncol(tbl) - 1) / 2
#   sum_levels <- 0
#   max_levels <- unique(tbl[, max_levels])
#   # max_levels <- 0
#   for(i in 1:dim){
#     assign(paste0("levels", i), tbl[, i + dim, with = FALSE])
#     assign(paste0("v", i), tbl[, i, with = FALSE])
#     sum_levels <- sum_levels + get(paste0("levels", i))[[1]]
#     # max_levels <- pmax(max_levels, get(paste0("levels", i))[[1]])
#   }
#   ans <- matrix(NA, nrow = dim, ncol = max_levels)
#   for(j in 1:(dim - 1)){ # loop through 1:(dim - 1) columns
#     tmp <- 1
#     for(k in (j + 1):dim){
#       tmp <- tmp * get(paste0("levels", k))[[1]]
#     }
#     if(is.na(get(paste0("v", j)))[[1]]){
#       for(l in 1:(get(paste0("levels", j)))[[1]]){
#         ans[j, l] <- (l - 1) * tmp
#       }
#     } else{
#       ans[j, 1:max_levels] <- (get(paste0("v", j))[[1]] - 1) * tmp
#     }
#   }
#   if(is.na(get(paste0("v", dim))[[1]])){ # this is for the last column
#     for(l in 1:dim){
#       ans[dim, l] <- l - 1
#     }
#   } else{
#     ans[dim, 1:max_levels] <- (get(paste0("v", dim))[[1]] - 1)
#   }
#     ans2 <- apply(X = ans, MARGIN = 2, FUN = sum, na.rm = TRUE)
#
#     truth <- ans
#
#     for(row in 1:nrow(ans)){
#       for(shift in 1:(max_levels)){
#         tmp <- ans
#         for(col in 1:(ncol(ans) - 1)){
#           tmp[row, col] <- ans[row, col + 1]
#         }
#         tmp[row, ncol(ans)] <- ans[row, 1]
#         ans <- tmp
#         ans2 <- c(ans2, apply(X = tmp, MARGIN = 2, FUN = sum, na.rm = TRUE))
#       }
#     }
#     ans3 <- unique(ans2)
#
#   if(is.vector(ans3)){
#     ans3 <- sort(ans3 + 1)
#     return(paste(ans3, collapse = ", "))
#   } else{
#     return(ans3 + 1)
#   }
# }
#
# for(i in 1:nrow(cond_expec0)){
#   cond_expec0[i, rownum := row_fun(.SD), .SDcol = c(paste0("feat", 1:dim), paste0("levels", 1:dim), "max_levels"), .I]
# }


