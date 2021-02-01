#' String extraction function
#'
#' Function extracting string between two specific characters, minor customization of this one
#' http://www.r-bloggers.com/how-to-extract-a-string-between-2-characters-in-r-and-sas/
#'
#' @param mystring Character vector to extract from.
#' @param initial.character Character determining the starting point of extractions
#' @param final.character Character determining the end point of extractions
#' @return snippet
#' @export


getstr = function(mystring, initial.character = "_", final.character = "_") {
  # check that all 3 inputs are character variables
  if (!is.character(mystring)) {
    stop('The parent string must be a character variable.')
  }

  if (!is.character(initial.character)) {
    stop('The initial character must be a character variable.')
  }


  if (!is.character(final.character)) {
    stop('The final character must be a character variable.')
  }

  add = 0
  if(initial.character == final.character){add=1}

  # pre-allocate a vector to store the extracted strings
  snippet = rep(0, length(mystring))

  for (i in 1:length(mystring)) {
    # extract the initial position
    initial.position <- gregexpr(initial.character, mystring[i])[[1]][1] + 1

    # extract the final position
    final.position <- gregexpr(final.character, mystring[i])[[1]][1+add] - 1

    # extract the substring between the initial and final positions, inclusively
    snippet[i] <- substr(mystring[i], initial.position, final.position)
  }
  return(snippet)
}


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
  no_categories <- length(cutoff) -1

  set.seed(1)
  sim <- mvrnorm(n = N_shapley, mu = mu, Sigma = Sigma)
  dt <- NULL
  if(is.matrix(cutoff)){
    for(i in 1:dim){
      dt <- cbind(dt, cut(sim[, i], cutoff[i, ], labels = c(1:no_categories), include.lowest = TRUE))
    }
  } else{ # This has been checked, but the if code chunk above has not
    for(i in 1:dim){
      dt <- cbind(dt, cut(sim[, i], cutoff, labels = c(1:no_categories)))
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

  dt[, 'epsilon' := 0]

  mod_matrix <- model.matrix(~.-1, data = dt[, 1:dim],
                             contrasts.arg = lapply(dt[, 1:dim],contrasts,contrasts=FALSE))
  dt <- cbind(dt, data.table(mod_matrix))

  ## 3. Calculate response
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

  joint_prob_dt[, feat_comb_id:=.I]

  return(list(joint_prob_dt, mn, prop))
}


# Helper functions
upper_func <- function(x,cutoff){
  cutoff[as.numeric(x)+1]
}

# Helper functions
lower_func <- function(x,cutoff){
  cutoff[as.numeric(x)]
}



#' Function to compute exact multivariate Normal probabilitites with true mu and Sigma parameters.
#'
#' @description
#'
#' @param mu Numeric or vector indicating the true mean of the Normal or joint Normal random variables used to calculate
#' the true Shapley values.
#' @param Sigma Numeric (if mu is Numeric) or matrix with values used for covariance matrix of the random variables used to
#' calculate the true Shapley values.
#' @param beta Numeric or vector. These are the true coefficients of the response modelthat we are trying to explain with the
#' Shapley values.
#' @param explainer explainer object from shapr package.
#' @param cutoff vector of Numerics. This indicates where to cutoff the Normal random variables to make levels.
#' @param response_mod function. The true response model that indicates how the features relate to the response.
#' @param algorithm function. The algorithm to produce the multivariate normal probabilitites using mvtnorm. Either
#' mvtnorm::GenzBretz() or mvtnorm::Miwa(). The former (default) is slightly seed dependent, but general and much faster
#' for large dimensions. For dim 3 and 4 Miwa may be slightly faster, but there speed is not an issue.
#' @param mc.cores Integer. The number of cores to use when parallelizing the normal probability calculation. 16 is the
#' default (using 90 sec on dim 10 on a 16 cored computer).
#' @details
#'
#' @return list First component is a data.table with the joint probability of each level. Second component is the mean of the responses of
#' the simulated Normal random variables. Third component is a Numeric vector with the proportion in each level of each variable.
#'
#' @export

create_exact_joint_prob <- function(mu, Sigma, beta, explainer, cutoff, response_mod, algorithm = mvtnorm::GenzBretz(),
                                    mc.cores = 16){

  feat_names <- colnames(explainer$x_train)
  dim <- length(feat_names)
  no_categories <- length(cutoff) - 1

  all_x_list <- list()
  for(i in 1:dim){
    all_x_list[[i]] <- 1:no_categories
  }
  all_x_dt <- do.call(CJ, all_x_list)
  names(all_x_dt) <- feat_names



  all_x_dt[, (feat_names) := lapply(.SD, as.factor),.SDcols = feat_names]

  ## Response comptutation
  mod_matrix <- model.matrix(~.-1, data = all_x_dt,
                             contrasts.arg = lapply(all_x_dt[, 1:dim], contrasts, contrasts = FALSE))

  all_responses <- response_mod(mod_matrix_full = cbind(1,mod_matrix),
                                beta = beta,
                                epsilon = rep(0, nrow(mod_matrix)))

  prop <- NULL
  for (i in 1:dim){
    prop <- c(prop,diff(pnorm(cutoff, mean = mu[i], sd = sqrt(Sigma[i,i]))))
  }
  names(prop) = rep(1:no_categories, times = dim)


  # Lists with vectors containing the lower and upper combinations
  upper_dt <- all_x_dt[, lapply(.SD,upper_func,cutoff=cutoff), .SDcols = feat_names]
  lower_dt <- all_x_dt[, lapply(.SD,lower_func,cutoff=cutoff), .SDcols = feat_names]

  upper_dt_list = as.list(as.data.table(t(upper_dt)))
  lower_dt_list = as.list(as.data.table(t(lower_dt)))

  corr <- cov2cor(Sigma)

  all_probs <- parallel::mcmapply(FUN = mvtnorm::pmvnorm,
                                  lower = lower_dt_list,
                                  upper = upper_dt_list,
                                  MoreArgs = list(mean = mu,
                                                  corr = corr,
                                                  algorithm = algorithm),
                                  mc.cores = mc.cores)

  all_probs <- all_probs/sum(all_probs)


  all_x_dt[, joint_prob := all_probs]

  setkeyv(all_x_dt, rev(feat_names)) # To get same ordering as previous version
  all_x_dt[, feat_comb_id := .I]

  mn <- sum(all_responses * all_probs)

  return(list(all_x_dt, mn, prop))
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

    mat0 <- marg_list[[i]]
    setkeyv(mat0, col_names)
    setkeyv(joint_prob_dt, col_names)
    cond_list[[i]] <- merge(mat0, joint_prob_dt, all.x = TRUE)
    cond_list[[i]][, cond_prob := joint_prob / marg_prob]
    cond_list[[i]][,(feat_names):=NULL] # To save memory
  }

  return(cond_list)
}

#' Function to extract the column number of the conditional expectation matrix as a function of
#' all the possible x_test values
#'
#' @description
#'
#' @param tbl Data.table. Consists of all possible x_test values.
#' @param S_dt ??
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
#' for the x_test observations. I.e. doing what cond_expec + extract_cond_expec does together,
#' just much faster.
#'
#' @description
#'
#' @param cond_list List. Calculated using the \code{cond_prob} function.
#' @param explainer explainer object from \code{shapr} package.
#' @param x_test Matrix. Consists of all the test observations. Has the same dimension
#' as the number of joint Normal random variables calculated in \code{sim_true_Normal} function.
#' @param cond_expec_dt data.table. Calculated using the \code{cond_expec} function.
#' @param prediction_zero Numeric. Number to assigned to phi_0 in Shapley framework.
#' @param joint_prob_dt data.table The first element in the list calculated using the \code{sim_true_Normal} function.
#'
#' @return data.table
#'
#' @export

cond_expec_new <- function(cond_list, explainer, x_test, prediction_zero, joint_prob_dt){

  feat_names <- colnames(explainer$x_train)
  dim <- length(feat_names)

  S_dt <- data.table(explainer$S)
  S_dt[, id := 0:(nrow(S_dt) - 1)]
  setnames(S_dt, c(feat_names, "id"))

  mat <- unique(x_test)
  mat <- mat[, lapply(.SD, as.factor), .SDcol = feat_names] # To be removed later
  mat[, rowid := .I] # Adding identifyer to match on
  # mat <- joint_prob_dt[mat,.(rowid,feat_comb_id), on=feat_names]


  cond_expec_list <- list()
  cond_expec_list[[1]] <- NULL

  joint_prob_dt[, predict := predict_model(explainer$model, newdata = .SD), .SDcols = feat_names]

  setkey(joint_prob_dt, feat_comb_id)

  tmp <- list()
  tmp0 <- NULL
  for(i in 2:nrow(explainer$S)){
    col_names <- feat_names[as.logical(explainer$S[i, ])]
    these_cols <- c(col_names,"feat_comb_id", "predict")
    tmp0 <- merge(cond_list[[i]], joint_prob_dt[, ..these_cols], by = "feat_comb_id") # Need the whole thing here
    tmp0[, expected_value := predict * cond_prob]
    cond_expec_list[[i]] <- tmp0[, list(cond_expec=sum(expected_value)), by = col_names]
    tmp[[i]] <- cbind(cond_expec_list[[i]][mat, .(rowid, cond_expec), on = col_names, allow.cartesian = TRUE],
                      colnum = i - 1)
  }
  tmp_dt <- rbindlist(tmp, use.names = T)

  final_dt <- dcast(tmp_dt, formula = "rowid~colnum", value.var = "cond_expec")
  x_test_id <- mat[x_test, on = feat_names]
  S_char_vec <- as.character(1:(nrow(explainer$S) - 1))
  final_dt_x_test <- cbind("0" = prediction_zero, final_dt[x_test_id, ..S_char_vec,on = "rowid"])

  return(final_dt_x_test)
}


#' Function to calculate the true Shapley values based on the conditional expectations calculated using \code{cond_expec}
#'
#' @description
#'
#' @param explainer explainer object from shapr package.
#' @param cond_expec_mat list. Calculated using \code{cond_expec_new} function.
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

linear_Kshap <- function(x_test_onehot_full, beta, prop){

  beta_matcher <- as.numeric(getstr(colnames(x_test_onehot_full)))
  no_features <- max(beta_matcher)

  phi0 <- beta[1] + sum(beta[-1] * prop)

  mult <- (t(x_test_onehot_full) - matrix(prop, nrow = length(prop), ncol = nrow(x_test_onehot_full)))
  phi_raw <- t(mult*beta[-1])
  phi <- matrix(NA, nrow = nrow(phi_raw), ncol = no_features)
  for (i in 1:no_features){
    phi[, i] <- rowSums(phi_raw[, which(beta_matcher == i)])
  }
  phi <- cbind(phi0,phi)
  colnames(phi)[-1] <- paste0("phi", 1:no_features)

  return(phi)
}

#' Function to calculate the mean average error (MAE) between the true Shapley values and the estimated Shapley values
#'
#' @description
#'
#' @param true_shapley vector of Numerics. The vector of true Shapley values.
#' @param shapley_method vector of Numerics. The vector of estimated Shapley values
#' @param weights vector of weights with length equal to  number of rows of true_shapley/shapley_method
#' @return vector of Shapley values.
#'
#' @export


MAE <- function(true_shapley, shapley_method, weights){
  mean(colSums((abs(true_shapley - shapley_method))* weights)[-1])
  # mean(apply(), 2, sum)[-1])
}

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

AE <- function(true_shapley, shapley_method){
  apply(abs(true_shapley - shapley_method)[,-1], 1, mean)
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

  timeit <- list()

  #
  mu <- parameters_list$mu
  beta <- parameters_list$beta
  # N_shapley <- parameters_list$N_shapley # number of var to simulate to calc true Shapley
  Sample_test <- parameters_list$Sample_test
  No_train_obs <- parameters_list$No_train_obs
  No_test_sample <- parameters_list$No_test_sample
  N_sample_gaussian <- parameters_list$N_sample_gaussian
  cutoff <- parameters_list$cutoff
  noise <- parameters_list$noise
  response_mod <- parameters_list$response_mod
  fit_mod <- parameters_list$fit_mod
  methods <- parameters_list$methods
  seed <- parameters_list$seed
  corr <- parameters_list$corr
  Sigma_diag <- parameters_list$Sigma_diag
  no_categories <- parameters_list$no_categories
  if(is.null(seed)) seed <- 1
  x_test_dt <- parameters_list$x_test_dt
  #
  dim <- length(mu)



  # check correct input
  if(!all(is.numeric(mu))){
    stop("mu vector must contain only numerics.")
  }
  if(!all(is.numeric(beta))){
    stop("beta vector must contain only numerics.")
  }
  if(Sample_test){
    if(!is.numeric(No_test_sample)){
      stop("If Sample_test is TRUE, No_test_sample must be a numeric.")
    }
  }
  if(!is.numeric(No_train_obs)){
    stop("No_train_obs must be a numeric.")
  }
  if(!is.boolean(noise)){
    stop("noise must be a boolean.")
  }
  if(!((length(beta) - 1) %% dim == 0)){
    stop("beta variable - 1 must be divisible by length of mu parameter.")
  }
  if((length(cutoff) - 1) != no_categories){
    if(!is.null(cutoff)){
      stop("cutoff vector must either be length of no_categories plus 1 or be NULL.")
    }
  }

  ## make sure Sigma is positive definite
  Sigma <- matrix(rep(corr, dim^2), nrow = dim, ncol = dim)
  for(i in 1:dim){
    Sigma[i, i] <- Sigma_diag
  }
  if(!lqmm::is.positive.definite(Sigma)) {
    print("Covariance matrix is not positive definite but will be converted.")
    Sigma <- make.positive.definite(Sigma)
    print("New Sigma matrix:")
    print(Sigma)
  }

  print(paste0("Dimension: ", dim), quote = FALSE, right = FALSE)
  print(paste0("Nb of categories: ", no_categories), quote = FALSE, right = FALSE)
  print(paste0("Correlation: ", corr), quote = FALSE, right = FALSE)
  print(paste0("Nb train observations: ", No_train_obs), quote = FALSE, right = FALSE)

  if(!is.null(x_test_dt)){
    print(paste0("Nb test observations: ", nrow(x_test_dt)), quote = FALSE, right = FALSE)
  }

  ## 1. simulate training data
  tm_current <- Sys.time()
  set.seed(seed)
  x1 <- mvrnorm(n =  No_train_obs, mu = mu, Sigma = Sigma)
  x <- x1

  dt <- NULL
  if(is.null(cutoff)){
    for(i in 1:no_categories){
      dt <- cbind(dt, cut(x[, i], quantile(x[, i], probs = (1 / no_categories * seq(0, no_categories, by = 1))), labels = 1:no_categories, include.lowest = TRUE)) # without include.lowest, you get NA at the boundaries
      cutoff <- c(cutoff, quantile(x[, i], probs = (1 / no_categories * seq(0, no_categories, by = 1))))
    }
    cutoff <- matrix(cutoff, nrow = dim, ncol = no_categories, byrow = TRUE)
  } else{
    for(i in 1:dim){
      dt <- cbind(dt, cut(x[, i], cutoff, labels = 1:no_categories))
    }
  }


  ## Get test data
  ## Get every combination of the levels and dimensions unless x_test_dt is passed to parameters_list
  if(is.null(x_test_dt)){
    x_test_list <- list()
    for(i in 1:dim){
      x_test_list[[i]] <- 1:no_categories
    }
    x_test_dt <- do.call(CJ, x_test_list)

    if(Sample_test){
      if(nrow(x_test_dt) > No_test_sample){
        sampled_rows <- sample(1:nrow(x_test_dt), size = No_test_sample, replace = FALSE)
        x_test_dt <- x_test_dt[sampled_rows, ]
      }
    }
  }

  No_test_obs <- nrow(x_test_dt)
  dt <- rbind(dt, x_test_dt)

  dt <- data.table(dt)
  setnames(dt, names(dt), paste0("feat_", 1:dim,"_"))
  feat_names <- names(dt[, 1:dim])

  dt_numeric <- dt
  dt <- dt[, lapply(.SD, as.factor)]

  set.seed(seed)
  if(noise == TRUE){
    epsilon1 <- rnorm(No_train_obs, 0, 0.1^2)
    epsilon2 <- rnorm(No_test_obs, 0, 0.1^2)
    epsilon <- c(epsilon1, epsilon2)

    dt_numeric[, epsilon := epsilon]
    dt[, epsilon := epsilon]
  } else{
    dt_numeric[, epsilon := 0]
    dt[, epsilon := 0]
  }

  ## 2. One-hot encoding of training data
  mod_matrix <- model.matrix(~.-1, data = dt[, 1:dim], contrasts.arg = lapply(dt[, 1:dim], contrasts, contrasts = FALSE))

  dt <- cbind(dt, data.table(mod_matrix))
  full_onehot_names <- colnames(mod_matrix)
  reduced_onehot_names <- full_onehot_names[-grep("_1$", full_onehot_names)] # names without reference levels

  ## 3. Calculate response
  dt[, response := response_mod(mod_matrix_full = cbind(1, mod_matrix), beta = beta, epsilon = epsilon)]
  dt_numeric[, response := dt[['response']]]


  ## 4. Fit model
  if(fit_mod == 'regression'){
    form <- as.formula(paste0("response~", paste(feat_names, collapse = "+")))
    model <- lm(formula = form, data = dt[(1:No_train_obs), ])

    fmla_onehot <- as.formula(paste("response ~", paste(reduced_onehot_names, collapse = " + ")))
    model_onehot <- lm(fmla_onehot, data = dt[(1:No_train_obs)])
  }

  ## 5. initalize shapr object with trained model -- this is used for calculating true shapley
  x_train <- dt[(1:No_train_obs), ..feat_names]
  x_test <- dt[-(1:No_train_obs), ..feat_names]
  y_train <- dt[(1:No_train_obs), .(response)]

  x_train_numeric <- dt_numeric[(1:No_train_obs), ..feat_names]
  x_test_numeric <- dt_numeric[-(1:No_train_obs), ..feat_names]

  # For computing the true Shapley values (with correlation 0)
  x_test_onehot_full <- dt[-(1:No_train_obs), ..full_onehot_names]

  x_test_onehot_reduced <- dt[-(1:No_train_obs), ..reduced_onehot_names]
  x_train_onehot_reduced <- dt[(1:No_train_obs), ..reduced_onehot_names]

  ##
  explainer <- shapr(x_train, model) # print(class(model)) # "lm"

  if(any(grepl("empirical", methods)) | any(grepl("gaussian", methods)) | any(grepl("ctree_onehot", methods))){
    explainer_onehot <- shapr(x_train_onehot_reduced, model_onehot)
  }

  ## Create custom function of model_type for lm
  model_type.numeric_lm <<- function(x) {
  }

  features.numeric_lm <<- function(x, cnms, feature_labels = NULL) {
    if (!is.null(feature_labels)) message_features_labels()

    nms <- tail(all.vars(x$terms), -1)
    if (!all(nms %in% cnms)) error_feature_labels()
    return(nms)
  }

  # Create custom function of predict_model for caret
  predict_model.numeric_lm <<- function(x, newdata) {
    newdata <- as.data.table(newdata)
    newdata0 <- newdata[, lapply(.SD, as.factor)]
    class(x) <- "lm"
    predict(x, newdata0)
  }

  class(model) <- "numeric_lm"
  explainer_numeric <- shapr(x_train_numeric, model)
  ## END custom function

  ## 6. calculate the true shapley values
  tm_true_Shapley <- Sys.time();
  print("Started calculating true Shapley values.", quote = FALSE, right = FALSE)

  set.seed(10)
  tm0 <- proc.time();
  # joint_prob_dt_list <- sim_true_Normal(mu, Sigma, beta, N_shapley = N_shapley, explainer, cutoff, response_mod)
  joint_prob_dt_list <- create_exact_joint_prob(mu, Sigma, beta, explainer, cutoff, response_mod)
  tm1 <- proc.time();
  timeit['Simulate_true_Normal'] <- list((tm1 - tm0))

  tm0 <- proc.time();
  marg_list <- marg_prob(joint_prob_dt_list[[1]], explainer)
  tm1 <- proc.time();
  timeit['Calculate_marginal_distributions'] <- list((tm1 - tm0))

  tm0 <- proc.time();
  cond_list <- cond_prob(marg_list, joint_prob_dt_list[[1]], explainer)
  tm1 <- proc.time();
  timeit['Calculate_conditional_distributions'] <- list((tm1 - tm0))

  tm0 <- proc.time();
  cond_expec_mat <- cond_expec_new(cond_list, explainer, x_test, prediction_zero = joint_prob_dt_list[[2]], joint_prob_dt = joint_prob_dt_list[[1]])
  tm1 <- proc.time();
  timeit['Calculate_expectations_distributions'] <- list((tm1 - tm0))

  rm(cond_list) # to save memory
  gc()

  tm0 <- proc.time();
  true_shapley <- true_Kshap(explainer, cond_expec_mat, x_test)
  tm1 <- proc.time();
  timeit['Calculate_true_Shapley'] <- list((tm1 - tm0))

  tm_true_Shapley1 <- Sys.time();
  print("Finished calculating true Shapley values.", quote = FALSE, right = FALSE)
  print(tm_true_Shapley1 - tm_true_Shapley)

  if(explainer$model_type == 'regression'){
    if(parameters_list$corr == 0){
      true_linear <- linear_Kshap(x_test_onehot_full = x_test_onehot_full, beta = beta, prop = joint_prob_dt_list[[3]])
    } else{
      true_linear <- NULL
    }
  } else{
    true_linear <- NULL
  }

  ## 8. calculate approximate shapley value with different methods
  p <- mean(y_train$response) # since y_train is no longer a matrix

  ## to compute sum of Shapley values - only for one-hot encoded variables
  beta_matcher <- as.numeric(getstr(reduced_onehot_names))
  no_features <- max(beta_matcher)
  phi_sum_mat <- matrix(NA, nrow = No_test_obs, ncol = no_features)

  tm_now <- proc.time();
  print("Started estimating Shapley values with various methods.", quote = FALSE, right = FALSE)

  explanation_list <- list()
  for(m in methods){
    if(m == 'empirical_ind'){
      tm0 <- proc.time()
      explanation_list[[m]] <- explain(
        x_test_onehot_reduced,
        approach = "empirical",
        type = "independence",
        explainer = explainer_onehot,
        prediction_zero = p,
        sample = FALSE,
        n_samples = 1000)
      tm1 <- proc.time()

      for (i in 1:no_features){
        phi_sum_mat[, i] <- rowSums(subset(explanation_list[[m]]$dt, select = which(beta_matcher == i) + 1))
      }
      colnames(phi_sum_mat) <- feat_names
      explanation_list[[m]]$dt_sum <- cbind(explanation_list[[m]]$dt[, 1], phi_sum_mat)

      print(paste0("Finished estimating Shapley value with ", m, " method."), quote = FALSE, right = FALSE)
      print(tm1 - tm0)
      timeit[m] <- list((tm1 - tm0))

    } else if(m == 'empirical'){
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

      for (i in 1:no_features){
        phi_sum_mat[, i] <- rowSums(subset(explanation_list[[m]]$dt, select = which(beta_matcher == i) + 1))
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

          for (i in 1:no_features){
            phi_sum_mat[, i] <- rowSums(subset(explanation_list[[paste0(m, "_nsamples", j, "_trial", k)]]$dt, select = which(beta_matcher == i) + 1))
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
      # x_test_onehot_reduced[, id := NULL]
      # x_test_onehot_reduced[, no_times := NULL]
      # x_test_onehot_reduced[, Indx := NULL]
      explanation_list[[m]] <- explain(
        x_test_onehot_reduced,
        approach = 'ctree',
        explainer = explainer_onehot,
        prediction_zero = p,
        sample = FALSE,
        mincriterion = 0.95)
      tm1 <- proc.time()

      for (i in 1:no_features){
        phi_sum_mat[, i] <- rowSums(subset(explanation_list[[m]]$dt, select = which(beta_matcher == i) + 1))
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

    }
    else if(m == 'kernelSHAP'){
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
  tm_now1 <- proc.time();
  print("Ended estimating Shapley values with various methods.", quote = FALSE, right = FALSE)

  return_list <- list()
  return_list[['true_shapley']] <- true_shapley
  return_list[['true_linear']] <- true_linear
  return_list[['joint_prob_true']] <- joint_prob_dt_list[[1]]
  return_list[['x_train-y_train']] <- cbind(x_train, y_train)
  return_list[['methods']] <- explanation_list
  return_list[['timing']] <- timeit
  return_list[['seed']] <- seed
  return_list[['parameters']] <- parameters_list
  print("--- End ---")
  return(return_list)

}
