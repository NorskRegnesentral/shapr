library(shapr)
library(MASS)
library(data.table)

# parameters
dim <- 3
no_categories <- 3
mu <- rep(0, dim)
set.seed(1); beta <- round(rnorm(dim * no_categories + 1), 1)
noise = TRUE
response_mod = response_mod <- function(mod_matrix_full, beta, epsilon){
  as.vector(mod_matrix_full %*% beta) + epsilon
}
fit_mod = "regression"
methods = c("ctree")
cutoff = cutoff <- c(-200, 0, 1, 200)
Sample_test = FALSE # Can be FALSE as well, then No_test_sample not used.
No_test_sample = 25
No_train_obs = 100
x_test_dt <- NULL
N_sample_gaussian = c(50)
seed = ifelse(exists("seed"), seed, 1)
corr <- 0.3
Sigma_diag <-1
x_test_dt <- NULL


## make sure Sigma is positive definite
Sigma <- matrix(rep(corr, dim^2), nrow = dim, ncol = dim)
for(i in 1:dim){
  Sigma[i, i] <- Sigma_diag
}

## 1. simulate training data
set.seed(seed)
x <- mvrnorm(n =  No_train_obs, mu = mu, Sigma = Sigma)
dt <- NULL
for(i in 1:dim){
  dt <- cbind(dt, cut(x[, i], cutoff, labels = 1:no_categories))
}

## Get test data
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

dt <- data.table(rbind(dt, x_test_dt))
setnames(dt, names(dt), paste0("feat_", 1:dim,"_"))
feat_names <- names(dt[, 1:dim])

dt <- dt[, lapply(.SD, as.factor)]

set.seed(seed)
epsilon1 <- rnorm(No_train_obs, 0, 0.1^2)
epsilon2 <- rnorm(No_test_obs, 0, 0.1^2)
epsilon <- c(epsilon1, epsilon2)
dt[, epsilon := epsilon]

## 2. One-hot encoding of training data
mod_matrix <- model.matrix(~.-1, data = dt[, 1:dim], contrasts.arg = lapply(dt[, 1:dim], contrasts, contrasts = FALSE))

dt <- cbind(dt, data.table(mod_matrix))
full_onehot_names <- colnames(mod_matrix)
reduced_onehot_names <- full_onehot_names[-grep("_1$", full_onehot_names)] # names without reference levels

## 3. Calculate response
dt[, response := response_mod(mod_matrix_full = cbind(1, mod_matrix), beta = beta, epsilon = epsilon)]

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

explainer <- shapr(x_train, model)

## 6. calculate the true shapley values

# Specifying the phi_0, i.e. the expected prediction without any features
p <- mean(y_train$response)

# Computing the actual Shapley values with kernelSHAP accounting for feature dependence using

explainer$x_test <- x_test
explainer$approach <- "categorical"

#
create_exact_joint_prob <- function(mu, Sigma, explainer, cutoff, algorithm = mvtnorm::GenzBretz(),
                                    mc.cores = 16){ # beta, response_mod

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

  # all_responses <- response_mod(mod_matrix_full = cbind(1,mod_matrix),
  #                               beta = beta,
  #                               epsilon = rep(0, nrow(mod_matrix)))

  # prop <- NULL
  # for (i in 1:dim){
  #   prop <- c(prop, diff(pnorm(cutoff, mean = mu[i], sd = sqrt(Sigma[i,i]))))
  # }
  # names(prop) = rep(1:no_categories, times = dim)
  #

  # Lists with vectors containing the lower and upper combinations
  upper_dt <- all_x_dt[, lapply(.SD, upper_func,cutoff=cutoff), .SDcols = feat_names]
  lower_dt <- all_x_dt[, lapply(.SD, lower_func,cutoff=cutoff), .SDcols = feat_names]

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

  all_x_dt[, id := .I]

  setkeyv(all_x_dt, rev(feat_names)) # To get same ordering as previous version


  #mn <- sum(all_responses * all_probs)

  return(all_x_dt) # list(all_x_dt, mn, prop)
}

upper_func <- function(x,cutoff){
  cutoff[as.numeric(x)+1]
}

lower_func <- function(x, cutoff){
  cutoff[as.numeric(x)]
}
#
set.seed(1)
joint_prob_dt_list <- create_exact_joint_prob(mu, Sigma, explainer, cutoff) # beta, response_mod

explanation <- explain(
  x_test,
  approach = "categorical",
  explainer = explainer,
  prediction_zero = p,
  joint_prob_dt = joint_prob_dt_list
) # START HERE


# just to check
explanation <- explain(
  x_test,
  approach = "ctree",
  explainer = explainer,
  prediction_zero = p
)

