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
corr <- 0
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
joint_prob_dt_list <- create_exact_joint_prob(mu, Sigma, explainer, cutoff)

explanation <- explain(
  x_test,
  approach = "categorical",
  explainer = explainer,
  prediction_zero = p,
  joint_prob_dt = joint_prob_dt_list
)
# CHECK IF THESE ARE THE CORRECT SHAPLEY VALUES
trueShapley <- explanation$dt
#
#             none     feat_1_    feat_2_      feat_3_
#  1: -0.006911122  0.17886549  0.3103481  0.128915771
#  2: -0.006911128  0.16861067  0.3387340  0.004515755
#  3: -0.006911114  0.12690519  0.3392079 -0.859868219
#  4: -0.006911130  0.17471228 -0.8221828  0.159523798
#  5: -0.006911134  0.14784422 -0.7854903  0.043430402
#  6: -0.006911117  0.08955061 -0.7767222 -0.812659507
#  7: -0.006911130  0.14350677  0.4749477  0.189732191
#  8: -0.006911130  0.09984569  0.5200367  0.082035303
#  9: -0.006911111  0.02101715  0.5390722 -0.763787135
# 10: -0.006911147 -0.90125979  0.3498207  0.164459758
# 11: -0.006911151 -0.90546713  0.3661116  0.046107234
# 12: -0.006911135 -0.94183954  0.3559195 -0.812943674
# 13: -0.006911156 -0.90723371 -0.7845309  0.198709184
# 14: -0.006911157 -0.92805428 -0.7599333  0.088663280
# 15: -0.006911139 -0.98101481 -0.7618314 -0.762093563
# 16: -0.006911156 -0.94081674  0.5102221  0.233672627
# 17: -0.006911155 -0.97843033  0.5432162  0.132023231
# 18: -0.006911133 -1.05192580  0.5515855 -0.708466141
# 19: -0.006911165  1.43832772  0.3675785  0.201560188
# 20: -0.006911167  1.43953484  0.3730405  0.088622117
# 21: -0.006911149  1.40929598  0.3505812 -0.764295240
# 22: -0.006911175  1.42997449 -0.7691524  0.240568261
# 23: -0.006911174  1.41456837 -0.7553838  0.135936809
# 24: -0.006911154  1.36774138 -0.7695490 -0.708686484
# 25: -0.006911177  1.39297428  0.5221834  0.282366042
# 26: -0.006911173  1.36077515  0.5443485  0.186131099
# 27: -0.006911150  1.29341322  0.5404508 -0.648224722
# none     feat_1_    feat_2_      feat_3_
# just to check

explanation <- explain(
  x_test,
  approach = "ctree",
  explainer = explainer,
  prediction_zero = p
)

ctreeShapley <- explanation$dt

#             none     feat_1_    feat_2_     feat_3_
#  1: -0.006911121  0.05591232  0.3695573  0.19265972
#  2: -0.006911125  0.05001815  0.3632142  0.09862805
#  3: -0.006911131  0.07275163  0.3536447 -0.82015136
#  4: -0.006911136  0.08237494 -0.7596332  0.18931150
#  5: -0.006911125  0.04315664 -0.7551840  0.11781170
#  6: -0.006911122  0.06310392 -0.7376259 -0.82530912
#  7: -0.006911129  0.05672025  0.5472564  0.20420998
#  8: -0.006911128  0.06433343  0.5532203  0.08436401
#  9: -0.006911125  0.06531236  0.5367966 -0.80580674
# 10: -0.006911122 -0.95260805  0.3772356  0.18839310
# 11: -0.006911128 -0.94186039  0.3475698  0.10104236
# 12: -0.006911123 -0.93622479  0.3475111 -0.81015010
# 13: -0.006911131 -0.93119394 -0.7443334  0.18247188
# 14: -0.006911129 -0.93179353 -0.7646544  0.09712356
# 15: -0.006911120 -0.93241490 -0.7485896 -0.82393529
# 16: -0.006911132 -0.92951138  0.5472124  0.18537691
# 17: -0.006911131 -0.94886437  0.5466317  0.09904174
# 18: -0.006911129 -0.93704746  0.5330673 -0.80482626
# 19: -0.006911130  1.45710417  0.3581656  0.19219661
# 20: -0.006911134  1.46917386  0.3766589  0.05536471
# 21: -0.006911122  1.44563471  0.3626619 -0.81271468
# 22: -0.006911130  1.45035479 -0.7337433  0.18477879
# 23: -0.006911128  1.45152698 -0.7472084  0.09080278
# 24: -0.006911129  1.46228675 -0.7438156 -0.82896527
# 25: -0.006911125  1.44843429  0.5417832  0.20730619
# 26: -0.006911132  1.46972930  0.5410881  0.08043740
# 27: -0.006911125  1.47314563  0.5390795 -0.82658584
# none     feat_1_    feat_2_     feat_3_

MAE(trueShapley, ctreeShapley, weights = 1)

# old stuff
# ----- functions
MAE <- function(true_shapley, shapley_method, weights){
  mean(colSums((abs(true_shapley - shapley_method))* weights)[-1])
  # mean(apply(), 2, sum)[-1])
}

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

col_fun <- function(tbl, S_dt){
  dim <- ncol(tbl)
  v <- tbl[, 1:dim]
  v_S <- data.table(ifelse(is.na(v), 0, 1))
  colnum <- S_dt[v_S, .(id), on = names(v_S)]
  return(colnum)
}

cond_expec_new2 <- function(cond_dt, explainer, x_test, prediction_zero){

  # setnames(cond_dt, old = c('id_combination', 'id'), new = c('colid', 'rowid'))
  setkeyv(cond_dt, c("id_combination", "id"))

  mat <- unique(x_test)
  mat <- mat[, lapply(.SD, as.factor), .SDcol = feat_names] # To be removed later
  mat[, id := .I] # Adding identifyer to match on

  ## New
  S_dt <- data.table(explainer$S)
  S_dt[S_dt == 0] <- NA
  S_dt[, id_combination := 1:nrow(S_dt)]
  setnames(S_dt, c(paste0(feat_names, "conditioned"), "id_combination"))

  cols <- c(feat_names, "id_combination")
  cond_dt_tmp <- cond_dt[, ..cols]
  cond_dt_tmp2 <- cond_dt_tmp[, lapply(.SD, as.character)]
  cond_dt_tmp3 <- cond_dt_tmp2[, lapply(.SD, as.numeric)]

  tmp <- cond_dt_tmp3[S_dt, on = 'id_combination']

  cols2 <- paste0(feat_names, "conditioned")
  tmp2 <- tmp[, ..feat_names] * tmp[, ..cols2]
  setnames(tmp2, c(paste0(feat_names, "conditioned")))

  setkeyv(cond_dt, "id_combination")
  cond_dt_new <- cbind(cond_dt, tmp2)

  ##
  cond_dt_new[, predict := predict_model(explainer$model, newdata = .SD), .SDcols = feat_names]

  setkey(cond_dt_new, "id")

  col_names <- c("id_combination", paste0(feat_names, "conditioned"))
  col_names2 <- paste0(feat_names, "conditioned")

  XX <- cond_dt_new[, .(cond_expec = sum(predict * cond_prob)), by = col_names]
  setkey(XX, "id_combination")

  XXY = cond_dt_new[XX, on = col_names]

  XXZ = XXY[XXY[, id_combination != 1]]

  final_dt <- dcast(XXZ, formula = "id~id_combination", value.var = "cond_expec")

  x_test_id <- mat[x_test, on = feat_names]
  S_char_vec <- as.character(2:(nrow(explainer$S)))
  final_dt_x_test <- cbind("1" = prediction_zero, final_dt[x_test_id, ..S_char_vec, on = "id"])
  return(final_dt_x_test)

}
# -----

set.seed(1)
joint_prob_dt <- create_exact_joint_prob(mu, Sigma, explainer, cutoff) # beta, response_mod

marg_list <- marg_prob(joint_prob_dt, explainer)

cond_list <- cond_prob(marg_list, joint_prob_dt, explainer)

cond_list0 <- cond_list
cond_list0[[1]] <- data.frame(marg_prob = 1, joint_prob = 1, id = joint_prob_dt$id, cond_prob = 1)
cond_dt <- rbindlist(cond_list0, id = 'id_combination')

joint_prob_dt0 <- joint_prob_dt
joint_prob_dt0[, joint_prob := NULL]

cond_dt <- cond_dt[joint_prob_dt0, on = 'id']

cond_expec_new2(cond_dt, explainer, x_test, p)

##

#                1          2          3            4          5          6           7          8
#  1: -0.006911126  0.1620030  0.3133681  0.132273236  0.5113667  0.3294761  0.44122313  0.6112182
#  2: -0.006911126  0.1620030  0.3133681  0.007682069  0.5113667  0.1486658  0.33769027  0.5049493
#  3: -0.006911126  0.1620030  0.3133681 -0.783840861  0.5113667 -0.7903810 -0.51710069 -0.4006662
#  4: -0.006911126  0.1620030 -0.8401850  0.132273236 -0.6616052  0.3294761 -0.66212987 -0.4948579
#  5: -0.006911126  0.1620030 -0.8401850  0.007682069 -0.6616052  0.1486658 -0.71591315 -0.6011268
#  6: -0.006911126  0.1620030 -0.8401850 -0.783840861 -0.6616052 -0.7903810 -1.52050257 -1.5067423
#  7: -0.006911126  0.1620030  0.4694667  0.132273236  0.5625957  0.3294761  0.68486243  0.8012755
#  8: -0.006911126  0.1620030  0.4694667  0.007682069  0.5625957  0.1486658  0.68112552  0.6950066
#  9: -0.006911126  0.1620030  0.4694667 -0.783840861  0.5625957 -0.7903810 -0.06161918 -0.2106089
# 10: -0.006911126 -0.9976571  0.3133681  0.132273236 -0.5606848 -0.7502548  0.44122313 -0.3938905
# 11: -0.006911126 -0.9976571  0.3133681  0.007682069 -0.5606848 -0.8947791  0.33769027 -0.5001594
# 12: -0.006911126 -0.9976571  0.3133681 -0.783840861 -0.5606848 -1.8013589 -0.51710069 -1.4057749
# 13: -0.006911126 -0.9976571 -0.8401850  0.132273236 -1.7443490 -0.7502548 -0.66212987 -1.4999665
# 14: -0.006911126 -0.9976571 -0.8401850  0.007682069 -1.7443490 -0.8947791 -0.71591315 -1.6062355
# 15: -0.006911126 -0.9976571 -0.8401850 -0.783840861 -1.7443490 -1.8013589 -1.52050257 -2.5118509
# 16: -0.006911126 -0.9976571  0.4694667  0.132273236 -0.5344507 -0.7502548  0.68486243 -0.2038331
# 17: -0.006911126 -0.9976571  0.4694667  0.007682069 -0.5344507 -0.8947791  0.68112552 -0.3101021
# 18: -0.006911126 -0.9976571  0.4694667 -0.783840861 -0.5344507 -1.8013589 -0.06161918 -1.2157175
# 19: -0.006911126  1.2850063  0.3133681  0.132273236  1.7618181  1.6108451  0.44122313  2.0005552
# 20: -0.006911126  1.2850063  0.3133681  0.007682069  1.7618181  1.4987306  0.33769027  1.8942863
# 21: -0.006911126  1.2850063  0.3133681 -0.783840861  1.7618181  0.6288823 -0.51710069  0.9886708
# 22: -0.006911126  1.2850063 -0.8401850  0.132273236  0.5637786  1.6108451 -0.66212987  0.8944792
# 23: -0.006911126  1.2850063 -0.8401850  0.007682069  0.5637786  1.4987306 -0.71591315  0.7882103
# 24: -0.006911126  1.2850063 -0.8401850 -0.783840861  0.5637786  0.6288823 -1.52050257 -0.1174052
# 25: -0.006911126  1.2850063  0.4694667  0.132273236  1.7531359  1.6108451  0.68486243  2.1906126
# 26: -0.006911126  1.2850063  0.4694667  0.007682069  1.7531359  1.4987306  0.68112552  2.0843437
# 27: -0.006911126  1.2850063  0.4694667 -0.783840861  1.7531359  0.6288823 -0.06161918  1.1787282
# 1          2          3            4          5          6           7          8







