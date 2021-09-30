
library(MASS)
library(data.table)
library(shapr)

# parameters
dim <- 3
no_categories <- 3
mu <- rep(0, dim)
set.seed(1); beta <- round(rnorm(dim * no_categories + 1), 1)
noise <- TRUE

response_mod <- response_mod <- function(mod_matrix_full, beta, epsilon) {
  as.vector(mod_matrix_full %*% beta) + epsilon
}

fit_mod <- "regression"
methods <- c("ctree")
cutoff <- cutoff <- c(-200, 0, 1, 200)
Sample_test <- FALSE # Can be FALSE as well, then No_test_sample not used.
No_test_sample <- 5
No_train_obs <- 1000
x_test_dt <- NULL
N_sample_gaussian <- 1000

################
corr <- 0 ####
################
Sigma_diag <- 1
x_test_dt <- NULL

## make sure Sigma is positive definite
Sigma <- matrix(rep(corr, dim^2), nrow = dim, ncol = dim)
for (i in 1:dim) {
  Sigma[i, i] <- Sigma_diag
}

## 1. simulate training data
set.seed(1)
x <- mvrnorm(n =  No_train_obs, mu = mu, Sigma = Sigma)
dt <- NULL
for (i in 1:dim) {
  dt <- cbind(dt, cut(x[, i], cutoff, labels = 1:no_categories))
}

## Get test data
if (is.null(x_test_dt)) {
  x_test_list <- list()
  for (i in 1:dim) {
    x_test_list[[i]] <- 1:no_categories
  }
  x_test_dt <- do.call(CJ, x_test_list)

  if (Sample_test) {
    if (nrow(x_test_dt) > No_test_sample) {
      sampled_rows <- sample(1:nrow(x_test_dt), size = No_test_sample, replace = FALSE)
      x_test_dt <- x_test_dt[sampled_rows, ]
    }
  }
}

No_test_obs <- nrow(x_test_dt)

dt <- data.table(rbind(dt, x_test_dt))
setnames(dt, names(dt), paste0("feat_", 1:dim,  "_"))
feat_names <- names(dt[, 1:dim])

dt <- dt[, lapply(.SD, as.factor)]

set.seed(1)
epsilon1 <- rnorm(No_train_obs, 0, 0.1^2)
epsilon2 <- rnorm(No_test_obs, 0, 0.1^2)
epsilon <- c(epsilon1, epsilon2)
dt[, epsilon := epsilon]

## 2. One-hot encoding of training data
mod_matrix <- model.matrix(~  . - 1, data = dt[, 1:dim], contrasts.arg = lapply(dt[, 1:dim],
                                                                                contrasts, contrasts = FALSE))

dt <- cbind(dt, data.table(mod_matrix))
full_onehot_names <- colnames(mod_matrix)

mod_matrix_not_complete <- model.matrix(~., data = dt[, 1:dim], contrasts.arg = lapply(dt[, 1:dim], contrasts,
                                                                                       contrasts = TRUE))
reduced_onehot_names <- colnames(mod_matrix_not_complete)
reduced_onehot_names <- reduced_onehot_names[reduced_onehot_names != "(Intercept)"]


## 3. Calculate response
dt[, response := response_mod(mod_matrix_full = cbind(1, mod_matrix), beta = beta, epsilon = epsilon)]

## 4. Fit model
form <- as.formula(paste0("response~", paste(feat_names, collapse = "+")))
model <- lm(formula = form, data = dt[(1:No_train_obs)])

fmla_onehot <- as.formula(paste("response ~", paste(reduced_onehot_names, collapse = "+")))
model_onehot <- lm(fmla_onehot, data = dt[(1:No_train_obs)])

## 5. Initalize shapr object with trained model -- this is used for calculating true shapley
x_train <- dt[(1:No_train_obs), ..feat_names]
x_test <- dt[- (1:No_train_obs), ..feat_names]
y_train <- dt[(1:No_train_obs), .(response)]

# For computing the true Shapley values (with correlation 0)
x_test_onehot_full <- dt[-(1:No_train_obs), ..full_onehot_names]
x_test_onehot_reduced <- dt[-(1:No_train_obs), ..reduced_onehot_names]
x_train_onehot_reduced <- dt[(1:No_train_obs), ..reduced_onehot_names]


## 6. calculate the true shapley values
p <- mean(y_train$response)

# Computing the actual Shapley values with kernelSHAP accounting for feature dependence using
explainer <- shapr(x_train, model)
explainer_onehot <- shapr(x_train_onehot_reduced, model_onehot)

source("inst/devel_scripts/create_exact_joint_prob.R")

joint_prob_dt <- create_exact_joint_prob(mu = mu, Sigma = Sigma, explainer = explainer, cutoff = cutoff)[]

#    feat_1_ feat_2_ feat_3_  joint_prob
# 1:       1       1       1 0.125000000
# 2:       1       1       2 0.085336187
# 3:       1       1       3 0.039663813
# 4:       1       2       1 0.085336187
# 5:       1       2       2 0.058258118
# 6:       1       2       3 0.027078069
# 7:       1       3       1 0.039663813
# 8:       1       3       2 0.027078069
# 9:       1       3       3 0.012585745

# no grouping - used as a test
p <- mean(y_train$response) # -0.05576529

set.seed(1)
explanation_truth <- explain(
  x_test,
  approach = "categorical",
  explainer = explainer,
  prediction_zero = p,
  joint_prob_dt = joint_prob_dt
)

head(explanation_truth$dt)
#         none    feat_1_    feat_2_     feat_3_
# 1: 0.0261835 0.09159106  0.3163901  0.15733783
# 2: 0.0261835 0.09159106  0.3163901  0.07027111
# 3: 0.0261835 0.09159106  0.3163901 -0.81895667
# 4: 0.0261835 0.09159106 -0.7834612  0.15733783
# 5: 0.0261835 0.09159106 -0.7834612  0.07027111
# 6: 0.0261835 0.09159106 -0.7834612 -0.81895667

## Start grouping stuff
group1 <- list(c(1), # no groups at all
               c(2),
               c(3))
group1_names = lapply(group1, function(x) {
  names(x_test)[x]
})

explainer_group1 <- shapr(x_train, model, group = group1_names)

set.seed(1)
explanation_truth_group1 <- explain(
  x_test,
  approach = "categorical",
  explainer = explainer_group1,
  prediction_zero = p,
  joint_prob_dt = joint_prob_dt
)

head(explanation_truth_group1$dt)
#         none     group1     group2      group3
# 1: 0.0261835 0.09159106  0.3163901  0.15733783
# 2: 0.0261835 0.09159106  0.3163901  0.07027111
# 3: 0.0261835 0.09159106  0.3163901 -0.81895667
# 4: 0.0261835 0.09159106 -0.7834612  0.15733783
# 5: 0.0261835 0.09159106 -0.7834612  0.07027111
# 6: 0.0261835 0.09159106 -0.7834612 -0.81895667

group2 <- list(c(1, 3), (2))
group2_names = lapply(group2, function(x) {
  names(x_test)[x]
})

explainer_group2 <- shapr(x_train, model, group = group2_names)

set.seed(1)
explanation_truth_group2 <- explain(
  x_test,
  approach = "categorical",
  explainer = explainer_group2,
  prediction_zero = p,
  joint_prob_dt = joint_prob_dt
)

head(explanation_truth_group2$dt)
# - grouping: (feat1, feat3), (feat2)
#         none     group1     group2
# 1: 0.0261835  0.2625670  0.3027520
# 2: 0.0261835  0.1755003  0.3027520
# 3: 0.0261835 -0.7137275  0.3027520
# 4: 0.0261835  0.2625670 -0.7970993
# 5: 0.0261835  0.1755003 -0.7970993
# 6: 0.0261835 -0.7137275 -0.7970993

group3 <- list(c(1, 3, 2))
group3_names = lapply(group3, function(x) {
  names(x_test)[x]
})

explainer_group3 <- shapr(x_train, model, group = group3_names)

set.seed(1)
explanation_truth_group3 <- explain(
  x_test,
  approach = "categorical",
  explainer = explainer_group3,
  prediction_zero = p,
  joint_prob_dt = joint_prob_dt
)

head(explanation_truth_group3$dt)
# - grouping: (feat1, feat2, feat3)
#          none     group1
# 1: 0.02618352  0.5653190
# 2: 0.02618352  0.4782523
# 3: 0.02618352 -0.4109755
# 4: 0.02618352 -0.5345323
# 5: 0.02618352 -0.6215990
# 6: 0.02618352 -1.5108268


## 8. calculate approximate shapley values with different methods
explanation_list <- list()

## ctree
# 1.1 no groupings
explanation_list[['ctree']] <- explain(
  x_test,
  approach = 'ctree',
  explainer = explainer,
  prediction_zero = p,
  sample = FALSE,
  w_threshold = 1,
  n_samples = 1000)

print(head(explanation_list[['ctree']]$dt))
#          none    feat_1_    feat_2_     feat_3_
# 1: 0.02618352 0.06703414  0.3158207  0.18246415
# 2: 0.02618352 0.06703414  0.3158207  0.09539743
# 3: 0.02618352 0.06703414  0.3158207 -0.79383035
# 4: 0.02618352 0.06703414 -0.7840306  0.18246415
# 5: 0.02618352 0.06703414 -0.7840306  0.09539743
# 6: 0.02618352 0.06703414 -0.7840306 -0.79383035

explanation_list[['ctree']]["MAE"] <-  MAE(explanation_truth$dt,  explanation_list[['ctree']]$dt,  weights = joint_prob_dt$joint_prob)
# 0.01675088

head(cbind(explanation_list[['ctree']]$dt$none, explanation_list[['ctree']]$dt$feat_1_ + explanation_list[['ctree']]$dt$feat_3_,
           explanation_list[['ctree']]$dt$feat_2_))

#            [,1]       [,2]       [,3]
# [1,] 0.02618352  0.2494983  0.3158207
# [2,] 0.02618352  0.1624316  0.3158207
# [3,] 0.02618352 -0.7267962  0.3158207
# [4,] 0.02618352  0.2494983 -0.7840306
# [5,] 0.02618352  0.1624316 -0.7840306
# [6,] 0.02618352 -0.7267962 -0.7840306

# with groupings
# 1.2
explanation_list[['ctree_group1']] <- explain(
  x_test,
  approach = 'ctree',
  explainer = explainer_group1, # c(1), c(2), c(3)
  prediction_zero = p,
  sample = FALSE,
  w_threshold = 1,
  n_samples = 1000)

explanation_list[['ctree_group1']]["MAE"] <-  MAE(explanation_truth_group1$dt,  explanation_list[['ctree_group1']]$dt,  weights = joint_prob_dt$joint_prob)

print(head(explanation_list[['ctree_group1']]$dt))
#          none     group1     group2      group3
# 1: 0.02618352 0.06703414  0.3158207  0.18246415
# 2: 0.02618352 0.06703414  0.3158207  0.09539743
# 3: 0.02618352 0.06703414  0.3158207 -0.79383035
# 4: 0.02618352 0.06703414 -0.7840306  0.18246415
# 5: 0.02618352 0.06703414 -0.7840306  0.09539743
# 6: 0.02618352 0.06703414 -0.7840306 -0.79383035


# 2.3
explanation_list[['ctree_group2']] <- explain(
  x_test,
  approach = 'ctree',
  explainer = explainer_group2, # c(1, 3), c(2)
  prediction_zero = p,
  sample = FALSE,
  w_threshold = 1,
  n_samples = 1000)

explanation_list[['ctree_group2']]["MAE"] <-  MAE(explanation_truth_group2$dt,  explanation_list[['ctree_group2']]$dt,  weights = joint_prob_dt$joint_prob)
# 0.01306869

print(head(explanation_list[['ctree_group2']]$dt))
#          none     group1     group2
# 1: 0.02618352  0.2494983  0.3158207
# 2: 0.02618352  0.1624316  0.3158207
# 3: 0.02618352 -0.7267962  0.3158207
# 4: 0.02618352  0.2494983 -0.7840306
# 5: 0.02618352  0.1624316 -0.7840306
# 6: 0.02618352 -0.7267962 -0.7840306


# 2.4
explanation_list[['ctree_group3']] <- explain(
  x_test,
  approach = 'ctree',
  explainer = explainer_group3, # c(1, 3, 2)
  prediction_zero = p,
  sample = FALSE,
  w_threshold = 1,
  n_samples = 1000)

explanation_list[['ctree_group3']]["MAE"] <-  MAE(explanation_truth_group3$dt,  explanation_list[['ctree_group3']]$dt,  weights = joint_prob_dt$joint_prob)

print(head(explanation_list[['ctree_group3']]$dt))
#          none     group1
# 1: 0.02618352  0.5653190
# 2: 0.02618352  0.4782523
# 3: 0.02618352 -0.4109755
# 4: 0.02618352 -0.5345323
# 5: 0.02618352 -0.6215990
# 6: 0.02618352 -1.5108268




## EMPIRICAL
# 3.1 - don't use grouping in shapr(), sum Shapley values
beta_matcher <- as.numeric(getstr(reduced_onehot_names))
no_features <- max(beta_matcher)
phi_sum_mat <- matrix(NA, nrow = No_test_obs, ncol = no_features)

explanation_list[['empirical_after_shapr']] <- explain(
  x_test_onehot_reduced,
  approach = 'empirical',
  explainer = explainer_onehot,
  prediction_zero = p,
  sample = FALSE,
  w_threshold = 1,
  n_samples = 1000)

for (i in 1:no_features){
  phi_sum_mat[, i] <- rowSums(subset(explanation_list[['empirical_after_shapr']]$dt, select = which(beta_matcher == i) + 1))
}
colnames(phi_sum_mat) <- feat_names
explanation_list[['empirical_after_shapr']]$dt_sum <- cbind(explanation_list[['empirical_after_shapr']]$dt[, 1], phi_sum_mat)

print(head(explanation_list[['empirical_after_shapr']]$dt_sum))
#         none    feat_1_    feat_2_     feat_3_
# 1: 0.02618351 0.05110211  0.3219652  0.19225162
# 2: 0.02618353 0.06494674  0.3280420  0.08526356
# 3: 0.02618351 0.10099959  0.3108747 -0.82284984
# 4: 0.02618350 0.08536755 -0.8078294  0.18792953
# 5: 0.02618352 0.09264480 -0.8014691  0.08722524
# 6: 0.02618350 0.09041001 -0.7952708 -0.80596598

explanation_list[['empirical_after_shapr']]["MAE"] <-  MAE(explanation_truth$dt,  explanation_list[['empirical_after_shapr']]$dt_sum, weights = joint_prob_dt$joint_prob)
# 0.02326849


# 3.2. use new grouping function in shapr()
# group1_onehot <- list(c(1, 2),
#                       c(3, 4),
#                       c(5, 6))
# group1_onehot_names = lapply(group1_onehot, function(x) {
#   names(x_train_onehot_reduced)[x]
#   })

group1_onehot_names <- group_factors(x_train)

explainer1_onehot <- shapr(x_train_onehot_reduced, model_onehot, group = group1_onehot_names)

explanation_list[['empirical_group_within_shapr']] <- explain(x_test_onehot_reduced, explainer1_onehot, approach = "empirical", prediction_zero = p)

print(head(explanation_list[['empirical_group_within_shapr']]$dt))

#          none     group1     group2      group3
# 1: 0.02618352 0.05036850  0.3177426  0.19720785
# 2: 0.02618351 0.07164224  0.3308221  0.07578790
# 3: 0.02618351 0.10473223  0.3024959 -0.81820363
# 4: 0.02618351 0.08210048 -0.8112801  0.19464729
# 5: 0.02618350 0.09356286 -0.7932949  0.07813302
# 6: 0.02618351 0.07325078 -0.7949201 -0.78915746

explanation_list[['empirical_group_within_shapr']]['MAE'] <-  MAE(explanation_truth$dt,  explanation_list[['empirical_group_within_shapr']]$dt, weights = joint_prob_dt$joint_prob)
# 0.02294435

# -----------------------------

## GAUSSIAN
# 3.1. no grouping

explanation_list[['gaussian_after_shapr']] <- explain(
  x_test_onehot_reduced,
  approach = 'gaussian',
  explainer = explainer_onehot,
  prediction_zero = p,
  w_threshold = 1,
  n_samples = 1000)

beta_matcher <- as.numeric(getstr(reduced_onehot_names))
no_features <- max(beta_matcher)
phi_sum_mat <- matrix(NA, nrow = No_test_obs, ncol = no_features)

for (i in 1:no_features){
  phi_sum_mat[, i] <- rowSums(subset(explanation_list[['gaussian_after_shapr']]$dt, select = which(beta_matcher == i) + 1))
}
colnames(phi_sum_mat) <- feat_names
explanation_list[['gaussian_after_shapr']]$dt_sum <- cbind(explanation_list[['gaussian_after_shapr']]$dt[, 1], phi_sum_mat)

print(head(explanation_list[['gaussian_after_shapr']]$dt_sum))

#         none    feat_1_    feat_2_     feat_3_
# 1: 0.02618351 0.02625388  0.3232636  0.21580152
# 2: 0.02618353 0.08285312  0.3228103  0.07258888
# 3: 0.02618351 0.08017430  0.3222968 -0.81344667
# 4: 0.02618350 0.08527343 -0.8122251  0.19241940
# 5: 0.02618352 0.09803206 -0.8108939  0.09126286
# 6: 0.02618350 0.10668359 -0.7948851 -0.82262532

explanation_list[['gaussian_after_shapr']]['MAE'] <-  MAE(explanation_truth$dt,  explanation_list[['gaussian_after_shapr']]$dt_sum, weights = joint_prob_dt$joint_prob)
# 0.0263573


# 3.2 use new grouping function in shapr()
group1_onehot_names <- group_factors(x_train)

explainer1_onehot <- shapr(x_train_onehot_reduced, model_onehot, group = group1_onehot_names)

explanation_list[['gaussian_group_within_shapr']] <- explain(x_test_onehot_reduced, explainer1_onehot, approach = "gaussian", prediction_zero = p)

print(head(explanation_list[['gaussian_group_within_shapr']]$dt))
#          none     group1     group2      group3
# 1: 0.02618352 0.03973925  0.3150407  0.21053904
# 2: 0.02618351 0.06546241  0.3499328  0.06285701
# 3: 0.02618351 0.05535990  0.3231187 -0.78945418
# 4: 0.02618352 0.05434157 -0.8030133  0.21413945
# 5: 0.02618351 0.09975965 -0.8013245  0.07996579
# 6: 0.02618350 0.09063675 -0.8087633 -0.79270020

explanation_list[['gaussian_group_within_shapr']]['MAE'] <-  MAE(explanation_truth$dt,  explanation_list[['gaussian_group_within_shapr']]$dt, weights = joint_prob_dt$joint_prob)
# 0.02816679



## We want to create a function to figure out the groupings so that we group the one-hot encoded variables and not the continuous variables
group_factors <- function(data) {
  group_names_list <- list()
  is_factor <- sapply(data, is.factor)
  factor_variables <- data[, is_factor, with = FALSE]
  dummylist <- make_dummies(data = data)
  k <- 1

  for(i in 1:ncol(data)) {
    if (is_factor[i] == FALSE) {
      group_names_list[[i]] <- colnames(data)[i]

    } else {
      lvs <- levels(factor_variables[[k]])

      group_names_list[[i]] <- paste0(colnames(factor_variables)[k], lvs[-1])
      k <- k + 1
    }
  }
  return(group_names_list)
}




