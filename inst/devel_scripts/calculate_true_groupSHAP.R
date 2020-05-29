
library(MASS)
library(data.table)
library(shapr)

source("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/shapr/inst/devel_scripts/3-calculate_true_shapley_withdatatable.R")

# parameters
dim <- 3
mu <- rep(0, dim)
no_categories <- 3
cutoff <- c(-200, 0, 1, 200)
no_categories <- length(cutoff) - 1
set.seed(1); beta <- round(rnorm(dim * no_categories + 1), 1)
Sample_test <- TRUE
No_train_obs <- 1000
No_test_sample <- 100
N_sample_gaussian <- 1000
noise <- TRUE
response_mod <- function(mod_matrix_full, beta, epsilon){
  as.vector(mod_matrix_full %*% beta) + epsilon
}
fit_mod <- "regression"
methods <- c("empirical", "gaussian")
seed <- 1
corr <- c(0, 0.1, 0.3, 0.5, 0.8, 0.9)[1:3]
Sigma_diag <- 1
x_test_dt <-NULL

## START
cc <- 0 # this is the correlation - can loop over this later

Sigma <- matrix(rep(cc, dim^2), nrow = dim, ncol = dim)
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
setnames(dt, names(dt), paste0("feat_", 1:dim, "_"))
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

explainer <- shapr(x_train, model)
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
## End custom function

## Start grouping stuff
group1 <- list(c(1), # now groups at all
               c(2),
               c(3))
group1_names = lapply(group1, function(x){names(x_test)[x]})
explainer_group1 <- shapr(x_train, model, group = group1_names)

group2 <- list(c(1, 2), # two groups: group1 and group2
               c(3))
group2_names = lapply(group2, function(x){names(x_test)[x]})
explainer_group2 <- shapr(x_train, model, group = group2_names)

## 6. calculate the true shapley values
print("Started calculating true Shapley values.", quote = FALSE, right = FALSE)

set.seed(10)
joint_prob_dt_list <- create_exact_joint_prob(mu, Sigma, beta, explainer, cutoff, response_mod)
marg_list <- marg_prob(joint_prob_dt_list[[1]], explainer)
# no grouping - used as a test
cond_list1<- cond_prob(marg_list,
                       joint_prob_dt = joint_prob_dt_list[[1]],
                       explainer,
                       model,
                       group = group1_names)
cond_expec_mat1 <- cond_expec_new(cond_list1,
                                 explainer,
                                 x_test,
                                 prediction_zero = joint_prob_dt_list[[2]],
                                 joint_prob_dt = joint_prob_dt_list[[1]],
                                 group_names = explainer_group1$X$id_combination)

head(cond_expec_mat1)
#             1          2          3           4          5          6          7          8
# 1: -0.05576529 0.06322219  0.2880212  0.12896896  0.4068885  0.2478362  0.4726352  0.5915025
# 2: -0.05576529 0.06322219  0.2880212  0.04190225  0.4068885  0.1607695  0.3855685  0.5044358
# 3: -0.05576529 0.06322219  0.2880212 -0.84732554  0.4068885 -0.7284583 -0.5036593 -0.3847920
# 4: -0.05576529 0.06322219 -0.8118300  0.12896896 -0.6929628  0.2478362 -0.6272160 -0.5083488
# 5: -0.05576529 0.06322219 -0.8118300  0.04190225 -0.6929628  0.1607695 -0.7142827 -0.5954155
# 6: -0.05576529 0.06322219 -0.8118300 -0.84732554 -0.6929628 -0.7284583 -1.6035105 -1.4846433

Kshap1 <- matrix(0, nrow = nrow(x_test), ncol = nrow(explainer_group1$W))
for (i in 1:nrow(x_test)) {
  Kshap1[i, ] = explainer_group1$W %*% t(as.matrix(cond_expec_mat1[i, ]))
}
Kshap1 <- data.table(Kshap1)
setnames(Kshap1, 1:(length(group1_names)+1), c("none", paste0("group", 1:length(group1_names))))

print(head(Kshap1)) # should be the same as no grouping
#           none    group1     group2      group3
# 1: -0.05576529 0.1189073  0.3437063  0.18465409
# 2: -0.05576529 0.1189073  0.3437063  0.09758738
# 3: -0.05576529 0.1189073  0.3437063 -0.79164041
# 4: -0.05576529 0.1189073 -0.7561449  0.18465409
# 5: -0.05576529 0.1189073 -0.7561449  0.09758738
# 6: -0.05576529 0.1189073 -0.7561449 -0.79164041

cond_list2 <- cond_prob(marg_list, # with grouping
                       joint_prob_dt = joint_prob_dt_list[[1]],
                       explainer,
                       model,
                       group = group2_names)
cond_expec_mat2 <- cond_expec_new(cond_list2,
                                  explainer,
                                  x_test,
                                  prediction_zero = joint_prob_dt_list[[2]],
                                  joint_prob_dt = joint_prob_dt_list[[1]],
                                  group_names = explainer_group2$X$id_combination)

head(cond_expec_mat2) # 1 = conditioning on nothing, 2 = conditioning on group 1, 3 = conditioning on group 2
# 4 = conditioning on group 1 and group 2

#             1           2          3          4
# 1: -0.05576529  0.12896896  0.4068885  0.5915025
# 2: -0.05576529  0.04190225  0.4068885  0.5044358
# 3: -0.05576529 -0.84732554  0.4068885 -0.3847920
# 4: -0.05576529  0.12896896 -0.6929628 -0.5083488
# 5: -0.05576529  0.04190225 -0.6929628 -0.5954155
# 6: -0.05576529 -0.84732554 -0.6929628 -1.4846433

Kshap2 <- matrix(0, nrow = nrow(x_test), ncol = nrow(explainer_group2$W))
for (i in 1:nrow(x_test)) {
  Kshap2[i, ] = explainer_group2$W %*% t(as.matrix(cond_expec_mat2[i, ]))
}
Kshap2 <- data.table(Kshap2)
setnames(Kshap2, 1:(length(group2_names)+1), c("none", paste0("group", 1:length(group2_names))))

print(head(Kshap2))
#           none      group1     group2
# 1: -0.05576529  0.18467413  0.4625936
# 2: -0.05576529  0.09760742  0.4625936
# 3: -0.05576529 -0.79162037  0.4625936
# 4: -0.05576529  0.18467413 -0.6372576
# 5: -0.05576529  0.09760742 -0.6372576
# 6: -0.05576529 -0.79162037 -0.6372576




## 8. calculate approximate shapley values with different methods
p <- mean(y_train$response)

print("Started estimating Shapley values with various methods.", quote = FALSE, right = FALSE)
explanation_list <- list()

## EMPIRICAL
beta_matcher <- as.numeric(getstr(reduced_onehot_names))
no_features <- max(beta_matcher)
phi_sum_mat <- matrix(NA, nrow = No_test_obs, ncol = no_features)

tm0 <- proc.time()
explanation_list[['empirical']] <- explain(
  x_test_onehot_reduced,
  approach = 'empirical',
  explainer = explainer_onehot,
  prediction_zero = p,
  sample = FALSE,
  w_threshold = 1,
  n_samples = 1000)
tm1 <- proc.time()

# 1. no groupings
for (i in 1:no_features){
  phi_sum_mat[, i] <- rowSums(subset(explanation_list[['empirical']]$dt, select = which(beta_matcher == i) + 1))
}
colnames(phi_sum_mat) <- feat_names
explanation_list[['empirical']]$dt_sum <- cbind(explanation_list[['empirical']]$dt[, 1], phi_sum_mat)

print(head(explanation_list[['empirical']]$dt_sum))
#         none    feat_1_    feat_2_     feat_3_
# 1: 0.02618351 0.05110211  0.3219652  0.19225162
# 2: 0.02618353 0.06494674  0.3280420  0.08526356
# 3: 0.02618351 0.10099959  0.3108747 -0.82284984
# 4: 0.02618350 0.08536755 -0.8078294  0.18792953
# 5: 0.02618352 0.09264480 -0.8014691  0.08722524
# 6: 0.02618350 0.09041001 -0.7952708 -0.80596598

mean(colSums((abs(Kshap1 - explanation_list[['empirical']]$dt_sum))* joint_prob_dt_list[[1]]$joint_prob)[-1])
# 0.03084034 # same as paper


# 2. grouping after explain()
beta_matcher <- c(1, 1, 2)
no_features <- max(beta_matcher)
phi_sum_mat <- matrix(NA, nrow = No_test_obs, ncol = no_features)

for (i in 1:no_features){
  phi_sum_mat[, i] <- rowSums(subset(explanation_list[['empirical']]$dt_sum, select = which(beta_matcher == i) + 1))
}
colnames(phi_sum_mat) <- c("group1", "group2")
explanation_list[['empirical']]$dt_sum_group <- cbind(explanation_list[['empirical']]$dt[, 1], phi_sum_mat)

mean(colSums((abs(Kshap2 - explanation_list[['empirical']]$dt_sum_group))* joint_prob_dt_list[[1]]$joint_prob)[-1])
#0.8489 # a lot worse than before the grouping -- does this have to do with weights i.e joint_prob_dt_list[[1]]$joint_prob ??


# 3. use new grouping function in shapr()
group1_onehot <- list(c(1,2,3,4),
                      c(5,6))
group1_onehot_names = lapply(group1_onehot, function(x){names(x_train_onehot_reduced)[x]})

explainer1_onehot <- shapr(x_train_onehot_reduced, model_onehot, group = group1_onehot_names)

explanation1_onehot <- explain(x_test_onehot_reduced, explainer1_onehot, approach = "empirical", prediction_zero = p)


print(head(explanation1_onehot$dt))
mean(colSums((abs(Kshap2 - explanation1_onehot$dt))* joint_prob_dt_list[[1]]$joint_prob)[-1])
#0.847 # almost the same as 2.

# -----------------------------

## GAUSSIAN

tm0 <- proc.time()
explanation_list[['gaussian']] <- explain(
  x_test_onehot_reduced,
  approach = 'gaussian',
  explainer = explainer_onehot,
  prediction_zero = p,
  sample = FALSE,
  w_threshold = 1,
  n_samples = 1000)
tm1 <- proc.time()

# 1. no grouping

beta_matcher <- as.numeric(getstr(reduced_onehot_names))
no_features <- max(beta_matcher)
phi_sum_mat <- matrix(NA, nrow = No_test_obs, ncol = no_features)

for (i in 1:no_features){
  phi_sum_mat[, i] <- rowSums(subset(explanation_list[['gaussian']]$dt, select = which(beta_matcher == i) + 1))
}
colnames(phi_sum_mat) <- feat_names
explanation_list[['gaussian']]$dt_sum <- cbind(explanation_list[['gaussian']]$dt[, 1], phi_sum_mat)

print(head(explanation_list[['gaussian']]$dt_sum))

#         none    feat_1_    feat_2_     feat_3_
# 1: 0.02618351 0.02625388  0.3232636  0.21580152
# 2: 0.02618353 0.08285312  0.3228103  0.07258888
# 3: 0.02618351 0.08017430  0.3222968 -0.81344667
# 4: 0.02618350 0.08527343 -0.8122251  0.19241940
# 5: 0.02618352 0.09803206 -0.8108939  0.09126286
# 6: 0.02618350 0.10668359 -0.7948851 -0.82262532

mean(colSums((abs(Kshap1 - explanation_list[['gaussian']]$dt_sum))* joint_prob_dt_list[[1]]$joint_prob)[-1])
# 0.03361 # same as paper

# 2. grouping after
beta_matcher <- c(1, 1, 2)
no_features <- max(beta_matcher)
phi_sum_mat <- matrix(NA, nrow = No_test_obs, ncol = no_features)

for (i in 1:no_features){
  phi_sum_mat[, i] <- rowSums(subset(explanation_list[['gaussian']]$dt_sum, select = which(beta_matcher == i) + 1))
}
colnames(phi_sum_mat) <- c("group1", "group2")
explanation_list[['gaussian']]$dt_sum_group <- cbind(explanation_list[['gaussian']]$dt[, 1], phi_sum_mat)

print(head(explanation_list[['gaussian']]$dt_sum_group))


mean(colSums((abs(Kshap2 - explanation_list[['gaussian']]$dt_sum_group))* joint_prob_dt_list[[1]]$joint_prob)[-1])
# 0.0849

# 3. use new grouping function in shapr()
group1_onehot <- list(c(1,2,3,4),
                      c(5,6))
group1_onehot_names = lapply(group1_onehot, function(x){names(x_train_onehot_reduced)[x]})

explainer1_onehot <- shapr(x_train_onehot_reduced, model_onehot, group = group1_onehot_names)

explanation1_onehot <- explain(x_test_onehot_reduced, explainer1_onehot, approach = "gaussian", prediction_zero = p)

mean(colSums((abs(Kshap2 - explanation1_onehot$dt))* joint_prob_dt_list[[1]]$joint_prob)[-1])
# 0.854

