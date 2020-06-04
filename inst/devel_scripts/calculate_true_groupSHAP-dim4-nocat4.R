
library(MASS)
library(data.table)
library(shapr)

source("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/shapr/inst/devel_scripts/3-calculate_true_shapley_withdatatable.R")

# parameters
dim <- 4
mu <- rep(0, dim)
no_categories <- 4
cutoff <- c(-200, -0.5, 0, 1, 200)
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
methods <- c("empirical", "gaussian", "ctree")
seed <- 1
corr <- c(0, 0.1, 0.3, 0.5, 0.8, 0.9)
Sigma_diag <- 1


## START
cc <- 0.5 # this is the correlation - can loop over this later

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
#dt_numeric[, response := dt[['response']]]


## 4. Fit model
if(fit_mod == 'regression'){
  form <- as.formula(paste0("response ~", paste(feat_names, collapse = "+")))
  model <- lm(formula = form, data = dt[(1:No_train_obs), ])

  fmla_onehot <- as.formula(paste("response ~", paste(reduced_onehot_names, collapse = " + ")))
  model_onehot <- lm(fmla_onehot, data = dt[(1:No_train_obs)])
}

## 5. initalize shapr object with trained model -- this is used for calculating true shapley
x_train <- dt[(1:No_train_obs), ..feat_names]
x_test <- dt[-(1:No_train_obs), ..feat_names]
y_train <- dt[(1:No_train_obs), .(response)]

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
## End custom function

## Start grouping stuff
group1 <- list(c(1), # no groups at all
               c(2),
               c(3),
               c(4))
group1_names = lapply(group1, function(x){names(x_test)[x]})
explainer_group1 <- shapr(x_train, model, group = group1_names)

#group2 <- list(c(1, 3, 4), c(2))
#group2 <- list(c(4, 3), c(2, 1))
group2 <- list(c(1, 3), c(2, 4))

group2_names = lapply(group2, function(x){names(x_test)[x]})
explainer_group2 <- shapr(x_train, model, group = group2_names)

# no grouping - used as a test
set.seed(10) # even if you set a seed, joint_prob_dt_list[[2]] will be different! Ask Martin? But only for corr > 0
joint_prob_dt_list <- create_exact_joint_prob(mu, Sigma, beta, explainer, cutoff, response_mod,
                                              algorithm = mvtnorm::Miwa())
marg_list <- marg_prob(joint_prob_dt_list[[1]], explainer)
cond_list0 <- cond_prob(marg_list,
                        joint_prob_dt = joint_prob_dt_list[[1]],
                        explainer,
                        model)

cond_expec_mat0 <- cond_expec_new(cond_list = cond_list0,
                                  explainer,
                                  x_test,
                                  prediction_zero = joint_prob_dt_list[[2]],
                                  joint_prob_dt = joint_prob_dt_list[[1]])
head(cond_expec_mat0)
# corr = 0
#             0          1          2          3          4           5          6          7          8          9         10
# 1: -0.1468753 -0.4490456  0.3702131 -0.6832946 -1.8906574  0.06803643 -0.9854712 -2.1928340 -0.1662125 -1.3735753 -2.4270829
# 2: -0.1468753 -0.3495479  0.2702243 -0.9826847  0.3369416  0.06754548 -1.1853636  0.1342628 -0.5655914  0.7540350 -0.4988741
# 3: -0.1468753 -0.3495479 -1.1299076  1.1175701  1.4188691 -1.33258645  0.9148912  1.2161903  0.1345315  0.4358305  2.6833082
# 4: -0.1468753 -1.4491717  0.3702131  0.0172506  0.3262157 -0.93208967 -1.2850521 -0.9760870  0.5343327  0.8432978  0.4903353
# 5: -0.1468753  0.9509390  0.2702243  1.1175701 -1.8906574  1.36803230  2.2153781 -0.7928494  1.5346634 -1.4735640 -0.6262183
# 6: -0.1468753 -0.4490456  0.3702131  1.1175701  0.3262157  0.06803643  0.8153935  0.0240391  1.6346521  0.8432978  1.5906548
#             11         12         13          14         15
# 1: -0.46838917 -1.6757519 -2.7292596 -1.91000089 -2.2121775
# 2: -0.76827022  0.5513561 -0.7015529 -0.08178074 -0.2844596
# 3: -0.06814736  0.2331517  2.4806294  1.70026963  1.4975908
# 4: -0.76797007 -0.4590049 -0.8119674  1.00741739 -0.2948853
# 5:  2.63247140 -0.3757560  0.4715897 -0.20912492  0.8886831
# 6:  1.33247552  0.5411212  1.2884782  2.10773688  1.8055603

Kshap0 <- matrix(0, nrow = nrow(x_test), ncol = nrow(explainer$W))
for (i in 1:nrow(x_test)) {
  Kshap0[i, ] = explainer$W %*% t(as.matrix(cond_expec_mat0[i, ]))
}
Kshap0 <- data.table(Kshap0)
dim <- ncol(x_test)
setnames(Kshap0, 1:(dim + 1), c("none", names(x_test)))

print(head(Kshap0)) # should be the same as no grouping
# corr = 0
#         none    feat_1_    feat_2_    feat_3_    feat_4_
# 1: -0.1468753 -0.3021751  0.5170836 -0.5364240 -1.7437868
# 2: -0.1468753 -0.2026773  0.4170949 -0.8358141  0.4838122
# 3: -0.1468753 -0.2026773 -0.9830370  1.2644407  1.5657397
# 4: -0.1468753 -1.3023011  0.5170836  0.1641212  0.4730863
# 5: -0.1468753  1.0978096  0.4170949  1.2644407 -1.7437868
# 6: -0.1468753 -0.3021751  0.5170836  1.2644407  0.4730863

# corr = 0.5
print(head(Kshap0)) # should be the same as no grouping
#         none    feat_1_    feat_2_    feat_3_    feat_4_
# 1: -0.1468755 -0.5116213  1.1272939 -0.9344149 -1.7236740
# 2: -0.1468751 -0.2006050  0.2048291 -0.3590274  0.1804335
# 3: -0.1468753  0.2431319 -1.3161966  1.1969677  1.5229784
# 4: -0.1468752 -1.4167006  0.5595822  0.2818591  0.4180815
# 5: -0.1468753  1.2656856  0.7001542  1.2078242 -2.1289119
# 6: -0.1468753 -0.6650569  0.7649510  1.1830748  0.6652929

# no ACTUAL grouping but use groups - used as a test
set.seed(10)
joint_prob_dt_list <- create_exact_joint_prob(mu, Sigma, beta, explainer, cutoff, response_mod,
                                              algorithm = mvtnorm::Miwa())
marg_list <- marg_prob(joint_prob_dt_list[[1]], explainer)
cond_list1 <- cond_prob(marg_list,
                        joint_prob_dt = joint_prob_dt_list[[1]],
                        explainer,
                        model,
                        group = group1_names)
cond_expec_mat1 <- cond_expec_new(cond_list = cond_list1, # no predict
                                  explainer,
                                  x_test,
                                  prediction_zero = joint_prob_dt_list[[2]],
                                  joint_prob_dt = joint_prob_dt_list[[1]], # has predict
                                  group_names = explainer_group1$X$id_combination)

Kshap1 <- matrix(0, nrow = nrow(x_test), ncol = nrow(explainer_group1$W))
for (i in 1:nrow(x_test)) {
  Kshap1[i, ] = explainer_group1$W %*% t(as.matrix(cond_expec_mat1[i, ]))
}
Kshap1 <- data.table(Kshap1)
setnames(Kshap1, 1:(length(group1_names)+1), c("none", paste0("group", 1:length(group1_names))))

## corr = 0 -------------------------------------------
print(head(Kshap1)) # SAME as no grouping
# none     group1     group2     group3     group4
# 1: -0.1468753 -0.3021751  0.5170836 -0.5364240 -1.7437868
# 2: -0.1468753 -0.2026773  0.4170949 -0.8358141  0.4838122
# 3: -0.1468753 -0.2026773 -0.9830370  1.2644407  1.5657397
# 4: -0.1468753 -1.3023011  0.5170836  0.1641212  0.4730863
# 5: -0.1468753  1.0978096  0.4170949  1.2644407 -1.7437868
# 6: -0.1468753 -0.3021751  0.5170836  1.2644407  0.4730863

# grouping: (feat1, feat3), (feat2, feat4)
head(cbind(Kshap0[,1], Kshap0[,2] + Kshap0[,4], Kshap0[,3] + Kshap0[,5]))

#         none    feat_1_    feat_2_
# 1: -0.1468753 -0.8385991 -1.2267031
# 2: -0.1468753 -1.0384914  0.9009071
# 3: -0.1468753  1.0617634  0.5827027
# 4: -0.1468753 -1.1381800  0.9901700
# 5: -0.1468753  2.3622502 -1.3266919
# 6: -0.1468753  0.9622656  0.9901700

## corr = 0.5 -------------------------------------------
print(head(Kshap1))
#         none     group1     group2     group3     group4
# 1: -0.1468755 -0.5116213  1.1272939 -0.9344149 -1.7236740
# 2: -0.1468751 -0.2006050  0.2048291 -0.3590274  0.1804335
# 3: -0.1468753  0.2431319 -1.3161966  1.1969677  1.5229784
# 4: -0.1468752 -1.4167006  0.5595822  0.2818591  0.4180815
# 5: -0.1468753  1.2656856  0.7001542  1.2078242 -2.1289119
# 6: -0.1468753 -0.6650569  0.7649510  1.1830748  0.6652929

# grouping: (feat1, feat3), (feat2, feat4)
head(cbind(Kshap0[,1], Kshap0[,2] + Kshap0[,4], Kshap0[,3] + Kshap0[,5]))
#         none    feat_1_    feat_2_
# 1: -0.1468755 -1.4460361 -0.5963800
# 2: -0.1468751 -0.5596324  0.3852626
# 3: -0.1468753  1.4400996  0.2067818
# 4: -0.1468752 -1.1348416  0.9776636
# 5: -0.1468753  2.4735097 -1.4287577
# 6: -0.1468753  0.5180179  1.4302439


set.seed(10)
joint_prob_dt_list <- create_exact_joint_prob(mu, Sigma, beta, explainer, cutoff, response_mod,
                                              algorithm = mvtnorm::Miwa())
marg_list <- marg_prob(joint_prob_dt_list[[1]], explainer)
cond_list2 <- cond_prob(marg_list,
                        joint_prob_dt = joint_prob_dt_list[[1]],
                        explainer,
                        model,
                        group = group2_names)
cond_expec_mat2 <- cond_expec_new(cond_list = cond_list2,
                                  explainer,
                                  x_test,
                                  prediction_zero = joint_prob_dt_list[[2]],
                                  joint_prob_dt = joint_prob_dt_list[[1]],
                                  group_names = explainer_group2$X$id_combination)

Kshap2 <- matrix(0, nrow = nrow(x_test), ncol = nrow(explainer_group2$W))
for (i in 1:nrow(x_test)) {
  Kshap2[i, ] = explainer_group2$W %*% t(as.matrix(cond_expec_mat2[i, ]))
}
Kshap2 <- data.table(Kshap2)
setnames(Kshap2, 1:(length(group2_names)+1), c("none", paste0("group", 1:length(group2_names))))

## corr = 0 -------------------------------------------

print(head(Kshap2)) # - grouping: (feat1, feat3), (feat2, feat4)
#         none     group1     group2
# 1: -0.1468753 -0.8385991 -1.2267031
# 2: -0.1468753 -1.0384914  0.9009071
# 3: -0.1468753  1.0617634  0.5827027
# 4: -0.1468753 -1.1381800  0.9901700
# 5: -0.1468753  2.3622502 -1.3266919
# 6: -0.1468753  0.9622656  0.9901700
mean(rowMeans(abs(cbind(Kshap0[,1], Kshap0[,2] + Kshap0[,4], Kshap0[,3]+ Kshap0[,5]) - Kshap2)[,-1]))
#1.811412e-10

## corr = 0.5 -------------------------------------------

print(head(Kshap2))  # - grouping: (feat1, feat3), (feat2, feat4)
#         none     group1     group2
# 1: -0.1468756 -1.4675892 -0.5748270
# 2: -0.1468751 -0.5402286  0.3658589
# 3: -0.1468752  1.4195141  0.2273673
# 4: -0.1468752 -1.1097917  0.9526137
# 5: -0.1468752  2.4031340 -1.3583819
# 6: -0.1468754  0.5861328  1.3621290
>

  mean(rowMeans(abs(cbind(Kshap0[,1], Kshap0[,2] + Kshap0[,4], Kshap0[,3]+ Kshap0[,5]) - Kshap2)[,-1]))
# 0.05530471

