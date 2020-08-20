
library(MASS)
library(data.table)
library(shapr)

source("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/shapr/inst/devel_scripts/3-calculate_true_shapley_withdatatable.R")

# parameters
dim <- 4
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
methods <- c("empirical", "gaussian", "ctree")
seed <- 1
corr <- c(0, 0.1, 0.3, 0.5, 0.8, 0.9)
Sigma_diag <- 1


## START
cc <- 0.8 # this is the correlation - can loop over this later

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
dt_numeric[, response := dt[['response']]]


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

## 6. calculate the true shapley values
print("Started calculating true Shapley values.", quote = FALSE, right = FALSE)

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
#           0         1        2         3          4        5         6          7        8
# 1: 0.7355795 0.8549079 1.079092 0.9284089  1.4361805 1.198297 1.0476142  1.5553858 1.271798
# 2: 0.7355795 0.8549079 1.079092 0.9284089  0.3491174 1.198297 1.0476142  0.4683228 1.271798
# 3: 0.7355795 0.8549079 1.079092 0.9284089 -0.6401114 1.198297 1.0476142 -0.5209061 1.271798
# 4: 0.7355795 0.8549079 1.079092 0.8285637  1.4361805 1.198297 0.9477691  1.5553858 1.171953
# 5: 0.7355795 0.8549079 1.079092 0.8285637  0.3491174 1.198297 0.9477691  0.4683228 1.171953
# 6: 0.7355795 0.8549079 1.079092 0.8285637 -0.6401114 1.198297 0.9477691 -0.5209061 1.171953
#             9         10       11         12         13         14          15
# 1:  1.7795697  1.6288867 1.391003  1.8987751  1.7480921  1.9722760  2.09148135
# 2:  0.6925067  0.5418237 1.391003  0.8117120  0.6610291  0.8852130  1.00441833
# 3: -0.2967221 -0.4474051 1.391003 -0.1775168 -0.3281998 -0.1040158  0.01518949
# 4:  1.7795697  1.5290416 1.291158  1.8987751  1.6482469  1.8724308  1.99163618
# 5:  0.6925067  0.4419786 1.291158  0.8117120  0.5611839  0.7853678  0.90457317
# 6: -0.2967221 -0.5472503 1.291158 -0.1775168 -0.4280449 -0.2038610 -0.08465567

Kshap0 <- matrix(0, nrow = nrow(x_test), ncol = nrow(explainer$W))
for (i in 1:nrow(x_test)) {
  Kshap0[i, ] = explainer$W %*% t(as.matrix(cond_expec_mat0[i, ]))
}
Kshap0 <- data.table(Kshap0)
dim <- ncol(x_test)
setnames(Kshap0, 1:(dim + 1), c("none", names(x_test)))

print(head(Kshap0)) # should be the same as no grouping
# corr = 0
#         none   feat_1_   feat_2_    feat_3_    feat_4_
# 1: 0.7355795 0.1192361 0.3434201 0.19273707  0.7005086
# 2: 0.7355795 0.1192361 0.3434201 0.19273707 -0.3865544
# 3: 0.7355795 0.1192361 0.3434201 0.19273707 -1.3757832
# 4: 0.7355795 0.1192361 0.3434201 0.09289191  0.7005086
# 5: 0.7355795 0.1192361 0.3434201 0.09289191 -0.3865544
# 6: 0.7355795 0.1192361 0.3434201 0.09289191 -1.3757832

# corr = 0.1 - Problem: can't repeat these results unless we use algorithm = mvtnorm::Miwa()
#         none   feat_1_   feat_2_    feat_3_    feat_4_
# 1: 0.7355795 0.1791839 0.3603415 0.19326213  0.6468375
# 2: 0.7355795 0.1729397 0.3697942 0.20608147 -0.4750280
# 3: 0.7355795 0.1645418 0.3756562 0.21755494 -1.4891152
# 4: 0.7355795 0.1733494 0.3702039 0.04564755  0.6846352
# 5: 0.7355795 0.1649699 0.3780621 0.06033181 -0.4353655
# 6: 0.7355795 0.1545425 0.3826554 0.07345432 -1.4478036

# corr = 0.5
#         none   feat_1_   feat_2_     feat_3_    feat_4_
# 1: 0.7355388 0.3133295 0.3824454  0.20772250  0.4729361
# 2: 0.7355388 0.3465754 0.4529450  0.23701919 -0.7652436
# 3: 0.7355388 0.3135667 0.4671210  0.25388861 -1.7672235
# 4: 0.7355388 0.3547737 0.4611870 -0.15381067  0.6094585
# 5: 0.7355387 0.3564310 0.5073055 -0.09652914 -0.6007363
# 6: 0.7355387 0.2899876 0.4972108 -0.05080710 -1.5738636

# corr = 0.8
#         none   feat_1_   feat_2_    feat_3_    feat_4_
# 1: 0.7355797 0.3533619 0.3732779  0.2423353  0.3852091
# 2: 0.7355796 0.4898304 0.5352099  0.2771869 -1.0439855
# 3: 0.7355796 0.4462459 0.5428595  0.2224822 -1.9508385
# 4: 0.7355795 0.5113042 0.5566837 -0.4008418  0.5908912
# 5: 0.7355795 0.5915933 0.6711536 -0.3141696 -0.7864827
# 6: 0.7355795 0.4480147 0.5961663 -0.2775587 -1.6020202

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
#         none    group1    group2     group3     group4
# 1: 0.7355795 0.1192361 0.3434201 0.19273707  0.7005086
# 2: 0.7355795 0.1192361 0.3434201 0.19273707 -0.3865544
# 3: 0.7355795 0.1192361 0.3434201 0.19273707 -1.3757832
# 4: 0.7355795 0.1192361 0.3434201 0.09289191  0.7005086
# 5: 0.7355795 0.1192361 0.3434201 0.09289191 -0.3865544
# 6: 0.7355795 0.1192361 0.3434201 0.09289191 -1.3757832

# grouping: (feat1, feat3), (feat2, feat4)
head(cbind(Kshap0[,1], Kshap0[,2] + Kshap0[,4], Kshap0[,3] + Kshap0[,5]))

# none   feat_1_     feat_2_
# 1: 0.7355795 0.3119732  1.04392870
# 2: 0.7355795 0.3119732 -0.04313432
# 3: 0.7355795 0.3119732 -1.03236316
# 4: 0.7355795 0.2121280  1.04392870
# 5: 0.7355795 0.2121280 -0.04313432
# 6: 0.7355795 0.2121280 -1.03236316

# grouping: (feat4, feat3), (feat2, feat1)
head(cbind(Kshap0[,1], Kshap0[,5]+Kshap0[,4], Kshap0[,2]+Kshap0[,3]))
#         none    feat_4_   feat_1_
# 1: 0.7355795  0.8932457 0.4626562
# 2: 0.7355795 -0.1938173 0.4626562
# 3: 0.7355795 -1.1830461 0.4626562
# 4: 0.7355795  0.7934006 0.4626562
# 5: 0.7355795 -0.2936625 0.4626562
# 6: 0.7355795 -1.2828913 0.4626562

# grouping: (feat4, feat3, feat1), (feat2)
head(cbind(Kshap0[,1], Kshap0[,5]+Kshap0[,4] +Kshap0[,2], Kshap0[,3]))
#         none     feat_4_   feat_2_
# 1: 0.7355795  1.01248184 0.3434201
# 2: 0.7355795 -0.07458118 0.3434201
# 3: 0.7355795 -1.06381002 0.3434201
# 4: 0.7355795  0.91263667 0.3434201
# 5: 0.7355795 -0.17442634 0.3434201
# 6: 0.7355795 -1.16365518 0.3434201

## corr = 0.1 -------------------------------------------
print(head(Kshap1)) #- not the same as above??
#         none    group1    group2     group3     group4
# 1: 0.7355795 0.1791839 0.3603415 0.19326213  0.6468375
# 2: 0.7355795 0.1729397 0.3697942 0.20608147 -0.4750280
# 3: 0.7355795 0.1645418 0.3756562 0.21755494 -1.4891152
# 4: 0.7355795 0.1733494 0.3702039 0.04564755  0.6846352
# 5: 0.7355795 0.1649699 0.3780621 0.06033181 -0.4353655
# 6: 0.7355795 0.1545425 0.3826554 0.07345432 -1.4478036

# grouping: (feat4, feat3, feat1), (feat2)
head(cbind(Kshap0[,1], Kshap0[,5] + Kshap0[,4] + Kshap0[,2], Kshap0[,3]))
#         none     feat_4_   feat_2_
# 1: 0.7355795  1.01928360 0.3603415
# 2: 0.7355795 -0.09600679 0.3697942
# 3: 0.7355795 -1.10701842 0.3756562
# 4: 0.7355795  0.90363207 0.3702039
# 5: 0.7355795 -0.21006376 0.3780621
# 6: 0.7355795 -1.21980675 0.3826554

# grouping: (feat4, feat3), (feat2, feat1)
head(cbind(Kshap0[,1], Kshap0[,5] + Kshap0[,4], Kshap0[,2] + Kshap0[,3]))
#         none    feat_4_   feat_1_
# 1: 0.7355795  0.8400997 0.5395254
# 2: 0.7355795 -0.2689465 0.5427339
# 3: 0.7355795 -1.2715602 0.5401980
# 4: 0.7355795  0.7302827 0.5435533
# 5: 0.7355795 -0.3750336 0.5430319
# 6: 0.7355795 -1.3743493 0.5371980

# grouping: (feat1, feat3), (feat2, feat4)
head(cbind(Kshap0[,1], Kshap0[,2] + Kshap0[,4], Kshap0[,3] + Kshap0[,5]))
#         none   feat_1_    feat_2_
# 1: 0.7355795 0.3724461  1.0071790
# 2: 0.7355795 0.3790212 -0.1052338
# 3: 0.7355795 0.3820967 -1.1134589
# 4: 0.7355795 0.2189969  1.0548390
# 5: 0.7355795 0.2253017 -0.0573034
# 6: 0.7355795 0.2279968 -1.0651481

## corr = 0.8 -------------------------------------------
print(head(Kshap1))
#         none   feat_1_   feat_2_    feat_3_    feat_4_
# 1: 0.7355797 0.3533619 0.3732779  0.2423353  0.3852091
# 2: 0.7355796 0.4898304 0.5352099  0.2771869 -1.0439855
# 3: 0.7355796 0.4462459 0.5428595  0.2224822 -1.9508385
# 4: 0.7355795 0.5113042 0.5566837 -0.4008418  0.5908912
# 5: 0.7355795 0.5915933 0.6711536 -0.3141696 -0.7864827
# 6: 0.7355795 0.4480147 0.5961663 -0.2775587 -1.6020202

# grouping: (feat4, feat3, feat1), (feat2)
head(cbind(Kshap0[,1], Kshap0[,5] + Kshap0[,4] + Kshap0[,2], Kshap0[,3]))
#         none    feat_4_   feat_2_
# 1: 0.7355797  0.9809064 0.3732779
# 2: 0.7355796 -0.2769682 0.5352099
# 3: 0.7355796 -1.2821105 0.5428595
# 4: 0.7355795  0.7013536 0.5566837
# 5: 0.7355795 -0.5090589 0.6711536
# 6: 0.7355795 -1.4315642 0.5961663

# grouping: (feat4, feat3), (feat2, feat1)
head(cbind(Kshap0[,1], Kshap0[,5] + Kshap0[,4], Kshap0[,2] + Kshap0[,3]))
#         none    feat_4_   feat_1_
# 1: 0.7355797  0.6275444 0.7266398
# 2: 0.7355796 -0.7667986 1.0250403
# 3: 0.7355796 -1.7283564 0.9891054
# 4: 0.7355795  0.1900494 1.0679879
# 5: 0.7355795 -1.1006523 1.2627470
# 6: 0.7355795 -1.8795789 1.0441810

# grouping: (feat1, feat3), (feat2, feat4)
head(cbind(Kshap0[,1], Kshap0[,2] + Kshap0[,4], Kshap0[,3] + Kshap0[,5]))
#         none   feat_1_    feat_2_
# 1: 0.7355797 0.5956973  0.7584870
# 2: 0.7355796 0.7670173 -0.5087756
# 3: 0.7355796 0.6687280 -1.4079790
# 4: 0.7355795 0.1104624  1.1475749
# 5: 0.7355795 0.2774238 -0.1153291
# 6: 0.7355795 0.1704560 -1.0058539


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
print(head(Kshap2)) # - grouping: (feat4, feat3, feat1), (feat2)
#         none      group1    group2
# 1: 0.7355795  1.01245105 0.3434508
# 2: 0.7355795 -0.07461196 0.3434508
# 3: 0.7355795 -1.06384080 0.3434508
# 4: 0.7355795  0.91260589 0.3434508
# 5: 0.7355795 -0.17445712 0.3434508
# 6: 0.7355795 -1.16368596 0.3434508

mean(rowMeans(abs(cbind(Kshap0[,1], Kshap0[,5] + Kshap0[,4] + Kshap0[,2], Kshap0[,3]) - Kshap2)[,-1]))
# 3.078375e-05

print(head(Kshap2)) # - grouping: (feat4, feat3), (feat2, feat1)
#         none     group1    group2
# 1: 0.7355795  0.8932457 0.4626562
# 2: 0.7355795 -0.1938173 0.4626562
# 3: 0.7355795 -1.1830461 0.4626562
# 4: 0.7355795  0.7934006 0.4626562
# 5: 0.7355795 -0.2936625 0.4626562
# 6: 0.7355795 -1.2828913 0.4626562

mean(rowMeans(abs(cbind(Kshap0[,1], Kshap0[,5] + Kshap0[,4], Kshap0[,2]+ Kshap0[,3]) - Kshap2)[,-1]))
# 1.659631e-10

print(head(Kshap2)) # - grouping: (feat1, feat3), (feat2, feat4)
#         none    group1      group2
# 1: 0.7355795 0.3119732  1.04392870
# 2: 0.7355795 0.3119732 -0.04313432
# 3: 0.7355795 0.3119732 -1.03236316
# 4: 0.7355795 0.2121280  1.04392870
# 5: 0.7355795 0.2121280 -0.04313432
# 6: 0.7355795 0.2121280 -1.03236316

mean(rowMeans(abs(cbind(Kshap0[,1], Kshap0[,2] + Kshap0[,4], Kshap0[,3]+ Kshap0[,5]) - Kshap2)[,-1]))
# 1.655213e-10

## corr = 0.1 -------------------------------------------
print(head(Kshap2))  # - grouping: (feat4, feat3, feat1), (feat2)
#         none     group1    group2
# 1: 0.7355795  1.0172259 0.3623992
# 2: 0.7355795 -0.0989353 0.3727227
# 3: 0.7355795 -1.1108961 0.3795339
# 4: 0.7355795  0.9011132 0.3727227
# 5: 0.7355795 -0.2128235 0.3808218
# 6: 0.7355795 -1.2230798 0.3859285

mean(rowMeans(abs(cbind(Kshap0[,1], Kshap0[,5] + Kshap0[,4] + Kshap0[,2], Kshap0[,3]) - Kshap2)[,-1]))
# 0.002664183


print(head(Kshap2))  # - grouping: (feat4, feat3), (feat2, feat1)
#         none     group1    group2
# 1: 0.7355795  0.8380135 0.5416116
# 2: 0.7355795 -0.2703397 0.5441271
# 3: 0.7355795 -1.2721900 0.5408279
# 4: 0.7355795  0.7297088 0.5441271
# 5: 0.7355795 -0.3727148 0.5407131
# 6: 0.7355795 -1.3692212 0.5320699

mean(rowMeans(abs(cbind(Kshap0[,1], Kshap0[,5] + Kshap0[,4], Kshap0[,3]+ Kshap0[,2]) - Kshap2)[,-1]))
#  0.001921985

print(head(Kshap2))  # - grouping: (feat1, feat3), (feat2, feat4)
#         none    group1      group2
# 1: 0.7355795 0.3735805  1.00604456
# 2: 0.7355795 0.3808301 -0.10704265
# 3: 0.7355795 0.3846540 -1.11601614
# 4: 0.7355795 0.2183842  1.05545177
# 5: 0.7355795 0.2256337 -0.05763544
# 6: 0.7355795 0.2294576 -1.06660893

mean(rowMeans(abs(cbind(Kshap0[,1], Kshap0[,2] + Kshap0[,4], Kshap0[,3]+ Kshap0[,5]) - Kshap2)[,-1]))
# 0.001747845

## corr = 0.8 -------------------------------------------
print(head(Kshap2)) # - grouping: (feat4, feat3, feat1), (feat2)
#         none     group1    group2
# 1: 0.7355797  0.8479554 0.5062289
# 2: 0.7355796 -0.3865293 0.6447710
# 3: 0.7355795 -1.4998171 0.7605662
# 4: 0.7355796  0.6132663 0.6447710
# 5: 0.7355795 -0.5877921 0.7498868
# 6: 0.7355795 -1.6339073 0.7985094

# vs group after
head(cbind(Kshap0[,1], Kshap0[,5] + Kshap0[,4] + Kshap0[,2], Kshap0[,3]))
#         none     group4    group2
# 1: 0.7355797  0.9809064 0.3732779
# 2: 0.7355796 -0.2769682 0.5352099
# 3: 0.7355796 -1.2821105 0.5428595
# 4: 0.7355795  0.7013536 0.5566837
# 5: 0.7355795 -0.5090589 0.6711536
# 6: 0.7355795 -1.4315642 0.5961663

mean(rowMeans(abs(cbind(Kshap0[,1], Kshap0[,5] + Kshap0[,4] + Kshap0[,2], Kshap0[,3]) - Kshap2)[,-1]))
# 0.08646991

print(head(Kshap2)) # - grouping: (feat4, feat3), (feat2,  feat1)
#         none     group1    group2
# 1: 0.7355797  0.6277397 0.7264446
# 2: 0.7355795 -0.7781990 1.0364407
# 3: 0.7355795 -1.8699914 1.1307406
# 4: 0.7355795  0.2215966 1.0364407
# 5: 0.7355795 -0.9533463 1.1154410
# 6: 0.7355796 -1.6576263 0.8222285

# vs group after
head(cbind(Kshap0[,1], Kshap0[,5] + Kshap0[,4], Kshap0[,2]+ Kshap0[,3]))
#         none    feat_4_   feat_1_
# 1: 0.7355797  0.6275444 0.7266398
# 2: 0.7355796 -0.7667986 1.0250403
# 3: 0.7355796 -1.7283564 0.9891054
# 4: 0.7355795  0.1900494 1.0679879
# 5: 0.7355795 -1.1006523 1.2627470
# 6: 0.7355795 -1.8795789 1.0441810

mean(rowMeans(abs(cbind(Kshap0[,1], Kshap0[,5] + Kshap0[,4], Kshap0[,2]+ Kshap0[,3]) - Kshap2)[,-1]))
# 0.1085097

print(head(Kshap2)) # - grouping: (feat1, feat3), (feat2,  feat4)
#         none    group1     group2
# 1: 0.7355797 0.6044336  0.7497507
# 2: 0.7355796 0.7733540 -0.5151123
# 3: 0.7355796 0.8476845 -1.5869354
# 4: 0.7355795 0.1243070  1.1337304
# 5: 0.7355794 0.2932274 -0.1311327
# 6: 0.7355794 0.3675579 -1.2029557

# vs group after
head(cbind(Kshap0[,1], Kshap0[,2] + Kshap0[,4], Kshap0[,3]+ Kshap0[,5]))
#         none   feat_1_    feat_2_
# 1: 0.7355797 0.5956973  0.7584870
# 2: 0.7355796 0.7670173 -0.5087756
# 3: 0.7355796 0.6687280 -1.4079790
# 4: 0.7355795 0.1104624  1.1475749
# 5: 0.7355795 0.2774238 -0.1153291
# 6: 0.7355795 0.1704560 -1.0058539

mean(rowMeans(abs(cbind(Kshap0[,1], Kshap0[,2] + Kshap0[,4], Kshap0[,3]+ Kshap0[,5]) - Kshap2)[,-1]))
# 0.09572444
