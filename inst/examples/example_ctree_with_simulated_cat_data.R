library(shapr)
library(MASS)
library(data.table)

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
N_sample_gaussian <- c(50)
seed <- ifelse(exists("seed"), seed, 1)
################
corr <- 0.5 ####
################
Sigma_diag <- 1
x_test_dt <- NULL

## make sure Sigma is positive definite
Sigma <- matrix(rep(corr, dim^2), nrow = dim, ncol = dim)
for (i in 1:dim) {
  Sigma[i, i] <- Sigma_diag
}

## 1. simulate training data
set.seed(seed)
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

set.seed(seed)
epsilon1 <- rnorm(No_train_obs, 0, 0.1^2)
epsilon2 <- rnorm(No_test_obs, 0, 0.1^2)
epsilon <- c(epsilon1, epsilon2)
dt[, epsilon := epsilon]

# THIS IS AN EXTRA THING IN CASE YOU WANT NON-NUMERICAL FACTORS!
# THIS ONLY WORKS FOR 3 FEATURES with 3 levels each!
# num_to_char <- function(dt) {
#   sapply(dt, function(x) {
#     if(x == 1) {
#       return("cat")
#     } else if (x == 2) {
#       return("dog")
#     } else {
#       return("horse")
#     }
#   })
# }
#
# num_to_char2 <- function(dt) {
#   sapply(dt, function(x) {
#     if(x == 1) {
#       return("female")
#     } else if (x == 2) {
#       return("male")
#     } else {
#       return("unknown")
#     }
#   })
# }
#
# num_to_char3 <- function(dt) {
#   sapply(dt, function(x) {
#     if(x == 1) {
#       return("farm")
#     } else if (x == 2) {
#       return("house")
#     } else {
#       return("apartment")
#     }
#   })
# }
#
# dt_new_factors <- copy(dt)
#
# dt_new_factors$feat_1_ <- dt_new_factors[, lapply(.SD, FUN = num_to_char), .SDcols = c("feat_1_")]
# dt_new_factors$feat_2_ <- dt_new_factors[, lapply(.SD, FUN = num_to_char2), .SDcols = c("feat_2_")]
# dt_new_factors$feat_3_ <- dt_new_factors[, lapply(.SD, FUN = num_to_char3), .SDcols = c("feat_3_")]
#
# dt_new_factors <- cbind(dt_new_factors[, lapply(.SD, as.factor), .SDcols =
# c("feat_1_", "feat_2_", "feat_3_")], epsilon = dt$epsilon)
#
# dt <- dt_new_factors
## END

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
if (fit_mod == "regression") {
  form <- as.formula(paste0("response~", paste(feat_names, collapse = "+")))
  model <- lm(formula = form, data = dt[(1:No_train_obs)])

  fmla_onehot <- as.formula(paste("response ~", paste(reduced_onehot_names, collapse = "+")))
  model_onehot <- lm(fmla_onehot, data = dt[(1:No_train_obs)])
}

## 5. initalize shapr object with trained model -- this is used for calculating true shapley
x_train <- dt[(1:No_train_obs), ..feat_names]
x_test <- dt[- (1:No_train_obs), ..feat_names][1:5]
y_train <- dt[(1:No_train_obs), .(response)]

## 6. calculate the true shapley values

## NEW
source("inst/scripts/4-calculate_true_shapley_new_functions.R")

p <- mean(y_train$response)

# Computing the actual Shapley values with kernelSHAP accounting for feature dependence using
explainer <- shapr(x_train, model)

## An example of how it works:
set.seed(1)
joint_prob_dtNEW <- create_exact_joint_probNEW(mu, Sigma, explainer, cutoff)

joint_prob_dtNEW[1, "joint_prob"] <- -1

# this doesn't work for non-numerical factors!!
explanation <- explain(
  x_test,
  approach = "categorical",
  explainer = explainer,
  prediction_zero = p,
  joint_prob_dt = joint_prob_dtNEW
)


## If joint_prob_dt is not passed
explanation <- explain(
  x_test,
  approach = "categorical",
  explainer = explainer,
  prediction_zero = p,
  joint_prob_dt = NULL,
  epsilon = 0.001
)

explanation <- explain(
  x_test,
  approach = "ctree",
  explainer = explainer,
  prediction_zero = p
)


## To test specific cases
## 1. Here we remove some levels from the true joint_prob_dt
joint_prob_dtNEW0 <- joint_prob_dtNEW[1:10, ]
joint_prob_dtNEW0$feat_1_ <- factor(joint_prob_dtNEW0$feat_1_, levels = c("1"))

str(joint_prob_dtNEW0)
explanation <- explain(
  x_test,
  approach = "categorical",
  explainer = explainer,
  prediction_zero = p,
  joint_prob_dt = joint_prob_dtNEW0 # not full levels for feature 1 - should give an error
)

## 2. Here we remove some levels from the training observations
x_train0 <- x_train[1:10, ]
x_train0$feat_2_ <- factor(x_train0$feat_2_, levels = c("1", "2"))
str(x_train0)
str(x_test)

explainer0 <- shapr(x_train0, model)

explanation <- explain(
  x_test,
  approach = "categorical",
  explainer = explainer0, # not full levels for feature 2 - should NOT give an error
  prediction_zero = p,
  joint_prob_dt = joint_prob_dtNEW
)

## 3. Here we convert training observations to numeric
x_train1 <- x_train[, lapply(.SD, as.numeric)]

form_num <- as.formula(paste0("response~", paste(feat_names, collapse = "+")))
model_num <- lm(formula = form, data = cbind(x_train1, y_train))

explainer1 <- shapr(x_train1, model_num)

explanation <- explain(
  x_test,
  approach = "categorical",
  explainer = explainer1, # x_train is numeric, should give an error
  prediction_zero = p,
  joint_prob_dt = joint_prob_dtNEW
)


explanation$dt
#            none     feat_1_    feat_2_       feat_3_
#  1: -0.03051645  0.22110766  0.3030733  0.1175182309
#  2: -0.03051648  0.23135903  0.3612335 -0.0568842323
#  3: -0.03051644  0.14372486  0.3373792 -0.8502528492
#  4: -0.03051647  0.24468937 -0.8628448  0.1542824861
#  5: -0.03051649  0.21413846 -0.7842834  0.0002811693
#  6: -0.03051645  0.08302466 -0.7863979 -0.7713476267
#  7: -0.03051647  0.19053258  0.4525885  0.1880265357
#  8: -0.03051648  0.11633293  0.5529742  0.0558495916
#  9: -0.03051642 -0.06748998  0.5772143 -0.6894246477
# 10: -0.03051650 -0.91715578  0.3864888  0.1661790837
# 11: -0.03051652 -0.89237705  0.4155944  0.0063039791
# 12: -0.03051647 -0.96529893  0.3623155 -0.7723523467
# 13: -0.03051652 -0.89824150 -0.7840967  0.2122782132
# 14: -0.03051653 -0.91426506 -0.7345900  0.0728042550
# 15: -0.03051648 -1.03066657 -0.7661291 -0.6841122499
# 16: -0.03051652 -0.95815401  0.5255809  0.2575336878
# 17: -0.03051652 -1.01782630  0.5969120  0.1398841023
# 18: -0.03051646 -1.18693692  0.5917274 -0.5906778460
# 19: -0.03051653  1.39605946  0.4154863  0.2194907079
# 20: -0.03051654  1.43553542  0.4151974  0.0743128320
# 21: -0.03051649  1.37972104  0.3277035 -0.6872359988
# 22: -0.03051655  1.40928282 -0.7607902  0.2769716739
# 23: -0.03051656  1.40795649 -0.7406779  0.1521949445
# 24: -0.03051650  1.30866247 -0.8064320 -0.5876140655
# 25: -0.03051655  1.34186286  0.5413800  0.3372420335
# 26: -0.03051655  1.29688780  0.5833166  0.2342896768
# 27: -0.03051648  1.14488468  0.5439171 -0.4791647766
# none     feat_1_    feat_2_       feat_3_

# With 9 obs
# none     feat_1_    feat_2_      feat_3_
# 1: -0.03051645  0.22112678  0.3030384  0.117533990
# 2: -0.03051648  0.23135589  0.3611816 -0.056829183
# 3: -0.03051644  0.14381365  0.3372754 -0.850237813
# 4: -0.03051647  0.24467027 -0.8628561  0.154312889
# 5: -0.03051649  0.21414902 -0.7843377  0.000324901
# 6: -0.03051645  0.08285628 -0.7863687 -0.771208477
# 7: -0.03051647  0.19058801  0.4525740  0.187985549
# 8: -0.03051648  0.11612747  0.5530621  0.055967207
# 9: -0.03051642 -0.06771600  0.5773065 -0.689290795


# With only 10 test observations
#            none     feat_1_    feat_2_       feat_3_
#  1: -0.03051645  0.22110766  0.3030733  0.1175182309
#  2: -0.03051648  0.23135903  0.3612335 -0.0568842323
#  3: -0.03051644  0.14372486  0.3373792 -0.8502528492
#  4: -0.03051647  0.24468937 -0.8628448  0.1542824861
#  5: -0.03051649  0.21413846 -0.7842834  0.0002811693
#  6: -0.03051645  0.08302466 -0.7863979 -0.7713476267
#  7: -0.03051647  0.19053258  0.4525885  0.1880265357
#  8: -0.03051648  0.11633293  0.5529742  0.0558495916
#  9: -0.03051642 -0.06748998  0.5772143 -0.6894246477
# 10: -0.03051650 -0.91715578  0.3864888  0.1661790837

# DIM = 4
#          none     feat_1_    feat_2_      feat_3_     feat_4_
#  1: 0.7980836  0.29759883  0.3668716  0.192099669  0.45731865
#  2: 0.7980836  0.33096607  0.4374219  0.221442865 -0.78107962
#  3: 0.7980836  0.29787182  0.4515877  0.238398752 -1.78305036
#  4: 0.7980836  0.33908108  0.4455848 -0.169410429  0.59380834
#  5: 0.7980835  0.34084321  0.4917553 -0.112074822 -0.61659752
#  6: 0.7980835  0.27461428  0.4817085 -0.066445274 -1.58989459
#  7: 0.7980835  0.31195012  0.4658068 -1.168942525  0.69704947
#  8: 0.7980835  0.28035544  0.4875057 -1.082692665 -0.48444214
#  9: 0.7980835  0.17473085  0.4490872 -1.003179413 -1.42385551
# 10: 0.7980836  0.34740291 -0.9782417  0.237910570  0.60209420
# 11: 0.7980835  0.35292006 -0.8950699  0.269860801 -0.62368254
# 12: 0.7980835  0.29094479 -0.8683388  0.290567194 -1.61308802
# 13: 0.7980835  0.35984717 -0.8824784 -0.106549396  0.73352163
# 14: 0.7980835  0.33287783 -0.8233925 -0.046312961 -0.46396891
# 15: 0.7980835  0.23661501 -0.8204897  0.003451381 -1.42431644
# 16: 0.7980835  0.30259222 -0.8462688 -1.090094048  0.83491187
# 17: 0.7980835  0.24116077 -0.8112862 -1.000574932 -0.33329599
# 18: 0.7980835  0.10480896 -0.8365241 -0.916695771 -1.25952870
# 19: 0.7980835  0.34235968  0.1535326  0.282783272  0.72712901
# 20: 0.7980835  0.31883471  0.2493273  0.318529786 -0.48602482
# 21: 0.7980835  0.22280106  0.2906822  0.344047097 -1.46080656
# 22: 0.7980835  0.32450458  0.2651293 -0.045843304  0.85718901
# 23: 0.7980834  0.26749134  0.3371720  0.018523344 -0.32734468
# 24: 0.7980835  0.13654230  0.3549078  0.073307884 -1.27285920
# 25: 0.7980834  0.23198540  0.3189918 -1.011735007  0.95853758
# 26: 0.7980834  0.13977124  0.3671775 -0.917839406 -0.19646715
# 27: 0.7980835 -0.03391969  0.3576570 -0.828055749 -1.10698255
# 28: 0.7980836 -1.01079278  0.4625220  0.246546116  0.61075126
# 29: 0.7980835 -0.96953806  0.5135892  0.279597480 -0.61975953
# 30: 0.7980835 -0.99590050  0.5091881  0.301656682 -1.61499846
# 31: 0.7980835 -0.95705462  0.5205960 -0.102708074  0.74336837
# 32: 0.7980835 -0.94711079  0.5464008 -0.041370085 -0.45885580
# 33: 0.7980835 -1.00610330  0.5162732  0.009867401 -1.42491644
# 34: 0.7980835 -0.97393247  0.5208509 -1.091987064  0.84607042
# 35: 0.7980834 -0.99693452  0.5209512 -1.001323884 -0.32682856
# 36: 0.7980835 -1.09497687  0.4614145 -0.915856890 -1.25865969
# 37: 0.7980835 -0.96407451 -0.8856771  0.298719032  0.75533644
# 38: 0.7980835 -0.95066026 -0.8219787  0.334348541 -0.46254320
# 39: 0.7980835 -1.00581567 -0.8137265  0.359894090 -1.44512881
# 40: 0.7980834 -0.93907953 -0.8102582 -0.033190209  0.88200683
# 41: 0.7980834 -0.95785755 -0.7715284  0.031019718 -0.30729239
# 42: 0.7980834 -1.04679588 -0.7886184  0.086127839 -1.26031543
# 43: 0.7980834 -0.98563748 -0.7935719 -1.006037857  0.98152629
# 44: 0.7980834 -1.03846666 -0.7801782 -0.912134309 -0.17807931
# 45: 0.7980834 -1.16714818 -0.8264462 -0.822565565 -1.09664173
# 46: 0.7980834 -0.97525815  0.2399568  0.351745084  0.88449871
# 47: 0.7980834 -0.99079709  0.3163670  0.390904084 -0.32066908
# 48: 0.7980834 -1.07991457  0.3393392  0.420971628 -1.28853465
# 49: 0.7980834 -0.97999708  0.3317745  0.036234681  1.00810534
# 50: 0.7980834 -1.02873007  0.3835501  0.104308028 -0.16814810
# 51: 0.7980834 -1.15225832  0.3813894  0.164147425 -1.10624183
# 52: 0.7980834 -1.06132523  0.3666078 -0.918465999  1.10610107
# 53: 0.7980834 -1.14484821  0.3932935 -0.820452758 -0.04021247
# 54: 0.7980834 -1.31077254  0.3628391 -0.725268440 -0.94296127
# 55: 0.7980835  1.13220963  0.5185543  0.305293626  0.74964069
# 56: 0.7980835  1.18024247  0.5509533  0.343456909 -0.47409199
# 57: 0.7980835  1.16116847  0.5253618  0.372129595 -1.46204247
# 58: 0.7980834  1.19598034  0.5566971 -0.033928010  0.88212386
# 59: 0.7980834  1.21315853  0.5624650  0.032978139 -0.31286595
# 60: 0.7980834  1.16179116  0.5101370  0.091165800 -1.27130145
# 61: 0.7980834  1.18937959  0.5345751 -1.012929896  0.98664862
# 62: 0.7980834  1.17395793  0.5136005 -0.916352528 -0.17866997
# 63: 0.7980834  1.08415537  0.4300193 -0.823320705 -1.10226131
# 64: 0.7980834  1.17278621 -0.8357864  0.365574858  0.89840090
# 65: 0.7980834  1.19305576 -0.7906791  0.406084775 -0.31262345
# 66: 0.7980834  1.14529029 -0.8035158  0.437939349 -1.28781913
# 67: 0.7980834  1.20839639 -0.7797161  0.044280812  1.02318943
# 68: 0.7980833  1.19692991 -0.7609460  0.113827388 -0.15879826
# 69: 0.7980834  1.11571820 -0.8001350  0.175581225 -1.10409468
# 70: 0.7980833  1.17260598 -0.7849163 -0.917799303  1.12306032
# 71: 0.7980833  1.12743436 -0.7925204 -0.818213079 -0.02888772
# 72: 0.7980834  1.00709411 -0.8627314 -0.721383966 -0.93910886
# 73: 0.7980834  1.15263743  0.2808823  0.429386997  1.03470737
# 74: 0.7980833  1.14405765  0.3388053  0.473114828 -0.16350127
# 75: 0.7980834  1.06211824  0.3404768  0.510126994 -1.12418878
# 76: 0.7980833  1.15906019  0.3538980  0.125038272  1.15479268
# 77: 0.7980833  1.11774259  0.3858176  0.198136692 -0.01404537
# 78: 0.7980833  1.00172910  0.3613461  0.265257400 -0.94462433
# 79: 0.7980833  1.08851344  0.3668586 -0.818880996  1.25309821
# 80: 0.7980833  1.01275189  0.3726504 -0.715496655  0.11454608
# 81: 0.7980833  0.85495697  0.3180412 -0.612416372 -0.78007331

# Extra code....
marg_list <- list()
marg_list[[1]] <- NA
for (i in 2:nrow(x$S)) {
  col_names <- feat_names[as.logical(x$S[i, ])]

  mat <- joint_prob_dt[, .(marg_prob = sum(joint_prob)), by = col_names]

  marg_list[[i]] <- mat
}
##


# Compute all conditional probabilities
cond_list <- list()
cond_list[[1]] <- data.frame(marg_prob = 1, joint_prob = 1, id_all = joint_prob_dt$id_all, cond_prob = 1)

for (i in 2:nrow(x$S)) {
  col_names <- feat_names[as.logical(x$S[i, ])]

  mat0 <- marg_list[[i]]
  setkeyv(mat0, col_names)
  setkeyv(joint_prob_dt, col_names)
  cond_list[[i]] <- merge(mat0, joint_prob_dt, all.x = TRUE)
  cond_list[[i]][, cond_prob := joint_prob / marg_prob]
  cond_list[[i]][, (feat_names) := NULL]
}
##

cond_dt <- rbindlist(cond_list, id = "id_combination")

joint_prob_dt0 <- copy(joint_prob_dt)
joint_prob_dt0[, joint_prob := NULL]

cond_dt <- cond_dt[joint_prob_dt0, on = "id_all"]
cols <- c(feat_names, "id_combination")
cols2 <- paste0(feat_names, "conditioned")

S_dt <- data.table(explainer$S)
S_dt[S_dt == 0] <- NA
S_dt[, id_combination := 1:nrow(S_dt)]
data.table::setnames(S_dt, c(cols2, "id_combination"))


tmp <- cond_dt[S_dt, on = "id_combination"]
#
tmp_features <- as.matrix(tmp[, ..feat_names])
#
tmp_S <- as.matrix(tmp[, ..cols2])
#
tmp_features[which(is.na(tmp_S))] <- NA
tmp_features_with_NA <- data.table::as.data.table(tmp_features)
data.table::setnames(tmp_features_with_NA, cols2)
#
data.table::setkeyv(cond_dt, "id_combination")
dt <- cbind(cond_dt, tmp_features_with_NA)
