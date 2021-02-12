



library(shapr)
library(MASS)
library(data.table)

#setwd("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/shapr/")

source("inst/paper_experiments/model_definitions.R")
source("inst/paper_experiments/1-general_experiment-function.R")

seed = 1
set.seed(seed)
beta <- round(rnorm(50), 1)
No_test_obs = 10
corr_vector = c(0, 0.1, 0.3, 0.7, 0.9)

lm_function = function(form, train_data){
  lm(formula = form, data = train_data)
}

gam_function = function(form, train_data){
  mgcv::gam(formula = form, data = train_data)
}



# experiment lm 2
make_response_lm2 = function(X, beta){
  feat_1_ = X[, feat_1_]
  feat_2_ = X[, feat_2_]
  feat_3_ = X[, feat_3_]
  feat_4_ = X[, feat_4_]
  return (
    beta[1] +
      beta[2] * feat_1_ +
      beta[3] * feat_2_ +
      beta[4] * feat_3_ +
      beta[5] * (feat_1_ * feat_2_) +
      beta[6] * (feat_3_ * feat_4_)
)
}

form_lm2 <- response ~
  (feat_1_ * feat_2_) +
  (feat_3_ * feat_4_)

corr <- 0
seed = 1
make_response_function = make_response_lm2
form = form_lm2
model_formula= form_lm2
model_name = "experiment_lm2"


print(paste0("Correlation: ", corr))
# parameters
dim <- 4
mu <- rep(0, dim)
set.seed(seed)
No_test_obs <- No_test_obs
No_train_obs <- 1000
#
Sigma_diag <- 1
Sigma <- matrix(rep(corr, dim^2), nrow = dim, ncol = dim)
for (i in 1:dim) {
  Sigma[i, i] <- Sigma_diag
}

## 1. Simulate training data
set.seed(seed)
dt <- data.table(mvrnorm(n =  No_train_obs + No_test_obs, mu = mu, Sigma = Sigma))
setnames(dt, names(dt), paste0("feat_", 1:dim, "_"))
feat_names <- names(dt[, 1:dim])

set.seed(seed)
dt[, epsilon := rnorm(No_train_obs + No_test_obs, 0, 0.1^2)]

## 2. Define first model
dt[, response := make_response_function(.SD, beta = beta), .SDcols = feat_names]
dt[, response := response + epsilon]

## 4. Fit regression model 1
model <- lm(formula = model_formula, data = dt[(1:No_train_obs)])

## 5. Initalize shapr object
x_train <- dt[(1:No_train_obs), ..feat_names]
x_test <- dt[- (1:No_train_obs), ..feat_names]
y_train <- dt[(1:No_train_obs), .(response)]

p <- mean(y_train$response)


# Post-grouping approach
explainer <- shapr(x_train, model)
print("shapr() for full features finished")
explanation <- explain(
  x = x_test,
  explainer = explainer,
  approach = "gaussian",
  prediction_zero = p,
  mu = mu,
  cov_mat = Sigma,
  n_samples = 100000
)

explainer_group <- shapr(x_train, model,group = list(A=c("feat_1_","feat_2_"),B=c("feat_3_","feat_4_")))
explanation_group <- explain(
  x = x_test,
  explainer = explainer_group,
  approach = "gaussian",
  prediction_zero = p,
  mu = mu,
  cov_mat = Sigma,
  n_samples = 10000
)


S_feat_vec <- apply(explainer$S,1,function(x)paste0(x,collapse="-"))
S_group_vec <- apply(explainer_group$S,1,function(x)paste0(x,collapse="-"))
id_combination_group <- match(S_group_vec,S_feat_vec)
id_comb_matcher <- data.table(id_combination_old = id_combination_group,
                              id_combination = seq_along(id_combination_group))

setnames(explanation$dt_mat,"id_combination","id_combination_old")
dt_res <- merge(explanation$dt_mat,id_comb_matcher,by="id_combination_old",all.y = T,all.x=F)



data.table::setkeyv(dt_res, c("id", "id_combination"))
dt_mat <- data.table::dcast(dt_res, id_combination ~ id, value.var = "k")
dt_mat[, id_combination := NULL]
kshap <- t(explainer_group$W %*% as.matrix(dt_mat))

dt_kshap_group <- data.table::as.data.table(kshap)
names(dt_kshap_group) = c("none","group1","group2")

explanation$dt[,group1:=feat_1_+feat_2_]
explanation$dt[,group2:=feat_3_+feat_4_]
explanation$dt[,.(none,group1,group2)]

dt_kshap_group

explanation_group$dt

