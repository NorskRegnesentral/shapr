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
x_test_list <- list()
for (i in 1:dim) {
  x_test_list[[i]] <- 1:no_categories
}
x_test_dt <- do.call(CJ, x_test_list)

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

## 3. Calculate response
dt[, response := response_mod(mod_matrix_full = cbind(1, mod_matrix), beta = beta, epsilon = epsilon)]

## 4. Fit model
form <- as.formula(paste0("response~", paste(feat_names, collapse = "+")))
model <- lm(formula = form, data = dt[(1:No_train_obs)])

## 5. Initalize shapr object with trained model -- this is used for calculating true shapley
x_train <- dt[(1:No_train_obs), ..feat_names]
x_test <- dt[- (1:No_train_obs), ..feat_names]
y_train <- dt[(1:No_train_obs), .(response)]

## 6. calculate the true shapley values
p <- mean(y_train$response)

# Computing the actual Shapley values with kernelSHAP accounting for feature dependence using
explainer <- shapr(x_train, model)

## 8. calculate approximate shapley values with different methods
explanation_list <- list()

## ctree
# no grouping
explanation_list[['ctree']] <- explain(
  x_test,
  approach = 'ctree',
  explainer = explainer,
  prediction_zero = p,
  sample = FALSE,
  w_threshold = 1,
  n_samples = 1000)

# 2 groups (1, 3), (2)
head(cbind(explanation_list[['ctree']]$dt$none, explanation_list[['ctree']]$dt$feat_1_ + explanation_list[['ctree']]$dt$feat_3_,
           explanation_list[['ctree']]$dt$feat_2_))

#            [,1]       [,2]       [,3]
# [1,] 0.02618352  0.2494983  0.3158207
# [2,] 0.02618352  0.1624316  0.3158207
# [3,] 0.02618352 -0.7267962  0.3158207
# [4,] 0.02618352  0.2494983 -0.7840306
# [5,] 0.02618352  0.1624316 -0.7840306
# [6,] 0.02618352 -0.7267962 -0.7840306

# 1 group (1, 3, 2)
head(cbind(explanation_list[['ctree']]$dt$none, explanation_list[['ctree']]$dt$feat_1_ + explanation_list[['ctree']]$dt$feat_3_ + explanation_list[['ctree']]$dt$feat_2_))
#            [,1]       [,2]
# [1,] 0.02618352  0.5653190
# [2,] 0.02618352  0.4782523
# [3,] 0.02618352 -0.4109755
# [4,] 0.02618352 -0.5345323
# [5,] 0.02618352 -0.6215990
# [6,] 0.02618352 -1.5108268


# with groupings
# 3 groups (1), (2), (3)
group1 <- list(c(1), # no groups at all
               c(2),
               c(3))
group1_names = lapply(group1, function(x) {
  names(x_test)[x]
})

explainer_group1 <- shapr(x_train, model, group = group1_names)

explanation_list[['ctree_group1']] <- explain(
  x_test,
  approach = 'ctree',
  explainer = explainer_group1, # c(1), c(2), c(3)
  prediction_zero = p,
  sample = FALSE,
  w_threshold = 1,
  n_samples = 1000)

print(head(explanation_list[['ctree_group1']]$dt))
#          none     group1     group2      group3
# 1: 0.02618352 0.06703414  0.3158207  0.18246415
# 2: 0.02618352 0.06703414  0.3158207  0.09539743
# 3: 0.02618352 0.06703414  0.3158207 -0.79383035
# 4: 0.02618352 0.06703414 -0.7840306  0.18246415
# 5: 0.02618352 0.06703414 -0.7840306  0.09539743
# 6: 0.02618352 0.06703414 -0.7840306 -0.79383035


# 2 groups (1, 3), (2)
group2 <- list(c(1, 3), (2))
group2_names = lapply(group2, function(x) {
  names(x_test)[x]
})

explainer_group2 <- shapr(x_train, model, group = group2_names)

explanation_list[['ctree_group2']] <- explain(
  x_test,
  approach = 'ctree',
  explainer = explainer_group2, # c(1, 3), c(2)
  prediction_zero = p,
  sample = FALSE,
  w_threshold = 1,
  n_samples = 1000)

print(head(explanation_list[['ctree_group2']]$dt))
#          none     group1     group2
# 1: 0.02618352  0.2494983  0.3158207
# 2: 0.02618352  0.1624316  0.3158207
# 3: 0.02618352 -0.7267962  0.3158207
# 4: 0.02618352  0.2494983 -0.7840306
# 5: 0.02618352  0.1624316 -0.7840306
# 6: 0.02618352 -0.7267962 -0.7840306


# 1 group (1, 3, 2)
group3 <- list(c(1, 3, 2))
group3_names = lapply(group3, function(x) {
  names(x_test)[x]
})

explainer_group3 <- shapr(x_train, model, group = group3_names)

explanation_list[['ctree_group3']] <- explain(
  x_test,
  approach = 'ctree',
  explainer = explainer_group3, # c(1, 3, 2)
  prediction_zero = p,
  sample = FALSE,
  w_threshold = 1,
  n_samples = 1000)

print(head(explanation_list[['ctree_group3']]$dt))
#          none     group1
# 1: 0.02618352  0.5653190
# 2: 0.02618352  0.4782523
# 3: 0.02618352 -0.4109755
# 4: 0.02618352 -0.5345323
# 5: 0.02618352 -0.6215990
# 6: 0.02618352 -1.5108268


