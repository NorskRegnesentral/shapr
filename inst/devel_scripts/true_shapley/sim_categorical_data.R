# simulate some categorical data with dependence

library(MASS)
library(data.table)
library(shapr)
library(ggplot2)
library(xgboost)


rm(list=ls())

## ---------------------- simulate training data ---------------------
N = 10000

## First categorical variable
set.seed(1)
Sigma <- matrix(rep(0, 9), 3, 3)
# Sigma <- matrix(c(1, 0.5, -0.5, 0.5, 1, 0.1, -0.5, 0.1, 1), 3, 3)
Sigma[1, 1] <- Sigma[2, 2] <- Sigma[3, 3] <- 1
mu = c(1, 2, -0.5)
x <- mvrnorm(n = N, mu = mu, Sigma = Sigma)

dt <- data.table(x)
# dt <- data.table(round(x))
setnames(dt, c("feat1", "feat2", "feat3"))

dt[, 'epsilon' := 0] # rnorm(N, 0, 0.1^2)

mod <- function(x1, x2, x3, noise, beta){
  return(beta[1] * x1 + beta[2] * x2 + beta[3] * x3 + noise) # x1 * x2
}

beta = c(1, 1, 1)
dt[, "response" := mod(feat1, feat2, feat3, epsilon, beta)]

## ------------------- plotting -------------------

# plot(y = dt$response, x = dt$feat1)
# plot(y = dt$response, x = dt$feat2)
# plot(y = dt$response, x = dt$feat3)
#
# plot(dt$feat1, dt$feat2, col = as.factor(dt$feat3))
# plot(dt$feat1, dt$feat3, col = as.factor(dt$feat2))
# plot(dt$feat3, dt$feat2, col = as.factor(dt$feat1))

# ggplot(data = dt, aes(x = feat1, y = feat2)) + geom_jitter(data = dt, aes(color = as.factor(feat3)))  +
#   stat_summary(fun.data= mean_cl_normal) +
#   geom_smooth(method='lm', formula = y ~ x)
#
# ggplot(data = dt, aes(x = feat1, y = feat3)) + geom_jitter(data = dt, aes(color = as.factor(feat2))) +
#   stat_summary(fun.data= mean_cl_normal) +
#   geom_smooth(method='lm', formula = y ~ x)
#
# ggplot(data = dt, aes(x = feat3, y = feat2)) + geom_jitter(data = dt, aes(color = as.factor(feat1))) +
#   stat_summary(fun.data= mean_cl_normal) +
#   geom_smooth(method='lm', formula = y ~ x)


## ---------------------- make model ---------------------

x_train <- as.matrix(dt[-(1:6), .(feat1, feat2, feat3)])
x_test <- as.matrix(dt[(1:6), .(feat1, feat2, feat3)])
y_train <- as.matrix(dt[-(1:6), .(response)])

## linear regression
model <- lm(response ~ feat1 + feat2 + feat3, data = dt[-(1:6), .(feat1, feat2, feat3, response)])

## xgboost
# dummyfunc <- caret::dummyVars(" ~ .", data = rbind(x_train, x_test))
# x_train_dummy <- predict(dummyfunc, newdata = x_train)
# x_test_dummy <- predict(dummyfunc, newdata = x_test)
#
# ##  Fitting a basic xgboost model to the training data
# model <- xgboost(
#   data = x_train_dummy,
#   label = y_train,
#   nround = 20,
#   verbose = FALSE
# )
#
# ## For xgboost
# model$dummyfunc <- dummyfunc


explainer <- shapr(x_train, model)
p <- mean(y_train)
explanation <- explain(
  x_test,
  approach = 'ctree',
  explainer = explainer,
  prediction_zero = p,
  sample = FALSE)

# Finally we plot the resulting explanations
plot(explanation) # plot_phi0 = FALSE

explanation$dt

## truth
dt_truth <- data.table(beta[1] * (x_test[,1] - mu[1]))
dt_truth[, 'estimate' := explanation$dt$feat1][]

dt_truth2 <- data.table(beta[2] * (x_test[, 2] - mu[2]))
dt_truth2[, 'estimate' := explanation$dt$feat2][]

dt_truth3 <- data.table(beta[3] * (x_test[, 3] - mu[3]))
dt_truth3[, 'estimate' := explanation$dt$feat3][]


## ----------------------------------------------------------------

corr <- seq(from = -0.4, to = 0.4, by = 0.2)

plot_list = list()

for(i in 1:(length(corr))){

  set.seed(1)

  mat0 <- matrix(rep(i, 9), 3, 3)
  mat0[1, 1] <- mat0[2, 2] <- mat0[3, 3] <- 1
  mat <- (1/2) * (mat0 %*% t(mat0))


  x <- mvrnorm(n = N, mu = c(1, 2, -0.5), Sigma = mat)

  dt <- data.table(round(x) + 2)
  setnames(dt, c("feat1", "feat2", "feat3"))

  dt[, 'epsilon' := rnorm(N, 0, 0.1^2)]

  mod <- function(x1, x2, x3, noise){
    return(x1 + x2 + x3 + noise) # x1 * x2
  }

  dt[, "response" := mod(feat1, feat2, feat3, epsilon)]

  # plot(y = dt$response, x = dt$feat1)
  # plot(y = dt$response, x = dt$feat2)
  # plot(y = dt$response, x = dt$feat3)

  # plot(dt$feat1, dt$feat2, col = as.factor(dt$feat3))
  # plot(dt$feat1, dt$feat3, col = as.factor(dt$feat2))
  # plot(dt$feat3, dt$feat2, col = as.factor(dt$feat1))


  ## ---------------------- make model ---------------------

  x_train <- as.matrix(dt[-(1:6), .(feat1, feat2, feat3)])
  x_test <- as.matrix(dt[(1:6), .(feat1, feat2, feat3)])
  y_train <- as.matrix(dt[-(1:6), .(response)])

  ## For xgboost
  dummyfunc <- caret::dummyVars(" ~ .", data = rbind(x_train, x_test))
  x_train_dummy <- predict(dummyfunc, newdata = x_train)
  x_test_dummy <- predict(dummyfunc, newdata = x_test)


  # Fitting a basic xgboost model to the training data
  model <- xgboost(
    data = x_train_dummy,
    label = y_train,
    nround = 20,
    verbose = FALSE
  )

  ## For xgboost
  model$dummyfunc <- dummyfunc

  explainer <- shapr(x_train, model)
  p <- mean(y_train)
  explanation <- explain(
    x_test,
    approach = 'ctree',
    explainer = explainer,
    prediction_zero = p,
    sample = FALSE)

  # Finally we plot the resulting explanations
  plot_list[[i]] <- explanation

}
corr
setwd("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/figures/")
plot(plot_list[[1]], plot_phi0 = FALSE)
ggsave("categorical_rho_neg4.pdf", plot = last_plot())
plot(plot_list[[2]], plot_phi0 = FALSE)
ggsave("categorical_rho_neg2.pdf", plot = last_plot())
plot(plot_list[[3]], plot_phi0 = FALSE)
ggsave("categorical_rho_0.pdf", plot = last_plot())
plot(plot_list[[4]], plot_phi0 = FALSE)
ggsave("categorical_rho_2.pdf", plot = last_plot())
plot(plot_list[[5]], plot_phi0 = FALSE)
ggsave("categorical_rho_4.pdf", plot = last_plot())




