# Compare the new implementations to the old ones, double checking that the results do not change.

rm(list = ls())

library(shapr)
library(MASS)

data("Boston")

x_var <- c("lstat", "rm", "dis", "indus")
y_var <- "medv"

x_train <- as.matrix(tail(Boston[, x_var], -6))
y_train <- tail(Boston[, y_var], -6)
x_test <- as.matrix(head(Boston[, x_var], 6))

# Fitting a basic xgboost model to the training data
model <- xgboost::xgboost(
  data = x_train,
  label = y_train,
  nround = 20
)

# Prepare the data for explanation
explainer <- shapr(x_train, model)

# ---------------------------------------------------------------------------------------------------------
# Ex 1: Explain predictions (empirical, AICc)
explanation <- explain(x_test, explainer, approach = "empirical", prediction_zero = mean(y_train), type = "AICc_each_k")
explanation

explainer.orig <- prepare_kshap(x_train,x_test)
explanation.orig <- compute_kshap(
  model = model,
  l = explainer.orig,
  pred_zero = mean(y_train),
  empirical_settings = list(type =
                              "AICc_each_k", fixed_sigma_vec = 0.1, AICc_no_samp_per_optim = 1000,
                            AIC_optim_max_eval = 20, AIC_optim_startval = 0.1, w_threshold = 0.95)
)
explanation.orig$Kshap
all(abs(explanation - explanation.orig$Kshap) < 1e-2)

# ---------------------------------------------------------------------------------------------------------
# Ex 2: Explain predictions (empirical, AICc full)
explanation <- explain(x_test, explainer, approach = "empirical", prediction_zero = mean(y_train), type = "AICc_full")
explanation

explainer.orig <- prepare_kshap(x_train,x_test)
explanation.orig <- compute_kshap(
  model = model,
  l = explainer.orig,
  pred_zero = mean(y_train),
  empirical_settings = list(type =
                              "AICc_full", fixed_sigma_vec = 0.1, AICc_no_samp_per_optim = 1000,
                            AIC_optim_max_eval = 20, AIC_optim_startval = 0.1, w_threshold = 0.95)
)
explanation.orig$Kshap
all(abs(explanation - explanation.orig$Kshap) < 1e-2)

# ---------------------------------------------------------------------------------------------------------
# Ex 3: Explain predictions (empirical, independence):
explanation <- explain(x_test, explainer, approach = "empirical", prediction_zero = mean(y_train), type = "independence")
explanation

explainer.orig <- prepare_kshap(x_train, x_test)
explanation.orig <- compute_kshap(
  model = model,
  l = explainer.orig,
  pred_zero = mean(y_train),
  empirical_settings = list(type =
                              "independence", fixed_sigma_vec = 0.1, w_threshold = 0.95)
)
explanation.orig$Kshap
all(abs(explanation - explanation.orig$Kshap) < 1e-2)

# ---------------------------------------------------------------------------------------------------------
# Ex 4: Explain predictions (empirical, fixed sigma)
explanation <- explain(x_test, explainer, approach = "empirical", prediction_zero = mean(y_train), type = "fixed_sigma")
explanation

explainer.orig <- prepare_kshap(x_train, x_test)
explanation.orig <- compute_kshap(
  model = model,
  l = explainer.orig,
  pred_zero = mean(y_train),
  empirical_settings = list(type =
                              "fixed_sigma", fixed_sigma_vec = 0.1, w_threshold = 0.95)
)
explanation.orig$Kshap
all(abs(explanation - explanation.orig$Kshap) < 1e-2)

# ---------------------------------------------------------------------------------------------------------
# Ex 5: Explain predictions (gaussian)
explanation <- explain(x_test, explainer, approach = "gaussian", prediction_zero = mean(y_train))
explanation


explainer.orig <- prepare_kshap(x_train, x_test)
explanation.orig <- compute_kshap(
  model = model,
  l = explainer.orig,
  pred_zero = mean(y_train),
  cond_approach ="Gaussian")
explanation.orig$Kshap
all(abs(explanation - explanation.orig$Kshap) < 1e-2)

# Note: since all Gaussian data is sampled at once for the original function while it is sampled seperately for each test obs. in the new function,
# we may compare the results by setting a seed, and comparing one observation at a time. Otherwise only the Shapley values of the first observation
# will be equal.
# Ex: observation 1
set.seed(1)
explanation <- explain(x_test[1,,drop=F], explainer, approach = "gaussian", prediction_zero = mean(y_train),n_samples=1e5)
explanation
explainer.orig <- prepare_kshap(x_train, x_test[2,,drop=F])
set.seed(1)
explanation.orig <- compute_kshap(model = model,l = explainer.orig,pred_zero = mean(y_train),cond_approach ="Gaussian",noSamp_MC = 1e5)
explanation.orig$Kshap
# Ex: observation 3
set.seed(1)
explanation <- explain(x_test[3,,drop=F], explainer, approach = "gaussian", prediction_zero = mean(y_train),n_samples=1e5)
explanation
explainer.orig <- prepare_kshap(x_train, x_test[3,,drop=F])
set.seed(1)
explanation.orig <- compute_kshap(model = model,l = explainer.orig,pred_zero = mean(y_train),cond_approach ="Gaussian",noSamp_MC = 1e5)
explanation.orig$Kshap
# Everything is equal!

# ---------------------------------------------------------------------------------------------------------
# Ex 6: Explain predictions (copula)
explanation <- explain(x_test, explainer, approach = "copula", prediction_zero = mean(y_train))
explanation

explainer.orig <- prepare_kshap(x_train, x_test)
explanation.orig <- compute_kshap(
  model = model,
  l = explainer.orig,
  pred_zero = mean(y_train),
  cond_approach = "copula")
explanation.orig$Kshap
all(abs(explanation - explanation.orig$Kshap) < 1e-3)

# As above, we compare one observation at a time using a seed.
# Ex: obs 1
set.seed(1)
explanation <- explain(x_test[1, ,drop=F], explainer, approach = "copula", prediction_zero = mean(y_train),n_samples=1e5)
explanation
explainer.orig <- prepare_kshap(x_train, x_test[1, ,drop=F])
set.seed(1)
explanation.orig <- compute_kshap(model = model,l = explainer.orig,pred_zero = mean(y_train),cond_approach = "copula",noSamp_MC = 1e5)
explanation.orig$Kshap
# Ex: obs 4
set.seed(1)
explanation <- explain(x_test[4, ,drop=F], explainer, approach = "copula", prediction_zero = mean(y_train),n_samples=1e5)
explanation
explainer.orig <- prepare_kshap(x_train, x_test[4, ,drop=F])
set.seed(1)
explanation.orig <- compute_kshap(model = model,l = explainer.orig,pred_zero = mean(y_train),cond_approach = "copula",noSamp_MC = 1e5)
explanation.orig$Kshap
# Everything is equal!

# ---------------------------------------------------------------------------------------------------------
# Ex 7: Explain predictions (combined)

# All empirical
comb_approach=list("empirical"=1:6)
emp_type=list("independence"=1:6)
explanation <- explain(x_test, explainer, approach = comb_approach,empirical.types=emp_type, prediction_zero = mean(y_train))
explanation
# Confirmed this is eq. to the ordinary empirical, comparing to ex. 1.

# Obs 1-3 empirical independence, 4-6 Gaussian
comb_approach=list("empirical"=1:3,"gaussian"=4:6)
emp_type=list("independence"=1:3)
explanation <- explain(x_test, explainer, approach = comb_approach,empirical.types=emp_type, prediction_zero = mean(y_train),nsamples=1e5)
explanation
# Confirmed 1-3 are eq. to ordinary empirical, and 4-6 to Gaussian, comparing to ex. 1 and 5.

# Obs 1-3 empirical w/ fixed sigma, 4-6 copula
comb_approach=list("empirical"=1:3,"copula"=4:6)
emp_type=list("fixed_sigma"=1:3)
explanation <- explain(x_test, explainer, approach = comb_approach,empirical.types=emp_type, prediction_zero = mean(y_train),nsamples=1e5)
explanation
# Confirmed 1-3 are eq. to fixed sigma and 4-6 are eq. to copula, comparing to ex. 4 and 6.

# All with fixed sigma
comb_approach=list("empirical"=1:6)
emp_type=list("fixed_sigma"=1:6)
explanation <- explain(x_test, explainer, approach = comb_approach,empirical.types=emp_type, prediction_zero = mean(y_train),nsamples=1e5)
explanation
# Equal to res. from ex. 4.

# Obs 1 and 2 empirical w/ fixed sigma, obs 3 with full AICc, 4-6 copula
comb_approach=list("empirical"=1:3,"copula"=4:6)
emp_type=list("fixed_sigma"=1:2,"AICc_full"=3)
explanation <- explain(x_test, explainer, approach = comb_approach,empirical.types=emp_type, prediction_zero = mean(y_train),nsamples=1e5)
explanation
# Confirmed 1-2 are eq. to results from ex 4, 3 to results from ex. 2, 4-6 approx. eq. to results from ex. 6.

# Obs 1 empirical w/ fixed sigma, obs 2 AICc each k, obs 4 with full AICc, 4-6 gaussian
comb_approach=list("empirical"=1:3,"gaussian"=4:6)
emp_type=list("fixed_sigma"=1,"AICc_each_k"=2,"AICc_full"=3)
explanation <- explain(x_test, explainer, approach = comb_approach,empirical.types=emp_type, prediction_zero = mean(y_train),nsamples=1e5)
explanation
# Confirmed obs. 1 is eq. to res from ex 1, obs 2 eq to res from ex 2, obs 3 eq to res from ex 3, pbs 4-6 to ex. 5.


# The old method does not work:
explainer.orig <- prepare_kshap(x_train, x_test)
explanation.orig <- compute_kshap(
  model = model,
  l = explainer.orig,
  pred_zero = mean(y_train),
  cond_approach = comb_approach
  )

explanation.orig$Kshap


