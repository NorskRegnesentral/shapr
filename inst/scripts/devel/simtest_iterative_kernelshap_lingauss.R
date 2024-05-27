
library(data.table)
library(MASS)
library(Matrix)

m <- 12
n_train <- 10000
n_explain <- 10
rho_1 <- 0.5
rho_2 <- 0.5
rho_3 <- 0.5
rho_4 <- 0
Sigma_1 <- matrix(rho_1, m/4, m/4) + diag(m/4) * (1 - rho_1)
Sigma_2 <- matrix(rho_2, m/4, m/4) + diag(m/4) * (1 - rho_2)
Sigma_3 <- matrix(rho_3, m/4, m/4) + diag(m/4) * (1 - rho_3)
Sigma_4 <- matrix(rho_4, m/4, m/4) + diag(m/4) * (1 - rho_4)

Sigma <- as.matrix(bdiag(Sigma_1, Sigma_2, Sigma_3, Sigma_4))
mu <- rep(0,m)

set.seed(123)



x_train <- as.data.table(MASS::mvrnorm(n_train,mu,Sigma))
x_explain <- as.data.table(MASS::mvrnorm(n_explain,mu,Sigma))

names(x_train) <- paste0("VV",1:m)
names(x_explain) <- paste0("VV",1:m)

beta <- c(5:1, rep(0, m - 5))
alpha <- 1
y_train <- as.vector(alpha + as.matrix(x_train) %*% beta + rnorm(n_train, 0, 1))
y_explain <- alpha + as.matrix(x_explain) %*% beta + rnorm(n_explain, 0, 1)

xy_train <- cbind(y_train, x_train)

model <- lm(y_train ~ .,data = xy_train)

p0 <- mean(y_train)


### First run proper shapr call on this

expl <- shapr::explain(model = model,
                       x_explain= x_explain,
                       x_train = x_train,
                       approach = "gaussian",
                       prediction_zero = p0,Sigma=Sigma,mu=mu)



cutoff_feats <- paste0("VV",1:12)
testObs_computed <- 4

full_pred <- predict(model,x_explain)[testObs_computed]
pred_not_to_decompose <- 0#sum(expl$shapley_values[5,V9])

### Need to create an lm analogoue to pred_mod_xgb here


set.seed(123)

ctree.mincriterion = 0.95
ctree.minsplit = 20
ctree.minbucket = 7
ctree.sample = TRUE


n_samples <- 1000
approach = "gaussian"
gaussian.mu <- mu
gaussian.cov_mat <- Sigma

# Reduce if < 20% prob of shapval > 0.2
shapley_threshold_val <-  0.2
shapley_threshold_prob <- 0.2

source("inst/scripts/devel/iterative_kernelshap_sourcefuncs.R")

run <- iterative_kshap_func(model,x_explain,x_train,
                            testObs_computed = testObs_computed,
                            cutoff_feats = cutoff_feats,
                            full_pred = full_pred,
                            pred_not_to_decompose = pred_not_to_decompose,
                            p0 = p0,
                            predict_model = predict.lm,
                            shapley_threshold_val = shapley_threshold_val,
                            shapley_threshold_prob = shapley_threshold_prob)
run$kshap_est_dt_list

kshap_final <- copy(run$kshap_est_dt_list)
setnafill(kshap_final,"locf")
kshap_final[.N,] # final estimate











cutoff_feats <- paste0("VV",1:6)
testObs_computed <- 5

full_pred <- predict(model,x_explain)[5]
p0 <- mean(y_train)
pred_not_to_decompose <- sum(expl$shapley_values[5,VV7:VV9])


run_minor <- iterative_kshap_func(model,x_explain,x_train,
                            testObs_computed = 5,
                            cutoff_feats = cutoff_feats,
                            full_pred = full_pred,
                            pred_not_to_decompose = pred_not_to_decompose,
                            p0 = p0,
                            predict_model = predict.lm,shapley_threshold_val = 0)


aa=run$keep_list[[8]]$dt_vS

bb=run_minor$keep_list[[6]]$dt_vS
setnames(bb,"p_hat_1","p_hat_1_approx")

cc=merge(aa,bb)
cc[,diff:=p_hat_1-p_hat_1_approx]


# TODO:

# 1. Run example with gaussian features where the truth is known in advance in a large setting, with e.g. 12 features or so. I want the estimate
# both for the full 12 features, and for subsets where one is removed.
# 2.

# Utfordringer:
# 1. Hvordan justere vekter og samplingrutine fra subset S når man allerede har et utvalg sampler (som også er noe biased).
# 2. Bruker altså E[f(x1=x1*,x2,x3=x3*,x4)|x1=x1*] som proxy for E[f(x1=x1*,x2,x3=x3*,x4)|x1=x1*,x3=x3*],
#men hva med E[f(x1=x1*,x2,x3,x4=x4*)|x1=x1*,x4=x4*]? Burde jeg bruke den for
#E[f(x1=x1*,x2,x3=x3*,x4=x4*)|x1=x1*,x4=x4*]?
# 3. Når jeg fjerner en variabel (som har lite å si), så settes shapley-verdien til det den har per da. MEN den verdien vil trolig være noe biased fordi den fjernes første gangen den går over terskelverdiene
# jeg har satt for ekskludering.

