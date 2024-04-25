
library(data.table)
library(MASS)
library(Matrix)

m <- 9
n_train <- 10000
n_explain <- 10
rho_1 <- 0.5
rho_2 <- 0
rho_3 <- 0.5
Sigma_1 <- matrix(rho_1, m/3, m/3) + diag(m/3) * (1 - rho_1)
Sigma_2 <- matrix(rho_2, m/3, m/3) + diag(m/3) * (1 - rho_2)
Sigma_3 <- matrix(rho_3, m/3, m/3) + diag(m/3) * (1 - rho_3)
Sigma <- as.matrix(bdiag(Sigma_1, Sigma_2, Sigma_3))
mu <- rep(0,m)

set.seed(123)



x_train <- as.data.table(MASS::mvrnorm(n_train,mu,Sigma))
x_explain <- as.data.table(MASS::mvrnorm(n_explain,mu,Sigma))

beta <- c(4:1, rep(0, m - 4))
alpha <- 1
y_train <- as.vector(alpha + as.matrix(x_train) %*% beta + rnorm(n_train, 0, 1))
y_explain <- alpha + as.matrix(x_explain) %*% beta + rnorm(n_explain, 0, 1)

xy_train <- cbind(y_train, x_train)

model <- lm(y_train ~ .,data = xy_train)


### First run proper shapr call on this

expl <- shapr::explain(model = model,
                       x_explain= x_explain,
                       x_train = x_train,
                       approach = "gaussian",
                       prediction_zero = p0,Sigma=Sigma,mu=mu)

cutoff_feats <- paste0("V",1:7)
testObs_computed <- 5

full_pred <- predict(model,x_explain)[5]
p0 <- mean(y_train)
pred_not_to_decompose <- sum(expl$shapley_values[5,V8:V9])

### Need to create an lm analogoue to pred_mod_xgb here


run <- iterative_kshap_func(model,x_explain,x_train,
                            testObs_computed = 5,
                            cutoff_feats = cutoff_feats,
                            full_pred = full_pred,
                            pred_not_to_decompose = pred_not_to_decompose,
                            p0 = p0,
                            predict_model = predict.lm)

### en eller annen bug her, finn ut av det...


run <- iterative_kshap_func(model,x_explain,x_train,
                            testObs_computed = 5,
                            cutoff_feats = cutoff_feats,
                            full_pred = full_pred,
                            pred_not_to_decompose = pred_not_to_decompose,
                            p0 = p0,
                            predict_model = predict.lm)

