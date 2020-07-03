### Testing independence between groups

library(data.table)
library(shapr)

cond_expec_new_cont = function(S,x_test,mu,Sigma,fitted_beta){

  dim = ncol(S)
  cond_expec_mat0 = matrix(NA,nrow = nrow(x_test),ncol=nrow(S))
  for (i in 2:(nrow(S)-1)){

    given.inds = which(S[i,]==1)
    integrate.inds = which(S[i,]==0)

    Sigma_given = Sigma[given.inds,given.inds]
    #      Sigma_integrate = Sigma[integrate.inds,integrate.inds]
    Sigma_integrate_given = Sigma[integrate.inds,given.inds,drop=F]

    mu_given = mu[given.inds]
    mu_integrate = mu[integrate.inds]

    x_test_given = as.matrix(x_test)[,given.inds]
    x_test_given = matrix(x_test_given,nrow=length(given.inds),byrow = T)

    #      Sigma_cond = Sigma_integrate - Sigma_integrate_given%*%solve(Sigma_given)%*%t(Sigma_integrate_given)
      mu_cond = mu_integrate + Sigma_integrate_given%*%solve(Sigma_given)%*%(x_test_given - mu_given)

    mu_cond_full = matrix(NA,ncol=nrow(x_test),nrow = dim)
    mu_cond_full[integrate.inds,] = mu_cond
    mu_cond_full[given.inds,] = x_test_given
    cond_expec_mat0[,i] = as.vector(t(fitted_beta)%*%rbind(1,mu_cond_full))

  }

  pred_zero = as.vector(t(fitted_beta)%*%c(1,mu))

  cond_expec_mat0[,1] = pred_zero

  cond_expec_mat0[,nrow(S)] = predict(model,x_test)

  return(as.data.table(cond_expec_mat0))
}


# parameters
dim <- 4
mu <- rep(0, dim)
Sample_test <- TRUE
No_train_obs <- 1000
no_beta_samp = 10
No_test_sample <- 10
N_sample_gaussian <- 1000
noise <- TRUE
response_mod <- function(mod_matrix_full, beta, epsilon){
  as.vector(mod_matrix_full %*% beta) + epsilon
}
fit_mod <- "regression"
methods <- c("empirical", "gaussian", "ctree")
seed <- 1
Sigma_diag <- 1


group2 <- list(c('feat_1_',"feat_2_", 'feat_4_'), c( 'feat_3_'))

feat_names = sort(unlist(group2))
group_num <- lapply(group2,FUN = function(x){match(x, feat_names)})
group_vec = rep(NA,dim)
for (i in 1:length(group2)){
  group_vec[group_num[[i]]] = i
}

cc = 0.4

Sigma <- matrix(rep(cc, dim^2), nrow = dim, ncol = dim)
for (i in 1:dim){
  for(j in 1:dim){
    if (group_vec[i]!=group_vec[j]){
      Sigma[i,j] = 0
    } else {
      Sigma[i,j] = cc
    }
  }
}
diag(Sigma) = Sigma_diag

l = 1
set.seed(l);  beta <- round(rnorm(dim+1), 1)
#beta[1] = 0

## 1. simulate training data
set.seed(seed)
x <- MASS::mvrnorm(n =  No_train_obs, mu = mu, Sigma = Sigma)

dt <- as.data.table(x)
set.seed(seed+1)
x_test = MASS::mvrnorm(n =  No_test_sample, mu = mu, Sigma = Sigma)
x_test_dt = as.data.table(x_test)

No_test_obs <- nrow(x_test_dt)
dt <- data.table(rbind(dt, x_test_dt))
setnames(dt, names(dt), paste0("feat_", 1:dim, "_"))
feat_names <- names(dt[, 1:dim])

set.seed(seed)
if(noise == TRUE){
  epsilon1 <- rnorm(No_train_obs, 0, 0.1^2)
  epsilon2 <- rnorm(No_test_obs, 0, 0.1^2)
  epsilon <- c(epsilon1, epsilon2)

  dt[, epsilon := epsilon]
} else{
  dt[, epsilon := 0]
}

x_traintest_mat = as.matrix(dt[,..feat_names])

## 3. Calculate response
dt[, response := response_mod(mod_matrix_full = cbind(1, x_traintest_mat), beta = beta, epsilon = epsilon)]

## 4. Fit model
if(fit_mod == 'regression'){
  form <- as.formula(paste0("response ~", paste(feat_names, collapse = "+")))
  model <- lm(formula = form, data = dt[(1:No_train_obs), ])
}
fitted_beta = model$coefficients

## 5. initalize shapr object with trained model -- this is used for calculating true shapley
x_train <- dt[(1:No_train_obs), ..feat_names]
x_test <- dt[-(1:No_train_obs), ..feat_names]
y_train <- dt[(1:No_train_obs), .(response)]


explainer <- shapr(x_train, model)

## Start grouping stuff
group1 <- list(c(1), # no groups at all
               c(2),
               c(3),
               c(4))
group1_names = lapply(group1, function(x){names(x_test)[x]})
explainer_group1 <- shapr(x_train, model, group = group1_names)


#group2_names = lapply(group2, function(x){names(x_test)[x]})
group2_names <- group2
explainer_group2 <- shapr(x_train, model, group = group2_names)

#### Here I need to compute the conditional expectation matrix


cond_expec_mat0 = cond_expec_new_cont(S = explainer$S,
                                      x_test = x_test,
                                      mu = mu,
                                      Sigma = Sigma,
                                      fitted_beta = fitted_beta)

# no grouping - used as a test
Kshap0 <- matrix(0, nrow = nrow(x_test), ncol = nrow(explainer$W))
for (i in 1:nrow(x_test)) {
  Kshap0[i, ] = explainer$W %*% t(as.matrix(cond_expec_mat0[i, ]))
}
Kshap0 <- data.table(Kshap0)
dim <- ncol(x_test)
setnames(Kshap0, 1:(dim + 1), c("none", names(x_test)))

# no ACTUAL grouping but use groups - used as a test
cond_expec_mat1 = cond_expec_new_cont(S = explainer_group1$S,
                                      x_test = x_test,
                                      mu = mu,
                                      Sigma = Sigma,
                                      fitted_beta = fitted_beta)


Kshap1 <- matrix(0, nrow = nrow(x_test), ncol = nrow(explainer_group1$W))
for (i in 1:nrow(x_test)) {
  Kshap1[i, ] = explainer_group1$W %*% t(as.matrix(cond_expec_mat1[i, ]))
}
Kshap1 <- data.table(Kshap1)
setnames(Kshap1, 1:(length(group1_names)+1), c("none", paste0("group", 1:length(group1_names))))

# grouping actually starts
cond_expec_mat2 = cond_expec_new_cont(S = explainer_group2$S,
                                      x_test = x_test,
                                      mu = mu,
                                      Sigma = Sigma,
                                      fitted_beta = fitted_beta)

Kshap2 <- matrix(0, nrow = nrow(x_test), ncol = nrow(explainer_group2$W))
for (i in 1:nrow(x_test)) {
  Kshap2[i, ] = explainer_group2$W %*% t(as.matrix(cond_expec_mat2[i, ]))
}
Kshap2 <- data.table(Kshap2)
setnames(Kshap2, 1:(length(group2_names)+1), c("none", paste0("group", 1:length(group2_names))))


### Direct approaches using exact Gaussian dist

prediction_zero = sum(fitted_beta*c(1,mu))

direct_kshap2 = explain(x_test,explainer_group2,
                        approach = "gaussian",
                        mu=mu,
                        cov_mat = Sigma,
                        n_samples = 10^6,
                        prediction_zero = prediction_zero)

direct_kshap0 = explain(x_test,explainer,
                        approach = "gaussian",
                        mu=mu,
                        cov_mat = Sigma,
                        n_samples = 10^6,
                        prediction_zero = prediction_zero)


##  -------------------------------------------

Kshap0_dt <- data.table(Kshap0)
names <- NULL
for(i in 1:length(group2)){
  names <- c(names, paste0('group', i))
  Kshap0_dt[, paste0('group', i) := rowSums(.SD), .SDcols = group2[[i]]]
  direct_kshap0$dt[, paste0('group', i) := rowSums(.SD), .SDcols = group2[[i]]]
}
feat_names = names(Kshap0)[-1]
results0 <- data.table(correlation = cc,
                       base_MAE = mean(rowMeans(abs(Kshap0 - Kshap1)[,-1])),
                       group_MAE = mean(rowMeans(abs(Kshap0_dt[,..names] - Kshap2[,-1]))),
                       direct_MAE = mean(rowMeans(abs(Kshap0[,-1] - direct_kshap0$dt[,..feat_names]))),
                       direct_group_MAE = mean(rowMeans(abs(Kshap0_dt[,..names] - direct_kshap2$dt[,-1]))))
for(i in 1:length(group2)){
  results0[, paste0('group', i) := paste0(group2[[i]], collapse = ", ") ]
}
results0[,beta:=list(beta)]

results0
Kshap0_dt
direct_kshap0$dt

Kshap2
direct_kshap2$dt

