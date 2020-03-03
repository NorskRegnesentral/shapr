
#### Script for a simulation study with mixed data (i.e. categorical + continuous) ####

# Assuming a linear model

library(shapr)
library(data.table)
library(MASS)

response_mod <- function(mod_matrix_full, beta, epsilon){
  as.vector(mod_matrix_full %*% beta) + epsilon
}


no_cont_var <- 2
no_cat_var <- 2
no_levels <- 3

no_tot_var <- no_cont_var + no_cat_var

Sigma_diag = 1
corr = 0.5
mu = rep(0, no_tot_var)
beta_0 <- 1
beta_cont <- c(1,-1)
beta_cat <- c(1,0,-1,
              2,3,-1)

beta <- c(beta_0,beta_cont,beta_cat)
N_shapley = 10000
No_train_obs = 100
No_test_obs = 20
cat_cutoff = c(-200, 0, 1, 200)
noise = FALSE
name = 'testing'
seed <- 123



# Basic tests #

if(length(beta)!=1+no_cont_var + no_cat_var*no_levels){
  stop("Incorrect length for beta")
}
if(length(mu)!=no_cont_var + no_cat_var){
  stop("Incorrect length for mu")
}
if(length(cat_cutoff)!=no_levels+1){
  stop("Incorrect length of cat_cutoff")
}



# Preparing covariate sampling distribution
Sigma <- matrix(corr, no_tot_var, no_tot_var)
diag(Sigma) <- Sigma_diag

set.seed(seed)
x_train <- mvrnorm(n =  No_train_obs, mu = mu, Sigma = Sigma)

dt_train <- data.table(x_train[,1:no_cont_var])
for (i in (no_cont_var+1):no_tot_var){
  dt_train <- cbind(dt_train, cut(x_train[, i], cat_cutoff, labels = 1:no_levels))
}

names(dt_train) <- c(paste0("cont_",1:no_cont_var,"_"),paste0("cat_",1:no_cat_var,"_"))

if(noise == TRUE){
  dt_train[, epsilon := rnorm(No_train_obs, 0, 0.1^2)] #
} else{
  dt_train[, epsilon := 0]
}


x_test <- mvrnorm(n =  No_test_obs, mu = mu, Sigma = Sigma)

dt_test <- data.table(x_test[,1:no_cont_var])
for (i in (no_cont_var+1):no_tot_var){
  dt_test <- cbind(dt_test, cut(x_test[, i], cat_cutoff, labels = 1:no_levels))
}

names(dt_test) <- c(paste0("cont_",1:no_cont_var,"_"),paste0("cat_",1:no_cat_var,"_"))

if(noise == TRUE){
  dt_test[, epsilon := rnorm(No_test_obs, 0, 0.1^2)] #
} else{
  dt_test[, epsilon := 0]
}

dt <- rbind(dt_train,dt_test)

cont_cols <- names(dt)[grep("cont",names(dt))]
cat_cols <- names(dt)[grep("cat",names(dt))]
feat_names <- c(cont_cols,cat_cols)

dt_numeric <- copy(dt)
dt_numeric[, (cat_cols) := lapply(.SD, as.numeric),.SDcols = cat_cols]


dt[, (cat_cols) := lapply(.SD, as.factor),.SDcols = cat_cols] # It is actually already a factor

train_obs <- 1:No_train_obs
test_obs <- (No_train_obs+1):(No_train_obs+No_test_obs)


## 2. One-hot encoding of data
mod_matrix <- model.matrix(~.-1, data = dt[, ..feat_names], contrasts.arg = lapply(dt[, ..cat_cols], contrasts, contrasts = FALSE))

dt <- cbind(dt, data.table(mod_matrix[,-(1:no_cont_var)])) # Adding the non-continuous columns to the original dt
full_onehot_names <- colnames(mod_matrix)
reduced_onehot_names <- full_onehot_names[-grep("_1$", full_onehot_names)] # names without reference levels

## 3. Calculate response
dt[, response := response_mod(mod_matrix_full = cbind(1, mod_matrix), beta = beta, epsilon = epsilon)]
dt_numeric[, response := dt[['response']]]

## 4. Fit model


form <- as.formula(paste0("response~", paste(feat_names, collapse = "+")))
model <- lm(formula = form, data = dt[train_obs, ])


## 5. initalize shapr object with trained model -- this is used for calculating true shapley
x_train <- dt[train_obs, ..feat_names] ## used in explainer()
x_test <- dt[test_obs, ..feat_names] ## used in cond_expec_mat()
y_train <- dt[train_obs, .(response)] ## used in cond_expec_mat()

x_train_numeric <- dt_numeric[train_obs, ..feat_names] ## used in explainer()
x_test_numeric <- dt_numeric[test_obs, ..feat_names] ## used in cond_expec_mat()


x_test_onehot_full <- dt[test_obs, ..full_onehot_names]

x_test_onehot_reduced <- dt[test_obs, ..reduced_onehot_names]
x_train_onehot_reduced <- dt[train_obs, ..reduced_onehot_names]

##
explainer <- shapr(x_train, model) # print(class(model)) # "lm"

## NEW
# Create custom function of model_type for lm
model_type.numeric_lm <- function(x) {
}

features.numeric_lm <- function(x, cnms, feature_labels = NULL) {
  if (!is.null(feature_labels)) message_features_labels()

  nms <- tail(all.vars(x$terms), -1)
  if (!all(nms %in% cnms)) error_feature_labels()
  return(nms)
}

# Create custom function of predict_model for caret
predict_model.numeric_lm <- function(x, newdata) {
  newdata <- as.data.table(newdata)

  cat_cols <- names(newdata)[grep("cat",names(newdata))]
  newdata[, (cat_cols) := lapply(.SD, as.factor),.SDcols = cat_cols]

#  newdata0 <- newdata[, lapply(.SD, as.factor)]
  class(x) <- "lm"
  predict(x, newdata)
}

model_numeric <- model
class(model_numeric) <- "numeric_lm"
explainer_numeric <- shapr(x_train_numeric, model_numeric)
## END



#### 6. calculate the true shapley values

### TO DO
# Define C vector/matricies for every combination
# Implement the arellano-valle function

S <- explainer$S

ind_cont_cols <- which(names(x_train) %in% cont_cols)
ind_cat_cols <- which(names(x_train) %in% cat_cols)

ind_cont_cols_logical <- names(x_train) %in% cont_cols
ind_cat_cols_logical <- names(x_train) %in% cat_cols


case_matrix <- NA*S

for (i in 2:(nrow(S)-1)){
  S_i <- which(as.logical(S[i,]))
  S_is_cont <-  any(S_i %in% ind_cont_cols)
  S_is_cat <-  any(S_i %in% ind_cat_cols)
  S_is_cont_and_cat <- as.logical(S_is_cont*S_is_cat)
  Sbar_i_logical <- as.logical(1-S[i,])

  if(S_is_cont_and_cat){
    case_matrix[i,which(ind_cont_cols_logical &Sbar_i_logical)] <- 5
    case_matrix[i,which(ind_cat_cols_logical &Sbar_i_logical)] <- 6
  } else {
    if(S_is_cont){
      case_matrix[i,which(ind_cont_cols_logical &Sbar_i_logical)] <- 1
      case_matrix[i,which(ind_cat_cols_logical &Sbar_i_logical)] <- 4
    }
    if(S_is_cat){
      case_matrix[i,which(ind_cont_cols_logical &Sbar_i_logical)] <- 2
      case_matrix[i,which(ind_cat_cols_logical &Sbar_i_logical)] <- 3
    }
  }
}

eps <- 10^-6 # Very low sensitivity to this value, just don't set it smaller than 10^-6 as that might give NaN's
x_test_C_lower <- copy(x_test)
x_test_C_lower[,(cont_cols):= lapply(.SD,function(x){x-eps}),.SDcols=cont_cols]
x_test_C_lower[,(cat_cols):= lapply(.SD,function(x){cat_cutoff[as.numeric(x)]}),.SDcols=cat_cols]

x_test_C_upper <- copy(x_test)
x_test_C_upper[,(cont_cols):= lapply(.SD,function(x){x+eps}),.SDcols=cont_cols]
x_test_C_upper[,(cat_cols):= lapply(.SD,function(x){cat_cutoff[as.numeric(x)+1]}),.SDcols=cat_cols]



dens_x_given_S_is_C_func <- function(x,C_lower,C_upper,xi,Omega,algorithm) {
  # Formula in equation (13) in this paper
  # https://www.jstor.org/stable/pdf/20445223.pdf?refreqid=excelsior%3A9fdbaaf0a8fe22e64418448ad4f8090b
  # letting V = x, and U correspond to the dimensions specified in C
  # C_lower is a vector of length dim with lower bounds for each dimension, C_upper similalry contains the upper bounds
  # Omega is the joint covariance matrix of x and the length of C_lower and C_upper (dim)
  # xi is the joint mean vector of x and the length of C_lower and C_upper (dim)
  # Note: x is always one dimensional

  these_U <- (1:length(C_lower))+1
  these_V <- 1

  Omega_U <- Omega[these_U,these_U,drop=F]
  Omega_V <- Omega[these_V,these_V,drop=F]
  Delta <- Omega[these_V,these_U,drop=F]

  xi_U <- xi[these_U]
  xi_V <- xi[these_V]

  C_lower <- unlist(C_lower)
  C_upper <- unlist(C_upper)

  mean_above <- as.vector(t(Delta)%*%solve(Omega_V)%*%(x-xi_V) + xi_U)
  sigma_above <- Omega_U - t(Delta)%*%solve(Omega_V)%*%Delta

  mean_below <- xi_U
  sigma_below <- Omega_U


  above <- mvtnorm::pmvnorm(lower = C_lower,upper = C_upper,
                            mean = mean_above,
                            sigma = sigma_above,
                            algorithm = algorithm)


  below <- mvtnorm::pmvnorm(lower = C_lower,upper = C_upper,
                            mean = mean_below,
                            sigma = sigma_below,
                            algorithm = algorithm)

  left <- dnorm(x,mean=xi_V,sd = sqrt(Omega_V))

  dens <- left*above/below

  return(dens)

}

# Splitting the main function in two to make it more efficient
# Here is the preparation function
prep_dens_x_given_S_is_C_func <- function(C_lower,C_upper,xi,Omega,algorithm) {

  these_U <- (1:length(C_lower))+1
  these_V <- 1

  Omega_U <- Omega[these_U,these_U,drop=F]
  Omega_V <- Omega[these_V,these_V,drop=F]
  Delta <- Omega[these_V,these_U,drop=F]

  xi_U <- xi[these_U]
  xi_V <- xi[these_V]

  C_lower <- unlist(C_lower)
  C_upper <- unlist(C_upper)

  mean_above_mult <- t(Delta)%*%solve(Omega_V)
  mean_above_add <- xi_U - t(Delta)%*%solve(Omega_V)%*%xi_V
  sigma_above <- Omega_U - t(Delta)%*%solve(Omega_V)%*%Delta

  mean_below <- xi_U
  sigma_below <- Omega_U

  below <- mvtnorm::pmvnorm(lower = C_lower,upper = C_upper,
                            mean = mean_below,
                            sigma = sigma_below,
                            algorithm = algorithm)


  left_mean <- xi_V
  left_sd <- sqrt(Omega_V)

  ret <- list(algorithm = algorithm,
              C_lower = C_lower,
              C_upper = C_upper,
              mean_above_mult = mean_above_mult,
              mean_above_add = mean_above_add,
              sigma_above = sigma_above,
              below = below,
              left_mean = left_mean,
              left_sd = left_sd)

  return(ret)

}

prep_dens_x_given_S_is_C_func_v2 <- function(C_lower, C_upper,xi,Omega,algorithm) {

  C_lower <- unlist(C_lower)
  C_upper <- unlist(C_upper)

  these_U <- (1:length(C_lower))+1
  these_V <- 1

  Omega_U <- Omega[these_U,these_U,drop=F]
  Omega_V <- Omega[these_V,these_V,drop=F]
  Delta <- Omega[these_V,these_U,drop=F]

  xi_U <- xi[these_U]
  xi_V <- xi[these_V]


  mean_above_mult <- t(Delta)%*%solve(Omega_V)
  mean_above_add <- xi_U - t(Delta)%*%solve(Omega_V)%*%xi_V
  sigma_above <- Omega_U - t(Delta)%*%solve(Omega_V)%*%Delta

  mean_below <- xi_U
  sigma_below <- Omega_U

  below <- mvtnorm::pmvnorm(lower = C_lower,upper = C_upper,
                            mean = mean_below,
                            sigma = sigma_below,
                            algorithm = algorithm)


  left_mean <- xi_V
  left_sd <- sqrt(Omega_V)

  ret <- list(algorithm = algorithm,
              C_lower = C_lower,
              C_upper = C_upper,
              mean_above_mult = mean_above_mult,
              mean_above_add = mean_above_add,
              sigma_above = sigma_above,
              below = below,
              left_mean = left_mean,
              left_sd = left_sd)

  return(ret)

}


# Here is the computation function, taking the preparation values as input
compute_dens_x_given_S_is_C_func <- function(x,ret_list) {

  mean_above <- as.vector(ret_list$mean_above_mult%*%x+ret_list$mean_above_add)


  above <- mvtnorm::pmvnorm(lower = ret_list$C_lower,upper = ret_list$C_upper,
                            mean = mean_above,
                            sigma = ret_list$sigma_above,
                            algorithm = algorithm)

  left <- dnorm(x,mean=ret_list$left_mean,sd = ret_list$left_sd)

  dens <- left*above/ret_list$below

  return(dens)

}


# Vectorizing the two functions and checking that they give the same result
vec_dens_x_given_S_is_C_func <- Vectorize(dens_x_given_S_is_C_func,vectorize.args="x")

vec_compute_dens_x_given_S_is_C_func = Vectorize(compute_dens_x_given_S_is_C_func,vectorize.args = "x")

vec_compute_dens_x_given_S_is_C_func_2 = Vectorize(compute_dens_x_given_S_is_C_func)

#aa=outer(x_int_grid,prep_list_all_x_test_C,FUN=vec_compute_dens_x_given_S_is_C_func_2)

vec_compute_dens_x_given_S_is_C_func_rev <- function(ret_list,x){
  vec_compute_dens_x_given_S_is_C_func(x,ret_list)
}

range_x_int <- c(min(mu)-4*sqrt(max(diag(Sigma))),max(mu)+4*sqrt(max(diag(Sigma))))
no_int_eval <- 500
h <- diff(range_x_int)/no_int_eval
x_int_grid <- seq(range_x_int[1]+h/2,range_x_int[2]-h/2,by=h)

x_int_grid_cat <- as.numeric(cut(x_int_grid, cat_cutoff, labels = c(1:no_levels)))


### Restructuring the beta vector per feature
beta_list <- list()
for(i in 1:no_cont_var){
  beta_list[[i]] <- beta_cont[i]
}
k <- 1
for(i in (no_cont_var+1):no_tot_var){
  beta_list[[i]] <- beta_cat[(k-1)*no_levels+(1:no_levels)]
  k <- k + 1
}

# Restructuring one_hot test data per feature
x_test_onehot_full_list <- list()
for(i in 1:no_cont_var){
  x_test_onehot_full_list[[i]] <- x_test_onehot_full[,..i]
}
k <- 1
for(i in (no_cont_var+1):no_tot_var){
  this_col <- no_cont_var+(k-1)*no_levels+(1:no_levels)
  x_test_onehot_full_list[[i]] <- x_test_onehot_full[,..this_col]
  k <- k + 1
}

phi_0_contrib <- rep(NA,no_tot_var)
for(i in 1:no_cont_var){
  phi_0_contrib[i] <- beta_list[[i]]*mu[i]
}
for(i in (no_cont_var+1):no_tot_var){
  phi_0_contrib[i] <- t(beta_list[[i]])%*%diff(pnorm(q = cat_cutoff,mean = mu[i], sd=sqrt(Sigma[i,i])))
}

phi0 <- beta[1] + sum(phi_0_contrib)


Vs_mat <- matrix(NA,ncol = No_test_obs, nrow = nrow(S))

Vs_mat[1,] <-   phi0
Vs_mat[nrow(S),] <- predict(model,x_test)


for (i in 2:(nrow(S)-1)){
  S_i <-   which(as.logical(S[i,]))
  Sbar_i <-   which(as.logical(1-S[i,]))

  S_features <- feat_names[S_i]

  x_test_S_i <- x_test[,..S_features]

  x_test_C_lower_S_i_list <- split(x_test_C_lower[,..S_features],f=1:No_test_obs)
  x_test_C_upper_S_i_list <- split(x_test_C_upper[,..S_features],f=1:No_test_obs)

  Vs_sum_contrib_mat <- matrix(NA,ncol=no_tot_var,nrow=No_test_obs)

  for (j in Sbar_i){
    j_is_cont <- j %in% ind_cont_cols

    Omega <- Sigma[c(j,S_i),c(j,S_i)]
    xi <- mu[c(j,S_i)]

    prep_list_all_x_test_C <- mapply(prep_dens_x_given_S_is_C_func,
                                     C_lower =x_test_C_lower_S_i_list,
                                     C_upper = x_test_C_upper_S_i_list,
                                     MoreArgs = list(xi = xi, Omega = Omega, algorithm = algorithm),
                                     SIMPLIFY = FALSE)

    intval_list_no_x=parallel::mclapply(X = prep_list_all_x_test_C,FUN = vec_compute_dens_x_given_S_is_C_func_rev,x=x_int_grid,
                                        mc.cores = 6)

  if (j_is_cont){
    ## Continuous expectation
    expectation_vec <- rep(NA,No_test_obs)
    for(k in 1:No_test_obs){
      expectation_vec[k] <- h*sum(intval_list_no_x[[k]]*x_int_grid)
    }
    Vs_sum_contrib_vec <- beta_list[[j]]*expectation_vec


  } else {
    # categorical expectation
    prob_mat <- matrix(NA,nrow=No_test_obs,ncol=no_levels)
    for(k in 1:No_test_obs){
      for (l in 1:no_levels){
        prob_mat[k,l] <- h*sum(intval_list_no_x[[k]][x_int_grid_cat==l])
      }
      prob_mat[k,] <- prob_mat[k,]/sum(prob_mat[k,])
    }
    Vs_sum_contrib_vec <- as.vector(beta_list[[j]]%*%t(prob_mat))

  }

    Vs_sum_contrib_mat[,j] <- Vs_sum_contrib_vec


  }

  for (j in S_i){
    Vs_sum_contrib_mat[,j] <- beta_list[[j]]%*%t(x_test_onehot_full_list[[j]])
  }
  Vs_mat[i,] <- rowSums(Vs_sum_contrib_mat)
  print(i)
}

exactShap <- matrix(NA,ncol=no_tot_var+1,nrow=No_test_obs)
for (i in 1:No_test_obs){
  exactShap[i,] <- c(explainer$W %*% Vs_mat[,i])
}

exactShap

max(abs(rowSums(exactShap)-predict(model,x_test))) #

#### Code for computing exact shapley values is done!
# Not tested properly though
# Testing when the features are independent

xj_start_mat <- mod_matrix[test_obs,]
Exj_vec <- c(0,0,diff(pnorm(q = cat_cutoff,mean = mu[1], sd=sqrt(Sigma[1,1]))),diff(pnorm(q = cat_cutoff,mean = mu[1], sd=sqrt(Sigma[1,1]))))

# True shapley values Assuming independence
tab <- t(beta[-1]*t(xj_start_mat- matrix(Exj_vec,ncol=length(Exj_vec),nrow=nrow(xj_start_mat),byrow = T)))
SHAP <- cbind(phi0,tab[,1:2],rowSums(tab[,3:5]),rowSums(tab[,6:8]))

rowSums(SHAP)
predict(model,x_test)

exactShap-SHAP # Close enough!
# phi0     cont_1_     cont_2_
# 101 -4.971156e-07 0.002440432 0.002440431 0.0004880859 -0.005368950
# 102 -4.971156e-07 0.002440431 0.002440431 0.0004880860 -0.005368949
# 103 -4.971156e-07 0.002440431 0.002440431 0.0004880860 -0.005368949
# 104 -4.971156e-07 0.002440431 0.002440431 0.0004880859 -0.005368949
# 105 -4.971156e-07 0.002440431 0.002440431 0.0004880861 -0.005368949
# 106 -4.971156e-07 0.002440431 0.002440431 0.0004880862 -0.005368950
# 107 -4.971156e-07 0.002440431 0.002440431 0.0004880858 -0.005368949
# 108 -4.971156e-07 0.002440432 0.002440431 0.0004880859 -0.005368950
# 109 -4.971156e-07 0.002440431 0.002440432 0.0004880859 -0.005368950
# 110 -4.971156e-07 0.002440432 0.002440431 0.0004880861 -0.005368950
# 111 -4.971156e-07 0.002440432 0.002440431 0.0004880861 -0.005368950
# 112 -4.971156e-07 0.002440432 0.002440431 0.0004880859 -0.005368949
# 113 -4.971156e-07 0.002440431 0.002440431 0.0004880859 -0.005368949
# 114 -4.971156e-07 0.002440431 0.002440431 0.0004880862 -0.005368949
# 115 -4.971156e-07 0.002440431 0.002440431 0.0004880860 -0.005368949
# 116 -4.971156e-07 0.002440431 0.002440431 0.0004880863 -0.005368950
# 117 -4.971156e-07 0.002440432 0.002440431 0.0004880860 -0.005368950
# 118 -4.971156e-07 0.002440432 0.002440431 0.0004880857 -0.005368950
# 119 -4.971156e-07 0.002440432 0.002440431 0.0004880861 -0.005368950
# 120 -4.971156e-07 0.002440431 0.002440431 0.0004880859 -0.005368949

#### Estimating the Shapley values ####

p <- mean(y_train$response) # since y_train is no longer a matrix


methods = c("ctree","kernelShap")

explanation_list <- list()
m = "ctree"
explanation_list[[m]] <- explain(
  x_test,
  approach = m,
  explainer = explainer,
  prediction_zero = p,
  sample = FALSE,
  w_threshold = 1,
  mincriterion = 0.95)

m = "kernelShap"
explanation_list[[m]] <- explain(
    x_test_numeric,
    approach = "empirical",
    type = "independence",
    explainer = explainer_numeric,
    prediction_zero = p,
    sample = FALSE,
    w_threshold = 1,
    mincriterion = 0.95)

exactShap
explanation_list$ctree$dt
explanation_list$kernelShap$dt

# also, probably needs speedup for simplified calling of density function for one large (500 or 1000) specific set of x-values

MAE <- function(true_shapley, shapley_method, weights = rep(1/nrow(true_shapley),nrow(true_shapley))){
  mean(apply(abs((true_shapley - shapley_method) * weights), 2, sum)[-1])
}


MAE(exactShap,explanation_list$ctree$dt)
MAE(exactShap,explanation_list$kernelShap$dt)


#### Testing the functions -- and they work! ####

# Just some testing values
C_lower <- x_test_C_lower[1,-1]
C_upper <- x_test_C_upper[1,-1]
Omega <- Sigma
xi <- rep(0,4)
algorithm <- mvtnorm::Miwa(steps = 128)


x_vec <- seq(-3,3,0.01)
start <- proc.time()
val <- vec_dens_x_given_S_is_C_func(x_vec,
                                    C_lower, C_upper,xi,Omega,algorithm)
end <- proc.time()
end-start

x_vec <- seq(-3,3,0.01)
start <- proc.time()
prep_list <- prep_dens_x_given_S_is_C_func(C_lower,C_upper,xi,Omega,algorithm=mvtnorm::Miwa(steps = 128))
val2 <- vec_compute_dens_x_given_S_is_C_func(x_vec,prep_list)
end <- proc.time()
end-start

x_vec <- seq(-3,3,0.01)
start <- proc.time()
prep_list <- prep_dens_x_given_S_is_C_func(C_lower,C_upper,xi,Omega,algorithm=mvtnorm::Miwa(steps = 256))
val3 <- vec_compute_dens_x_given_S_is_C_func(x_vec,prep_list)
end <- proc.time()
end-start

x_vec <- seq(-3,3,0.01)
start <- proc.time()
prep_list <- prep_dens_x_given_S_is_C_func(C_lower,C_upper,xi,Omega,algorithm=mvtnorm::GenzBretz())
val4 <- vec_compute_dens_x_given_S_is_C_func(x_vec,prep_list)
end <- proc.time()
end-start


all.equal(val,val2)
# The prep/compute version is twive as fast

all.equal(val2,val3)
# number of step with Miwa does not matter

plot(x_vec,val2,type="l")
lines(x_vec,val4,col=2,lty=2)



