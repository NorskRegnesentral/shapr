
#### Script for a simulation study with mixed data (i.e. categorical + continuous) ####

# Assuming a linear model

library(shapr)
library(data.table)

response_mod <- function(mod_matrix_full, beta, epsilon){
  as.vector(mod_matrix_full %*% beta) + epsilon
}


no_cont_var <- 2
no_cat_var <- 2
no_levels <- 3

no_tot_var <- no_cont_var + no_cat_var

Sigma_diag = 1
corr = 0.2
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


dt[, (cat_cols) := lapply(.SD, as.factor),.SDcols = cat_cols]

train_obs <- 1:No_train_obs
test_obs <- (No_train_obs+1):(No_train_obs+No_test_obs)


## 2. One-hot encoding of data
mod_matrix <- model.matrix(~.-1, data = dt[, ..feat_names], contrasts.arg = lapply(dt[, ..cat_cols], contrasts, contrasts = FALSE))

dt <- cbind(dt, data.table(mod_matrix[,-(1:no_cont_var)])) # Adding the non-continuous columns to the original dt
full_onehot_names <- colnames(mod_matrix)
reduced_onehot_names <- full_onehot_names[-grep("_1$", full_onehot_names)] # names without reference levels

## 3. Calculate response
dt[, response := response_mod(mod_matrix_full = cbind(1, mod_matrix), beta = beta, epsilon = epsilon)]

## 4. Fit model


form <- as.formula(paste0("response~", paste(feat_names, collapse = "+")))
model <- lm(formula = form, data = dt[train_obs, ])


## 5. initalize shapr object with trained model -- this is used for calculating true shapley
x_train <- dt[train_obs, ..feat_names] ## used in explainer()
x_test <- dt[test_obs, ..feat_names] ## used in cond_expec_mat()
y_train <- dt[train_obs, .(response)] ## used in cond_expec_mat()

x_test_onehot_full <- dt[test_obs, ..full_onehot_names]

x_test_onehot_reduced <- dt[test_obs, ..reduced_onehot_names]
x_train_onehot_reduced <- dt[train_obs, ..reduced_onehot_names]

##
explainer <- shapr(x_train, model) # print(class(model)) # "lm"

#### 6. calculate the true shapley values

### TO DO
# Define C vector/matricies for every combination
# Implement the arellano-valle function

S <- explainer$S

ind_cont_cols <- which(names(x_train) %in% cont_cols)
ind_cat_cols <- which(names(x_train) %in% cat_cols)

case_matrix <- NA*S

for (i in 2:(nrow(S)-1)){
  S_i <- which(as.logical(S[i,]))
  S_is_cont <-  any(S_i %in% ind_cont_cols)
  S_is_cat <-  any(S_i %in% ind_cat_cols)
  S_is_cont_and_cat <- as.logical(S_is_cont*S_is_cat)

  if(S_is_cont_and_cat){
    case_matrix[i,ind_cont_cols] <- 5
    case_matrix[i,ind_cat_cols] <- 6
  } else {
    if(S_is_cont){
      case_matrix[i,ind_cont_cols] <- 1
      case_matrix[i,ind_cat_cols] <- 4
    }
    if(S_is_cat){
      case_matrix[i,ind_cont_cols] <- 2
      case_matrix[i,ind_cat_cols] <- 3
    }
  }
}

eps <- 10^-6
x_test_C_lower <- copy(x_test)
x_test_C_lower[,(cont_cols):= lapply(.SD,function(x){x-eps}),.SDcols=cont_cols]
x_test_C_lower[,(cat_cols):= lapply(.SD,function(x){cat_cutoff[as.numeric(x)]}),.SDcols=cat_cols]

x_test_C_upper <- copy(x_test)
x_test_C_upper[,(cont_cols):= lapply(.SD,function(x){x+eps}),.SDcols=cont_cols]
x_test_C_upper[,(cat_cols):= lapply(.SD,function(x){cat_cutoff[as.numeric(x)+1]}),.SDcols=cat_cols]


# Just some testing values
C_lower <- x_test_C_lower[1,-1]
C_upper <- x_test_C_upper[1,-1]
Omega <- Sigma
xi <- rep(0,4)
algorithm <- mvtnorm::Miwa(steps = 128)

dens_x_given_S_is_C_func <- function(x,C_lower,C_upper,xi,Omega,algorithm) {
  # Formula in equation (13) in this paper
  # https://www.jstor.org/stable/pdf/20445223.pdf?refreqid=excelsior%3A9fdbaaf0a8fe22e64418448ad4f8090b
  # letting V = x, and U correspond to the dimensions specified in C
  # C_lower is a vector of length dim with lower bounds for each dimension, C_upper similalry contains the upper bounds
  # Omega is the joint covariance matrix of x and the length of C_lower and C_upper (dim)
  # xi is the mean vector of x and the length of C_lower and C_upper (dim)
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

x_vec <- seq(-3,3,0.01)
start <- proc.time()
val <- vec_dens_x_given_S_is_C_func(x_vec,
                                    C_lower, C_upper,xi,Omega,algorithm)
end <- proc.time()
end-start

x_vec <- seq(-3,3,0.01)
start <- proc.time()
val2 <- vec_compute_dens_x_given_S_is_C_func(x_vec,prep_list)
end <- proc.time()
end-start

all.equal(val,val2)
# The prep/compute version is twive as fast
