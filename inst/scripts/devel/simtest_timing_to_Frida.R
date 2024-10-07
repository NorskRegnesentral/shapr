library(data.table)
library(MASS)
library(Matrix)
library(shapr)
library(future)
library(xgboost)

shapley_threshold_prob <- 0.2
shapley_threshold_val <- 0.1

m <- 12
n_train <- 5000
n_explain <- 100
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


g <- function(a,b){
  a*b+a*b^2+a^2*b
}

beta <- c(0.2, -0.8, 1.0, 0.5, -0.8, rep(0, m - 5))
gamma <- c(0.8,-1)
alpha <- 1
y_train <- alpha +
  as.vector(as.matrix(cos(x_train))%*%beta) +
  unlist(gamma[1]*g(x_train[,1],x_train[,2])) +
  unlist(gamma[1]*g(x_train[,3],x_train[,4])) +
  rnorm(n_train, 0, 1)
y_explain <- alpha +
  as.vector(as.matrix(cos(x_explain))%*%beta) +
  unlist(gamma[1]*g(x_explain[,1],x_explain[,2])) +
  unlist(gamma[1]*g(x_explain[,3],x_explain[,4])) +
  rnorm(n_train, 0, 1)

xy_train <- cbind(y_train, x_train)

set.seed(123)
model <- xgboost(
  data = as.matrix(x_train),
  label = y_train,
  nround = 50,
  verbose = FALSE
)

pred_train <- predict(model, as.matrix(x_train))

this_order <- order(unlist(x_train[,1]))

plot(unlist(x_train[this_order,1]),pred_train[this_order],type="l")

p0 <- mean(y_train)


### First run proper shapr call on this


set.seed(465132)
inds = 1:5#1:n_explain

expl <- explain(
  model = model,
  x_explain= x_explain[inds,],
  x_train = x_train,
  approach = "gaussian",
  prediction_zero = p0,
  n_coalitions = 100,
  Sigma=Sigma,
  mu=mu,
  iterative = TRUE,
  unique_sampling = FALSE,
  iterative_args = list(initial_n_coalitions = 50,
                            fixed_n_coalitions_per_iter = 50,
                            max_iter = 10,
                            convergence_tolerance = 10^(-10),
                            compute_sd = TRUE),
  shapley_reweighting = "none",
  print_iter_info = TRUE
)

# Number of (non-unique) coalitions per iteration
sapply(expl$internal$iter_list,function(dt) dt$X[,sum(sample_freq)])

# Timing of main function call
expl$timing$main_timing_secs

# Timings per iteration
expl$timing$iter_timing_secs_dt[]

