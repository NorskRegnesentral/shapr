library(data.table)
library(MASS)
library(Matrix)
# library(shapr)
devtools::load_all()
library(future)
library(xgboost)

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

# plot(unlist(x_train[this_order,1]),pred_train[this_order],type="l")

p0 <- mean(y_train)


### First run proper shapr call on this


set.seed(465132)
inds = 1:5#1:n_explain

x_explain= x_explain[inds,]
x_train = x_train
approach = "gaussian"
prediction_zero = p0
adaptive = TRUE
adaptive_arguments = list(initial_n_coalitions = 256,
                          fixed_n_coalitions_per_iter = 256,
                         convergence_tolerance = 0.00001)

paired_shap_sampling = FALSE # TODO: Make TRUE the default later on
max_n_coalitions = NULL
group = NULL
n_MC_samples = 1e3
n_batches = NULL
seed = 1
keep_samp_for_vS = FALSE
predict_model = NULL
get_model_specs = NULL
MSEv_uniform_comb_weights = TRUE
verbose = 0
print_shapleyres = TRUE # tmp
print_iter_info = TRUE # tmp
shapley_reweighting = "none" # tmp # "on_N" # TODO: Make "on_N" the default later on.
prev_shapr_object = NULL
#

devtools::load_all()
# rm(list = c("predict_model"))

# debugonce(explain)
expl <- explain(
  model = model,
  x_explain= x_explain,
  x_train = x_train,
  approach = approach,
  prediction_zero = prediction_zero,
  adaptive = adaptive,
  adaptive_arguments = adaptive_arguments,
  shapley_reweighting = shapley_reweighting
)
