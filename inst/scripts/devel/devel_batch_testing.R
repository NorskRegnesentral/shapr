
#remotes::install_github("NorskRegnesentral/shapr") # Installs GitHub version of shapr

library(shapr)
library(data.table)
library(MASS)
library(Matrix)

# Just sample some data to work with
m <- 9
n_train <- 10000
n_explain <- 10
rho_1 <- 0.5
rho_2 <- 0
rho_3 <- 0.4
Sigma_1 <- matrix(rho_1, m/3, m/3) + diag(m/3) * (1 - rho_1)
Sigma_2 <- matrix(rho_2, m/3, m/3) + diag(m/3) * (1 - rho_2)
Sigma_3 <- matrix(rho_3, m/3, m/3) + diag(m/3) * (1 - rho_3)
Sigma <- as.matrix(bdiag(Sigma_1, Sigma_2, Sigma_3))
mu <- rep(0,m)

set.seed(123)



x_train <- as.data.table(MASS::mvrnorm(n_train,mu,Sigma))
x_explain <- as.data.table(MASS::mvrnorm(n_explain,mu,Sigma))

names(x_train) <- paste0("VV",1:m)
names(x_explain) <- paste0("VV",1:m)

beta <- c(4:1, rep(0, m - 4))
alpha <- 1
y_train <- as.vector(alpha + as.matrix(x_train) %*% beta + rnorm(n_train, 0, 1))
y_explain <- alpha + as.matrix(x_explain) %*% beta + rnorm(n_explain, 0, 1)

xy_train <- cbind(y_train, x_train)

p0 <- mean(y_train)

# We need to pass a model object and a proper prediction function to shapr for it to work, but it can be anything as we don't use it
model <- lm(y_train ~ ., data = x_train)

### First run proper shapr call on this
library(progressr)
library(future)
# Not necessary, and only apply to the explain() call below
progressr::handlers(global = TRUE) # For progress bars
#future::plan(multisession, workers = 2) # Parallized computations
#future::plan(sequential)

expl <- explain(model = model,
                x_explain= x_explain,
                x_train = x_train,
                approach = "ctree",
                prediction_zero = p0,
                n_batches = 100,
                n_samples = 1000,
                iterative = TRUE,
                print_iter_info = TRUE,
                print_shapleyres = TRUE)


n_combinations <- 5
max_batch_size <- 10
min_n_batches <- 10

