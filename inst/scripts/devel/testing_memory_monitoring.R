

library(shapr)
library(future)
library(MASS)
library(microbenchmark)
library(data.table)
library(peakRAM)

p_vec <- 20#2:10
n_train_vec <- 1000
n_test_vec <- 100#c(2,10,20)
n_batches_vec <- c(1,2,4,8,16,24,32)#seq(2,20,by=5)
n_cores_vec <- c(1,2,4,8,16,24,32)#c(1,seq(2,32,by=5))
approach_vec <- c("empirical","gaussian","ctree")#rev(c("empirical","gaussian"))
reps <- 2

max_n <- 10^5
max_p <- 20
rho <- 0.3
Sigma <- matrix(rho,max_p,max_p)
diag(Sigma) <- 1
mu <- rep(0,max_p)
beta <- c(1,seq_len(max_p)/max_p)
sigma_eps <- 1

set.seed(123)
x_all <- MASS::mvrnorm(max_n,mu = mu,Sigma = Sigma)
y_all <- as.vector(cbind(1,x_all)%*%beta)+rnorm(max_n,mean = 0,sd = sigma_eps)



set.seed(123)
these_p <- sample.int(max_p,size=p_vec[1])
these_train <- sample.int(max_n,size=n_train_vec[1])
these_test <- sample.int(max_n,size=n_test_vec[1])

x_train <- as.data.frame(x_all[these_train,these_p])
x_test <- as.data.frame(x_all[these_test,these_p])

y_train <- y_all[these_train]

xy_train <- cbind(x_train,y=y_train)

model <- lm(formula = y~.,data=xy_train)

explainer <- shapr(x_train, model,n_combinations = 1000)
p <- mean(y_train)


future::plan("multicore",workers=4)
#future::plan("multisession",workers=4)
#future::plan("sequential")

peakRAM(explain(
    x_test,
    approach = "gaussian",
    explainer = explainer,
    prediction_zero = p,n_batches = 4)
    )

# ,
#     explain(
#       x_test,
#       approach = "empirical",
#       explainer = explainer,
#       prediction_zero = p,n_batches = 2),
#     explain(
#       x_test,
#       approach = "empirical",
#       explainer = explainer,
#       prediction_zero = p,n_batches = 4))

#     explain(
#       x_test,
#       approach = "empirical",
#       explainer = explainer,
#       prediction_zero = p,n_batches = 8),
#     explain(
#       x_test,
#       approach = "empirical",
#       explainer = explainer,
#       prediction_zero = p,n_batches = 16),
#     explain(
#       x_test,
#       approach = "empirical",
#       explainer = explainer,
#       prediction_zero = p,n_batches = 32)
# )

# s <- proc.time()
# explain(
#   x_test,
#   approach = "empirical",
#   explainer = explainer,
#   prediction_zero = p,n_batches = 32)
# print(proc.time()-s)
#
