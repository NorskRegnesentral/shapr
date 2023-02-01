#.libPaths("/disk/home/jullum/R/x86_64-pc-linux-gnu-library/4.1","/opt/R/4.1.1/lib/R/library")
sys_time_initial <- Sys.time()

# libraries
library(shapr)
library(future)
library(MASS)
library(microbenchmark)
library(data.table)
library(profmem)

# Initial setup
max_n <- 10^5
max_p <- 16
rho <- 0.3
sigma <- 1
mu_const <- 0
beta0 <- 1
sigma_eps <- 1

mu <- rep(mu_const,max_p)
beta <- c(beta0,seq_len(max_p)/max_p)
Sigma <- matrix(rho,max_p,max_p)
diag(Sigma) <- sigma

set.seed(123)
x_all <- MASS::mvrnorm(max_n,mu = mu,Sigma = Sigma)
y_all <- as.vector(cbind(1,x_all)%*%beta)+rnorm(max_n,mean = 0,sd = sigma_eps)

# Arguments from bash
#args <- commandArgs(trailingOnly = TRUE)
#if(length(args)==0) args = c(1,10,1000,100,10,1,"empirical","sequential","timing_test_2023.csv")


this_rep <- 1
p <- 6
n_train <- 100
n_explain <- 100
n_batches <- 100
n_cores <- 1
approach <- "empirical"
multicore_method <- "sequential"
logfilename <- "bla"

set.seed(123)


these_p <- sample.int(max_p,size=p)
these_train <- sample.int(max_n,size=n_train)
these_explain <- sample.int(max_n,size=n_explain)

x_train <- as.data.frame(x_all[these_train,these_p,drop=F])
x_explain <- as.data.frame(x_all[these_explain,these_p,drop=F])

colnames(x_explain) <- colnames(x_train) <- paste0("X",seq_len(p))

y_train <- y_all[these_train]

xy_train <- cbind(x_train,y=y_train)

model <- lm(formula = y~.,data=xy_train)

prediction_zero <- mean(y_train)

n_batches_use <- min(2^p-2,n_batches)


explanation_many <- explain(
    model = model,
    x_explain = x_explain,
    x_train = x_train,
    approach = approach,
    n_batches = n_batches_use,
    prediction_zero = prediction_zero
  )


#explanation_single <- explain(
#  model = model,
#  x_explain = x_explain,
#  x_train = x_train,
#  approach = approach,
#  n_batches = 1,
#  prediction_zero = prediction_zero
#)



#S_batch_many <- copy(explanation_many$internal$objects$S_batch)
#internal_many <- copy(explanation_many$internal)

#S_batch_single <- list(`1`=sort(unlist(copy(explanation_many$internal$objects$S_batch),use.names=FALSE)))
#internal_single <- copy(explanation_many$internal)

feature_specs <- shapr:::get_feature_specs(NULL, model)


internal <- setup(
  x_train = x_train,
  x_explain = x_explain,
  approach = approach,
  prediction_zero = prediction_zero,
  n_combinations = 2^p,
  group = NULL,
  n_samples = 1e3,
  n_batches = n_batches_use,
  seed = 123,
  keep_samp_for_vS = FALSE,
  feature_specs = feature_specs)

internal <- setup_computation(internal, model, NULL)

S_batch_many <- internal$objects$S_batch
S_batch_single <- list(`1`=sort(unlist(copy(S_batch_many),use.names=FALSE)))

testfunc <- function(S, internal) {
  dt <- shapr:::batch_prepare_vS(S = S, internal = internal) # Make it optional to store and return the dt_list
  return(S)
}

internal$parameters$empirical.fixed_sigma <- rep(0.1,2)

#pp_many <- profmem({
s <- proc.time()
ret <- future.apply::future_lapply(
  X = S_batch_many,
  FUN = testfunc,
  internal = internal)
proc.time()-s
#},threshold=10^4)

#pp_single <- profmem({
s <- proc.time()
  ret <- future.apply::future_lapply(
    X = S_batch_single,
    FUN = testfunc,
    internal = internal)
proc.time()-s
#},threshold=10^4)

plot(pp_many$bytes)
points(pp_single$bytes,col=2)

sum(pp_many$bytes)
sum(pp_single$bytes)
