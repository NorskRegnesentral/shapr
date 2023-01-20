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
p <- 4
n_train <- 1000
n_explain <- 50
n_batches <- 10
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


sys_time_start_explain <- Sys.time()

pp.old <- profmem({
explanation <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = approach,
  n_batches = n_batches_use,
  prediction_zero = prediction_zero
)
},threshold=10^4)

sys_time_end_explain <- Sys.time()

pp[!is.na(pp$bytes) &pp$bytes >6*10^5,]

plot(pp$bytes)
points(pp.old$bytes,col=2)
#"bytes"]
pp.old[!is.na(pp.old$bytes) &pp.old$bytes >6*10^5,"bytes"]

secs_explain <- as.double(difftime(sys_time_end_explain,sys_time_start_explain),units="secs")
print(secs_explain)

timing <- list(p = p,
               n_train = n_train,
               n_explain = n_explain,
               n_batches = n_batches,
               n_cores = n_cores,
               approach = approach,
               sys_time_initial = as.character(sys_time_initial),
               sys_time_start_explain = as.character(sys_time_start_explain),
               sys_time_end_explain = as.character(sys_time_end_explain),
               secs_explain = secs_explain,
               this_rep = this_rep,
               max_n = max_n,
               max_p = max_p,
               rho = rho,
               sigma = sigma,
               mu_const = mu_const,
               beta0 = beta0,
               sigma_eps = sigma_eps)

#print(unlist(timing))
data.table::fwrite(timing,logfilename,append = T)
