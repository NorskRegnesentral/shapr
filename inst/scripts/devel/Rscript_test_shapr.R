#.libPaths("/disk/home/jullum/R/x86_64-pc-linux-gnu-library/4.1","/opt/R/4.1.1/lib/R/library")
sys_time_initial <- Sys.time()

# libraries
library(shapr)
library(future)
library(MASS)
library(microbenchmark)
library(data.table)

# Initial setup
max_n <- 10^5
max_p <- 13
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

# Arguments form bash
args <- commandArgs(trailingOnly = TRUE)
if(length(args)==0) args = c(1,10,1000,100,6,2,"empirical","multisession","test.csv")

this_rep <- as.numeric(args[1])
p <- as.numeric(args[2])
n_train <- as.numeric(args[3])
n_test <- as.numeric(args[4])
n_batches <- as.numeric(args[5])
n_cores <- as.numeric(args[6])
approach <- args[7]
multicore_method <- args[8]
logfilename <- args[9]

set.seed(123)


these_p <- sample.int(max_p,size=p)
these_train <- sample.int(max_n,size=n_train)
these_test <- sample.int(max_n,size=n_test)

x_train <- as.data.frame(x_all[these_train,these_p])
x_test <- as.data.frame(x_all[these_test,these_p])

colnames(x_test) <- colnames(x_train) <- paste0("X",seq_len(p))

y_train <- y_all[these_train]

xy_train <- cbind(x_train,y=y_train)

model <- lm(formula = y~.,data=xy_train)

sys_time_start_shapr <- Sys.time()
explainer <- shapr(x_train, model)
sys_time_end_shapr <- Sys.time()

prediction_zero <- mean(y_train)

n_batches_use <- min(nrow(explainer$S),n_batches)

future::plan(multicore_method,workers=n_cores)

sys_time_start_explain <- Sys.time()
explanation <- explain(
  x_test,
  approach = approach,
  explainer = explainer,
  prediction_zero = prediction_zero,
  n_batches = n_batches_use
)
sys_time_end_explain <- Sys.time()
future::plan(sequential) # To close multisessions etc

timing <- list(p = p,
               n_train = n_train,
               n_test = n_test,
               n_batches = n_batches,
               n_cores = n_cores,
               approach = approach,
               sys_time_initial = as.character(sys_time_initial),
               sys_time_start_shapr = as.character(sys_time_start_shapr),
               sys_time_end_shapr = as.character(sys_time_end_shapr),
               sys_time_start_explain = as.character(sys_time_start_explain),
               sys_time_end_explain = as.character(sys_time_end_explain),
               secs_shapr = as.double(difftime(sys_time_end_shapr,sys_time_start_shapr),units="secs"),
               secs_explain = as.double(difftime(sys_time_end_explain,sys_time_start_explain),units="secs"),
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
