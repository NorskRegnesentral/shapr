library(MASS)
library(shapr)
library(data.table)
library(xgboost)
library(future)

#har beregnet i=1 til i=1843, skal begynne med i=1844 for å fortsette

n_vars <- c(5, 7, 10, 8, 12) #number of features
n_train <- c(1e3, 1e4, 5e4)
n_test <- c(5, 20, 10, 30, 50, 100)
correlation <- c(0.5)
model <- c("xgboost")
n_batches <- c(1, 10, 30, 50, 2, 4, 6, 8, 15, 20, 25, 35, 40, 45) #2^n_vars er max, hopp over tilfeller hvor overskrider max
n_samples <- c(1e3)
computer_name <- c(paste0(Sys.info()[["sysname"]], Sys.info()[["user"]])) #my computer name as system name+username
n_cores <- c(1,2,4,6,8)
approach <- c("independence", "gaussian")#, "copula", "empirical", "ctree")

combos <- expand.grid(n_vars,
                      n_test,
                      n_train,
                      correlation,
                      model,
                      n_batches,
                      n_samples,
                      n_cores,
                      computer_name,
                      approach
                      )

names(combos) <- c("n_vars",
                   "n_test",
                   "n_train",
                   "correlation",
                   "model",
                   "n_batches",
                   "n_samples",
                   "n_cores",
                   "computer_name",
                   "approach"
                   )

for (i in 1:nrow(combos)){

n_vars <- combos[i,"n_vars"]
n_train <- combos[i,"n_train"]
n_test <- combos[i,"n_test"]
n_batches <- combos[i,"n_batches"]

if(n_batches>=2^(n_vars-2)){ #do not need to test large n_batches when num. features is small
  next
}

#simulate multivariate gaussian data
rho<-combos[i,"correlation"]
sigma <- matrix(rho,n_vars+1,n_vars+1)
diag(sigma) <-1
data <- mvrnorm(n = n_train + n_test, rep(0, n_vars+1), sigma)
colnames(data)[n_vars+1] <- "Y"
colnames(data)[1:n_vars] <- paste0("X", 1:n_vars)

x_train <- data[-1:-n_test, 1:n_vars] #first n_vars columns are x variables
y_train <- data[-1:-n_test, n_vars+1]
x_test <- data[1:n_test, 1:n_vars]

if (combos[i,"model"]=="xgboost"){
  model <- xgboost(
    data = x_train,
    label = y_train,
    nround = 20,
    verbose = FALSE
  )
} else if (combos[i,"model"]=="lm"){
  formula <- paste(colnames(data)[1:n_vars], collapse="+")
  formula <- as.formula(paste0("Y~",formula)) # Y ~ X1 + X2 +...
  model <- lm(formula, data.frame(data[-1:-n_test,]))
}

p <- mean(y_train)
approach <- combos[i,"approach"]
n_cores <- combos[i,"n_cores"]

time0 <- proc.time()
plan(multisession, workers = n_cores)
explain_final(x_train,
              x_test,
              model,
              approach=as.character(approach),
              prediction_zero=p,
              n_batches = n_batches)
time1 <- proc.time()
tot_time <- time1-time0

combos$time[i] <- tot_time[["elapsed"]]
fwrite(combos[i,], file = "inst/scripts/vilde/results_independence_gaussian.csv",append = TRUE)
}
