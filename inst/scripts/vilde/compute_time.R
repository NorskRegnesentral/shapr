library(MASS)
library(shapr)

# skisse til script for å måle tid

# plan:
# simulere normalfordelte data ( også inkludere betingede variabler)
# finn en funksjon som simulerer multivariat normalfordeling
# sjekke hvordan ulike approaches, batches, antall variabler, osv påvirker tid.
# dvs. vi ser på:
# n_train, n_test, #features (max 10), correlation, model (xsboost,lm), approach,n_batches, n_samples (til monte carlo integral)
# organiser mulige kombinasjoner av disse med expand.grid, så kan man loope mellom rader i data.frame fra denne funksjonen og legge tiden til som en ny kolonne
# beregn tid med proc.time eller microbenchmark
# samle resultater i en csv fil


n_vars <- c(5, 10) #number of features
n_train <- c(1e3, 1e4)
n_test <- c(5, 20)
correlation <- c(0, 0.25, 0.75) #rho, 0, 0.25, 0.75
model <- c("xgboost", "lm")
approach <- c("independence", "gaussian", "copula", "empirical", "ctree") #add more
n_batches <- c(1, 2,4,6,8,10, 15,20,25,30,35,40,45,50) #2^n_vars er max, hopp over tilfeller hvor overskrider max
n_samples <- c(1e3, 1e4)

combos <- expand.grid(n_vars,
                      n_test,
                      n_train,
                      correlation,
                      model,
                      approach,
                      n_batches,
                      n_samples
                      )
names(combos) <- c("n_vars", "n_test", "n_train", "correlation", "model", "approach", "n_batches", "n_samples")

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

time0 <- proc.time()
explain_final(x_train,
              x_test,
              model,
              approach=as.character(approach),
              prediction_zero=p,
              n_batches = n_batches)
time1 <- proc.time()
tot_time <- time1-time0

combos$time[i] <- tot_time[["elapsed"]]
fwrite(combos[i,], file = "C:/Users/vilde.NR/Documents/GitHub/shapr/inst/scripts/vilde/results_test.csv",append = TRUE)
}
