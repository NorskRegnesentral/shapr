library(MASS)

# skisse til script for å måle tid

n_vars <- c(5, 10, 20) #number of features
n_train <- c(1e2, 1e3, 1e4)
n_test <- c(5, 10, 20)
correlation <- c(TRUE, FALSE)
model <- c("xgboost", "lm")
approach <- c("independence") #add more
n_batches <- c(1, 10, 100)
n_samples <- c(1e3, 1e4, 1e5)

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

# simulere normalfordelte data (burde også inkludere betingede variabler)
# finn en funksjon som simulerer multivariat normalfordeling
# sjekke hvordan ulike approaches, batches, antall variabler, osv påvirker tid.
# dvs. vi ser på:
# n_train, n_test, #features (max 10), correlation, model (xsboost,lm), approach,
# n_batches, n_samples (til monte carlo integral)
# organiser mulige kombinasjoner av disse med expand.grid, så kan man loope mellom rader
# i data.frame fra denne funksjonen og legge tiden til som en ny kolonne
# beregn tid med proc.time eller microbenchmark

i <- 1 # loop later

correlation <- combos[i,"correlation"]
n_vars <- combos[i,"n_vars"]
n_train <- combos[i, "n_train"]
n_test <- combos[i, "n_test"]

if (correlation == FALSE){
  sigma <- diag(n_vars+1) # independent features
} else {
  sigma <- diag(n_vars+1) # to be updated to make non-identity covariance matrix
}

data <- mvrnorm(n = n_train + n_test, rep(0, n_vars+1), sigma)
x_train <- data[-1:-n_train, 1:n_vars] #first n_vars columns are x variables
y_train <- data[-1:-n_train, n_vars+1]
x_test <- data[1:n_train, 1:n_vars]

# samle resultater i en csv fil

