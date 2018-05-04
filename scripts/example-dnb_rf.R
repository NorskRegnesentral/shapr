rm(list = ls())

library(data.table)
library(ranger)

data_dir <- "/nr/project/stat/BFFGB18/LIME/lime/R/"
Xtrain <- as.data.table(read.table(paste0(data_dir, "/train6.csv"), sep = ";", header = TRUE))
Xtest <- as.data.table(read.table(paste0(data_dir, "/test6.csv"), sep = ";", header = TRUE))
Xtrain <- Xtrain[, .SD, .SDcols = 1:10]
Xtest <- Xtest[, .SD, .SDcols = 1:10]

model <- ranger(as.factor(default) ~ .,
                data = Xtrain,
                num.trees = 500,
                num.threads = 3,
                verbose = TRUE,
                probability = TRUE,
                importance = "impurity",
                mtry = sqrt(ncol(Xtrain) - 1))

p_default <- Xtrain[, mean(default)]
nms <- colnames(Xtrain)[-1]
Xtrain <- Xtrain[, .SD, .SDcols = nms]
Xtest <- Xtest[, .SD, .SDcols = nms]
setcolorder(Xtest, nms)
m <- ncol(Xtrain)

S <- scale_data(Xtrain, Xtest, scale = TRUE)

Xtrain = as.matrix(S$Xtrain)
Xtest = as.matrix(S$Xtest)


X <- get_combinations(m = m, exact = FALSE, nrows = 1e4)
X <- get_weights(X = X)
W <- get_weighted_matrix(X = X)
x <- distance_cpp2(Xtrain, Xtest, m)

devtools::load_all()
sigma <- 1.75 * (nrow(Xtrain)) ^ (-1 / 6)
test <- sample_unit(x[,1,], X[["features"]], nsamples = 1e4, sigma)

test1 <- impute_data_unit(test, Xtrain, Xtest)
str(test1)
