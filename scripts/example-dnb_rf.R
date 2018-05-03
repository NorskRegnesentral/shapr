rm(list = ls())

library(data.table)
library(ranger)

data_dir <- "/nr/project/stat/BFFGB18/LIME/lime/R/"
Xtrain <- as.data.table(read.table(paste0(data_dir, "/train6.csv"), sep = ";", header = TRUE))
Xtest <- as.data.table(read.table(paste0(data_dir, "/test6.csv"), sep = ";", header = TRUE))
Xtrain <- Xtrain[, .SD, .SDcols = 1:7]
Xtest <- Xtest[, .SD, .SDcols = 1:7]

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

X <- kernelShap(
    m = m,
    Xtrain = Xtrain,
    Xtest = Xtest,
    nsamples = 200,
    exact = TRUE,
    nrows = 1e3
)

X <- get_predictions(model = model, DT = X$DT, W = X$W, p_default = p_default, ranger = TRUE)
