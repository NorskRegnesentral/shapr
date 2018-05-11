rm(list = ls())

library(shapr)
library(data.table)
library(xgboost)

## Load training/test data and train model --------------------------------
data_dir = "/nr/project/stat/BFFGB18/LIME/lime/R/"
Xtrain = as.data.table(read.table(paste0(data_dir, "/train6.csv"), sep = ";", header = TRUE))
Xtest = as.data.table(read.table(paste0(data_dir, "/test6.csv"), sep = ";", header = TRUE))
Xtrain <- Xtrain[, .SD, .SDcols = 1:4]
Xtest <- Xtest[, .SD, .SDcols = 1:4]


params <- list(eta =  0.2,
               objective = "binary:logistic",
               eval_metric = "auc",
               tree_method="hist") # gpu_hist

xgb.train <- xgb.DMatrix(data = as.matrix(Xtrain[,-1]),
                         label = Xtrain$default)
xgb.test <- xgb.DMatrix(data = as.matrix(Xtest[,-1]),
                        label = Xtest$default)

model <- xgb.train(data = xgb.train,
                   params = params,
                   nrounds = 10,
                   print_every_n = 5,
                   ntread = 8,
                   watchlist = list(train=xgb.train,test=xgb.test))

## Both these ways to predict gives the same answer.
aa=predict(model,xgb.test)
bb=predict(model,as.matrix(Xtest[,-1]))

p_default = Xtrain[, mean(default)]
nms = colnames(Xtrain)[-1]
Xtrain = Xtrain[, .SD, .SDcols = nms]
Xtest = Xtest[1:2, .SD, .SDcols = nms]
setcolorder(Xtest, nms)
m = ncol(Xtrain)

## Pre computation before kernel shap --------------------------------
l <- kernelShap(
    m = m,
    Xtrain = Xtrain,
    Xtest = Xtest,
    exact = TRUE,
    nrows = 1e4
)


##### OK untill here ####

## Loop through all test observations --------------------------------
sigma <- .1
ll <- list()
for (i in Xtest[, .I]) {

    print(sprintf("%d out of %d", i, Xtest[, .N]))
    ll[[i]] <- get_prediction_data(
        model = model,
        D = l$D[, i,],
        S = l$S,
        Xtrain = as.matrix(l$Xtrain),
        Xtest = as.matrix(l$Xtest)[i, , drop = FALSE],
        sigma = sigma,
        w_threshold = 1,
        n_threshold = 1e5,
        verbose = FALSE
    )
    ll[[i]][, id := i]
}

DT <- rbindlist(ll)
Kshap <- matrix(0, nrow = Xtest[, .N], ncol = nrow(l$W))
for (i in Xtest[, .I]) {
    Kshap[i, ] = l$W %*% DT[id == i, k]
}


