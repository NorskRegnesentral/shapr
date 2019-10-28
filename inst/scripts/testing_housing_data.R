library(xgboost)
library(shapr)
library(data.table)
library(party)

if(.Platform$OS.type=="windows"){
  projDir <- "M:"
} else {
  projDir <- "/nr/project/stat"
}

## ------------- some functions ----------------------
check_for_cont <- function(col_ind, data){
  return(!is.factor(data[, col_ind]))
}

check_for_col_NA <- function(col_ind, data){
  sum_NA <- sum(is.na(data[, col_ind]))
  if(sum_NA > 0) return(FALSE)
  else return(TRUE)
}

check_for_row_NA <- function(row_ind, data){
  sum_NA <- sum(is.na(data[row_ind, ]))
  if(sum_NA > 0) return(FALSE)
  else return(TRUE)
}

## ------------------------- data ------------------------

dataDir <- paste(projDir, "BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/data/", sep = "/")

setwd(dataDir)

## read csv
train_data <- read.table(file = "train.csv", sep = ",", header = TRUE, stringsAsFactors = TRUE)
test_data <- read.table(file = "test.csv", sep = ",", header = TRUE, stringsAsFactors = TRUE)


## ---------------------- categorical data --------------------- ##

## first we remove the columns with NA
train_noNA <- train_data[, sapply(X = 1:ncol(train_data), FUN = check_for_col_NA, data = train_data)]
# names(train_noNA)[-length(train_noNA)]

## then we keep only the columns from the train data
test_noNA0 <- test_data[, names(train_noNA)[-length(train_noNA)]]
## then we remove the rows with at least one NA
test_noNA <- test_noNA0[sapply(X = 1:nrow(test_noNA0), FUN = check_for_row_NA, data = test_noNA0) ,]

x_var <- c("MSSubClass", "MSZoning", "LotArea", "Street")
y_var <- "SalePrice"

## ----------------------------------------- ##

x_train <- train_noNA[, x_var]
x_test <- test_noNA[, x_var]

y_train <- train_noNA[, y_var]
y_train <- y_train / 1000000 ## convert to millions


### SPECIAL STUFF  STARTS ###
library(caret)
dummyfunc <- caret::dummyVars(" ~ .", data = rbind(x_train, x_test))
x_train_dummy <- predict(dummyfunc, newdata = x_train)
x_test_dummy <- predict(dummyfunc, newdata = x_test)
### SPECIAL STUFF ENDS ###


# Fitting a basic xgboost model to the training data
model <- xgboost(
  data = x_train_dummy,
  label = y_train,
  nround = 20,
  verbose = FALSE
)

### SPECIAL STUFF STARTS ###
model$dummyfunc <- dummyfunc
### SPECIAL STUFF ENDS ###

explainer <- shapr(x_train, model)

p <- mean(y_train)

start_time <- Sys.time()
explanation <- explain( ## this takes a very long time to run = 21.55 minutes?
  x = as.data.table(x_test),
  approach = 'ctree',
  explainer = explainer,
  prediction_zero = p,
  sample = FALSE)

end_time <- Sys.time()
end_time - start_time


##        none   MSSubClass      MSZoning      LotArea        Street
# 1: 0.1809212  0.002035604 -0.0181289541  0.050419428 -4.421495e-04
# 2: 0.1809212  0.004978804  0.0028594295  0.076544900  1.281169e-04
# 3: 0.1809212  0.049614544  0.0013229458  0.030647468  4.802960e-04
# 4: 0.1809212  0.065409112 -0.0005392566  0.034090890  5.155915e-04
# 5: 0.1809212  0.039462031  0.0060510941 -0.008906204 -4.031790e-03
# ---
#   1443: 0.1809212 -0.012370053 -0.0182184710 -0.042392534 -5.778365e-04
# 1444: 0.1809212 -0.012753544 -0.0186019623 -0.042776025  5.726370e-04
# 1445: 0.1809212 -0.016221998  0.0020150212  0.044441587  8.794181e-05
# 1446: 0.1809212 -0.032160189  0.0045814492 -0.001777592  6.406629e-04
# 1447: 0.1809212  0.045843928  0.0011496367 -0.007289328  5.202563e-04

plot(explanation) ## this doesn't work?

