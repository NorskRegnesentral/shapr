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
dataDirMJ <- ".."


setwd(dataDirMJ)

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

#### Run xgboost with all variables to find the most influential categorical variables ####
x_var <- colnames(train_noNA)[-which(colnames(train_noNA)%in%c("Id","SalePrice"))]

## ----------------------------------------- ##

x_train <- train_noNA[, x_var]
x_test <- test_noNA[, x_var]

y_train <- train_noNA[, y_var]
y_train <- y_train / 1000000 ## convert to millions


### SPECIAL STUFF  STARTS ###
library(caret)
dummyfunc <- caret::dummyVars(" ~ .", data = rbind(x_train, x_test),fullRank=T)
x_train_dummy <- predict(dummyfunc, newdata = x_train)
x_test_dummy <- predict(dummyfunc, newdata = x_test)
### SPECIAL STUFF ENDS ###

aa=sapply(x_train,class)
bb = names(aa[aa=="factor"])

# Fitting a basic xgboost model to the training data
model <- xgboost(
  data = x_train_dummy,
  label = y_train,
  nround = 50,
  verbose = FALSE
)

imp <- xgb.importance(model=model)

these <- strsplit(imp$Feature,split = ".",fixed = T)
orgnam <- sapply(these,FUN = function(x){x[[1]]})

imp[,orgnam:=..orgnam]
imp[,isfact:=orgnam %in% ..bb]

x_var_cat_2 <- head(imp[isfact==TRUE,orgnam],2)

#### The two most influential categorical variables ####
#> x_var_cat_2
#[1] "Neighborhood" "ExterQual"

#### refined model starts ####
x_var <- c("MSSubClass", "LotArea", x_var_cat_2)
y_var <- "SalePrice"

x_train <- train_noNA[, x_var]
x_test <- test_noNA[, x_var]

y_train <- train_noNA[, y_var]
y_train <- y_train / 1000000 ## convert to millions


### SPECIAL STUFF  STARTS ###
library(caret)
dummyfunc <- caret::dummyVars(" ~ .", data = rbind(x_train, x_test),fullRank=T)
x_train_dummy <- predict(dummyfunc, newdata = x_train)
x_test_dummy <- predict(dummyfunc, newdata = x_test)
### SPECIAL STUFF ENDS ###

# Fitting a basic xgboost model to the training data
model <- xgboost(
  data = x_train_dummy,
  label = y_train,
  nround = 50,
  verbose = FALSE
)

xgb.importance(model=model)

### refined model ends ####


### SPECIAL STUFF STARTS ###
model$dummyfunc <- dummyfunc
### SPECIAL STUFF ENDS ###

explainer <- shapr(x_train, model)

p <- mean(y_train)

start_time <- Sys.time()
explanation <- explain( ## this takes a very long time to run = 21.55 minutes?
  x = as.data.table(x_test)[sample.int(n = nrow(x_test),size = 10),],
  approach = 'ctree',
  explainer = explainer,
  prediction_zero = p,
  sample = FALSE)

end_time <- Sys.time()
end_time - start_time

explanation$dt

print(explanation$dt)

##        none   MSSubClass      MSZoning      LotArea        Street
# 1: 0.1809212 0.002035604 -0.0181289541  5.041943e-02 -0.0004421495
# 2: 0.1809212 0.004978804  0.0028594295  7.654490e-02  0.0001281169
# 3: 0.1809212 0.049614544  0.0013229458  3.064747e-02  0.0004802960
# 4: 0.1809212 0.065409112 -0.0005392566  3.409089e-02  0.0005155915
# 5: 0.1809212 0.039462031  0.0060510941 -8.906204e-03 -0.0040317896
# 6: 0.1809212 0.046030168 -0.0001414244  9.982445e-05  0.0004323340

shapr:::plot.shapr(explanation,plot_phi0 = F) ## this doesn't work?

