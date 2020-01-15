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

#### Run xgboost with all variables to find the most influential categorical variables ####
x_var <- colnames(train_noNA)[-which(colnames(train_noNA) %in% c("Id","SalePrice"))]

## ----------------------------------------- ##

x_train <- train_noNA[, x_var]
x_test <- test_noNA[, x_var]

y_train <- train_noNA[, y_var]
y_train <- y_train / 1000000 ## convert to millions


### SPECIAL STUFF  STARTS ###
library(caret)
dummyfunc <- caret::dummyVars(" ~ .", data = rbind(x_train, x_test), fullRank = TRUE)
x_train_dummy <- predict(dummyfunc, newdata = x_train)
x_test_dummy <- predict(dummyfunc, newdata = x_test)
### SPECIAL STUFF ENDS ###

all_classes = sapply(x_train, class)
just_factors = names(all_classes[all_classes == "factor"])

# Fitting a basic xgboost model to the training data
model <- xgboost(
  data = x_train_dummy,
  label = y_train,
  nround = 50,
  verbose = FALSE
)

imp <- xgb.importance(model = model) # this is a data.table

these <- strsplit(imp$Feature, split = ".", fixed = TRUE) # this is a list
org_name <- sapply(these, FUN = function(x){x[[1]]}) # these are characters


imp[, org_name := ..org_name] # adds a column of names
imp[, isfact := org_name %in% ..just_factors] # adds column of TRUE and FALSE if is a factor

x_var_cat_2 <- head(imp[isfact == TRUE, org_name], 2) # gets the top two influential factors

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
dummyfunc <- caret::dummyVars(" ~ .", data = rbind(x_train, x_test), fullRank = TRUE)
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
### refined model ends ####


### SPECIAL STUFF STARTS ###
model$dummyfunc <- dummyfunc
### SPECIAL STUFF ENDS ###

explainer <- shapr(x_train, model)

p <- mean(y_train)

explanation <- explain( ## this takes a very long time to run = 21.55 minutes?
  x = as.data.table(x_test)[sample.int(n = nrow(x_test), size = 10),],
  approach = 'ctree',
  explainer = explainer,
  prediction_zero = p,
  sample = FALSE)

explanation$dt

print(explanation$dt)

##        none    MSSubClass      LotArea  Neighborhood   ExterQual
# 1: 0.1809212 -0.0015896353 -0.001297028 -0.0184937379 -0.01887452
# 2: 0.1809212 -0.0015896353 -0.001297028 -0.0184937379 -0.01887452
# 3: 0.1809212  0.0013136252 -0.002913748 -0.0400191850  0.05876679
# 4: 0.1809212 -0.0177548338 -0.014128103 -0.0074718145 -0.01329531
# 5: 0.1809212 -0.0111758481  0.106600967  0.0209405020 -0.02426726
# 6: 0.1809212 -0.0012980537  0.010658637 -0.0133954487  0.03129612
# 7: 0.1809212 -0.0011562300 -0.020129375 -0.0407791503  0.03772138
# 8: 0.1809212 -0.0274628245 -0.018296157  0.0160902201  0.01797039
# 9: 0.1809212  0.0009630143 -0.027619531  0.0003228987  0.03130924
# 10: 0.1809212  0.0077905131  0.027156874 -0.0179940605 -0.01797191

shapr:::plot.shapr(explanation, plot_phi0 = FALSE) ## removes the 'none' possibility

