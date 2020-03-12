library(xgboost)
library(shapr)
library(data.table)
library(MASS)
library(lqmm) ## to check if Sigma is positive definite
library(rapportools) # for testing booleans
library(ggplot2)


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

check_for_NA2 <- function(X){
  # ifelse(X == -7, NA, ifelse(X == -8, NA, ifelse(X == -9, NA, X)))
  ifelse(X == -9, NA, X)
}

find_id <- function(data, Value1, Value2, Value3, Value4){
  data[ExternalRiskEstimate == Value1 & MSinceOldestTradeOpen == Value2 & MSinceMostRecentTradeOpen == Value3 & AverageMInFile == Value4, "Id"]

}

find_row <- function(data, Value1, Value2, Value3, Value4){
  data[ExternalRiskEstimate == Value1 & MSinceOldestTradeOpen == Value2 & MSinceMostRecentTradeOpen == Value3 & AverageMInFile == Value4]

}
## -----------------------

data <- read.table(file = "/nr/project/stat/BigInsight/Projects/Explanations/Data/FICO_HELOC_dataset_v1.csv", sep = ",", header = TRUE, stringsAsFactors = TRUE )
data <- data.table(data)
nrow(data) # 10459
data[, Id := 1:nrow(data)]
data[, pred := 1]

## Demo 1
# x = find_id(data, Value1 = 61, Value2 = 49, Value3 = 19, Value4 = 29)
# data[Id == x[[1]], pred := 0.952]
#
# ## Demo 2
# x = find_id(data, Value1 = 59, Value2 = 131, Value3 = 7, Value4 = 81)
# data[Id == x[[1]], pred := 0.895]
#
#
# ## Demo3
# x = find_id(data, Value1 = 92, Value2 = 372, Value3 = 10, Value4 = 176)
# data[Id == x[[1]], pred := 0.049]
#
#
# ## Some random ones
# ## 4
# set.seed(10)
# sample(1:nrow(data), 1)
# data[491, pred := 0.888]
#
# ## 5
# set.seed(1)
# sample(1:nrow(data), 1)
# data[1017, pred := 0.594]
#
#
# ## 6
# set.seed(2)
# sample(1:nrow(data), 1)
# data[4806, pred := 0.696]
#
#
# ## 7
# set.seed(3)
# sample(1:nrow(data), 1)
# data[3770, pred := 0.332]
#
#
# ## 8
# set.seed(4)
# sample(1:nrow(data), 1)
# data[5624, pred := 0.241]
#
# #
# # ## 9
# # set.seed(5)
# # sample(1:nrow(data), 1)
# # data[2255] # pred := 0.241
#
#
# ## 10
# set.seed(6)
# sample(1:nrow(data), 1)
# data[6965, pred := 0.769]
#
# write.csv(data, "/nr/project/stat/BigInsight/Projects/Explanations/Data/FICO_HELOC_dataset_predictions.csv")


data0 <- copy(data)
cols <- colnames(data0)
data0 <- data0[, RiskPerformance := NULL][ , (cols) := lapply(.SD, FUN = check_for_NA2), .SDcols = cols]

data2 <- data[complete.cases(cbind(data[,"RiskPerformance"], data0)), ]

dim(data0)[1] - dim(data2)[1] # 598 have -9 everywhere

data2[, MaxDelqEver := as.factor(MaxDelqEver)]
data2[, MaxDelq2PublicRecLast12M := as.factor(MaxDelq2PublicRecLast12M)]

##
set.seed(1)
ss <- sample(1:nrow(data2), 500, replace = FALSE)
train_data <- data2[-ss,]
test_data <- data2[ss ,]

dim(train_data) # 9459 obs, 24 columns
dim(test_data)



##
y_var <- "RiskPerformance"
cat_var <- c("MaxDelqEver", "MaxDelq2PublicRecLast12M")
cont_var <- c("ExternalRiskEstimate", "MSinceOldestTradeOpen", "MSinceMostRecentTradeOpen", "AverageMInFile", "NumSatisfactoryTrades", "NumTrades60Ever2DerogPubRec", "NumTrades90Ever2DerogPubRec",
              "PercentTradesNeverDelq", "MSinceMostRecentDelq", "NumTotalTrades", "NumTradesOpeninLast12M", "PercentInstallTrades",
              "MSinceMostRecentInqexcl7days", "NumInqLast6M", "NumInqLast6Mexcl7days", "NetFractionRevolvingBurden", "NetFractionInstallBurden", "NumRevolvingTradesWBalance",
              "NumInstallTradesWBalance", "NumBank2NatlTradesWHighUtilization", "PercentTradesWBalance")

some_var <- c(cat_var, cont_var)#[1:10]

y_train <- train_data[, ..y_var]
x_train <- train_data[, ..some_var]
x_test <- test_data[, ..some_var]

train_dt <- cbind(y_train, x_train)
test_dt <- x_test

# Fitting a basic xgboost model to the training data
# model <- xgboost(
#   data = x_train,
#   label = y_train,
#   nround = 20,
#   verbose = FALSE,
#   objective = binary:logistic
# )


model <- glm(RiskPerformance~., data = train_dt, family = "binomial")
summary(model)

train_dt0 <- copy(train_dt)

explainer <- shapr(train_dt0[, RiskPerformance := NULL], model,n_combinations = 5000)

train_dt[, RiskPerformance1 := as.numeric(RiskPerformance) - 1]

p <- mean(train_dt[['RiskPerformance1']])

tm <- Sys.time()
explanation <- explain(
  x = test_dt[1:5,],
  approach = 'ctree',
  explainer = explainer,
  prediction_zero = p,
  sample = TRUE
)
tm2 <- Sys.time()
print(tm2 - tm) # 2.8 minutes for 10 features and 6 test observations
# for 500 test observations and 10 features

# Printing the Shapley values for the test data
print(explanation$dt)

p1 <- plot(explanation)

# ggplot2::ggsave("shapley_value_prediction_housing_data_Gaussian_10features_6testobs.png", plot = p1, device = NULL, path = NULL,
#                 scale = 1, width = 6, height = 6,
#                 dpi = 300, limitsize = TRUE)






























