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

demo_id_1 = find_id(data, Value1 = 61, Value2 = 49, Value3 = 19, Value4 = 29)
demo_id_2 = find_id(data, Value1 = 59, Value2 = 131, Value3 = 7, Value4 = 81)
demo_id_3 = find_id(data, Value1 = 92, Value2 = 372, Value3 = 10, Value4 = 176)

test_ids = unlist(c(demo_id_1,demo_id_2,demo_id_3,491,1017,4806,3770,5624))
test_preds_duke = c(0.952,0.895,0.049,0.888,0.594,0.696,0.332,0.241)

test_data0 = data.table(Id =test_ids,pred_duke=test_preds_duke)
setkey(test_data0)



data0 <- copy(data)
cols <- colnames(data0)
data0 <- data0[, RiskPerformance := NULL][ , (cols) := lapply(.SD, FUN = check_for_NA2), .SDcols = cols]

data2 <- data[complete.cases(cbind(data[,"RiskPerformance"], data0)), ]

dim(data0)[1] - dim(data2)[1] # 598 have -9 everywhere

data2[, MaxDelqEver := as.factor(MaxDelqEver)]
data2[, MaxDelq2PublicRecLast12M := as.factor(MaxDelq2PublicRecLast12M)]


##
test_data = merge(data2,test_data0,by="Id",all.x = F,all.y=T)
data3 = data2[!(Id%in%test_data$Id)]

prop_train = 0.8

set.seed(123)
ss <- sample(1:nrow(data3), round(prop_train*nrow(data3)), replace = FALSE)
train_data <- data3[ss,]
valid_data <- data3[-ss ,]

dim(train_data) # 9459 obs, 24 columns
dim(valid_data)



##
y_var <- "RiskPerformance"
cat_var <- c("MaxDelqEver", "MaxDelq2PublicRecLast12M")
cont_var <- c("ExternalRiskEstimate", "MSinceOldestTradeOpen", "MSinceMostRecentTradeOpen", "AverageMInFile", "NumSatisfactoryTrades", "NumTrades60Ever2DerogPubRec", "NumTrades90Ever2DerogPubRec",
              "PercentTradesNeverDelq", "MSinceMostRecentDelq", "NumTotalTrades", "NumTradesOpeninLast12M", "PercentInstallTrades",
              "MSinceMostRecentInqexcl7days", "NumInqLast6M", "NumInqLast6Mexcl7days", "NetFractionRevolvingBurden", "NetFractionInstallBurden", "NumRevolvingTradesWBalance",
              "NumInstallTradesWBalance", "NumBank2NatlTradesWHighUtilization", "PercentTradesWBalance")

some_var <- c(cat_var, cont_var)#[1:10]

# Coding Bad as 1, to get probability of defaulting (this is what the Duke people do)
y_train <- unlist((train_data[,..y_var]=="Bad")*1)
y_valid <- unlist((valid_data[,..y_var]=="Bad")*1)
y_test <- unlist((test_data[,..y_var]=="Bad")*1)

x_train <- train_data[, ..some_var]
x_valid <- valid_data[, ..some_var]
x_test <- test_data[, ..some_var]


#### MJ starts preparing for xgboost fit ####

library(caret)
dummyfunc <- caret::dummyVars(" ~ .", data = rbind(x_train,x_valid,x_test))
x_train_dummy=predict(dummyfunc, newdata = x_train)
x_valid_dummy=predict(dummyfunc, newdata = x_valid)
x_test_dummy=predict(dummyfunc, newdata = x_test)

colnames_dummy = colnames(x_train_dummy)
montone_constrains_dt = data.table(colname = colnames_dummy,
                               constraints = c(rep(0,16),
                               rep(-1,5),
                               rep(1,2),
                               rep(-1,2),
                               0,
                               1,
                               0,
                               -1,
                               rep(1,4),
                               rep(0,2),
                               1,
                               0))



xgbMatrix.train <- xgb.DMatrix(data=x_train_dummy,
                               label = y_train)

xgbMatrix.valid <- xgb.DMatrix(data=x_valid_dummy,
                               label = y_valid)

xgbMatrix.train.valid = xgb.DMatrix(data=rbind(x_train_dummy,x_valid_dummy),
                                    label = c(y_train,y_valid))


xgbMatrix.test <- xgb.DMatrix(data=x_test_dummy,
                              label = y_test)


params <- list(eta = 0.3,
               max_depth = 3,
               objective= 'binary:logistic',
               eval_metric = c("auc"),
               tree_method="hist")

params_monotone = params
params_monotone$monotone_constraints = montone_constrains_dt$constraints
#params_monotone$max_bin = 512


early_stopping_rounds <- 50 # Training stops when the validation AUC scores stops increasing for early_stopping_rounds number of iterations
print_every_n <- 10 # How often the xgboost model shoud print AUC-scores during training
nrounds <- 1000 # Max number of iterations
this.seed <- 1234 # Seed used in fitting procedure

set.seed(this.seed)
tt = proc.time()
 xgbFit_cv_regular <- xgb.cv(data=xgbMatrix.train.valid,
                             params = params,
                             nrounds = nrounds,
                             early_stopping_rounds = early_stopping_rounds,
                             callbacks = list(cb.cv.predict(save_models = TRUE)),
                             print_every_n = print_every_n,
                             nfold = 5)
 proc.time()-tt

set.seed(this.seed)
 tt = proc.time()
 xgbFit_cv_monotone <- xgb.cv(data=xgbMatrix.train.valid,
                             params = params_monotone,
                             nrounds = nrounds,
                             early_stopping_rounds = early_stopping_rounds,
                             callbacks = list(cb.cv.predict(save_models = TRUE)),
                             print_every_n = print_every_n,
                             nfold = 5)
 proc.time()-tt

 # Performance on validation data
 xgbFit_cv_regular$evaluation_log[iter==xgbFit_cv_regular$best_iteration]
 xgbFit_cv_monotone$evaluation_log[iter==xgbFit_cv_monotone$best_iteration]


set.seed(this.seed)
tt = proc.time()
xgbFit_regular <- xgb.train(data=xgbMatrix.train,
                   params = params,
                   nrounds = nrounds,
                   watchlist = list(train = xgbMatrix.train, # train
                                    test = xgbMatrix.test, # test
                                    validation = xgbMatrix.valid), # validation (important that this is given last)
                   early_stopping_rounds = early_stopping_rounds,
                   print_every_n = print_every_n)
proc.time()-tt

set.seed(this.seed)
tt = proc.time()
xgbFit_monotone <- xgb.train(data=xgbMatrix.train,
                            params = params_monotone,
                            nrounds = nrounds,
                            watchlist = list(train = xgbMatrix.train, # train
                                             test = xgbMatrix.test, # test
                                             validation = xgbMatrix.valid), # validation (important that this is given last)
                            early_stopping_rounds = early_stopping_rounds,
                            print_every_n = print_every_n)
proc.time()-tt

# Performance on validation data
xgbFit_monotone$best_score
xgbFit_regular$best_score

cv.pred_regular <- cv.pred_monotone <- NULL
for (i in 1:5){
  cv.pred_regular<-cbind(cv.pred_regular,predict(xgbFit_cv_regular$models[[i]],xgbMatrix.test,ntreelimit = xgbFit_cv_regular$best_iteration))
  cv.pred_monotone<-cbind(cv.pred_monotone,predict(xgbFit_cv_monotone$models[[i]],xgbMatrix.test,ntreelimit = xgbFit_cv_monotone$best_iteration))
}


test_data$pred_cv_monotone = rowMeans(cv.pred_monotone)
test_data$pred_cv_regular = rowMeans(cv.pred_regular)

test_data$pred_monotone = predict(xgbFit_monotone,xgbMatrix.test)
test_data$pred_regular = predict(xgbFit_regular,xgbMatrix.test)

test_data[,AE_cv_monotone:=abs(pred_cv_monotone-pred_duke)]
test_data[,AE_cv_regular:=abs(pred_cv_regular-pred_duke)]

test_data[,AE_monotone:=abs(pred_monotone-pred_duke)]
test_data[,AE_regular:=abs(pred_regular-pred_duke)]

mean(test_data$AE_monotone)
mean(test_data$AE_regular)
mean(test_data$AE_cv_monotone)
mean(test_data$AE_cv_regular)

save(x_train,x_valid,x_test,test_data,file = "/nr/project/stat/BigInsight/Projects/Explanations/Data/FICO_data_for_modelling.RData")


# Prepare the cv-model for fitting into the shapr machinery


#### XGBoost cv modell prepared for shapr call ###
model_type.xgb.cv.synchronous <- function(x) {
  type =  "regression"
  if(is.null(x$dummyfunc)  && !is.null(x$params$objective) && x$params$objective == "binary:logistic") type = "classification"
  if(!is.null(x$dummyfunc) && !is.null(x$params$objective) && x$params$objective == "binary:logistic") type = "cat_regression"
  return(type)
}

predict_model.xgb.cv.synchronous <- function(x, newdata) {
  if (!requireNamespace("stats", quietly = TRUE)) {
    stop("The xgboost package is required for predicting xgboost models")
  }
  if (model_type(x) == "cat_regression") {
    newdata_dummy <- as.matrix(predict(x$dummyfunc, newdata = newdata))
    cv.pred <- NULL
    for (i in 1:length(x$folds)){
      cv.pred = cbind(cv.pred,predict(x$models[[i]], newdata_dummy,ntreelimit = x$best_iteration))
    }
  } else {
    cv.pred <- NULL
    for (i in 1:length(x$folds)){
      cv.pred = cbind(cv.pred,predict(x$models[[i]], as.matrix(newdata),ntreelimit = x$best_iteration))
    }
  }
  return(rowMeans(cv.pred))
}

features.xgb.cv.synchronous <- function(x, cnms, feature_labels = NULL) {
  if (!is.null(feature_labels)) message_features_labels()

  nms <- x$feature_names

  if (!all(nms %in% cnms)) error_feature_labels()

  return(nms)
}



p <- mean(y_train)


xgbFit_cv_monotone$dummyfunc = dummyfunc
xgbFit_cv_monotone$feature_names = colnames(x_train) # Need to add this manually as not stored in xgboost CV object

# Testing
predict_model.xgb.cv.synchronous(xgbFit_cv_monotone,x_test)

n_combinations = 5000

explainer_cv_monotone <- shapr(x_train, xgbFit_cv_monotone,n_combinations = n_combinations)
tm <- Sys.time()
explanation_cv_monotone <- explain(
  x = x_test,
  approach = 'ctree',
  explainer = explainer_cv_monotone,
  prediction_zero = p,
  sample = TRUE
)
tm2 <- Sys.time()
print(tm2 - tm)

save(explanation_cv_monotone,explainer_cv_monotone,xgbFit_cv_monotone,file = "/nr/project/stat/BigInsight/Projects/Explanations/Data/FICO_explanations_cv_monotone.RData")


xgbFit_cv_regular$dummyfunc = dummyfunc
xgbFit_cv_regular$feature_names = colnames(x_train) # Need to add this manually as not stored in xgboost CV object

explainer_cv_regular <- shapr(x_train, xgbFit_cv_regular,n_combinations = n_combinations)
tm <- Sys.time()
explanation_cv_regular <- explain(
  x = x_test,
  approach = 'ctree',
  explainer = explainer_cv_regular,
  prediction_zero = p,
  sample = TRUE
)
tm2 <- Sys.time()
print(tm2 - tm) # 2.8 minutes for 10 features and 6 test observations

save(explanation_cv_regular,explainer_cv_regular,xgbFit_cv_regular,file = "/nr/project/stat/BigInsight/Projects/Explanations/Data/FICO_explanations_cv_regular.RData")


#p1 <- plot(explanation)
# ggplot2::ggsave("shapley_value_prediction_housing_data_Gaussian_10features_6testobs.png", plot = p1, device = NULL, path = NULL,
#                 scale = 1, width = 6, height = 6,
#                 dpi = 300, limitsize = TRUE)






























