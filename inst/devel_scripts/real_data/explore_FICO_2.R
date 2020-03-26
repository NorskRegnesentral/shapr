library(xgboost)
library(shapr)
library(data.table)
library(MASS)
library(lqmm) ## to check if Sigma is positive definite
library(rapportools) # for testing booleans
library(ggplot2)
library(xtable)
library(reshape2)


## this function is the same as 'plot' in shapr but I have made the font bigger
plot_shapr <- function(x,
                       digits = 3,
                       plot_phi0 = TRUE,
                       index_x_test = NULL,
                       top_k_features = NULL,
                       ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is not installed. Please run install.packages('ggplot2')")
  }

  if (is.null(index_x_test)) index_x_test <- seq(nrow(x$x_test))
  if (is.null(top_k_features)) top_k_features <- ncol(x$x_test) + 1
  id <- phi <- NULL # due to NSE notes in R CMD check

  # melting Kshap
  cnms <- colnames(x$x_test)
  KshapDT <- data.table::copy(x$dt)
  KshapDT[, id := .I]
  meltKshap <- data.table::melt(KshapDT, id.vars = "id", value.name = "phi")
  meltKshap[, sign := factor(sign(phi), levels = c(1, -1), labels = c("Increases", "Decreases"))]

  # Converting and melting Xtest
  desc_mat <- format(x$x_test, digits = digits)
  for (i in 1:ncol(desc_mat)) {
    desc_mat[, i] <- paste0(cnms[i], " = ", desc_mat[, i])
  }
  desc_dt <- data.table::as.data.table(cbind(none = "none", desc_mat))
  melt_desc_dt <- data.table::melt(desc_dt[, id := .I], id.vars = "id", value.name = "description")

  # Data table for plotting
  plotting_dt <- merge(meltKshap, melt_desc_dt)

  # Adding the predictions
  predDT <- data.table::data.table(id = KshapDT$id, pred = x$p)
  plotting_dt <- merge(plotting_dt, predDT, by = "id")

  # Adding header for each individual plot
  header <- variable <- pred <- description <- NULL # due to NSE notes in R CMD check
  plotting_dt[, header := paste0("id: ", id, ", pred = ", format(pred, digits = digits + 1))]

  if (!plot_phi0) {
    plotting_dt <- plotting_dt[variable != "none"]
  }
  plotting_dt <- plotting_dt[id %in% index_x_test]
  plotting_dt[, rank := data.table::frank(-abs(phi)), by = "id"]
  plotting_dt <- plotting_dt[rank <= top_k_features]
  plotting_dt[, description := factor(description, levels = unique(description[order(abs(phi))]))]

  # Plotting
  gg <- ggplot2::ggplot(plotting_dt) +
    ggplot2::facet_wrap(~header, scales = "free_y", labeller = "label_value", ncol = 2) +
    ggplot2::geom_col(ggplot2::aes(x = description, y = phi, fill = sign)) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = c("steelblue", "lightsteelblue"), drop = TRUE) +
    ggplot2::labs(
      y = "Feature contribution",
      x = "Feature",
      fill = "",
      title = "Shapley value prediction explanation"
    ) +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = ggplot2::element_text(hjust = 0.5),
      text = element_text(size=31)
    )

  return(gg)
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
# data <- read.table(file = "/nr/project/stat/BigInsight/Projects/Explanations/Data/fico.csv", sep = ",", header = TRUE,
#                    stringsAsFactors = TRUE )

## -----------------------

data <- read.table(file = "/nr/project/stat/BigInsight/Projects/Explanations/Data/FICO_HELOC_dataset_v1.csv", sep = ",", header = TRUE, stringsAsFactors = TRUE )
data <- data.table(data)
nrow(data) # 10459
ncol(data)
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


# ### Computing accuracy and AUC on validation data
#
# pred_valid = predict(xgbFit_monotone,xgbMatrix.valid)
#
# # NO this is not the way to do it
# # pred_all = NULL
# #
# # for (i in 1:5){
# #   pred = predict(xgbFit_cv_regular$models[[i]],xgbMatrix.train.valid,ntreelimit = xgbFit_cv_regular$best_iteration)
# #   pred[xgbFit_cv_regular$folds[[i]]] = NA
# #
# #   pred_all <-cbind(pred_all,pred)
# # }
# #
# #
# # pred_all_final = 1/4*rowSums(pred_all,na.rm = T)
#
# pred_all = NULL
#
# mod = xgbFit_cv_regular#xgbFit_cv_monotone#xgbFit_cv_regular
#
# for (i in 1:5){
#   pred = predict(mod$models[[i]],xgbMatrix.train.valid,ntreelimit = mod$best_iteration)
#   pred_all[mod$folds[[i]]] = pred[mod$folds[[i]]]
#
#   #  pred_all <-cbind(pred_all,pred)
# }
#
# for (i in seq(0.3,0.7,0.01)){
#   confmat = SDMTools::confusion.matrix(c(y_train,y_valid),pred_all,threshold = i)
#   N = sum(confmat[,1])
#   P = sum(confmat[,2])
#   TN = confmat[1,1]
#   TP = confmat[2,2]
#   (accuracy = (TP+TN)/(P+N))
#   print(accuracy)
# }
# confmat = SDMTools::confusion.matrix(c(y_train,y_valid),pred_all,threshold = 0.5)
# N = sum(confmat[,1])
# P = sum(confmat[,2])
# TN = confmat[1,1]
# TP = confmat[2,2]
# (accuracy = (TP+TN)/(P+N))
# print(accuracy)
#
# #### Trying to estimate accuracy in the correct way, accoutning for our 1/5 splitting
# this.seed <- 1234 # Seed used in fitting procedure
#
# tt = proc.time()
# folds = xgbFit_cv_regular$folds
#
# finalpred_regular = rep(NA,dim(xgbMatrix.train.valid)[1])
# cv.pred_regular_list= list()
# xgbFit_cv_regular_list = list()
# for (i in 1:5){
#   idxset_train = sort(unlist(folds[-i]))
#   idxset_test = sort(unlist(folds[i]))
#
#   xgbMatrix_temp_train = slice(xgbMatrix.train.valid,idxset=idxset_train)
#   xgbMatrix_temp_test = slice(xgbMatrix.train.valid,idxset=idxset_test)
#
#   set.seed(this.seed)
#   xgbFit_cv_regular_list[[i]] <- xgb.cv(data=xgbMatrix_temp_train,
#                                         params = params,
#                                         nrounds = nrounds,
#                                         early_stopping_rounds = early_stopping_rounds,
#                                         callbacks = list(cb.cv.predict(save_models = TRUE)),
#                                         print_every_n = print_every_n,
#                                         nfold = 5)
#
#   print(i)
#
#   tmp =  NULL
#   for (j in 1:5){
#     tmp <-cbind(tmp,predict(xgbFit_cv_regular_list[[i]]$models[[j]],
#                             xgbMatrix_temp_test,
#                             ntreelimit = xgbFit_cv_regular_list[[i]]$best_iteration))
#   }
#   finalpred_regular[idxset_test] = rowMeans(tmp)
#
# }
# proc.time()-tt
#
# for (i in seq(0.3,0.7,0.01)){
#   confmat = SDMTools::confusion.matrix(c(y_train,y_valid),finalpred_regular,threshold = i)
#   N = sum(confmat[,1])
#   P = sum(confmat[,2])
#   TN = confmat[1,1]
#   TP = confmat[2,2]
#   (accuracy = (TP+TN)/(P+N))
#   print(accuracy)
# }
# confmat = SDMTools::confusion.matrix(c(y_train,y_valid),finalpred_regular,threshold = 0.5)
# N = sum(confmat[,1])
# P = sum(confmat[,2])
# TN = confmat[1,1]
# TP = confmat[2,2]
# (accuracy = (TP+TN)/(P+N))
# print(accuracy)
# #> print(accuracy)
# #[1] 0.7367299 # THis is the one we report
#
# #### THE monotone version too
#
# this.seed <- 1234 # Seed used in fitting procedure
#
# tt = proc.time()
# folds = xgbFit_cv_regular$folds
#
# finalpred_monotone = rep(NA,dim(xgbMatrix.train.valid)[1])
# cv.pred_monotone_list= list()
# xgbFit_cv_monotone_list = list()
# for (i in 1:5){
#   idxset_train = sort(unlist(folds[-i]))
#   idxset_test = sort(unlist(folds[i]))
#
#   xgbMatrix_temp_train = slice(xgbMatrix.train.valid,idxset=idxset_train)
#   xgbMatrix_temp_test = slice(xgbMatrix.train.valid,idxset=idxset_test)
#
#   set.seed(this.seed)
#   xgbFit_cv_monotone_list[[i]] <- xgb.cv(data=xgbMatrix_temp_train,
#                                          params = params_monotone,
#                                          nrounds = nrounds,
#                                          early_stopping_rounds = early_stopping_rounds,
#                                          callbacks = list(cb.cv.predict(save_models = TRUE)),
#                                          print_every_n = print_every_n,
#                                          nfold = 5)
#
#   print(i)
#
#   tmp =  NULL
#   for (j in 1:5){
#     tmp <-cbind(tmp,predict(xgbFit_cv_monotone_list[[i]]$models[[j]],
#                             xgbMatrix_temp_test,
#                             ntreelimit = xgbFit_cv_monotone_list[[i]]$best_iteration))
#   }
#   finalpred_monotone[idxset_test] = rowMeans(tmp)
#
# }
# proc.time()-tt
#
# for (i in seq(0.3,0.7,0.01)){
#   confmat = SDMTools::confusion.matrix(c(y_train,y_valid),finalpred_monotone,threshold = i)
#   N = sum(confmat[,1])
#   P = sum(confmat[,2])
#   TN = confmat[1,1]
#   TP = confmat[2,2]
#   (accuracy = (TP+TN)/(P+N))
#   print(accuracy)
# }
# confmat = SDMTools::confusion.matrix(c(y_train,y_valid),finalpred_monotone,threshold = 0.5)
# N = sum(confmat[,1])
# P = sum(confmat[,2])
# TN = confmat[1,1]
# TP = confmat[2,2]
# (accuracy = (TP+TN)/(P+N))
# print(accuracy)
#
#
#
#
# ##########




#save(x_train,x_valid,x_test,test_data,file = "/nr/project/stat/BigInsight/Projects/Explanations/Data/FICO_data_for_modelling.RData")


# Prepare the cv-model for fitting into the shapr machinery


#### XGBoost cv model prepared for shapr call ###
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

# Using only the demo cases
x_test = x_test[c(2,4,5),]


p <- mean(y_train)


xgbFit_cv_monotone$dummyfunc = dummyfunc
xgbFit_cv_monotone$feature_names = colnames(x_train) # Need to add this manually as not stored in xgboost CV object

# Testing
predict_model.xgb.cv.synchronous(xgbFit_cv_monotone,x_test)

n_combinations = 5000

#### Continue here, defining explainer_numeric similalry to how we did for the numeric_lm ####

model_type.xgb.cv.synchronous.indep <- function(x) {
  type =  "regression"
  if(is.null(x$dummyfunc)  && !is.null(x$params$objective) && x$params$objective == "binary:logistic") type = "classification"
  if(!is.null(x$dummyfunc) && !is.null(x$params$objective) && x$params$objective == "binary:logistic") type = "cat_regression"
  return(type)
}

features.xgb.cv.synchronous.indep <- function(x, cnms, feature_labels = NULL) {
  if (!is.null(feature_labels)) message_features_labels()

  nms <- x$feature_names

  if (!all(nms %in% cnms)) error_feature_labels()

  return(nms)
}

predict_model.xgb.cv.synchronous.indep <- function(x, newdata) {
  if (!requireNamespace("stats", quietly = TRUE)) {
    stop("The xgboost package is required for predicting xgboost models")
  }
  if (model_type(x) == "cat_regression") {
    newdata$MaxDelqEver = factor(newdata$MaxDelqEver,levels = c("2", "3", "4", "5", "6", "7", "8"))
    newdata$MaxDelq2PublicRecLast12M = factor(newdata$MaxDelq2PublicRecLast12M,levels = c("0", "1", "2", "3", "4", "5", "6", "7" ,"9"))

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
} # This is super-hacky but works!

xgbFit_cv_monotone_indep = xgbFit_cv_monotone
class(xgbFit_cv_monotone_indep) = "xgb.cv.synchronous.indep"

# Define numeric train and test data
x_test_num = copy(x_test)
x_test_num$MaxDelqEver = as.numeric(as.character(x_test_num$MaxDelqEver))
x_test_num$MaxDelq2PublicRecLast12M = as.numeric(as.character(x_test_num$MaxDelq2PublicRecLast12M))

x_train_num = copy(x_train)
x_train_num$MaxDelqEver = as.numeric(as.character(x_train_num$MaxDelqEver))
x_train_num$MaxDelq2PublicRecLast12M = as.numeric(as.character(x_train_num$MaxDelq2PublicRecLast12M))

# Testing
model_type(xgbFit_cv_monotone_indep)
features(xgbFit_cv_monotone_indep,colnames(x_train))



predict_model(xgbFit_cv_monotone_indep,x_test_num)


set.seed(123)
explainer_cv_monotone_num <- shapr(x_train_num, xgbFit_cv_monotone_indep,n_combinations = n_combinations)
set.seed(123)

explanation_cv_monotone_ind <- explain(
  x = x_test_num,
  approach = "empirical",
  type = "independence",
  explainer = explainer_cv_monotone_num,
  prediction_zero = p,
  w_threshold = 1)


save(explanation_cv_monotone_ind,explainer_cv_monotone_num,xgbFit_cv_monotone,
     file = "/nr/project/stat/BigInsight/Projects/Explanations/Data/FICO_explanations_cv_monotone_indep_2.RData")


xgbFit_cv_regular$dummyfunc = dummyfunc
xgbFit_cv_regular$feature_names = colnames(x_train) # Need to add this manually as not stored in xgboost CV object


xgbFit_cv_regular_indep = xgbFit_cv_regular
class(xgbFit_cv_regular_indep) = "xgb.cv.synchronous.indep"


set.seed(123)
explainer_cv_regular_num <- shapr(x_train_num, xgbFit_cv_regular_indep,n_combinations = n_combinations)
set.seed(123)

explanation_cv_regular_ind <- explain(
  x = x_test_num,
  approach = "empirical",
  type = "independence",
  explainer = explainer_cv_regular_num,
  prediction_zero = p,
  w_threshold = 1)

save(explanation_cv_regular_ind,explainer_cv_regular_num,xgbFit_cv_regular,
     file = "/nr/project/stat/BigInsight/Projects/Explanations/Data/FICO_explanations_cv_regular_indep_2.RData")




set.seed(123)
explainer_cv_monotone <- shapr(x_train, xgbFit_cv_monotone,n_combinations = n_combinations)
#tm <- Sys.time()
set.seed(123)

explanation_cv_monotone <- explain(
  x = x_test,
  approach = 'ctree',
  explainer = explainer_cv_monotone,
  prediction_zero = p,
  sample = TRUE
)

save(explanation_cv_monotone,explainer_cv_monotone,xgbFit_cv_monotone,
     file = "/nr/project/stat/BigInsight/Projects/Explanations/Data/FICO_explanations_cv_monotone_ctree_2.RData")


set.seed(123)
explainer_cv_regular <- shapr(x_train, xgbFit_cv_regular,n_combinations = n_combinations)
set.seed(123)
explanation_cv_regular <- explain(
  x = x_test,
  approach = 'ctree',
  explainer = explainer_cv_regular,
  prediction_zero = p,
  sample = TRUE
)

save(explanation_cv_regular,explainer_cv_regular,xgbFit_cv_regular,
     file = "/nr/project/stat/BigInsight/Projects/Explanations/Data/FICO_explanations_cv_regular_ctree_2.RData")
