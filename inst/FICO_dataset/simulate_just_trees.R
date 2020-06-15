library(xgboost)
library(shapr)
library(data.table)
library(MASS)
library(lqmm) ## to check if Sigma is positive definite
library(rapportools) # for testing booleans
library(ggplot2)
library(xtable)
library(reshape2)

simulateAllTrees <- function(given_ind,
                             x_train,
                             comb_indici,
                             comb_mincriterion,
                             mincriterion,
                             minsplit,
                             minbucket,
                             use_partykit = "on_error") {
  dependent_ind <- (1:dim(x_train)[2])[-given_ind]

  if (length(given_ind) %in% c(0, ncol(x_train))) {
    datact <- list()
  } else {

    ## currently no tests made to make sure that comb_indici and comb_mincriterion both exist
    ## if only one is provided, no split is made.
    if (!is.null(comb_indici) & !is.null(comb_mincriterion)) {
      if (length(given_ind) <= comb_indici) {
        mincriterion <- comb_mincriterion[1] # if alpha = 0.05 --> split tree if p < 0.05
      } else {
        mincriterion <- comb_mincriterion[2]
      }
    }

    x <- x_train[, given_ind, with = FALSE]
    y <- x_train[, dependent_ind, with = FALSE]

    df <- data.table::data.table(cbind(y, x))

    colnames(df) <- c(paste0("Y", 1:ncol(y)), paste0("V", given_ind))

    ynam <- paste0("Y", 1:ncol(y))
    fmla <- as.formula(paste(paste(ynam, collapse = "+"), "~ ."))

    # Running party:ctree if that works. If that fails, run partykit instead
    if (use_partykit == "on_error"){
      datact <- tryCatch(expr = {
        party::ctree(fmla, data = df, controls = party::ctree_control(minbucket = minbucket,
                                                                      mincriterion = mincriterion))
      },error = function(ex){
        warning("party::ctree ran into the error: ",ex, "Using partykit::ctree instead!")
        partykit::ctree(fmla, data = df, control = partykit::ctree_control(minbucket = minbucket,
                                                                           mincriterion = mincriterion,
                                                                           splitstat = "maximum"))
      })
    } else if (use_partykit == "never") {
      datact <- party::ctree(fmla, data = df, controls = party::ctree_control(minbucket = minbucket,
                                                                              mincriterion = mincriterion))
    } else {
      warning("Using partykit::ctree instead of party::ctree!")
      datact <- partykit::ctree(fmla, data = df, control = partykit::ctree_control(minbucket = minbucket,
                                                                                   mincriterion = mincriterion,
                                                                                   splitstat = "maximum"))
    }
  }

  return(list(tree = datact, given_ind = given_ind, dependent_ind = dependent_ind)) # return the whole tree
}


sample_ctree <- function(tree,
                         n_samples,
                         x_test,
                         x_train,
                         p,
                         sample) {
  datact <- tree$tree
  using_partykit <- (class(datact)[1]!="BinaryTree")

  cnms <- colnames(x_test)
  if (length(tree$given_ind) %in% c(0, p)) {
    ret <- x_test # matrix(x_test, ncol = p, nrow = 1)
  } else {
    given_ind <- tree$given_ind
    # given_ind_vec <- rep(0, length(x_test)) ## I don't think we actually use this?
    # given_ind_vec[given_ind] <- 1

    dependent_ind <- tree$dependent_ind

    x_test_given <- x_test[, given_ind, drop = FALSE, with = FALSE]

    xp <- x_test_given # data.table(matrix(x_test_given, nrow = 1, ncol = length(x_test_given))) # change by MJ
    colnames(xp) <- paste0("V", given_ind) # this is important for where() below

    if (using_partykit){
      fit.nodes <- predict(object = datact,type = "node")
      pred.nodes <- predict(object = datact, newdata = xp,type = "node") ## newdata must be data.frame +have the same colnames as x

    } else {
      fit.nodes <- party::where(object = datact)
      pred.nodes <- party::where(object = datact, newdata = xp) ## newdata must be data.frame +have the same colnames as x
    }

    rowno <- 1:dim(x_train)[1]

    # newrowno <- sample(rowno[fit.nodes == pred.nodes], n_samples, replace = TRUE)
    # depDT <- data.table::data.table(matrix(x_train[newrowno, dependent_ind], ncol = length(dependent_ind)))
    # givenDT <- data.table::data.table(matrix(x_test[1, given_ind], ncol = length(given_ind)))
    # ret <- data.table::data.table(matrix(0, nrow = n_samples, ncol = length(x_test)))
    # ret[, paste0("V", dependent_ind) := depDT]
    # ret[, paste0("V", given_ind) := givenDT]

    if (!sample) {
      if (length(rowno[fit.nodes == pred.nodes]) <= n_samples) {
        depDT <- data.table::data.table(x_train[rowno[fit.nodes == pred.nodes], dependent_ind,
                                                drop = FALSE, with = FALSE])
        givenDT <- data.table::data.table(x_test[1, given_ind, drop = FALSE, with = FALSE])

        ret <- cbind(depDT, givenDT)
        setcolorder(ret, colnames(x_train))

        # ret <- data.table::data.table(matrix(0, nrow = length(rowno[fit.nodes == pred.nodes]), ncol = length(x_test)))
        # ret[, paste0("V", dependent_ind) := depDT]
        # ret[, paste0("V", given_ind) := givenDT]
      } else {
        newrowno <- sample(rowno[fit.nodes == pred.nodes], n_samples, replace = TRUE)

        depDT <- data.table::data.table(x_train[newrowno, dependent_ind, drop = FALSE, with = FALSE])
        givenDT <- data.table::data.table(x_test[1, given_ind, drop = FALSE, with = FALSE])

        # ret <- data.table::data.table(matrix(0, nrow = n_samples, ncol = length(x_test)))
        # ret[, paste0("V", dependent_ind) := depDT]
        # ret[, paste0("V", given_ind) := givenDT]

        ret <- cbind(depDT, givenDT)
        setcolorder(ret, colnames(x_train))
      }
    } else {
      newrowno <- sample(rowno[fit.nodes == pred.nodes], n_samples, replace = TRUE)

      depDT <- data.table::data.table(x_train[newrowno, dependent_ind, drop = FALSE, with = FALSE])
      givenDT <- data.table::data.table(x_test[1, given_ind, drop = FALSE, with = FALSE])

      # ret <- data.table::data.table(matrix(0, nrow = n_samples, ncol = length(x_test)))
      # ret[, paste0("V", dependent_ind) := depDT]
      # ret[, paste0("V", given_ind) := givenDT]

      ret <- cbind(depDT, givenDT)
      setcolorder(ret, colnames(x_train))
    }
  }
  colnames(ret) <- cnms

  return(as.data.table(ret))
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

set.seed(1234)
test_ids = sample(1:nrow(data),size = 100,replace = F)#unlist(c(demo_id_1,demo_id_2,demo_id_3,491,1017,4806,3770,5624))
test_data0 = data.table(Id = test_ids)
setkey(test_data0)

data0 <- copy(data)
cols <- colnames(data0)
data0 <- data0[, RiskPerformance := NULL][ , (cols) := lapply(.SD, FUN = check_for_NA2), .SDcols = cols]
data2 <- data[complete.cases(cbind(data[, "RiskPerformance"], data0)), ]

data2[, MaxDelqEver := as.factor(MaxDelqEver)]
data2[, MaxDelq2PublicRecLast12M := as.factor(MaxDelq2PublicRecLast12M)]

##
test_data = merge(data2, test_data0, by = "Id", all.x = F, all.y = T)
data3 = data2[!(Id %in% test_data$Id)]

prop_train = 0.8

set.seed(123)
ss <- sample(1:nrow(data3), round(prop_train*nrow(data3)), replace = FALSE)
train_data <- data3[ss,]
valid_data <- data3[-ss ,]

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

# Performance on validation data
xgbFit_cv_regular$evaluation_log[iter==xgbFit_cv_regular$best_iteration]
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

xgbFit_regular$best_score

cv.pred_regular <- NULL
for (i in 1:5){
  cv.pred_regular<-cbind(cv.pred_regular,predict(xgbFit_cv_regular$models[[i]],xgbMatrix.test,ntreelimit = xgbFit_cv_regular$best_iteration))
}

test_data$pred_cv_regular = rowMeans(cv.pred_regular)
test_data$pred_regular = predict(xgbFit_regular,xgbMatrix.test)

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

p <- mean(y_train)

# Testing
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


# Define numeric train and test data
x_test_num = copy(x_test)
x_test_num$MaxDelqEver = as.numeric(as.character(x_test_num$MaxDelqEver))
x_test_num$MaxDelq2PublicRecLast12M = as.numeric(as.character(x_test_num$MaxDelq2PublicRecLast12M))

x_train_num = copy(x_train)
x_train_num$MaxDelqEver = as.numeric(as.character(x_train_num$MaxDelqEver))
x_train_num$MaxDelq2PublicRecLast12M = as.numeric(as.character(x_train_num$MaxDelq2PublicRecLast12M))

xgbFit_cv_regular$dummyfunc = dummyfunc
xgbFit_cv_regular$feature_names = colnames(x_train) # Need to add this manually as not stored in xgboost CV object

xgbFit_cv_regular_indep = xgbFit_cv_regular
class(xgbFit_cv_regular_indep) = "xgb.cv.synchronous.indep"

predict_model.xgb.cv.synchronous.indep(xgbFit_cv_regular_indep,head(x_test_num,2))

set.seed(123)
explainer_cv_regular_indep <- shapr(x_train_num, xgbFit_cv_regular_indep,n_combinations = n_combinations)
set.seed(123)
explainer_cv_regular <- shapr(x_train, xgbFit_cv_regular, n_combinations = n_combinations)

chunks = split(1:100, ceiling(seq_along(1:100)/10))
#-----------------
set.seed(123)
# explanation_cv_regular <- explain(
#   x = x_test[chunks[[i]]],
#   approach = 'ctree',
#   explainer = explainer_cv_regular,
#   prediction_zero = p,
#   sample = TRUE,
#   mc_cores = 4
# )

#i = 10
x = x_test#[chunks[[i]]]
approach = 'ctree'
explainer = explainer_cv_regular
prediction_zero = p
sample = TRUE
mc_cores = 1


comb_indici = NULL
comb_mincriterion = NULL

mincriterion = 0.95
minsplit = 20
minbucket = 7
sample = TRUE

# Add arguments to explainer object
explainer$x_test <- data.table::as.data.table(x)
explainer$approach <- approach
explainer$comb_indici <- comb_indici
explainer$comb_mincriterion <- comb_mincriterion
explainer$mincriterion <- mincriterion
explainer$minsplit <- minsplit
explainer$minbucket <- minbucket
explainer$sample <- sample

x <- explainer

seed = 1
n_samples = 1e3
index_features = NULL
mc_cores = 1
mc_cores_simulateAllTrees = mc_cores
mc_cores_sample_ctree = mc_cores

id <- id_combination <- w <- NULL # due to NSE notes in R CMD check

n_xtest <- nrow(x$x_test)
dt_l <- list()
if (!is.null(seed)) set.seed(seed)

if (is.null(index_features)) {
  features <- x$X$features
} else {
  features <- x$X$features[index_features]
}

if (!is.null(x$comb_indici)) {
  stopifnot(x$comb_indici >= 0)
  stopifnot(x$comb_indici <= ncol(x$x_train))
  stopifnot(length(x$comb_indici) == 1)
  stopifnot(!is.null(x$comb_mincriterion))
}
if (!is.null(x$comb_mincriterion)) {
  stopifnot(!is.null(x$comb_indici))
  stopifnot(length(x$comb_mincriterion) == 2)
  stopifnot(all(x$comb_mincriterion <= 1))
  stopifnot(all(x$comb_mincriterion >= 0))
}

print("starting to simulate trees")


chunks_features = split(1:length(features), ceiling(seq_along(1:length(features))/250))

## this is the list of all 2^10 trees (if number of features = 10)
# this takes about 7 hours !
# for(i in 1:length(chunks_features)){
#   tt <- Sys.time()
#   all_trees <- parallel::mclapply(
#     X = features[chunks_features[[i]]],
#     FUN = simulateAllTrees,
#     x_train = x$x_train,
#     comb_indici = x$comb_indici,
#     comb_mincriterion = x$comb_mincriterion,
#     mincriterion = x$mincriterion,
#     minsplit = x$minsplit,
#     minbucket = x$minbucket,
#     mc.cores = mc_cores_simulateAllTrees,
#     mc.set.seed = FALSE
#   )
#   saveRDS(all_trees, file = paste0("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/data/trees/all_trees_chunk_",  i, "_.rds"))
#   print(Sys.time() - tt)
#   print(paste0('saved the ', i, ' th chunk of trees.'))
#
# }

#print('trees completed')
#

# load trees
#files <- list.files("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/data/trees", pattern = "*.rds")

tt_start <- Sys.time()
for(k in 2:13){

  tt <- Sys.time()
  all_trees <- readRDS(paste0("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/data/trees/all_trees_chunk_", k, "_.rds"))
  print(Sys.time() - tt)
  print("Data loaded")

  tt <- Sys.time()
  for (i in seq(n_xtest)) {
    l <- parallel::mclapply(
      X = all_trees,
      FUN = sample_ctree,
      n_samples = n_samples,
      x_test = x$x_test[i, , drop = FALSE],
      x_train = x$x_train,
      p = ncol(x$x_test),
      sample = x$sample,
      mc.cores = mc_cores_sample_ctree,
      mc.set.seed = FALSE
    )

    dt_l[[i]] <- data.table::rbindlist(l, idcol = "id_combination")
    dt_l[[i]][, w := 1 / n_samples]
    dt_l[[i]][, id := i]
    if (!is.null(index_features)) dt_l[[i]][, id_combination := index_features[id_combination]]
  }
  print(Sys.time() - tt)

  dt <- data.table::rbindlist(dt_l, use.names = TRUE, fill = TRUE)
  dt[id_combination %in% c(1, 2^ncol(x$x_test)), w := 1.0]

  ## only return unique dt
  dt2 <- dt[, sum(w), by = c("id_combination", colnames(x$x_test), "id")]
  setnames(dt2, "V1", "w")
  dt2[, id_combination2 := chunks_features[[k]][id_combination]]
  dt3 <- dt2[['id_combination2']]
  dt4 <- cbind(dt3, dt2)
  dt4[, 'id_combination' := NULL]
  dt4[, 'id_combination2' := NULL]
  setnames(dt4, 'dt3', 'id_combination')

  tt <- Sys.time()
  saveRDS(dt4, file = paste0("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/data/trees/dt_chunk_",  k, ".rds"))
  print(Sys.time() - tt)
  #
  print("Saved file")
  rm(all_trees)
  rm(dt_l)
  rm(dt)
  rm(dt2)
  rm(dt3)
  rm(dt4)

  gc()

}

print(Sys.time() - tt_start)




