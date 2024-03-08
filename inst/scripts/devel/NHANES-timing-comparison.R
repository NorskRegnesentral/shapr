source("/nr/project/stat/BigInsight//Projects//Explanations//EffektivShapley//helpFunctions.R")

#library(treeshap)
library(xgboost)
library(data.table)
library(shapr)

plotFig <- 0

datafolder <- "/nr/project/stat/BigInsight/Projects/Explanations/EffektivShapley/NHANES-data/"

x_explain0 <- fread(file.path(datafolder,"newdata/Xtest_imp.csv"))
y_test0    <- fread(file.path(datafolder,"newdata/ytest.csv"))$V1
x_train0   <- fread(file.path(datafolder,"newdata/Xtrain_imp.csv"))
y_train0   <- fread(file.path(datafolder,"newdata/ytrain.csv"))$V1

names(x_train0)[52:55]   <- c("urine_albumin_is_gt_30","urine_albumin_is_gt_100","urine_albumin_is_get_300", "urine_albumin_is_gt_1000")
names(x_explain0)[52:55] <- c("urine_albumin_is_gt_30","urine_albumin_is_gt_100","urine_albumin_is_get_300", "urine_albumin_is_gt_1000")

model <- xgboost::xgb.load(file.path(datafolder,"newdata/xgb_model_imp.json"))

preds <- predict(model,as.matrix(x_explain0),outputmargin = TRUE)
#preds <- log(predict(model,as.matrix(x_explain)))

#ind   <- rev(order(preds))[1:2]
#ind   <- rev(order(preds))[9:10]
#x_explain <- x_explain[ind,]

timing_vS_computation <- function(type="Martin"){
  shap_approach = "kernel"
  paired_shap_sampling = FALSE
  n_permutations = NULL
  n_combinations = NULL
  group = NULL
  n_samples = 1e3
  n_batches = NULL
  seed = 1
  keep_samp_for_vS = TRUE
  get_model_specs = NULL
  MSEv_uniform_comb_weights = TRUE
  timing = TRUE
  n_batches = 1

  feature_specs <- shapr:::get_feature_specs(get_model_specs, model)


  # Sets up and organizes input parameters
  # Checks the input parameters and their compatability
  # Checks data/model compatability
  internal <- shapr:::setup(
    x_train = x_train,
    x_explain = x_explain,
    approach = approach,
    shap_approach = shap_approach,
    paired_shap_sampling = paired_shap_sampling,
    prediction_zero = prediction_zero,
    n_combinations = n_combinations,
    n_permutations = n_permutations,
    group = group,
    n_samples = n_samples,
    n_batches = n_batches,
    seed = seed,
    keep_samp_for_vS = keep_samp_for_vS,
    feature_specs = feature_specs,
    MSEv_uniform_comb_weights = MSEv_uniform_comb_weights,
    timing = timing
  )


  # Gets predict_model (if not passed to explain)
  predict_model <- shapr:::get_predict_model(
    predict_model = predict_model,
    model = model
  )


  # Sets up the Shapley (sampling) framework and prepares the
  # conditional expectation computation for the chosen approach
  # Note: model and predict_model are ONLY used by the AICc-methods of approach empirical to find optimal parameters
  internal <- shapr:::setup_computation(internal, model, predict_model)


  if(type == "Martin"){
    time0 <- Sys.time()
    vS_list <- compute_vS(internal, model, predict_model)
    time1 <- Sys.time()
  } else {
    not_in_indexTab <- which(is.na(match(seq_len(ncol(x_train0)),indexTab)))
    x_train0[,..not_in_indexTab]

    internal$data$x_train <- cbind(internal$data$x_train,x_train0[,..not_in_indexTab])
    internal$data$x_explain <- cbind(internal$data$x_explain,x_explain0[testObs,..not_in_indexTab])
    internal$parameters$n_features <- ncol(x_train0)
    internal$parameters$feature_names <- colnames(internal$data$x_train)
    internal$objects$feature_specs$labels <- colnames(internal$data$x_train)


    class(model) = "xgb.Booster"
    predict_model <- function(model,newdata){

      newdata_reordered <- matrix(NA,ncol=ncol(newdata),nrow=nrow(newdata))
      seq_indexTab <- seq_along(indexTab)
      seq_not_in_indexTab <- length(indexTab)+seq_along(not_in_indexTab)

      newdata_reordered[,indexTab] <- as.matrix(newdata[,..seq_indexTab])
      newdata_reordered[,not_in_indexTab] <- as.matrix(newdata[,..seq_not_in_indexTab])

      predict(model,newdata_reordered,outputmargin =TRUE)
    }

    time0 <- Sys.time()

    vS_list <- shapr:::compute_vS(internal, model, predict_model)
    time1 <- Sys.time()

  }

  return(list(S = internal$objects$S,
              X = internal$objects$X,
              vS_list = vS_list,
              time = time1 - time0))

}



treeShaps=predict(model,as.matrix(x_explain0),predcontrib = TRUE)
library(progressr)
progressr::handlers(global = TRUE)


nSmall <- 0
numVarArray <- array(0,dim(treeShaps)[1])
testObs_vec <- c(5,12,27)#,24)

res_list <- list()

i0=0
for(testObs in testObs_vec)
{
  i0 <- i0+1
  #testObs <- 2
  numVar <- length(treeShaps[testObs,])-1
  ind <- rev(order(abs(treeShaps[testObs,-80])))
  toExplain <- sum(treeShaps[testObs,-80])
  treeShapSort <- treeShaps[testObs,ind]

  for(i in 1:numVar)
    if(abs(cumsum(as.numeric(treeShapSort))[i]-toExplain)/abs(toExplain) < 0.05)
    {
      nVarRed <- i
      break
    }
  if(plotFig)
  {
    plot.ts(cumsum(as.numeric(treeShapSort)),ylab="Sum av N foerste verdier", xlab="N")
    abline(h=toExplain,lty=2)
    abline(v=nVarRed,lty=2)
  }
  #if(nVarRed > 10)
  # nVarRed <- 6


  corMat <- cor(x_train0)
  indFull <- ind[1:nVarRed]
  for(i in 1:nVarRed) indFull <- c(indFull,as.numeric(which(abs(corMat[ind[i],])>0.5)))

  indexTab  <- unique(indFull)
  nVarRed   <- length(indexTab)
  indep.ind <- which(is.na(match(1:numVar,indexTab)))
  avPred <- treeShaps[testObs,"BIAS"]
  numVarArray[testObs] <- nVarRed

  model_shapr <- model
  class(model_shapr) <- "blabla"

  tmp_data <- as.matrix(x_explain0[testObs,])
  predict_model_shapr <- function(model_shapr,newdata){


    newdata_full <- matrix(rep(tmp_data,each=dim(newdata)[1]),nrow=dim(newdata)[1])
    newdata_full[,indexTab] <- as.matrix(newdata)
    class(model_shapr) = "xgb.Booster"

    predict(model_shapr,newdata_full,outputmargin =TRUE)
  }

  #aa=predict_model_shapr(model_shapr,x_train[,.SD,.SDcols = indexTab])

  #tmp_data <- as.matrix(x_train[,colMeans(.SD)])
  #bb=predict_model_shapr(model_shapr,x_train[,.SD,.SDcols = indexTab])



  model = model_shapr
  x_train = x_train0[,.SD,.SDcols = indexTab]
  x_explain = x_explain0[testObs,.SD,.SDcols = indexTab]
  approach = "ctree"
  prediction_zero = avPred
  predict_model = predict_model_shapr

  timing_Martin <- timing_vS_computation(type="Martin")
  timing_Kjersti <- timing_vS_computation(type="Kjersti")

  res_list[[i0]] <- list(testObs = testObs,timing_Martin=timing_Martin,timing_Kjersti=timing_Kjersti)
  print(testObs)
}


#
res_list[[1]]$timing_Kjersti$time
res_list[[1]]$timing_Martin$time

res_list[[2]]$timing_Kjersti$time
res_list[[2]]$timing_Martin$time

res_list[[3]]$timing_Kjersti$time
res_list[[3]]$timing_Martin$time



# Compute the v(S):
# Get the samples for the conditional distributions with the specified approach
# Predict with these samples
# Perform MC integration on these to estimate the conditional expectation (v(S))
S <- internal$objects$S_batch[[1]]

keep_samp_for_vS <- internal$parameters$keep_samp_for_vS
feature_names <- internal$parameters$feature_names
type <- internal$parameters$type
horizon <- internal$parameters$horizon
n_endo <- internal$data$n_endo
output_size <- internal$parameters$output_size
explain_idx <- internal$parameters$explain_idx
explain_lags <- internal$parameters$explain_lags
y <- internal$data$y
xreg <- internal$data$xreg

dt <- batch_prepare_vS(S = S, internal = internal) # Make it optional to store and return the dt_list

pred_cols <- paste0("p_hat", seq_len(output_size))

compute_preds(
  dt, # Updating dt by reference
  feature_names = feature_names,
  predict_model = predict_model,
  model = model,
  pred_cols = pred_cols,
  type = type,
  horizon = horizon,
  n_endo = n_endo,
  explain_idx = explain_idx,
  explain_lags = explain_lags,
  y = y,
  xreg = xreg
)
dt_vS <- compute_MCint(dt, pred_cols)


vS_list <- compute_vS(internal, model, predict_model)

timing_list$compute_vS <- Sys.time()


# Compute Shapley values based on conditional expectations (v(S))
# Organize function output
output <- finalize_explanation(
  vS_list = vS_list,
  internal = internal
)



