
library(xgboost)
library(data.table)

# Install the github version of the shapr pacakge
#remotes::install_github("NorskRegnesentral/shapr")

library(shapr)

library(progressr)
progressr::handlers(global = TRUE) # To get progress updates

library(future)
future::plan(multisession, workers = 4) # for paralellization (on both linux and windows)
future::plan(sequential)


plotFig <- 0

datafolder <- "/nr/project/stat/BigInsight/Projects/Explanations/EffektivShapley/NHANES-data/"
datafolder <- "M:/BigInsight/Projects/Explanations/EffektivShapley/NHANES-data/"

x_explain <- fread(file.path(datafolder,"newdata/Xtest_imp.csv"))
y_test    <- fread(file.path(datafolder,"newdata/ytest.csv"))$V1
x_train   <- fread(file.path(datafolder,"newdata/Xtrain_imp.csv"))
y_train   <- fread(file.path(datafolder,"newdata/ytrain.csv"))$V1

names(x_train)[52:55]   <- c("urine_albumin_is_gt_30","urine_albumin_is_gt_100","urine_albumin_is_get_300", "urine_albumin_is_gt_1000")
names(x_explain)[52:55] <- c("urine_albumin_is_gt_30","urine_albumin_is_gt_100","urine_albumin_is_get_300", "urine_albumin_is_gt_1000")

model <- xgboost::xgb.load(file.path(datafolder,"newdata/xgb_model_imp.json"))

preds <- predict(model,as.matrix(x_explain),outputmargin = TRUE)
preds_train <- predict(model,as.matrix(x_train),outputmargin = TRUE)

pred_mod_xgb <- function(model,newdata){
  xgboost:::predict.xgb.Booster(model,as.matrix(newdata),outputmargin = TRUE)
}

#preds <- log(predict(model,as.matrix(x_explain)))

#ind   <- rev(order(preds))[1:2]
#ind   <- rev(order(preds))[9:10]
#x_explain <- x_explain[ind,]


treeShaps=predict(model,as.matrix(x_explain),predcontrib = TRUE)

prediction_zero <- treeShaps[1,"BIAS"]
feature_names <- colnames(x_train)


#### testing here

# TODO:
# 1. Check that the order of the features is correct such that the correct id combinations are used in the shapley value
# computations later on.
# 2. Control the computed shapley values with what you get with the standard approach in th nhanes-shapr-reduced setting e.g. all but 2, 3 or 4 features
# to zero. should be about the same if the procedure works.


n_features <- ncol(x_train)
abscorMat <- abs(cor(x_train))
max_cutoff_features <- 5
max_cutoff_remaining_imp <- 0.10
p0 <- treeShaps[1,"BIAS"]
S_replacement_for_remaining_cutoff_feats <- 1 # 1 for conditioning on these, 0 for marginalizing them out



fix_zero_and_full_prediction <- TRUE

# For a specific testObs:

testObs <- 24
testObs <- 5

org_imp <- abs(treeShaps[testObs,-(n_features+1)]) # absolute value of treeshap values
norm_org_imp <- org_imp/sum(org_imp)
cor_imp <- as.vector(org_imp%*%abscorMat^2)
names(cor_imp) <- names(x_train)
norm_cor_imp <- cor_imp/sum(cor_imp)
plot(norm_cor_imp,type="l",ylim=c(0,0.25))
lines(org_imp/sum(org_imp),col="red")

sorted_norm_cor_imp <- sort(norm_cor_imp,decreasing = TRUE)
cumsum_sorted_norm_cor_imp <- cumsum(sorted_norm_cor_imp)


cutoff0 <- which(cumsum_sorted_norm_cor_imp<=1-max_cutoff_remaining_imp)
cutoff <- ifelse(length(cutoff0)>=max_cutoff_features,cutoff0[max_cutoff_features],cutoff0[length(cutoff)])
cutoff_imp <- sorted_norm_cor_imp[1:cutoff]
cutoff_feats <- names(cutoff_imp)

excluded_feature_cols <- setdiff(names(x_train),cutoff_feats)

x_train_red <- x_train[,..cutoff_feats]
x_explain_red <- x_explain[,..cutoff_feats]
x_explain_excluded <<- x_explain[,..excluded_feature_cols]


ctree.mincriterion = 0.95
ctree.minsplit = 20
ctree.minbucket = 7
ctree.sample = TRUE
n_samples <- 1000


library(shapr)



model_shapr <- model
class(model_shapr) <- "blabla"

predict_model_shapr <<- function(model_shapr,newdata){

  newdata_excluded <- unlist(x_explain_excluded[testObs,])

  newdata_excluded <- matrix(rep(newdata_excluded,each=dim(newdata)[1]),nrow=dim(newdata)[1])
  colnames(newdata_excluded) <- colnames(x_explain_excluded)
  newdata_full <- as.data.table(cbind(newdata_excluded,as.matrix(newdata)))
  setcolorder(newdata_full,names(x_train))

  class(model_shapr) = "xgb.Booster"

  xgboost:::predict.xgb.Booster(model_shapr,as.matrix(newdata_full),outputmargin =TRUE)
}

aa=predict_model_shapr(model_shapr,x_explain_red[testObs,])
bb = predict(model,as.matrix(x_explain[testObs,]),outputmargin = TRUE)
aa
bb

full_pred <- sum(treeShaps_dt[testObs_computed,])
pred_to_decompose <- sum(treeShaps_dt[testObs,..cutoff_feats])
pred_not_to_decompose <- sum(treeShaps_dt[testObs,..excluded_feature_cols])
org_p0 <- treeShaps_dt[testObs,BIAS]
p0 <- org_p0 + pred_not_to_decompose


expl <- shapr::explain(model = model_shapr,
                       x_explain= x_explain_red[testObs,],
                       x_train = x_train_red,
                       approach = "ctree",
                       prediction_zero = p0,
                       n_batches = 1,
                       predict_model = predict_model_shapr)

seq_along(all_trees)

### OK, so now we have the trees we need


testObs_computed < - 1:10
full_pred <- rowSums(treeShaps_dt[testObs_computed,])
pred_to_decompose <- rowSums(treeShaps_dt[testObs_computed,..cutoff_feats])
pred_not_to_decompose <- rowSums(treeShaps_dt[testObs_computed,..excluded_feature_cols])
org_p0 <- treeShaps_dt[testObs_computed,BIAS][1]
p0 <- org_p0 + pred_not_to_decompose

pred_mod_xgb <- function(model,newdata){
  xgboost:::predict.xgb.Booster(model,as.matrix(newdata),outputmargin = TRUE)
}



index_features <- expl$internal$objects$X[approach=="ctree",id_combination]
samp_list <- list()
for(j in seq_along(testObs_computed)){

  l <- lapply(
    X = all_trees,
    FUN = shapr:::sample_ctree,
    n_samples = n_samples,
    x_explain = x_explain_red_here[j, , drop = FALSE],
    x_train = x_train_red,
    n_features = ncol(x_train_red),
    sample = ctree.sample
  )

  samp_list[[j]] <- data.table::rbindlist(l, idcol = "id_combination")
  samp_list[[j]][, w := 1 / n_samples]
  if (!is.null(index_features)) samp_list[[j]][, id_combination := index_features[id_combination]]

}


vS_feature_dt <- rbindlist(samp_list,idcol="id")

x_excluded_here <- x_explain[testObs_computed,..excluded_feature_cols]

dt <- cbind(vS_feature_dt,x_excluded_here)
setcolorder(dt,names(x_train))



dt[, p_hat_1 := pred_mod_xgb(model, newdata = .SD), .SDcols = feature_names]
dt[,w:=1/.N,by=.(id_combination,id)]
dt[,p_hat:=p_hat_1]

full_pred_id_combination <- expl$internal$objects$X[,max(id_combination)]
dt_vS <- data.table(id_combination=c(1,full_pred_id_combination))
for(i in seq_along(testObs_computed)){
  this = paste0("p_hat_",i)
  dt_vS[,(this):=c(p0[1],pred_to_decompose[i])]
}


dt_vS0 <- shapr:::compute_MCint(dt)

dt_vS <- rbind(dt_vS,
               dt_vS0)
setorder(dt_vS,id_combination)

id_combination_features_char_mapping <- expl$internal$objects$X[,.(id_combination,features_char=sapply(features,function(x)paste0(x, collapse = "_")))]


#### NEXT:

# do regular feature_sampling like in iterative_kernelshap_v2, compute the W and so on,
# then add the features_char to that X_tmp as well.
# then insetad, of doing the vS estimation etc, use the mapping above to reduce the full dt_vS
# to the one we need for that specific sample, and then use that in the kernelshap computations
# in the end

