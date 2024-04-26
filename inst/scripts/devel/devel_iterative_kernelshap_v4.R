#source("/nr/project/stat/BigInsight//Projects//Explanations//EffektivShapley//helpFunctions.R")

#library(treeshap)
library(xgboost)
library(data.table)

# Install the github version of the shapr pacakge
#remotes::install_github("NorskRegnesentral/shapr")

library(shapr)

#library(progressr)
#progressr::handlers(global = TRUE) # To get progress updates

#library(future)
#future::plan(multisession, workers = 10) # for paralellization (on both linux and windows)


plotFig <- 0

datafolder <- "/nr/project/stat/BigInsight/Projects/Explanations/EffektivShapley/NHANES-data/"
#datafolder <- "M:/BigInsight/Projects/Explanations/EffektivShapley/NHANES-data/"

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
treeShaps_dt <- as.data.table(treeShaps)

prediction_zero <- treeShaps[1,"BIAS"]
feature_names <- colnames(x_train)


n_features <- ncol(x_train)
abscorMat <- abs(cor(x_train))
max_cutoff_features <- 12
max_cutoff_remaining_imp <- 0.10
p0 <- treeShaps[1,"BIAS"]
S_replacement_for_remaining_cutoff_feats <- 1 # 1 for conditioning on these, 0 for marginalizing them out



fix_zero_and_full_prediction <- TRUE

# For a specific testObs:

testObs <- 24
testObs <- 5

org_imp <- abs(treeShaps[testObs,-(n_features+1)]) # absolute value of treeshap values
norm_org_imp <- org_imp/sum(org_imp)
cor_imp <- as.vector(org_imp%*%abscorMat)
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


ctree.mincriterion = 0.95
ctree.minsplit = 20
ctree.minbucket = 7
ctree.sample = TRUE
n_samples <- 1000

#
# X_full <- shapr:::feature_combinations(
#   m = max_comp_features,
#   exact = TRUE,
#   n_combinations = NULL,
#   weight_zero_m = 10^6,
#   group_num = NULL
# )
#
# ## Get feature matrix ---------
# S_full <- feature_matrix_cpp(
#   features = X_full[["features"]],
#   m = max_comp_features
# )
#
# S_full <- cbind(S_full,matrix(S_replacement_for_remaining_cutoff_feats,ncol=max_cutoff_features-max_comp_features,nrow=nrow(S_full)))
#
# S_full_dt <- as.data.table(S_full)
# S_full_dt[,id_combination_full:=.I]
# S_full_dt[,computed_in_loop:=as.numeric(NA)]
#
# S_full_dt[.N,computed_in_loop:=0] # We don't need to compute the full model
# S_full_dt[.N, p_hat_1:=preds[testObs]] # Already computed

# Not sure if this should be included here or not, as the smallest model now condiitonal on some features
# so is not really the same as the zero model
# I think it is best to model this every time, yes


#S_full_dt[1,computed_in_loop:=0] # We don't need to compute the zero model
#S_full_dt[1, p_hat_1:=prediction_zero]



#cutoff_feats <- c("age", "systolic_blood_pressure", "pulse_pressure","cholesterol","sex_isFemale")
#cutoff_feats <- c("age", "systolic_blood_pressure", "pulse_pressure","sex_isFemale")


testObs_computed <- 5
initial_n_combinations <- min(20,2^length(cutoff_feats)-2)
n_combinations_per_iter <- 10
n_boot_ests <- 50
unique_sampling <- TRUE
paired_sampling <- TRUE
shapley_reweighting_strategy = "on_N"

full_pred <- rowSums(treeShaps_dt[testObs_computed,])
pred_not_to_decompose <- rowSums(treeShaps_dt[testObs_computed,setdiff(names(x_train),cutoff_feats),with=FALSE])
org_p0 <- treeShaps_dt[testObs_computed,BIAS][1]
p0 <- org_p0 + pred_not_to_decompose


pred_to_decompose <- rowSums(treeShaps_dt[testObs_computed,..cutoff_feats])
all.equal(p0+pred_to_decompose,full_pred)

avg_contrib <- (pred_to_decompose-p0)/max_cutoff_features

shapley_threshold_val <- 0.5*abs(avg_contrib)
shapley_threshold_prob <- 0.1

source("inst/scripts/devel/iterative_kernelshap_sourcefuncs.R")


run <- iterative_kshap_func(model,x_explain,x_train,
                            testObs_computed = 5,
                            cutoff_feats = cutoff_feats,
                            full_pred = full_pred,
                            pred_not_to_decompose = pred_not_to_decompose,
                            p0 = p0,
                            predict_model = pred_mod_xgb,shapley_threshold_val = 0)


# > run$kshap_est_dt_list
# id        none        age systolic_blood_pressure pulse_pressure cholesterol sex_isFemale sedimentation_rate
# <num>       <num>      <num>                   <num>          <num>       <num>        <num>              <num>
#   1:     5 -0.04902647 -1.0016052               1.1139986     -0.4651089  0.73732056   -0.8589101        -0.02779262
# 2:     5 -0.07681912 -0.9806543              -0.4052843     -0.2934339  0.81888558   -0.7275904                 NA
# 3:     5 -0.19418058 -0.8855216              -0.4169003     -0.2142155  0.43098906   -0.3347426                 NA
# 4:     5 -0.19418057 -0.8973133              -0.4162866     -0.2081667  0.34529454   -0.2436982                 NA
# 5:     5 -0.19418058 -0.8729341              -0.4603462     -0.1813250  0.25912380   -0.1980799                 NA
# 6:     5 -0.19418058 -0.8761841              -0.4279890     -0.2174582  0.23652575   -0.1814537                 NA
# 7:     5 -0.19418058 -0.8427914              -0.4399255     -0.1966310  0.25560289   -0.1725260                 NA
# 8:     5 -0.24059884 -0.7856045              -0.3859089     -0.1845766  0.12019702   -0.1560790                 NA
# 9:     5 -0.26751683 -0.8208803              -0.3750180     -0.1821221  0.07407189   -0.1301433                 NA
# 10:     5 -0.26751683 -0.8136775              -0.3565904     -0.1814943  0.08342489   -0.1444714                 NA
# 11:     5 -0.26751683 -0.7984644              -0.3432444     -0.1767080  0.08333424   -0.1581385                 NA
# 12:     5 -0.26751683 -0.7899428              -0.3549505     -0.1699892  0.07462353   -0.1675959                 NA
# 13:     5 -0.26751682 -0.7830507              -0.3502242     -0.1904407  0.06499468   -0.1704928                 NA
# 14:     5 -0.20252218 -0.8037698              -0.3199667     -0.1671251          NA   -0.1614122                 NA
# 15:     5 -0.20252218 -0.8075028              -0.3322662     -0.1723476          NA   -0.1727935                 NA
# 16:     5 -0.20252218 -0.7996048              -0.3339683     -0.1716902          NA   -0.1741585                 NA
# 17:     5 -0.20252217 -0.8008640              -0.3433761     -0.1822710          NA   -0.1656217                 NA
# alkaline_phosphatase serum_albumin  hematocrit        bmi   uric_acid   hemoglobin
# <num>         <num>       <num>      <num>       <num>        <num>
#   1:           -0.3781377    0.63320903 -1.35785152 -0.3760717  0.25653102 -0.482232097
# 2:           -0.3851922    0.25503709 -0.57270563 -0.1173615 -0.12949210  0.358933847
# 3:           -0.4576284    0.04915689 -0.20049662         NA -0.06534623  0.010396414
# 4:           -0.4163163    0.01187791 -0.16141412         NA -0.07814638  0.002672738
# 5:           -0.4416357    0.01555170 -0.09979225         NA -0.06547364 -0.016585131
# 6:           -0.3959717    0.01634772 -0.07112320         NA -0.07121864 -0.072971369
# 7:           -0.4165494   -0.05452300 -0.04641826         NA -0.05157687 -0.096157781
# 8:           -0.4213987   -0.04977619          NA         NA -0.02691799 -0.123668986
# 9:           -0.3855029   -0.05408934          NA         NA          NA -0.114476156
# 10:           -0.3785017   -0.08992159          NA         NA          NA -0.106928163
# 11:           -0.3677076   -0.09055425          NA         NA          NA -0.136677295
# 12:           -0.3608800   -0.09215406          NA         NA          NA -0.127271120
# 13:           -0.3538958   -0.08140431          NA         NA          NA -0.123646149
# 14:           -0.3454929   -0.08524831          NA         NA          NA -0.137873074
# 15:           -0.3467456   -0.09017149          NA         NA          NA -0.131327688
# 16:           -0.3526648   -0.09751468          NA         NA          NA -0.123553522
# 17:           -0.3541978   -0.09218908          NA         NA          NA -0.114635210


# > rbindlist(kshap_est_dt_list)
# id       none        age systolic_blood_pressure pulse_pressure cholesterol sex_isFemale
# <num>      <num>      <num>                   <num>          <num>       <num>        <num>
#   1:     5 -0.2363602 -1.0324196              -0.1781393     -0.3543366  0.06134782   -0.2264046
# 2:     5 -0.2363602 -1.0208127              -0.1703291     -0.3363762  0.06526367   -0.2098054
# 3:     5 -0.2363602 -0.9342698              -0.3268114     -0.2672583  0.06246510   -0.1982597
# 4:     5 -0.2363602 -0.9039752              -0.3601522     -0.2459947  0.04721183   -0.2141459
# 5:     5 -0.2363602 -0.9148729              -0.3779553     -0.2252278  0.05646760   -0.2229283
# 6:     5 -0.2363602 -0.9158867              -0.3790719     -0.2325314  0.06220265   -0.2119778
# sedimentation_rate
# <num>
#   1:        -0.05300468
# 2:        -0.11089728
# 3:        -0.11882282
# 4:        -0.10590079
# 5:        -0.09844023
# 6:        -0.10569169

#id       none        age systolic_blood_pressure pulse_pressure cholesterol sex_isFemale
#<num>      <num>      <num>                   <num>          <num>       <num>        <num>
#  1:     5 -0.2363602 -1.0324196              -0.1781393     -0.3543366  0.06134782   -0.2264046
#2:     5 -0.2363602 -0.9307354              -0.3629497     -0.2451795  0.03561040   -0.2266980
#3:     5 -0.2363603 -0.6323135              -0.5460737     -0.3050392          NA   -0.2821362
#sedimentation_rate
#<num>
#  1:        -0.05300468
#2:                 NA
#3:                 NA

# TODO:

# Check that the estimation is stopped when all features are sampled (and the variance set to 0) DONE


# Adjust the total prediction when a feature is removed DONE
# check that the  correct feature is excluded when a feature is exc luded DONE (I think)
# Include the removed features in the printout (+ the total prediction and what is distributed on the features or so) # DONE

# Check why the function always fails on inter=3 with the error:
# Error: Not compatible with requested type: [type=NULL; target=double].


#[1] 3
#id             none              age systolic_blood_pressure   pulse_pressure             cholesterol     sex_isFemale
#<num>           <char>           <char>                  <char>           <char>                  <char>           <char>
#  1:     5 -0.236 ( 0) [ 0] -0.731 ( 0) [ 0]        -0.394 ( 0) [ 0] -0.272 ( 0) [ 0] 0.006 [done estimating] -0.339 ( 0) [ 0]
#sedimentation_rate other_features
#<char>         <char>
#  1: -0.053 [done estimating]         -0.176
#





################

cutoff_feats <- c("age", "systolic_blood_pressure", "pulse_pressure","sex_isFemale")
#cutoff_feats <- c("age", "systolic_blood_pressure", "pulse_pressure","cholesterol","sex_isFemale")
#cutoff_feats <- c("age", "systolic_blood_pressure", "pulse_pressure","cholesterol","sex_isFemale","sedimentation_rate" )
excluded_feature_cols <- setdiff(colnames(x_train),cutoff_feats)

model_shapr <- model
class(model_shapr) <- "blabla"
x_explain_excluded <- x_explain[testObs_computed,..excluded_feature_cols]

predict_model_shapr <<- function(model_shapr,newdata){

  newdata_excluded <- unlist(x_explain_excluded)

  newdata_excluded <- matrix(rep(newdata_excluded,each=dim(newdata)[1]),nrow=dim(newdata)[1])
  colnames(newdata_excluded) <- colnames(x_explain_excluded)
  newdata_full <- as.data.table(cbind(newdata_excluded,as.matrix(newdata)))
  setcolorder(newdata_full,names(x_train))

  class(model_shapr) = "xgb.Booster"

  xgboost:::predict.xgb.Booster(model_shapr,as.matrix(newdata_full),outputmargin =TRUE)
}

aa=predict_model_shapr(model_shapr,x_explain[testObs_computed,..cutoff_feats])
bb = predict(model,as.matrix(x_explain[testObs_computed,]),outputmargin = TRUE)
aa
bb

full_pred <- rowSums(treeShaps_dt[testObs_computed,])
pred_to_decompose <- rowSums(treeShaps_dt[testObs_computed,..cutoff_feats])
pred_not_to_decompose <- rowSums(treeShaps_dt[testObs_computed,..excluded_feature_cols])
org_p0 <- treeShaps_dt[testObs_computed,BIAS][1]
p0 <- org_p0 + pred_not_to_decompose


expl <- shapr::explain(model = model_shapr,
                       x_explain= x_explain[testObs_computed,..cutoff_feats],
                       x_train = x_train[,..cutoff_feats],
                       approach = "ctree",
                       prediction_zero = p0,
                       predict_model = predict_model_shapr)

expl$shapley_values
#none        age systolic_blood_pressure pulse_pressure sex_isFemale
#<num>      <num>                   <num>          <num>        <num>
#  1: -0.3117251 -0.9014703              -0.4400504     -0.3003352   -0.3021037
#none        age systolic_blood_pressure pulse_pressure cholesterol sex_isFemale
#<num>      <num>                   <num>          <num>       <num>        <num>
#  1: -0.2746173 -0.9903395              -0.4304803     -0.2968699  0.01581737   -0.2791949
#none        age systolic_blood_pressure pulse_pressure cholesterol sex_isFemale sedimentation_rate
#<num>      <num>                   <num>          <num>       <num>        <num>              <num>
#  1: -0.2363602 -0.9538142               -0.415132     -0.2640743  0.01415042   -0.2701904         -0.1302638






aa=merge(expl$internal$objects$X,expl$internal$output$dt_vS,by="id_combination")
aa[,S_char:=sapply(features,paste0, collapse = "_")]

bb <- merge(aa[,.(id_combination,S_char,shapr_est=p_hat1_1)],dt_vS[,.(S_char,iter_est=p_hat_1)],by="S_char")

setorder(bb,id_combination)
bb
# OK, seems there are only some minor differences in the predictions (which is expected due to randomness in the ctree sampling)


t(expl$internal$objects$W%*%as.matrix(bb[,iter_est]))
expl$shapley_values
kshap_est_dt
W
expl$internal$objects$W
##############

### TODO:

#Figure out what is wrong with the shapley_weights when doing on_all as they are not giving the same as when calling feature_exact(current_m) and looking at the shapley_weights there

X_shapr <-shapr:::feature_exact(6)[]

X0[,shapley_weight := shapr:::shapley_weights(m = m, N = N, n_components = n_features, weight_zero_m=10^6)]

X0 <- X_from_feature_set_v3(feature_sample_all,m=current_m,sample_ids=seq_along(feature_sample_all))[]

aa=shapley_reweighting(X0, strategy = "on_all")[]$shapley_weight

all.equal(aa,X_shapr$shapley_weight)

X0$shapley_weight



