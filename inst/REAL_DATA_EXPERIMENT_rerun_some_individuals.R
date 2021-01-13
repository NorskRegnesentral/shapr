
#### Real data experiment

# Preparations

require(devtools)
# Need R 3.6 and the xgboost version used
#install_version("xgboost", version = "0.71.2", repos ="http://cran.us.r-project.org")
library(xgboost)

#devtools::install_github("NorskRegnesentral/shapr@v0.1.3")
library(shapr)

sessionInfo()$otherPkgs$xgboost$Version # Should be 0.71.2

sessionInfo()$otherPkgs$shapr$Version # Should be 0.1.3
R.version$version.string # Should be 3.6.something


# Then run install and restart of the shapr package to get the "Kode paper 1"-version of the shapr package

#rm(list = ls())

library(data.table)
library(mvtnorm)
library(condMVNorm)
library(stringi)

library(xgboost)
library(GIGrvg)
library(ghyp)


experiment = "Real"
true_model <- "Unknown"
fitted_model <- "XGBoost"
variables <- "Unknown" # Gaussian, Gaussianmix, or GenHyp
notes <- "Real data experiment"
X_dim <- 28
source.local <- ifelse(exists("source.local"),source.local,FALSE)
#these_run_ind = 76:92
run_these_tests <- c(1,29,705)





# May adjust these for the differnet methods
this.seed <- 12345
nrows_kernelShap <- 10^4
w_threshold = 1 # For a fairer comparison, all models use the same number of samples (n_threshold)
n_threshold = 10^3 # Number of samples used in the Monte Carlo integration

X_GenHyp <- (variables=="GenHyp")
(joint_csv_filename <- paste0("all_results_experiment_",experiment,"_dim_",X_dim,"_",true_model,"_",fitted_model,"_",variables,".csv")) # May hardcode this to NULL for not saving to joint in testing circumstances
(initial_current_csv_filename <- paste0("current_results_experiment_",experiment,"_dim_",X_dim,"_",true_model,"_",fitted_model,"_",variables))


source("inst/paper_experiments/paper_helper_funcs.R",local = source.local) # Helper functions these experiments (mainly computing the true Shapley values)

source("inst/paper_experiments/source_specifying_seed_and_filenames.R",local = source.local) # Setting random or fixed seed and filenames.

#### Loading data ####
XYtrain <-  fread("/nr/project/stat/BFFGB18/LIME/lime/R/train6.csv")
XYtest <-   fread("/nr/project/stat/BFFGB18/LIME/lime/R/test6.csv")


dim(XYtrain)

XYtrain[,V1:=NULL]
XYtest[,V1:=NULL]

setnames(XYtrain,"default","y")
setnames(XYtest,"default","y")

# Testing, reducing the dimension of the data
XYtest <- XYtest
XYtrain <- XYtrain


nTrain <- nrow(XYtrain)
nTest <- nrow(XYtest)

Xtrain <- copy(XYtrain)
Xtest <- copy(XYtest)

Xtrain[,y:=NULL]
Xtest[,y:=NULL]

################## Fitting the model ######
FastROC <- function(y, x) {
  # y = actual
  # x = predicted
  x1 = x[y==1]
  n1 = length(x1)
  x2 = x[y==0]
  n2 = length(x2)
  r = rank(c(x1,x2))
  return((sum(r[1:n1]) - n1*(n1+1)/2) / (n1*n2))
}

# xgb.train <- xgb.DMatrix(data = as.matrix(XYtrain[,-"y"]),
#                          label = XYtrain[,y])
# xgb.test <- xgb.DMatrix(data = as.matrix(XYtest[,-"y"]),
#                          label = XYtest[,y])
#
# params <- list(eta =  0.1,
#                objective = "binary:logistic",
#                eval_metric = "auc",
#                tree_method="hist") # gpu_hist
#
# model.xgb <- xgb.train(data = xgb.train,
#                    params = params,
#                    nrounds = 500,
#                    print_every_n = 10,
#                    ntread = 5,
#                    watchlist = list(train = xgb.train,
#                                     test = xgb.test),
#                    early_stopping_rounds = 20,
#                    verbose = 1)
#
# pred.xgb = predict(object=model.xgb,newdata = xgb.test)
# (auc.xgb <- FastROC(XYtest$y,pred.xgb))
#
# library(ranger)
#
# model.rf <- ranger(formula = y~.,data = XYtrain,probability = T,num.trees=500)
# pred.rf <- predict(object=model,data=XYtest)$pred[,2]
#
# (auc.rf <- FastROC(XYtest$y,pred.rf))

xgb.train <- xgb.DMatrix(data = as.matrix(XYtrain[,-"y"]),
                         label = XYtrain[,y])
xgb.test <- xgb.DMatrix(data = as.matrix(XYtest[,-"y"]),
                        label = XYtest[,y])

params <- list(eta =  0.1,
               objective = "binary:logistic",
               eval_metric = "auc",
               tree_method="hist") # gpu_hist



model <- xgb.train(data = xgb.train,
                   params = params,
                   nrounds = 50,
                   print_every_n = 10,
                   ntread = 5,
                   watchlist = list(train = xgb.train,
                                    test = xgb.test),
                   verbose = 1)

pred_zero = mean(XYtrain$y)

### Modify the prediction function in shapr to also store the samples


prediction <- function(dt, prediction_zero, explainer) {

  # Checks on input data
  id <- w <- id_combination <- p_hat <- NULL # due to NSE notes in R CMD check
  stopifnot(
    data.table::is.data.table(dt),
    !is.null(dt[["id"]]),
    !is.null(dt[["id_combination"]]),
    !is.null(dt[["w"]])
  )

  # Setup
  cnms <- colnames(explainer$x_test)
  data.table::setkeyv(dt, c("id", "id_combination"))

  # Check that the number of test observations equals max(id)
  stopifnot(nrow(explainer$x_test) == dt[, max(id)])

  # Reducing the prediction data.table
  max_id_combination <- dt[, max(id_combination)]
  V1 <- keep <- NULL # due to NSE notes in R CMD check
  dt[, keep := TRUE]
  first_element <- dt[, tail(.I, 1), .(id, id_combination)][id_combination %in% c(1, max_id_combination), V1]
  dt[id_combination %in% c(1, max_id_combination), keep := FALSE]
  dt[first_element, c("keep", "w") := list(TRUE, 1.0)]
  dt <- dt[keep == TRUE][, keep := NULL]

  # Predictions
  dt[id_combination != 1, p_hat := predict_model(explainer$model, newdata = .SD), .SDcols = cnms]
  dt[id_combination == 1, p_hat := prediction_zero]
  p_all <- dt[id_combination == max(id_combination), p_hat]
  names(p_all) <- 1:nrow(explainer$x_test)

  # Calculate contributions
  dt_res <- dt[, .(k = sum((p_hat * w) / sum(w))), .(id, id_combination)]
  data.table::setkeyv(dt_res, c("id", "id_combination"))
  dt_mat <- data.table::dcast(dt_res, id_combination ~ id, value.var = "k")
  dt_mat[, id_combination := NULL]
  kshap <- t(explainer$W %*% as.matrix(dt_mat))

  dt_kshap <- data.table::as.data.table(kshap)
  colnames(dt_kshap) <- c("none", cnms)

  r <- list(dt = dt_kshap, model = explainer$model, p = p_all, x_test = explainer$x_test,samples_dt = dt)
  attr(r, "class") <- c("shapr", "list")

  return(r)
}


assignInNamespace("prediction",prediction,ns="shapr")

shapr.plot = function (x, digits = 3, plot_phi0 = TRUE, index_x_test = NULL,
                       top_k_features = NULL, feature_order = "per_ind", ...)
{
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is not installed. Please run install.packages('ggplot2')")
  }
  if (is.null(index_x_test))
    index_x_test <- seq(nrow(x$x_test))
  if (is.null(top_k_features))
    top_k_features <- ncol(x$x_test) + 1
  id <- phi <- NULL
  cnms <- colnames(x$x_test)
  KshapDT <- data.table::copy(x$dt)
  KshapDT[, `:=`(id, .I)]
  meltKshap <- data.table::melt(KshapDT, id.vars = "id", value.name = "phi")
  meltKshap[, `:=`(sign, factor(sign(phi), levels = c(1, -1),
                                labels = c("Increases", "Decreases")))]
  desc_mat <- format(x$x_test, digits = digits)
  for (i in 1:ncol(desc_mat)) {
    desc_mat[, i] <- paste0(cnms[i], " = ", desc_mat[, i])
  }
  desc_dt <- data.table::as.data.table(cbind(none = "none",
                                             desc_mat))
  melt_desc_dt <- data.table::melt(desc_dt[, `:=`(id, .I)],
                                   id.vars = "id", value.name = "description")
  plotting_dt <- merge(meltKshap, melt_desc_dt)
  predDT <- data.table::data.table(id = KshapDT$id, pred = x$p)
  plotting_dt <- merge(plotting_dt, predDT, by = "id")
  header <- variable <- pred <- description <- NULL
  plotting_dt[, `:=`(header, paste0("id: ", id, ", pred = ",
                                    format(pred, digits = digits + 1)))]
  if (!plot_phi0) {
    plotting_dt <- plotting_dt[variable != "none"]
  }
  plotting_dt <- plotting_dt[id %in% index_x_test]
  if(all(feature_order=="per_ind")){
    plotting_dt[,feature_order := rank(abs(phi))]
  } else if(all(feature_order=="across_inds")){
    this_order <- plotting_dt[,mean(abs(phi)),by=variable][,rank(V1)]
    plotting_dt[,feature_order := rep(this_rank,length(index_x_test))]
  } else {
    plotting_dt[,feature_order := feature_order,by=id]
  }

  plotting_dt[, `:=`(rank, data.table::frank(-abs(phi))), by = "id"]
  plotting_dt <- plotting_dt[rank <= top_k_features]
  gg <- ggplot2::ggplot(plotting_dt) + ggplot2::facet_wrap(~header,
                                                           scales = "free_y", labeller = "label_value", ncol = 3) +
    ggplot2::geom_col(ggplot2::aes(x = reorder(description,feature_order), y = phi,
                                   fill = sign)) + ggplot2::coord_flip() + ggplot2::scale_fill_manual(values = c("steelblue",
                                                                                                                 "lightsteelblue"), drop = TRUE) + ggplot2::labs(y = "Feature contribution",
                                                                                                                                                                 x = "Feature", fill = "", title = "Shapley value prediction explanation") +
    ggplot2::theme(legend.position = "bottom", plot.title = ggplot2::element_text(hjust = 0.5))
  return(gg)
}


set.seed(this.seed)

current.Xtest <- Xtest[run_these_tests,]#copy(Xtest[run_list[[run_ind]],])
explainer <- shapr(Xtrain,
                   model,
                   n_combinations = nrows_kernelShap)

Shapley.approx = list()

Shapley.approx$comb_Gaussian_sigma.01 <- explain(
  current.Xtest,
  approach = c(rep("empirical",3),rep("gaussian",28-3)),
  explainer = explainer,
  prediction_zero = pred_zero,
  type = "fixed_sigma",
  n_samples = n_threshold,
  w_threshold = w_threshold,
  seed = this.seed
)

Shapley.approx$independence <- explain(
  current.Xtest,
  approach = "empirical",
  explainer = explainer,
  prediction_zero = pred_zero,
  type = "independence",
  n_samples = n_threshold,
  w_threshold = w_threshold,
  seed = this.seed
)

Shapley.approx$empirical <- explain(
  current.Xtest,
  approach = "empirical",
  explainer = explainer,
  prediction_zero = pred_zero,
  type = "fixed_sigma",
  n_samples = n_threshold,
  w_threshold = w_threshold,
  seed = this.seed
)

this_rank <- rank(colMeans(rbind(abs(Shapley.approx$empirical$dt),
                                     abs(Shapley.approx$independence$dt))))

shapr.plot(Shapley.approx$empirical,feature_order = this_rank)+ggplot2::ylim(-0.1,0.2)
shapr.plot(Shapley.approx$independence,feature_order = this_rank)+ggplot2::ylim(-0.1,0.2)


saveRDS(explainer,"/nr/project_stat/BigInsight/Projects/Explanations/Kode/Kode paper 1/REAL_DATA_EXPERIMENT_RERUN_EXPLAINER_new.rds")
saveRDS(Shapley.approx,"/nr/project_stat/BigInsight/Projects/Explanations/Kode/Kode paper 1/REAL_DATA_EXPERIMENT_RERUN_DATA_new.rds")
#Shapley.approx <- readRDS("/nr/project_stat/BigInsight/Projects/Explanations/Kode/Kode paper 1/REAL_DATA_EXPERIMENT_RERUN_DATA_new.rds")

org_SHAP <- data.table::fread("/nr/project_stat/BigInsight/Projects/Explanations/Kode/Shapley_Anders/all_results_experiment_Real_dim_28_Unknown_XGBoost_Unknown_newnames.csv")

features <- colnames(org_SHAP)[-c(1:2)]

org_SHAP[Method %in% c("empirical_independence","comb_Gaussian_sigma.01") & test_no %in% run_these_tests,rowSums(.SD),.SDcols=features,by=Method]

rowSums(Shapley.approx$comb_Gaussian_sigma.01$dt)
rowSums(Shapley.approx$comb_Gaussian_sigma.01_new_S$dt)
rowSums(Shapley.approx$comb_Gaussian_sigma.01_new_samples$dt)

rowSums(Shapley.approx$independence$dt)
rowSums(Shapley.approx$independence_new_S$dt)
rowSums(Shapley.approx$independence_new_samples$dt)





Shapley.approx$comb_Gaussian_sigma.01_old <- Shapley.approx$comb_Gaussian_sigma.01
Shapley.approx$comb_Gaussian_sigma.01_old$dt <- org_SHAP[Method %in% c("comb_Gaussian_sigma.01") & test_no %in% run_these_tests][,-c(1:2)]
names(Shapley.approx$comb_Gaussian_sigma.01_old$dt) <- names(Shapley.approx$comb_Gaussian_sigma.01$dt)

Shapley.approx$independence_old <- Shapley.approx$independence
Shapley.approx$independence_old$dt <- org_SHAP[Method %in% c("empirical_independence") & test_no %in% run_these_tests][,-c(1:2)]
names(Shapley.approx$independence_old$dt) <- names(Shapley.approx$independence$dt)




old_rank_mean <- rank(colMeans(rbind(abs(Shapley.approx$comb_Gaussian_sigma.01_old$dt),
                                     abs(Shapley.approx$independence_old$dt))))


aa <- shapr.plot(Shapley.approx$comb_Gaussian_sigma.01,feature_order = old_rank_mean)
aa <- aa+ggplot2::ylim(-0.1,0.2)+ggplot2::ggtitle("New Shapley values, gaussian + empirical")
#ggplot2::ggsave("/nr/project_stat/BigInsight/Projects/Explanations/Kode/Kode paper 1/new_dep2.pdf")
bb <- shapr.plot(Shapley.approx$comb_Gaussian_sigma.01_old,feature_order = old_rank_mean)
bb<- bb+ggplot2::ylim(-0.1,0.2)+ggplot2::ggtitle("Old Shapley values, gaussian + empirical")

library(gridExtra)
pdf("/nr/project_stat/BigInsight/Projects/Explanations/Kode/Kode paper 1/empirical_both.pdf",width = 20,height = 12)
grid.arrange(bb,aa,newpage = F,ncol=1)
dev.off()


aa <- shapr.plot(Shapley.approx$independence,feature_order = old_rank_mean)
aa <- aa+ggplot2::ylim(-0.1,0.2)+ggplot2::ggtitle("New Shapley values, independence")
#ggplot2::ggsave("/nr/project_stat/BigInsight/Projects/Explanations/Kode/Kode paper 1/new_dep2.pdf")
bb <- shapr.plot(Shapley.approx$independence_old,feature_order = old_rank_mean)
bb<- bb+ggplot2::ylim(-0.1,0.2)+ggplot2::ggtitle("Old Shapley values, independence")

pdf("/nr/project_stat/BigInsight/Projects/Explanations/Kode/Kode paper 1/independence_both.pdf",width = 20,height = 12)
grid.arrange(bb,aa,newpage = F,ncol=1)
dev.off()


#
#
# ### OLD
#
# aa <- shapr.plot(Shapley.approx$comb_Gaussian_sigma.01,feature_order = old_rank_mean)
# aa <- aa+ggplot2::ylim(-0.1,0.2)+ggplot2::ggtitle("New Shapley values, gaussian + empirical")
# #ggplot2::ggsave("/nr/project_stat/BigInsight/Projects/Explanations/Kode/Kode paper 1/new_dep2.pdf")
# bb <- shapr.plot(Shapley.approx$comb_Gaussian_sigma.01_old,feature_order = old_rank_mean)
# bb<- bb+ggplot2::ylim(-0.1,0.2)+ggplot2::ggtitle("Old Shapley values, gaussian + empirical")
# #ggplot2::ggsave("/nr/project_stat/BigInsight/Projects/Explanations/Kode/Kode paper 1/old_dep2.pdf")
# cc <- shapr.plot(Shapley.approx$comb_Gaussian_sigma.01_new_S,feature_order = old_rank_mean)
# cc <- cc+ggplot2::ylim(-0.1,0.2)+ggplot2::ggtitle("New Shapley values, gaussian + empirical, new S")
# dd <- shapr.plot(Shapley.approx$comb_Gaussian_sigma.01_new_samples,feature_order = old_rank_mean)
# dd <- dd+ggplot2::ylim(-0.1,0.2)+ggplot2::ggtitle("New Shapley values, gaussian + empirical, new samples")
#
# library(gridExtra)
# pdf("/nr/project_stat/BigInsight/Projects/Explanations/Kode/Kode paper 1/empirical_all.pdf",width = 20,height = 20)
# grid.arrange(bb,aa,cc,dd,newpage = F,ncol=1)
# dev.off()
#
# aa <- shapr.plot(Shapley.approx$independence,feature_order = old_rank_mean)
# aa <- aa+ggplot2::ylim(-0.1,0.2)+ggplot2::ggtitle("New Shapley values, independence")
# #ggplot2::ggsave("/nr/project_stat/BigInsight/Projects/Explanations/Kode/Kode paper 1/new_dep2.pdf")
# bb <- shapr.plot(Shapley.approx$independence_old,feature_order = old_rank_mean)
# bb<- bb+ggplot2::ylim(-0.1,0.2)+ggplot2::ggtitle("Old Shapley values, independence")
# #ggplot2::ggsave("/nr/project_stat/BigInsight/Projects/Explanations/Kode/Kode paper 1/old_dep2.pdf")
# cc <- shapr.plot(Shapley.approx$independence_new_S,feature_order = old_rank_mean)
# cc <- cc+ggplot2::ylim(-0.1,0.2)+ggplot2::ggtitle("New Shapley values, independence, new S")
# dd <- shapr.plot(Shapley.approx$independence_new_samples,feature_order = old_rank_mean)
# dd <- dd+ggplot2::ylim(-0.1,0.2)+ggplot2::ggtitle("New Shapley values, independence, new samples")
#
# pdf("/nr/project_stat/BigInsight/Projects/Explanations/Kode/Kode paper 1/independence_all.pdf",width = 20,height = 20)
# grid.arrange(bb,aa,cc,dd,newpage = F,ncol=1)
# dev.off()
#
