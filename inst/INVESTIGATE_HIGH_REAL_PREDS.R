
#### Rerun model for al data experiment of paper 1

require(devtools)
# Need R 3.6 and the xgboost version used
#install_version("xgboost", version = "0.71.2", repos ="http://cran.us.r-project.org")
library(xgboost)
sessionInfo()$otherPkgs$xgboost$Version # Should be 0.71.2
R.version$version.string # Should be 3.6.something


#rm(list = ls())

library(data.table)






# May adjust these for the differnet methods
this.seed <- 12345
nrows_kernelShap <- 10^4
w_threshold = 1 # For a fairer comparison, all models use the same number of samples (n_threshold)
n_threshold = 10^3 # Number of samples used in the Monte Carlo integration

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


pred_test <- predict(model,xgb.test)

### Compare results to previously run predictions

shapley_values <- fread("inst/paper_experiments/res/all_results_experiment_Real_dim_28_Unknown_XGBoost_Unknown_newnames.csv")
shapley_values[,pred:=rowSums(.SD),.SDcols=names(shapley_values)[-(1:2)]]
DT_pred_test <- shapley_values[Method=="empirical_independence",.(test_no,pred)]
setkey(DT_pred_test,test_no)
pred_test_org <- rep(NA,length(pred_test))
pred_test_org[unlist(DT_pred_test[,test_no])] <- unlist(DT_pred_test[,pred])

# When available, the original predictions matches the other ones.
summary(pred_test-pred_test_org)

shap <- shapley_values[Method %in% c("empirical_independence","comb_Gaussian_sigma.01")]
setkey(shap,Method,test_no)
shap[,reps :=.N,by=test_no]
shap <- shap[reps==2][,reps:=NULL]
identical(shap[Method=="empirical_independence",test_no],shap[Method=="comb_Gaussian_sigma.01",test_no])
# Test observations are the same for both methods


shap[,high_prob:=F]
shap[pred>shap[,quantile(pred,0.98)],high_prob:=T]
shap_high_prob <- shap[high_prob==T]


data <- copy(XYtest[,test_no:=1:.N])
data <- data[shap[Method=="empirical_independence",.(test_no,high_prob,pred)],on="test_no"]
setkey(data,test_no)
setcolorder(data)

data_high_prob <- data[high_prob==T]

library(corrplot)

features <- names(Xtest)
p <- length(features)
special_feats <- 13:14
print(features[special_feats])

cor_all <- data[,cor(.SD),.SDcols=features]
cor_high_prob <- data_high_prob[,cor(.SD),.SDcols=features]
diff <- cor_all-cor_high_prob

pdf(file = "inst/newres/corrplots.pdf",height = 15,width = 15)
par(mfrow=c(2,2))
corrplot(cor_all,title = "All test data",mar=c(0,0,1,0))
rect(xleft = 1-0.5,xright = p+0.5, ybottom = p-special_feats[2]+0.5,ytop = p-special_feats[1]+1.5,border = "green",lwd=3)

corrplot(cor_high_prob,title = "All high prob test data",mar=c(0,0,1,0))
rect(xleft = 1-0.5,xright = p+0.5, ybottom = p-special_feats[2]+0.5,ytop = p-special_feats[1]+1.5,border = "green",lwd=3)

corrplot(diff,is.corr=F,title = "Difference",mar=c(0,0,1,0))
rect(xleft = 1-0.5,xright = p+0.5, ybottom = p-special_feats[2]+0.5,ytop = p-special_feats[1]+1.5,border = "green",lwd=3)
dev.off()

feat_1 <- features[special_feats][1]
feat_2 <- features[special_feats][2]

range_feat_1 <- range(data_high_prob[,feat_1,with=F])
range_feat_2 <- range(data_high_prob[,feat_2,with=F])
range_feat_1 <- range_feat_1+diff(range_feat_1)*c(-0.1,0.1)
range_feat_2 <- range_feat_2+diff(range_feat_2)*c(-0.1,0.1)

feat_1_val <- seq(range_feat_1[1],range_feat_1[2],length.out=100)
feat_2_val <- seq(range_feat_2[1],range_feat_2[2],length.out=100)
feat_grid <- expand.grid(feat_1_val,feat_2_val)
library(ggplot2)
lm_obj=lm(data_high_prob[,c(feat_2,feat_1),with=F])
lm_obj_all=lm(data[,c(feat_2,feat_1),with=F])

pdf("inst/newres/ind_pred_plot.pdf")
#gg <- list()
for(i in 1:data_high_prob[,.N]){
  this_test_no <- data_high_prob[i,test_no]

  data_i <- data_high_prob[i,..features]
  data_i[,ID:=1]
  data_i <- merge(data_i,data.table(ID=rep(1,nrow(feat_grid))),by="ID")[,ID:=NULL]
  data_i[,(feat_1):=feat_grid[,1]]
  data_i[,(feat_2):=feat_grid[,2]]

  this_pred <- data_high_prob[i,pred]
  plot_DT <- as.data.table(cbind(feat_grid,pred=predict(model,as.matrix(data_i[,..features]))))
  names(plot_DT)[1:2] = c(feat_1,feat_2)


  dep_val <- unlist(round(shap_high_prob[test_no==this_test_no,c("Method",features[special_feats]),with=F][1,2:3],4))
  indep_val <- unlist(round(shap_high_prob[test_no==this_test_no,c("Method",features[special_feats]),with=F][2,2:3],4))

  chr_dep = paste0("Dep: ",names(dep_val)[1],": ",(dep_val)[1],", ",names(dep_val)[2],": ",(dep_val)[2])
  chr_indep = paste0("Indep: ",names(indep_val)[1],": ",(indep_val)[1],", ",names(indep_val)[2],": ",(indep_val)[2])

  title = paste0("Shapley values\n",chr_dep,"\n",chr_indep)

  lims = c(-0.7,0.7)#max(abs(range(plot_DT[,pred]-this_pred)))*c(-1,1)
  #gg[[i]] <-
  print(ggplot(plot_DT,aes(x= get(feat_1), y = get(feat_2)))+geom_raster(mapping=aes(fill = pred-this_pred))+
          geom_point(data = data[,features[special_feats],with=F],col=5,size=4,alpha=0.6)+
          geom_point(data = data_high_prob[,features[special_feats],with=F],col=1,size=4,alpha=0.6)+
          geom_point(data = data_high_prob[i,features[special_feats],with=F],col=3,size=4,shape=4,stroke=2)+
          xlim(range_feat_1)+ylim(range_feat_2) + xlab(feat_1)+ylab(feat_2)+
          scale_fill_gradientn(colours=c("red","white","blue"),limits=lims)+
          geom_abline(intercept=lm_obj$coefficients[1],slope=lm_obj$coefficients[2])+
#          geom_abline(intercept=lm_obj$coefficients[1]*1.1,slope=lm_obj_all$coefficients[2],col=5)+
          ggtitle(title))


}
dev.off()

cbind(shap_high_prob[,c("Method" ,features[special_feats]),with=F][Method=="empirical_independence"][,-1],
      shap_high_prob[,c("Method" ,features[special_feats]),with=F][Method=="comb_Gaussian_sigma.01"][,-1])

dcast(,as.formula(...~Method))


