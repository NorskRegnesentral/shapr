
### NOTES:

# Too deep trees seems to give bad behavior when inserting NA. If the trees are shallow this is not a problem
# May try to do this also with ctree to see if it is better there, but I doubt it
# Another solution might be to do it with cforest, since the mixing there will give more mixing also if I the trees are deep. That is a promising lead, I think.



#source("/nr/project/stat/BigInsight//Projects//Explanations//EffektivShapley//helpFunctions.R")

#library(treeshap)
library(xgboost)
library(data.table)

# Install the github version of the shapr pacakge
#remotes::install_github("NorskRegnesentral/shapr")

library(shapr)

library(progressr)
progressr::handlers(global = TRUE) # To get progress updates

library(future)
future::plan(multisession, workers = 10) # for paralellization (on both linux and windows)


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


n_features <- ncol(x_train)
abscorMat <- abs(cor(x_train))
max_comp_features <- 8
max_cutoff_features <- 40
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

excluded_feature_cols <- setdiff(names(x_train),cutoff_feats)

x_train_red <- x_train[,..cutoff_feats]
x_explain_red <- x_explain[,..cutoff_feats]

#### OK, so now we test the procedure to estimate x1|x3, but using the model with x1|x2,x3,x4 and compare that to the one we get from x1|x3 directly

# We try with rpart first

x_train_red0 <- copy(x_train_red)
names(x_train_red0) <- paste0("x",1:ncol(x_train_red0))
x_explain_red0 <-copy(x_explain_red)
names(x_explain_red0) <- paste0("x",1:ncol(x_explain_red0))


model_direct <- rpart(x1 ~ x3, data = x_train_red0,
                      control = rpart.control(cp=10^(-8)))

model_indirect <- rpart(x1 ~ x2+x3+x4, data = x_train_red0,
                        control = rpart.control(cp=10^(-3)))

model_indirect_no <- rpart(x1 ~ x2+x3+x4, data = x_train_red0,
                           control = rpart.control(maxsurrogate = 0,cp=10^(-3)))

model_indirect2 <- rpart(x1 ~ x2+x3+x4+x5+x6+x7+x8+x9+x10, data = x_train_red0,
                        control = rpart.control(cp=10^(-3),minbucket = 100))

front <- 1:5
back <- seq(40)[-front]

a = proc.time()
model_test <- rpart(paste0(paste0("x",front,collapse="+"),"~",paste0("x",back,collapse="+")), data = x_train_red0,
                         control = rpart.control(cp=10^(-3),minbucket = 10,maxsurrogate = ))
proc.time()-a

NA_cols <- paste0("x",seq(ncol(x_explain_red0)))[-3]

newdata <-x_explain_red0[i,1:4]
#newdata$x2 = NA_real_

predict(model_indirect,newdata,type="matrix")


for(i in 1:10){
  tmp_dt <- x_explain_red0[i,]
  tmp_dt_NA <- copy(tmp_dt)
  tmp_dt_NA[,(NA_cols) := NA_real_]
  this_feature <- 1L

  x1_samps_direct <- unlist(sample_rpart(model_direct,x_train_red0,tmp_dt)[,1])
  x1_samps_indirect <- unlist(sample_rpart(model_indirect,x_train_red0,tmp_dt_NA)[,1])
  x1_samps_indirect_no <- unlist(sample_rpart(model_indirect_no,x_train_red0,tmp_dt_NA)[,1])
  x1_samps_indirect2 <- unlist(sample_rpart(model_indirect2,x_train_red0,tmp_dt_NA)[,1])

  par(mfrow=c(2,2))
  hist(x1_samps_direct)
  hist(x1_samps_indirect)
  hist(x1_samps_indirect_no)
  hist(x1_samps_indirect2)

}

##### try the same thing with ctree and then cforest
library(party)

model_direct <- party::ctree(x1 ~ x3, data = x_train_red0,
                      control = ctree_control(minbucket = 5, mincriterion = 0.9))

model_indirect <- party::ctree(x1 ~ x2+x3+x4, data = x_train_red0,
                        control = ctree_control(minbucket = 5, mincriterion = 0.9,maxsurrogate =2))

model_indirect_no <- party::ctree(x1 ~ x2+x3+x4, data = x_train_red0,
                           control = ctree_control(minbucket = 5, mincriterion = 0.9,maxsurrogate = 0))

model_indirect2 <- party::ctree(x1 ~ x2+x3+x4+x5+x6+x7+x8+x9+x10, data = x_train_red0,
                         control = ctree_control(minbucket = 5, mincriterion = 0.9,maxsurrogate =2))

model_indirect2_no <- party::ctree(x1 ~ x2+x3+x4+x5+x6+x7+x8+x9+x10, data = x_train_red0,
                                control = ctree_control(minbucket = 5, mincriterion = 0.9,maxsurrogate =0))


tmp_dt <- x_explain_red0[i,]
tmp_dt_NA <- copy(tmp_dt)
tmp_dt_NA[,(NA_cols) := NA_real_]
this_feature <- 1L

x1_samps_direct <- unlist(sample_ctree(model_direct,x_train_red0,tmp_dt)[,1])
x1_samps_indirect <- unlist(sample_ctree(model_indirect,x_train_red0,tmp_dt_NA)[,1])
x1_samps_indirect_no <- unlist(sample_ctree(model_indirect_no,x_train_red0,tmp_dt_NA)[,1])
x1_samps_indirect2 <- unlist(sample_ctree(model_indirect2,x_train_red0,tmp_dt_NA)[,1])
x1_samps_indirect2_no <- unlist(sample_ctree(model_indirect2_no,x_train_red0,tmp_dt_NA)[,1])


par(mfrow=c(2,3))
hist(x1_samps_direct)
hist(x1_samps_indirect)
hist(x1_samps_indirect_no)
hist(x1_samps_indirect2)
hist(x1_samps_indirect2_no)

### Forest:


model_direct <- party::cforest(x1 ~ x3, data = x_train_red0,
                             control = cforest_control(minbucket = 5, mincriterion = 0.9,ntree =50))

model_indirect <- party::cforest(x1 ~ x2+x3+x4, data = x_train_red0,
                               control = cforest_control(minbucket = 5, mincriterion = 0.9,maxsurrogate =2,ntree =50))

model_indirect_no <- party::cforest(x1 ~ x2+x3+x4, data = x_train_red0,
                                  control = cforest_control(minbucket = 5, mincriterion = 0.9,maxsurrogate = 0,ntree =50))

model_indirect2 <- party::cforest(x1 ~ x2+x3+x4+x5+x6+x7+x8+x9+x10, data = x_train_red0,
                                control = cforest_control(minbucket = 5, mincriterion = 0.9,maxsurrogate =4,ntree =50))

model_indirect2_no <- party::cforest(x1 ~ x2+x3+x4+x5+x6+x7+x8+x9+x10, data = x_train_red0,
                                   control = cforest_control(minbucket = 5, mincriterion = 0.9,maxsurrogate =0,ntree =50))


tmp_dt <- x_explain_red0[i,]
tmp_dt_NA <- copy(tmp_dt)
tmp_dt_NA[,(NA_cols) := NA_real_]
this_feature <- 1L

x1_samps_direct <- unlist(sample_cforest(model_direct,x_train_red0,tmp_dt)[,1])
x1_samps_indirect <- unlist(sample_cforest(model_indirect,x_train_red0,tmp_dt_NA)[,1])
x1_samps_indirect_no <- unlist(sample_cforest(model_indirect_no,x_train_red0,tmp_dt_NA)[,1])
x1_samps_indirect2 <- unlist(sample_cforest(model_indirect2,x_train_red0,tmp_dt_NA)[,1])
x1_samps_indirect2_no <- unlist(sample_cforest(model_indirect2_no,x_train_red0,tmp_dt_NA)[,1])


par(mfrow=c(2,3))
hist(x1_samps_direct)
hist(x1_samps_indirect)
hist(x1_samps_indirect_no)
hist(x1_samps_indirect2)
hist(x1_samps_indirect2_no)



###############


sample_cforest <- function(model,x_tr,x_new,n_samples=100){

  rowno <- seq_len(nrow(x_tr))

  simData_list <- list()
  for(j in seq_along(fit.nodes)){
    simData_list[[j]] <- x_new[rep(1,n_samples)]
  }

  fit.nodes <- predict_node.ctree(model = model)
  pred.nodes <- predict_node.ctree(model = model, newdata = simData_list[[1]]) # Same for all
  for(j in seq_along(fit.nodes)){
    for (pred_node in unique(pred.nodes[[j]])){
      these_rows <- which(pred.nodes[[j]] == pred_node)

      newrowno <- sample(rowno[fit.nodes[[j]] == pred_node], length(these_rows),replace = TRUE)
      tmp <- unlist(x_tr[newrowno, this_feature, with=F])
      data.table::set(simData_list[[j]],i = these_rows,j=this_feature, value = tmp)
    }
  }
  simData <- rbindlist(simData_list)


  return(simData)
}


sample_ctree <- function(model,x_tr,x_new,n_samples=1000){

  rowno <- seq_len(nrow(x_tr))

  simData <-x_new[rep(1,n_samples)]

  fit.nodes <- predict_node.ctree(model = model)
  pred.nodes <- predict_node.ctree(model = model, newdata = simData)
  for (pred_node in unique(pred.nodes)){
    these_rows <- which(pred.nodes == pred_node)

    newrowno <- sample(rowno[fit.nodes == pred_node], length(these_rows),replace = TRUE)
    tmp <- unlist(x_tr[newrowno, this_feature, with=F])
    data.table::set(simData,i = these_rows,j=this_feature, value = tmp)
  }

  return(simData)
}


predict_node.ctree <- function(model,newdata=NULL){
  party::where(object=model,newdata=newdata)
}


sample_rpart <- function(model,x_tr,x_new,n_samples=1000){

  rowno <- seq_len(nrow(x_tr))

  simData <-x_new[rep(1,n_samples)]

  fit.nodes <- predict_node.rpart(model = model)
  pred.nodes <- predict_node.rpart(model = model, newdata = simData)
  for (pred_node in unique(pred.nodes)){
    these_rows <- which(pred.nodes == pred_node)

    newrowno <- sample(rowno[fit.nodes == pred_node], length(these_rows),replace = TRUE)
    tmp <- unlist(x_tr[newrowno, this_feature, with=F])
    data.table::set(simData,i = these_rows,j=this_feature, value = tmp)
  }
  return(simData)

}


predict_node.rpart <- function(model,newdata=NULL,version = "new"){
  if(version=="new"){
    # Using a modified version of rpart_leaves from the last answer here:  https://stackoverflow.com/questions/17597739/get-id-name-of-rpart-model-nodes
    # with getFromNamespace instead of ::: to avoid CRAN notes
    if(is.null(newdata)){
      return(model$where)
    }
    if (is.null(attr(newdata, "terms"))) {
      Terms <- delete.response(model$terms)
      newdata <- model.frame(Terms, newdata, na.action = na.pass,
                             xlev = attr(model, "xlevels"))
      if (!is.null(cl <- attr(Terms, "dataClasses")))
        .checkMFClasses(cl, newdata, TRUE)
    }
    newdata <- getFromNamespace("rpart.matrix", ns = "rpart")(newdata)
    where <- unname(getFromNamespace("pred.rpart", ns = "rpart")(model, newdata))
    return(where)
  } else {
    # Using partykit::as.party(model)
    # See: "https://stackoverflow.com/questions/36748531/getting-the-observations-in-a-rparts-node-i-e-cart"
    where <- predict(object=partykit::as.party(model),newdata=newdata,type="node")
    return(where)
  }
}


#####



library(rpart)
tmp_df <-
  data.frame(Y = as.factor(c(1, 1, 1, 1, 1, 0, 0, 0, 0, 0)),
             weight = 10:1,
             height = c(10:7, 5, 6, 4:1))




  tm_0 <- rpart(Y ~ weight + height, data = tmp_df,
                control = rpart.control(minsplit = 1,
                                        minbucket=1,
                                        cp=0,
                                        maxdepth = 1,
                                        usesurrogate = 0))

tm <- rpart(Y ~ weight + height, data = tmp_df,
            control = rpart.control(minsplit =1,
                                    minbucket=1,
                                    cp=0,
                                    maxdepth = 1))


tmp_new_df <- data.frame(weight = c(rep(NA_real_, 4), 3:6), height = rep(3:6, 2))


predict(tm_0, newdata = tmp_new_df)

predict(tm, newdata = tmp_new_df)





