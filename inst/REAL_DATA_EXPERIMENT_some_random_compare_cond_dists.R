library(data.table)
library(shapr)

resfolder <- "/disk/home/jullum/Prosjekter_lokalt/tmp/SHAPR_SIMRES/final"

figfolder <- "/nr/project_stat/BigInsight/Projects/Explanations/Kode/Kode paper 1/figs"
dir.create(figfolder)

explainer<- readRDS(paste0(resfolder,"/REAL_DATA_EXPERIMENT_RERUN_EXPLAINER_mod.rds"))
tmp <- readRDS(paste0(resfolder,"/REAL_DATA_EXPERIMENT_RERUN_S_INTEREST.rds"))

these_ID_combinations <- tmp[[2]]
S_interest_list <- tmp$S_interest_list

features <- names(explainer$x_train)

# XYtest <-   fread("/nr/project/stat/BFFGB18/LIME/lime/R/test6.csv")
# XYtest[,V1:=NULL]
# XYtest[,default :=NULL]
# features <- names(XYtest)
# p <- length(features)
#
# S_interest_list <- list(which(features=="inn_std_mean_correct_end"),
#                         which(features %in% c("kk_mean_end","br_max_end")),
#                         which(!(features %in% c("sum_min_end","sum_mean_end","sum_max_end"))),
#                         which(features %in% c("inn_std_mean_correct_end","tr_std_mean_correct_end","sk_std_mean_correct_end","kk_min_end",
#                                               "kk_mean_end","kk_max_end","kk_std_end","kk_std_mean_correct_end")))
#
# id_comb <- rep(NA,length(S_interest_list))
# for (i in 1:length(S_interest_list)){
#
#   S <- S_interest_list[[i]]
#   Sbar <- (1:p)[!(1:p %in% S)]
#
#   for (j in 1:nrow(explainer$X)){
#     if(identical(unlist(explainer$X[j,features]),S)){
#       id_comb[i] <- explainer$X[j,id_combination]
#     }
#
#   }
#
#
#
# print(i)
# }

mcov <- stats::cov(explainer$x_train)
res_dat <- NULL
p <- ncol(explainer$x_train)

for (i in 1:5){
  Shapley.approx <- readRDS(paste0(resfolder,"/REAL_DATA_EXPERIMENT_RERUN_SHAPR_random_IDs_",i,"_.rds"))
  samps_DT <- rbind(cbind(Shapley.approx$comb_Gaussian_sigma.01$samples_dt[id_combination %in% these_ID_combinations],method="empirical−0.1+Gaussian"),
                    cbind(Shapley.approx$independence$samples_dt[id_combination %in% these_ID_combinations],method="original"))
  merge_dt <- data.table(id=1:length(Shapley.approx$these_IDs),ID=Shapley.approx$these_IDs)

  samps_DT

  samps_DT <- samps_DT[merge_dt,on="id"]
  samps_DT[,id:=NULL]
  setnames(samps_DT,"ID","id")

  for(k in 1:length(Shapley.approx$these_IDs)){
    for (j in 1:length(S_interest_list)){
      S_feats0 <- S_interest_list[[j]]
      Sbar_feats0 <- (1:p)[!(1:p %in% S_feats0)]

      data_original <- samps_DT[id_combination==these_ID_combinations[j] & id==Shapley.approx$these_IDs[k] & method == "original",..features]
      data_empgaus <- samps_DT[id_combination==these_ID_combinations[j] & id==Shapley.approx$these_IDs[k] & method == "empirical−0.1+Gaussian",..features]

      dist_mat_train_original <- shapr:::mahalanobis_distance_cpp(featureList = list(1:28),
                                                                   Xtrain_mat = as.matrix(explainer$x_train),
                                                                   Xtest_mat = as.matrix(data_original),
                                                                   mcov = mcov, S_scale_dist = F)[,,1]
      dist_mat_train_empgaus <- shapr:::mahalanobis_distance_cpp(featureList = list(1:28),
                                                                      Xtrain_mat = as.matrix(explainer$x_train),
                                                                      Xtest_mat = as.matrix(data_empgaus),
                                                                      mcov = mcov, S_scale_dist = F)[,,1]

      mindist_original <- apply(X = dist_mat_train_original,MARGIN = 2,FUN=min)
      mindist_empgaus <- apply(X = dist_mat_train_empgaus,MARGIN = 2,FUN=min)

      mean_min_10dist_original <- colMeans(apply(X = dist_mat_train_original,MARGIN = 2,FUN=sort)[1:10,])
      mean_min_10dist_empgaus <- colMeans(apply(X = dist_mat_train_empgaus,MARGIN = 2,FUN=sort)[1:10,])


      tmp <- data.table(id=Shapley.approx$these_IDs[k],
                        id_combination=these_ID_combinations[j],
                        val = mindist_original,
                        method = "original",
                        type = "min")

      tmp2 <- data.table(id=Shapley.approx$these_IDs[k],
                        id_combination=these_ID_combinations[j],
                        val = mindist_empgaus,
                        method = "empgaus",
                        type = "min")

      tmp3 <- data.table(id=Shapley.approx$these_IDs[k],
                        id_combination=these_ID_combinations[j],
                        val = mean_min_10dist_original,
                        method = "original",
                        type = "mean_min_10dist")

      tmp4 <- data.table(id=Shapley.approx$these_IDs[k],
                         id_combination=these_ID_combinations[j],
                         val = mean_min_10dist_empgaus,
                         method = "empgaus",
                         type = "mean_min_10dist")

      res_dat <- rbind(res_dat,tmp,tmp2,tmp3,tmp4)

      print(c(i,j,k))

    }

  }

}

#res_dat0 <- copy(res_dat)
#res_dat <- copy(res_dat0)


#res_dat[,.N,by=.(id,id_combination,type,method)][N==4000]

res_dat[method=="original",original_mean:=mean(val),by=.(id,id_combination,type)]

res_dat[,val:=val/mean(original_mean,na.rm=T),by=.(id,id_combination,type)]

desc_dt <- data.table(id_combination = these_ID_combinations,
                      S=c("inn_std_mean","kk_mean, br_max","All except sum_min,\n sum_mean and sum_max",
                          "inn_std_mean, tr_std_mean\n sk_std_mean, kk_min, kk_mean\n kk_max, kk_std, kk_std_mean"))

res_data <- merge(res_dat,desc_dt,by="id_combination")

tmp <- readRDS(paste0(resfolder,"/REAL_DATA_EXPERIMENT_RERUN_S_INTEREST.rds"))


desc_2 <- NULL
for (i in 1:length(tmp$run_list)){
  desc_2 <- rbind(desc_2,cbind(tmp$run_list[[i]],tmp$run_list_preds[[i]]))
  }
desc_2_DT <- as.data.table(desc_2)
names(desc_2_DT)=c("id_combination","prob")


vec <- NULL
for(i in 1:nrow(desc_2_DT)){
  vec <- c(vec,paste0("ID: ",desc_2_DT$id_combination[i],"\n pred: ",round(desc_2_DT$prob[i],3)))
}

names(vec) = desc_2_DT$id_combination



library(ggplot2)
gg_min <- ggplot(res_data[type=="min"],aes(val,color=method,fill=method,linetype=method))+
#  geom_histogram(alpha=0.1,bins = 100)+scale_x_log10()+
  geom_density(alpha=0.1,adjust=1)+scale_x_log10()+
  facet_grid(vars(id),vars(S),labeller=labeller(id=as_labeller(vec),S=label_both),scales = "free")+
  xlab("Distance to training observation (log-scale)")+
  ggtitle("Distance to nearest training observations per method, subset and individuals")
gg_min
#ggsave(file.path(figfolder,"plot_min_random.pdf"),width=10,height=10)

res_data[method=="empgaus",method:="empirical-0.1-Gaussian"]

these_plot_IDs <- c(860,1119,1570,1)#c(40,1119,1570,1,860, 1279,  37, 1719, 1792)

plot_dt <- res_data[type=="mean_min_10dist" & id %in% these_plot_IDs & id_combination!=2025]
plot_dt[,id:=factor(id,levels=these_plot_IDs)]
levels(plot_dt$id) <- paste0("Individual ",LETTERS[seq_along(these_plot_IDs)])

gg_mean_min_10dist <- ggplot(plot_dt,aes(val,color=method,fill=method))+
  #  geom_histogram(alpha=0.1,bins = 100)+scale_x_log10()+
  geom_density(alpha=0.1)+
  scale_x_log10()+
#  facet_grid(vars(id),vars(S),labeller=labeller(id=as_labeller(vec),S=label_both),scales = "free")+
  facet_grid(vars(id),vars(S),labeller=labeller(S=label_both),scales = "free")+
  xlab("Relative distance (log scale)")+
  ggtitle("Spread of mean distance to 10 nearest training observations per method, subset and individuals")+
  theme(legend.position = "bottom")
gg_mean_min_10dist
ggsave(file.path(figfolder,paste0("plot_mean_min_10_ID_",paste0(these_plot_IDs,collapse="_"),".pdf")),width=8,height=5)


#gg_min+ggtitle(paste0("Distance to nearest training observations for method samples\nfor the subset with largest influence on the",
#                      "largest Shapley value difference"))
#ggsave(file.path(figfolder,"plot_min_largest.pdf"),width = 6,height=6)
















dt <- fread(paste0(resfolder,"/REAL_DATA_EXPERIMENT_RERUN_SHAPR_dt.csv"))
dt0 <- copy(dt)
names(dt0)[1:29] <- 1:29
melt_dt0 <- melt(dt0,id.vars = c("type","ID"))

ggplot(melt_dt0,aes(x=as.numeric(variable)-1,y=value,fill=as.factor(ID),col=type))+geom_line(alpha=0.3,size=1)#+facet_wrap(vars(type))
#ggsave(file.path(figfolder,"high_prob_Shapley_values.pdf"),width = 10,height=6)
features <- colnames(dt)[-c(1,30,31)]


DT2 <- dt[,lapply(.SD,function(x)abs(diff(x))),by=.(ID),.SDcols=features]
DT3 <- DT2[,do.call(pmax, .SD),by=ID,.SDcols=features]
DT2 <- merge(DT2,DT3,by="ID")


largest_diff <- rep(NA,nrow(DT2))
for (i in 1:nrow(DT2)){
  largest_diff[i] <-names(dt)[which(DT2[i,][1]==DT2[i,V1])][1]
}

DT <- data.table(largest_diff,max_diff = DT2$V1,ID=DT2$ID)

DT <- merge(DT,dt[type=="independence",sum(.SD),.SDcols=features,by=ID],by="ID")
setnames(DT,"V1","prob")
setkey(DT,max_diff)

these_IDs <- tail(DT[,ID],4)

#fwrite(dt[ID %in% these_IDs,],file.path(figfolder,"Shapley_vals_these_IDs.csv"))


### Also finding smallest diff among the values which are the 5 largest for these individuals

for(i in DT[,ID]){
  top_5 <- names(tail(sort(unlist(dt[type=="empirical" & ID==i,..features])),5))
  this <- DT2[ID == i,..top_5]
  DT[ID==i,smallest_diff_top_5:=names(which.min(this))]
  DT[ID==i,min_diff_top_5:=min(this)]
}



explainer<- readRDS(paste0(resfolder,"/REAL_DATA_EXPERIMENT_RERUN_EXPLAINER_",1,"_.rds"))
samps_DT <- fread(paste0(resfolder,"/REAL_DATA_EXPERIMENT_RERUN_SHAPR_samples_dt.csv"))
setnames(samps_DT,"type","method")
samps_DT[,id:=NULL]
setnames(samps_DT,"ID","id")


dt_res <- samps_DT[, .(k = sum((p_hat * w) / sum(w))), .(id, id_combination,method)]
data.table::setkeyv(dt_res, c("method","id", "id_combination"))
dt_mat <- data.table::dcast(dt_res, method+id_combination ~ id, value.var = "k",)
#dt_mat[, id_combination := NULL]
#kshap_independence <- t(explainer$W %*% as.matrix(copy(dt_mat[method=="independence"])[,method:=NULL]))
#kshap_dependence <- t(explainer$W %*% as.matrix(copy(dt_mat[method=="empirical"])[,method:=NULL]))

subset_diff <- dt_res[,list(diff=diff(k)),by=.(id,id_combination)]


for (i in these_IDs){

  feat_no <- which(features==DT[ID==i,largest_diff])

  subset_weight0 <- explainer$W[feat_no,]

  subset_diff[id==i,subset_weight:=subset_weight0]

  feat_no_small <- which(features==DT[ID==i,smallest_diff_top_5])
  subset_weight0_small <- explainer$W[feat_no_small,]
  subset_diff[id==i,subset_weight_smallest:=subset_weight0_small]


}

subset_diff[,influence:=abs(diff*subset_weight)]
subset_diff[,rank:=frank(-influence),by=id]
setkey(subset_diff,"rank","id")

subset_diff[,influence_smallest:=abs(diff*subset_weight_smallest)]
subset_diff[,rank_smallest:=frank(-influence_smallest),by=id]
setkey(subset_diff,"rank_smallest","id")



res <- data.table(id=these_IDs,
                  feature="",
                  median_mindist1_empirical=0,
                  median_mindist1_independence=0,
                  median_mindist10_empirical=0,
                  median_mindist10_independence=0,
                  mean_mindist1_empirical=0,
                  mean_mindist1_independence=0,
                  mean_mindist10_empirical=0,
                  mean_mindist10_independence=0,
                  S_feats=vector("list",length(these_IDs)),
                  Sbar_feats=vector("list",length(these_IDs)))

res_data <- NULL

for (i in these_IDs){
  this_id=i
  res[id==this_id,feature:=DT[ID==i,largest_diff]]

  explainer$X[subset_diff[rank<=10 & id==this_id,id_combination],]
  #explainer$S[subset_diff[rank<=3& id==this_id,id_combination],]


  this_id_comb <- subset_diff[rank==1 & id==this_id,id_combination]

  S_feats0 <- features[as.logical(explainer$S[this_id_comb,])]
  Sbar_feats0 <- features[!as.logical(explainer$S[this_id_comb,])]

  cond_dist <- samps_DT[id_combination==this_id_comb & id == this_id ,c(Sbar_feats0,"p_hat","w","method"),with=F]

  #ggplot(cond_dist,aes(x=get(Sbar_feats0)))+geom_histogram(bins=100)+facet_grid(vars(method))


  #####
  data_empirical <- samps_DT[id_combination==this_id_comb & id == this_id & method == "empirical",..features]
  data_independence <- samps_DT[id_combination==this_id_comb & id == this_id & method == "independence",..features]


  mcov <- stats::cov(explainer$x_train)

  dist_mat_train_empirical <- shapr:::mahalanobis_distance_cpp(featureList = list(1:28),
                                                               Xtrain_mat = as.matrix(explainer$x_train),
                                                               Xtest_mat = as.matrix(data_empirical),
                                                               mcov = mcov, S_scale_dist = F)[,,1]
  dist_mat_train_independence <- shapr:::mahalanobis_distance_cpp(featureList = list(1:28),
                                                                  Xtrain_mat = as.matrix(explainer$x_train),
                                                                  Xtest_mat = as.matrix(data_independence),
                                                                  mcov = mcov, S_scale_dist = F)[,,1]

  # Shortest distance

  mindist_empirical <- apply(X = dist_mat_train_empirical,MARGIN = 2,FUN=min)
  mindist_independence <- apply(X = dist_mat_train_independence,MARGIN = 2,FUN=min)

  sortdist_empirical <- apply(X = dist_mat_train_empirical,MARGIN = 2,FUN=sort)
  sortdist_independence <- apply(X = dist_mat_train_independence,MARGIN = 2,FUN=sort)

  # 10-shortest distance
  print(paste0("ID:",this_id))
  summary(mindist_empirical)
  summary(mindist_independence)

  summary(as.vector(sortdist_empirical[1:10,]))
  summary(as.vector(sortdist_independence[1:10,]))

  tmp <- data.table(id=this_id,
                    features=DT[ID==i,largest_diff],
                    val = mindist_empirical,
                    method = "empirical",
                    type = "min")

  res_data <- rbind(res_data,tmp)

  tmp <- data.table(id=this_id,
                    features=DT[ID==i,largest_diff],
                    val = mindist_independence,
                    method = "independence",
                    type = "min")

  res_data <- rbind(res_data,tmp)

  tmp <- data.table(id=this_id,
                    features=DT[ID==i,largest_diff],
                    val = as.vector(sortdist_empirical[1:10,]),
                    method = "empirical",
                    type = "min_10")

  res_data <- rbind(res_data,tmp)

  tmp <- data.table(id=this_id,
                    features=DT[ID==i,largest_diff],
                    val = as.vector(sortdist_independence[1:10,]),
                    method = "independence",
                    type = "min_10")

  res_data <- rbind(res_data,tmp)


  res[id==this_id,(colnames(res)[-c(1:2,11:12)]) := list(median(mindist_empirical),
                                                  median(mindist_independence),
                                                  median(as.vector(sortdist_empirical[1:10,])),
                                                  median(as.vector(sortdist_independence[1:10,])),
                                                  mean(mindist_empirical),
                                                  mean(mindist_independence),
                                                  mean(as.vector(sortdist_empirical[1:10,])),
                                                  mean(as.vector(sortdist_independence[1:10,]))
  )]
  res[id==this_id,S_feats := list(list(S_feats0))]
  res[id==this_id,Sbar_feats := list(list(Sbar_feats0))]


}

res

res_data
library(ggplot2)

gg <- ggplot(res_data,aes(x=as.factor(method),y=val))+
  geom_boxplot()+
  facet_grid(id~type,scales = "free")+scale_y_log10()
gg

labeller <- function(value){
  S_feats <- unlist(res[id==value,S_feats])
  Sbar_feats <- unlist(res[id==value,Sbar_feats])
  prob <- DT[ID==value,prob]

  if(length(S_feats)<length(Sbar_feats)){
    S_lab <- paste0(S_feats,collapse=", ")
  } else {
    S_lab <- paste0("all except ",paste0(Sbar_feats,collapse=", "))
  }

  paste0("ID: ",value,", pred: ",round(prob,3),"\nfeature: ",res_data[id==value,features][1],",\nS: ",S_lab)
}
vec <- sapply(these_IDs,labeller)
names(vec) = these_IDs


gg_min <- ggplot(res_data[type=="min"],aes(x=method,y=val))+
  geom_boxplot()+
  facet_wrap(vars(id),scales = "free",labeller = as_labeller(vec))+scale_y_log10()+ylab("Distance to training observation (log-scale)")
gg_min+ggtitle(paste0("Distance to nearest training observations for method samples\nfor the subset with largest influence on the",
               "largest Shapley value difference"))
ggsave(file.path(figfolder,"plot_min_largest.pdf"),width = 6,height=6)

gg_min_10 <- ggplot(res_data[type=="min_10"],aes(x=method,y=val))+
  geom_boxplot()+
  facet_wrap(vars(id),scales="free",labeller = as_labeller(vec))+scale_y_log10()+ylab("Distance to training observations (log-scale)")
gg_min_10+ggtitle(paste0("Distance to 10 nearest training observations for method samples\nfor the subset with largest influence on the",
                  "largest Shapley value difference"))
ggsave(file.path(figfolder,"plot_min_10_largest.pdf"),width = 6,height=6)














##### SIMILAR PLOT FOR THE LARGEST INFLUENCE TO THE SMALLEST DIFFERENCE IN SHAPLEY VALUES FOR THE SAME INDIVIDUALS

res_smallest <- data.table(id=these_IDs,
                  feature="",
                  median_mindist1_empirical=0,
                  median_mindist1_independence=0,
                  median_mindist10_empirical=0,
                  median_mindist10_independence=0,
                  mean_mindist1_empirical=0,
                  mean_mindist1_independence=0,
                  mean_mindist10_empirical=0,
                  mean_mindist10_independence=0,
                  S_feats=vector("list",length(these_IDs)),
                  Sbar_feats=vector("list",length(these_IDs)))

res_data_smallest <- NULL

for (i in these_IDs){
  this_id=i
  res_smallest[id==this_id,feature:=DT[ID==i,smallest_diff_top_5]]

  explainer$X[subset_diff[rank_smallest<=10 & id==this_id,id_combination],]
  #explainer$S[subset_diff[rank<=3& id==this_id,id_combination],]


  this_id_comb <- subset_diff[rank_smallest==1 & id==this_id,id_combination]

  S_feats0 <- features[as.logical(explainer$S[this_id_comb,])]
  Sbar_feats0 <- features[!as.logical(explainer$S[this_id_comb,])]

  cond_dist <- samps_DT[id_combination==this_id_comb & id == this_id ,c(Sbar_feats0,"p_hat","w","method"),with=F]

  #ggplot(cond_dist,aes(x=get(Sbar_feats0)))+geom_histogram(bins=100)+facet_grid(vars(method))


  #####
  data_empirical <- samps_DT[id_combination==this_id_comb & id == this_id & method == "empirical",..features]
  data_independence <- samps_DT[id_combination==this_id_comb & id == this_id & method == "independence",..features]


  mcov <- stats::cov(explainer$x_train)

  dist_mat_train_empirical <- shapr:::mahalanobis_distance_cpp(featureList = list(1:28),
                                                               Xtrain_mat = as.matrix(explainer$x_train),
                                                               Xtest_mat = as.matrix(data_empirical),
                                                               mcov = mcov, S_scale_dist = F)[,,1]
  dist_mat_train_independence <- shapr:::mahalanobis_distance_cpp(featureList = list(1:28),
                                                                  Xtrain_mat = as.matrix(explainer$x_train),
                                                                  Xtest_mat = as.matrix(data_independence),
                                                                  mcov = mcov, S_scale_dist = F)[,,1]

  # Shortest distance

  mindist_empirical <- apply(X = dist_mat_train_empirical,MARGIN = 2,FUN=min)
  mindist_independence <- apply(X = dist_mat_train_independence,MARGIN = 2,FUN=min)

  sortdist_empirical <- apply(X = dist_mat_train_empirical,MARGIN = 2,FUN=sort)
  sortdist_independence <- apply(X = dist_mat_train_independence,MARGIN = 2,FUN=sort)

  # 10-shortest distance
  print(paste0("ID:",this_id))
  summary(mindist_empirical)
  summary(mindist_independence)

  summary(as.vector(sortdist_empirical[1:10,]))
  summary(as.vector(sortdist_independence[1:10,]))

  tmp <- data.table(id=this_id,
                    features=DT[ID==i,largest_diff],
                    val = mindist_empirical,
                    method = "empirical",
                    type = "min")

  res_data_smallest <- rbind(res_data_smallest,tmp)

  tmp <- data.table(id=this_id,
                    features=DT[ID==i,largest_diff],
                    val = mindist_independence,
                    method = "independence",
                    type = "min")

  res_data_smallest <- rbind(res_data_smallest,tmp)

  tmp <- data.table(id=this_id,
                    features=DT[ID==i,largest_diff],
                    val = as.vector(sortdist_empirical[1:10,]),
                    method = "empirical",
                    type = "min_10")

  res_data_smallest <- rbind(res_data_smallest,tmp)

  tmp <- data.table(id=this_id,
                    features=DT[ID==i,largest_diff],
                    val = as.vector(sortdist_independence[1:10,]),
                    method = "independence",
                    type = "min_10")

  res_data_smallest <- rbind(res_data_smallest,tmp)


  res_smallest[id==this_id,(colnames(res_smallest)[-c(1:2,11:12)]) := list(median(mindist_empirical),
                                                         median(mindist_independence),
                                                         median(as.vector(sortdist_empirical[1:10,])),
                                                         median(as.vector(sortdist_independence[1:10,])),
                                                         mean(mindist_empirical),
                                                         mean(mindist_independence),
                                                         mean(as.vector(sortdist_empirical[1:10,])),
                                                         mean(as.vector(sortdist_independence[1:10,]))
  )]
  res_smallest[id==this_id,S_feats := list(list(S_feats0))]
  res_smallest[id==this_id,Sbar_feats := list(list(Sbar_feats0))]


}

res_smallest

res_data_smallest
library(ggplot2)

gg_smallest <- ggplot(res_data_smallest,aes(x=as.factor(method),y=val))+
  geom_boxplot()+
  facet_grid(id~type,scales = "free")+scale_y_log10()
gg_smallest

labeller <- function(value){
  S_feats <- unlist(res_smallest[id==value,S_feats])
  Sbar_feats <- unlist(res_smallest[id==value,Sbar_feats])
  prob <- DT[ID==value,prob]

  if(length(S_feats)<length(Sbar_feats)){
    S_lab <- paste0(S_feats,collapse=", ")
  } else {
    S_lab <- paste0("all except ",paste0(Sbar_feats,collapse=", "))
  }

  paste0("ID: ",value,", pred: ",round(prob,3),"\nfeature: ",res_data_smallest[id==value,features][1],",\nS: ",S_lab)
}
vec <- sapply(these_IDs,labeller)
names(vec) = these_IDs


gg_min_smallest <- ggplot(res_data_smallest[type=="min"],aes(x=method,y=val))+
  geom_boxplot()+
  facet_wrap(vars(id),scales = "free",labeller = as_labeller(vec))+scale_y_log10()+ylab("Distance to training observation (log-scale)")
gg_min_smallest+ggtitle(paste0("Distance to nearest training observations for method samples\nfor the subset with largest influence on the",
               "smallest Shapley value\n differences among the 5 largest Shapley values"))
ggsave(file.path(figfolder,"plot_min_smallest.pdf"),width = 6,height=6)

gg_min_10_smallest <- ggplot(res_data_smallest[type=="min_10"],aes(x=method,y=val))+
  geom_boxplot()+
  facet_wrap(vars(id),scales="free",labeller = as_labeller(vec))+scale_y_log10()+ylab("Distance to training observations (log-scale)")
gg_min_10_smallest+ggtitle(paste0("Distance to 10 nearest training observations for method samples\nfor the subset with largest influence on the",
                  "smallest Shapley value\n differences among the 5 largest Shapley values"))
ggsave(file.path(figfolder,"plot_min_10_smallest.pdf"),width = 6,height=6)




