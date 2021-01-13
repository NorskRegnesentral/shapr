
library(data.table)
library(shapr)

#explainer <- readRDS("/nr/project_stat/BigInsight/Projects/Explanations/Kode/Kode paper 1/REAL_DATA_EXPERIMENT_RERUN_EXPLAINER_new.rds")
#Shapley.approx <- readRDS("/nr/project_stat/BigInsight/Projects/Explanations/Kode/Kode paper 1/REAL_DATA_EXPERIMENT_RERUN_DATA_new.rds")
run_these_tests <- c(1,29,705)
features <- colnames(Shapley.approx$empirical$x_test)

DT <- rbind(cbind(Shapley.approx$empirical$dt,method="empirical",test_no = run_these_tests),
            cbind(Shapley.approx$independence$dt,method="independence",test_no = run_these_tests))

DT2 <- DT[,lapply(.SD,function(x)abs(diff(x))),by=.(test_no),.SDcols=features]
DT3 <- DT2[,do.call(pmax, .SD),by=test_no,.SDcols=features]
DT2 <- merge(DT2,DT3,by="test_no")

largest_diff <- rep(NA,3)
for (i in 1:3){
  largest_diff[i] <-names(DT)[which(DT2[i,][1]==DT2[i,V1])][1]
}

samps_DT <- rbind(Shapley.approx$empirical$samples_dt[,method:="empirical"],
                  Shapley.approx$independence$samples_dt[,method:="independence"])


dt_res <- samps_DT[, .(k = sum((p_hat * w) / sum(w))), .(id, id_combination,method)]
data.table::setkeyv(dt_res, c("method","id", "id_combination"))
dt_mat <- data.table::dcast(dt_res, method+id_combination ~ id, value.var = "k",)
#dt_mat[, id_combination := NULL]
#kshap_independence <- t(explainer$W %*% as.matrix(copy(dt_mat[method=="independence"])[,method:=NULL]))
#kshap_dependence <- t(explainer$W %*% as.matrix(copy(dt_mat[method=="empirical"])[,method:=NULL]))

subset_diff <- dt_res[,list(diff=diff(k)),by=.(id,id_combination)]


for (i in 1:3){
  this_id <- i

  print(which(features ==largest_diff[i]))


  feat_no <- which(features==largest_diff[this_id])

  subset_weight0 <- explainer$W[feat_no,]


  subset_diff[id==this_id,subset_weight:=subset_weight0]


}

subset_diff[,influence:=abs(diff*subset_weight)]

subset_diff[,rank:=frank(-influence),by=id]

setkey(subset_diff,"rank","id")



this_id=1

explainer$X[subset_diff[rank<=10 & id==this_id,id_combination],]

explainer$S[subset_diff[rank<=3& id==this_id,id_combination],]


this_id_comb <- subset_diff[rank==1 & id==this_id,id_combination]

S_feats <- features[as.logical(explainer$S[this_id_comb,])]
Sbar_feats <- features[!as.logical(explainer$S[this_id_comb,])]

cond_dist <- samps_DT[id_combination==this_id_comb & id == this_id ,c(Sbar_feats,"p_hat","w","method"),with=F]

#ggplot(cond_dist,aes(x=get(Sbar_feats)))+geom_histogram(bins=100)+facet_grid(vars(method))

f(x*_S,x_Sbar)

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


##################

# > print(paste0("ID:",this_id))
# [1] "ID:1"
# > summary(mindist_empirical)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 0.00000 0.01118 0.03631 0.04758 0.07979 0.13867
# > summary(mindist_independence)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max.
# 0.000     1.112     4.142    52.006    17.840 19032.172
# >
#   > summary(as.vector(sortdist_empirical[1:10,]))
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max.
# 0.0000    0.3548    0.7608    2.7030    1.6248 1155.5910
# > summary(as.vector(sortdist_independence[1:10,]))
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max.
# 0.000     2.239     6.062    77.300    22.296 21546.632
# >


# > print(paste0("ID:",this_id))
# [1] "ID:2"
# > summary(mindist_empirical)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 0.0000  0.0633  0.2094  0.2712  0.4371  0.8187
# > summary(mindist_independence)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
# 0.00     4.65     5.95    83.48    10.12 36773.93
# >
#   > summary(as.vector(sortdist_empirical[1:10,]))
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max.
# 0.0000    0.7289    1.2880    6.4037    2.7163 1770.3597
# > summary(as.vector(sortdist_independence[1:10,]))
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
# 0.00     5.47     6.87   126.45    17.23 41094.88

# > print(paste0("ID:",this_id))
# [1] "ID:3"
# > summary(mindist_empirical)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max.
# 1.000e-08 1.179e-03 2.937e-03 6.195e-03 6.971e-03 3.535e-02
# > summary(mindist_independence)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
# 0.000    0.215    0.769   29.520    5.526 4344.071
# >
#   > summary(as.vector(sortdist_empirical[1:10,]))
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max.
# 0.000     0.537     1.162    15.610     2.545 11968.916
# > summary(as.vector(sortdist_independence[1:10,]))
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
# 0.000    1.129    2.943   49.051   11.040 5944.377


# energy distance


dist_mat_train <- shapr:::mahalanobis_distance_cpp(featureList = list(1:28),
                                                          Xtrain_mat = as.matrix(explainer$x_train),
                                                          Xtest_mat = as.matrix(explainer$x_train),
                                                          mcov = mcov, S_scale_dist = F)[,,1]

dist_mat_empirical <- shapr:::mahalanobis_distance_cpp(featureList = list(1:28),
                                                   Xtrain_mat = as.matrix(data_empirical),
                                                   Xtest_mat = as.matrix(data_empirical),
                                                   mcov = mcov, S_scale_dist = F)[,,1]

dist_mat_independence <- shapr:::mahalanobis_distance_cpp(featureList = list(1:28),
                                                        Xtrain_mat = as.matrix(data_independence),
                                                        Xtest_mat = as.matrix(data_independence),
                                                        mcov = mcov, S_scale_dist = F)[,,1]


M_ii_train <- mean(sqrt(dist_mat_train))
M_jj_empirical <- mean(sqrt(dist_mat_empirical))
M_jj_independence <- mean(sqrt(dist_mat_independence))

M_ij_empirical <- mean(sqrt(dist_mat_train_empirical))
M_ij_independence <- mean(sqrt(dist_mat_train_independence))

(edist_empirical <- 2*M_ij_empirical-M_ii_train-M_jj_empirical)
(edist_independence <- 2*M_ij_independence-M_ii_train-M_jj_independence)

#
# library(energy)
#
# n <- nrow(data_empirical)
# m <- nrow(explainer$x_train)
#
# edist_empirical <- energy::edist(rbind(data_empirical,explainer$x_train),sizes = c(n,m))*(n+m)/(n*m)
# edist_independence <- energy::edist(rbind(data_independence,explainer$x_train),sizes = c(n,m))*(n+m)/(n*m)
#
#
#
# tmp0 <- as.data.frame(t(Shapley.approx$empirical$x_test[this_id,]))
# for(i in 1:10){
#   tmp0[i,] <- tmp0[1,]
# }
# tmp0 <- as.data.table(tmp0)
#
# explainer$x_train[,dist:=as.vector(dist_mat)]
# explainer$x_train[,rank:=frank(dist)]
#
# tmp0[,(Sbar_feats):=explainer$x_train[rank<=10,Sbar_feats,with=F]]
# tmp0[,dist:=explainer$x_train[rank<=10,dist]]
# tmp0[,w:=exp(-0.5*dist/0.1^2)]
#
# tmp0[,p_hat :=predict_model(model,.SD),.SDcols=c(features)]
#
# cond_dist <- rbind(cond_dist,cbind(tmp0[,c(Sbar_feats,"p_hat","w"),with=F],method="train_data"))
# cond_dist[,w:=w/sum(w),by=method]
#
# #ggplot(cond_dist,aes(x=kk_std_end))+geom_histogram(bins=100)+facet_grid(vars(method))
# gg0=ggplot(cond_dist,aes(x=get(Sbar_feats)))+geom_histogram(bins=100)+facet_grid(vars(method))+xlim(-1,4)
# gg=ggplot(cond_dist,aes(x=get(Sbar_feats),y=..density..,weight=w))+geom_histogram(bins=100)+facet_grid(vars(method))+xlim(-1,4)
#
# cond_dist[,list(mean = mean(p_hat),
#                 weighted_mean=sum(p_hat*w),
#                 median=median(p_hat),
#                 L_quart=quantile(p_hat,0.25),
#                 U_quart=quantile(p_hat,0.75)),by=method]
#
# tmp <- as.data.frame(t(Shapley.approx$empirical$x_test[this_id,]))
# for(i in 1:1000){
#   tmp[i,] <- tmp[1,]
# }
# tmp <- as.data.table(tmp)
#
# tmp[,(Sbar_feats):=seq(-1,4,length.out=1000)]
# tmp[,p_hat :=predict_model(model,.SD),.SDcols=c(features)]
#
# gg2 = ggplot(tmp,aes(x=get(Sbar_feats),y=p_hat))+geom_line()+xlim(-1,4)
#
#
# pdf(paste0("/nr/project_stat/BigInsight/Projects/Explanations/Kode/Kode paper 1/cond_dist_comparison_id_",this_id,".pdf"),width = 6,height = 12)
# grid.arrange(gg,gg2,heights=c(3,1))
# dev.off()
#
# pdf(paste0("/nr/project_stat/BigInsight/Projects/Explanations/Kode/Kode paper 1/prob_comparison_id_",this_id,".pdf"),width = 6,height = 9)
# ggplot(cond_dist,aes(x=p_hat,y=..density..,weight=w))+geom_histogram(bins=100)+facet_grid(vars(method))
# dev.off()
#
# # Id=1
# #                   method      mean weighted_mean    median   L_quart   U_quart
# #1: empirical 0.5660471     0.5660471 0.4510602 0.3679422 0.7621515
# #2:           independence 0.6090768     0.6090768 0.7621515 0.4092430 0.7621515
# #3:             train_data 0.6468734     0.6473635 0.7621515 0.4510602 0.7621515
#
# # ID=2
# #                   method      mean weighted_mean   median   L_quart  U_quart
# #1: empirical 0.3411187     0.3411187 0.299217 0.1669828 0.493803
# #2:           independence 0.3813394     0.3813394 0.493803 0.2095651 0.493803
# #3:             train_data 0.4510041     0.4519239 0.493803 0.4938030 0.493803
#
# # ID=3
# #                   method       mean weighted_mean     median    L_quart    U_quart
# #1: empirical 0.07747985    0.07745061 0.03203833 0.01195433 0.09546224
# #2:           independence 0.06888405    0.06888405 0.02627398 0.01211709 0.08851747
# #3:             train_data 0.07747985    0.07745061 0.03203833 0.01195433 0.09546224
#


