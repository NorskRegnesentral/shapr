


#explainer <- readRDS("/nr/project_stat/BigInsight/Projects/Explanations/Kode/Kode paper 1/REAL_DATA_EXPERIMENT_RERUN_EXPLAINER_new.rds")
#Shapley.approx <- readRDS("/nr/project_stat/BigInsight/Projects/Explanations/Kode/Kode paper 1/REAL_DATA_EXPERIMENT_RERUN_DATA_new.rds")
run_these_tests <- c(1,29,705)
features <- colnames(Shapley.approx$comb_Gaussian_sigma.01$x_test)

DT <- rbind(cbind(Shapley.approx$comb_Gaussian_sigma.01$dt,method="comb_Gaussian_sigma.01",test_no = run_these_tests),
            cbind(Shapley.approx$independence$dt,method="independence",test_no = run_these_tests))

DT[,lapply(.SD,function(x)abs(diff(x))),by=.(test_no),.SDcols=features][,do.call(pmax, .SD),by=test_no,.SDcols=features]

# FOr all these 3, kk_std_mean_correct_end gives the largest differences
feat_no <- which(features=="kk_std_mean_correct_end")

samps_DT <- rbind(Shapley.approx$comb_Gaussian_sigma.01$samples_dt[,method:="comb_Gaussian_sigma.01"],
            Shapley.approx$independence$samples_dt[,method:="independence"])


dt_res <- samps_DT[, .(k = sum((p_hat * w) / sum(w))), .(id, id_combination,method)]
data.table::setkeyv(dt_res, c("method","id", "id_combination"))
dt_mat <- data.table::dcast(dt_res, method+id_combination ~ id, value.var = "k",)
#dt_mat[, id_combination := NULL]
#kshap_independence <- t(explainer$W %*% as.matrix(copy(dt_mat[method=="independence"])[,method:=NULL]))
#kshap_dependence <- t(explainer$W %*% as.matrix(copy(dt_mat[method=="comb_Gaussian_sigma.01"])[,method:=NULL]))

subset_diff <- dt_res[,list(diff=diff(k)),by=.(id,id_combination)]

subset_weight <- explainer$W[feat_no,]


subset_diff[,subset_weight:=rep(subset_weight,3)]

subset_diff[,influence:=abs(diff*subset_weight)]

subset_diff[,rank:=frank(-influence),by=id]

setkey(subset_diff,"rank","id")

explainer$S[subset_diff[rank<=3,id_combination],]

# id==1
this_id <- 1

this_id_comb <- subset_diff[rank==1 & id==this_id,id_combination]

S_feats <- features[as.logical(explainer$S[this_id_comb,])]
Sbar_feats <- features[!as.logical(explainer$S[this_id_comb,])]

cond_dist <- samps_DT[id_combination==this_id_comb & id == this_id ,c(Sbar_feats,"p_hat","w","method"),with=F]

#ggplot(cond_dist,aes(x=get(Sbar_feats)))+geom_histogram(bins=100)+facet_grid(vars(method))

#####

dist_mat <- shapr:::distance_matrix(
  explainer$x_train,
  Shapley.approx$comb_Gaussian_sigma.01$x_test[this_id,],
  explainer$X$features[this_id_comb]
)

tmp0 <- as.data.frame(t(Shapley.approx$comb_Gaussian_sigma.01$x_test[this_id,]))
for(i in 1:10){
  tmp0[i,] <- tmp0[1,]
}
tmp0 <- as.data.table(tmp0)

explainer$x_train[,dist:=as.vector(dist_mat)]
explainer$x_train[,rank:=frank(dist)]

tmp0[,(Sbar_feats):=explainer$x_train[rank<=10,Sbar_feats,with=F]]
tmp0[,dist:=explainer$x_train[rank<=10,dist]]
tmp0[,w:=exp(-0.5*dist/0.1^2)]

tmp0[,p_hat :=predict_model(model,.SD),.SDcols=c(features)]

cond_dist <- rbind(cond_dist,cbind(tmp0[,c(Sbar_feats,"p_hat","w"),with=F],method="train_data"))
cond_dist[,w:=w/sum(w),by=method]

#ggplot(cond_dist,aes(x=kk_std_end))+geom_histogram(bins=100)+facet_grid(vars(method))
gg0=ggplot(cond_dist,aes(x=get(Sbar_feats)))+geom_histogram(bins=100)+facet_grid(vars(method))+xlim(-1,4)
gg=ggplot(cond_dist,aes(x=get(Sbar_feats),y=..density..,weight=w))+geom_histogram(bins=100)+facet_grid(vars(method))+xlim(-1,4)

cond_dist[,list(mean = mean(p_hat),
                weighted_mean=sum(p_hat*w),
                median=median(p_hat),
                L_quart=quantile(p_hat,0.25),
                U_quart=quantile(p_hat,0.75)),by=method]

tmp <- as.data.frame(t(Shapley.approx$comb_Gaussian_sigma.01$x_test[this_id,]))
for(i in 1:1000){
  tmp[i,] <- tmp[1,]
}
tmp <- as.data.table(tmp)

tmp[,(Sbar_feats):=seq(-1,4,length.out=1000)]
tmp[,p_hat :=predict_model(model,.SD),.SDcols=c(features)]

gg2 = ggplot(tmp,aes(x=get(Sbar_feats),y=p_hat))+geom_line()+xlim(-1,4)


pdf(paste0("/nr/project_stat/BigInsight/Projects/Explanations/Kode/Kode paper 1/cond_dist_comparison_id_",this_id,".pdf"),width = 6,height = 12)
grid.arrange(gg,gg2,heights=c(3,1))
dev.off()

pdf(paste0("/nr/project_stat/BigInsight/Projects/Explanations/Kode/Kode paper 1/prob_comparison_id_",this_id,".pdf"),width = 6,height = 9)
ggplot(cond_dist,aes(x=p_hat,y=..density..,weight=w))+geom_histogram(bins=100)+facet_grid(vars(method))
dev.off()

# Id=1
#                   method      mean weighted_mean    median   L_quart   U_quart
#1: comb_Gaussian_sigma.01 0.5660471     0.5660471 0.4510602 0.3679422 0.7621515
#2:           independence 0.6090768     0.6090768 0.7621515 0.4092430 0.7621515
#3:             train_data 0.6468734     0.6473635 0.7621515 0.4510602 0.7621515

# ID=2
#                   method      mean weighted_mean   median   L_quart  U_quart
#1: comb_Gaussian_sigma.01 0.3411187     0.3411187 0.299217 0.1669828 0.493803
#2:           independence 0.3813394     0.3813394 0.493803 0.2095651 0.493803
#3:             train_data 0.4510041     0.4519239 0.493803 0.4938030 0.493803

# ID=3
#                   method       mean weighted_mean     median    L_quart    U_quart
#1: comb_Gaussian_sigma.01 0.07747985    0.07745061 0.03203833 0.01195433 0.09546224
#2:           independence 0.06888405    0.06888405 0.02627398 0.01211709 0.08851747
#3:             train_data 0.07747985    0.07745061 0.03203833 0.01195433 0.09546224



