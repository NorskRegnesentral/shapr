library(data.table)
library(shapr)

resfolder <- "/disk/home/jullum/Prosjekter_lokalt/tmp/SHAPR_SIMRES"

figfolder <- "/nr/project_stat/BigInsight/Projects/Explanations/Kode/Kode paper 1/figs"
dir.create(figfolder)

run_list <- readRDS(paste0(resfolder,"/REAL_DATA_EXPERIMENT_RERUN_runlist.rds"))



dt <- fread(paste0(resfolder,"/REAL_DATA_EXPERIMENT_RERUN_SHAPR_dt.csv"))
dt0 <- copy(dt)
names(dt0)[1:29] <- 1:29
melt_dt0 <- melt(dt0,id.vars = c("type","ID"))

#ggplot(melt_dt0,aes(x=as.numeric(variable)-1,y=value,fill=as.factor(ID),col=type))+geom_line(alpha=0.3,size=1)#+facet_wrap(vars(type))
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

dt_combinations <- explainer$X
weighted_mat <- explainer$W
dt_combinations[,abs_mean_kSweight:=colMeans(abs(weighted_mat))]

subset_diff <- subset_diff[dt_combinations[,.(id_combination,abs_mean_kSweight)],on="id_combination"]

subset_diff[,influence:=abs(diff*abs_mean_kSweight)]
subset_diff[,rank:=frank(-influence),by=id]
setkey(subset_diff,"rank","id")
