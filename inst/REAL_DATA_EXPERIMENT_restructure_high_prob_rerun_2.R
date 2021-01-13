library(data.table)
library(shapr)


resfolder <- "/disk/home/jullum/Prosjekter_lokalt/tmp/SHAPR_SIMRES"

run_list <- readRDS(paste0(resfolder,"/REAL_DATA_EXPERIMENT_RERUN_runlist.rds"))



dt <- fread(paste0(resfolder,"/REAL_DATA_EXPERIMENT_RERUN_SHAPR_dt.csv"))

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

dt[ID %in% these_IDs,]


these_IDs

need_list_i <- rep(NA,5)
for(i in 1:5){
  need_list_i[i] <- any(these_IDs %in% run_list[[i]])
}


for(i in which(need_list_i)){
  Shapley.approx_tmp <- readRDS(paste0(resfolder,"/REAL_DATA_EXPERIMENT_RERUN_SHAPR_",i,"_.rds"))

  matcher <- data.table(id=1:length(run_list[[i]]),ID=run_list[[i]])

  setkey(matcher,id)
  dt <- rbind(cbind(Shapley.approx_tmp$empirical$dt,type="empirical",ID=matcher$ID),
              cbind(Shapley.approx_tmp$independence$dt,type="independence",ID=matcher$ID))
  setkey(dt,ID)

   setkey(Shapley.approx_tmp$independence$samples_dt,id)
   Shapley.approx_tmp$independence$samples_dt <- Shapley.approx_tmp$independence$samples_dt[matcher]
   Shapley.approx_tmp$independence$samples_dt[,type:="independence"]
   Shapley.approx_tmp$independence$samples_dt <- Shapley.approx_tmp$independence$samples_dt[ID %in%these_IDs]


   setkey(Shapley.approx_tmp$empirical$samples_dt,id)
   Shapley.approx_tmp$empirical$samples_dt <- Shapley.approx_tmp$empirical$samples_dt[matcher]
   Shapley.approx_tmp$empirical$samples_dt[,type:="empirical"]
   Shapley.approx_tmp$empirical$samples_dt <- Shapley.approx_tmp$empirical$samples_dt[ID %in%these_IDs]



   samples_dt <- rbind(Shapley.approx_tmp$independence$samples_dt,
                       Shapley.approx_tmp$empirical$samples_dt)
   setkey(samples_dt,ID)


  fwrite(samples_dt,paste0(resfolder,"/REAL_DATA_EXPERIMENT_RERUN_SHAPR_samples_dt.csv"),append = T)
  #fwrite(dt,paste0(resfolder,"/REAL_DATA_EXPERIMENT_RERUN_SHAPR_dt.csv"),append = T)

  rm(samples_dt,Shapley.approx_tmp)
  gc()

  print(i)
}




