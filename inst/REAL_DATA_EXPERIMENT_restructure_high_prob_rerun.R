library(data.table)
library(shapr)


resfolder <- "/disk/home/jullum/Prosjekter_lokalt/tmp/SHAPR_SIMRES"

run_list <- readRDS(paste0(resfolder,"/REAL_DATA_EXPERIMENT_RERUN_runlist.rds"))


# explainer_list <- Shapley.approx_list <- list()
# i <- 1
#
# Shapley.approx_list[[i]] <- readRDS(paste0(resfolder,"/REAL_DATA_EXPERIMENT_RERUN_SHAPR_",i,"_.rds"))


for(i in 1:5){
  Shapley.approx_tmp <- readRDS(paste0(resfolder,"/REAL_DATA_EXPERIMENT_RERUN_SHAPR_",i,"_.rds"))

  matcher <- data.table(id=1:length(run_list[[i]]),ID=run_list[[i]])

  setkey(matcher,id)
  dt <- rbind(cbind(Shapley.approx_tmp$empirical$dt,type="empirical",ID=matcher$ID),
              cbind(Shapley.approx_tmp$independence$dt,type="independence",ID=matcher$ID))
  setkey(dt,ID)

  # setkey(Shapley.approx_tmp$independence$samples_dt,id)
  # Shapley.approx_tmp$independence$samples_dt <- Shapley.approx_tmp$independence$samples_dt[matcher]
  # Shapley.approx_tmp$independence$samples_dt[,type:="independence"]
  #
  # setkey(Shapley.approx_tmp$empirical$samples_dt,id)
  # Shapley.approx_tmp$empirical$samples_dt <- Shapley.approx_tmp$empirical$samples_dt[matcher]
  # Shapley.approx_tmp$empirical$samples_dt[,type:="empirical"]
  #
  # samples_dt <- rbind(Shapley.approx_tmp$independence$samples_dt,
  #                                        Shapley.approx_tmp$empirical$samples_dt)
  # setkey(samples_dt,ID)
  #
  #
  # fwrite(samples_dt,paste0(resfolder,"/REAL_DATA_EXPERIMENT_RERUN_SHAPR_samples_dt.csv"),append = T)
  fwrite(dt,paste0(resfolder,"/REAL_DATA_EXPERIMENT_RERUN_SHAPR_dt.csv"),append = T)

  rm(dt,Shapley.approx_tmp)
  gc()

  print(i)
}


#
#
#
#
# explainer_list[[i]] <- readRDS(paste0(resfolder,"/REAL_DATA_EXPERIMENT_RERUN_EXPLAINER_",i,"_.rds"))
#
#
# Shapley.approx <- list()
# Shapley.approx$dt <- rbindlist
#
#
# features <- colnames(Shapley.approx$empirical$x_test)
#
# DT <- rbind(cbind(Shapley.approx$empirical$dt,method="empirical",test_no = run_these_tests),
#             cbind(Shapley.approx$independence$dt,method="independence",test_no = run_these_tests))
#
