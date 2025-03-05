
library(data.table)

library(ggplot2)

result_folder <- "inst/simtest/results"

files <- list.files(result_folder, full.names = TRUE, pattern = ".*\\.rds$")


sh_error_dt_list <- list()
vS_error_dt_list <- list()
est_time_secs_list <- list()
for(i in seq_along(files)){
  file <- files[i]

  res <- readRDS(file)

  true_sh_dt <- get_sh_dt(res$true_explainer)
  true_vS_dt <- get_vS_dt(res$true_explainer)

  for(j in seq_along(res$est_explainer)){
    est_name <- names(res$est_explainer)[j]
    est_explainer <- res$est_explainer[[est_name]]

    est_sh_dt <- get_sh_dt(est_explainer)
    est_vS_dt <- get_vS_dt(est_explainer)
    est_time_secs <- get_computation_time(est_explainer)


    est_sh_dt <- est_sh_dt[variable!="none"]
    est_vS_dt <- est_vS_dt[!(id_coalition %in% c(1,max(id_coalition)))]

    sh_error_dt <- true_sh_dt[est_sh_dt, list(explain_id, variable, error = value-i.value), on=c("explain_id","variable")]
    vS_error_dt <- true_vS_dt[est_vS_dt, list(explain_id, id_coalition, error = value-i.value), on=c("explain_id","id_coalition")]

    sh_error_dt[,estimator := est_name]
    vS_error_dt[,estimator := est_name]

    sh_error_dt[, model_name := res$model_name]
    vS_error_dt[, model_name := res$model_name]

    sh_error_dt[, data_dist_name := res$data_dist_name]
    vS_error_dt[, data_dist_name := res$data_dist_name]

    sh_error_dt[, rho := res$data_dist_param]
    vS_error_dt[, rho := res$data_dist_param]

    sh_error_dt_list[[length(sh_error_dt_list)+1]] <- sh_error_dt
    vS_error_dt_list[[length(vS_error_dt_list)+1]] <- vS_error_dt

    est_time_secs_list[[length(est_time_secs_list)+1]] <- data.table(estimator=est_name,
                                                                     model_name = res$model_name,
                                                                     data_dist_name = res$data_dist_name,
                                                                     rho = res$data_dist_param,
                                                                     time_secs=est_time_secs)
  }

}


sh_error_dt_all <- rbindlist(sh_error_dt_list)
vS_error_dt_all <- rbindlist(vS_error_dt_list)
est_time_secs_all <- rbindlist(est_time_secs_list)



sh_error_dt_all[rho==0.5 & model_name=="gam_many_interactions",mean(abs(error)), by=estimator]
vS_error_dt_all[rho==0.5 & model_name=="gam_many_interactions",mean(abs(error)), by=estimator]
est_time_secs_all[rho==0.5 & model_name=="gam_many_interactions",mean(time_secs), by=estimator]



sh_error_dt_all[,model_name2:=str_replace_all(model_name,"_","\n")]
vS_error_dt_all[,model_name2:=str_replace_all(model_name,"_","\n")]
est_time_secs_all[,model_name2:=str_replace_all(model_name,"_","\n")]


sh_error_dt_all[,model_type:=ifelse(grepl("^gam",model_name),"gam","other")]
sh_error_dt_all[model_type=="other",model_type:=ifelse(grepl("^lm_to_gam",model_name),"lm_to_gam","other")]
sh_error_dt_all[model_type=="other",model_type:=ifelse(grepl("^lm",model_name),"lm","other")]

vS_error_dt_all[,model_type:=ifelse(grepl("^gam",model_name),"gam","other")]
vS_error_dt_all[model_type=="other",model_type:=ifelse(grepl("^lm_to_gam",model_name),"lm_to_gam","other")]
vS_error_dt_all[model_type=="other",model_type:=ifelse(grepl("^lm",model_name),"lm","other")]

sh_error_dt_all[model_type == "gam", interaction := gsub("gam_","",model_name)]
sh_error_dt_all[model_type == "lm", interaction := gsub("lm_","",model_name)]
sh_error_dt_all[!is.na(interaction), interaction := gsub("_interactions","",interaction)]
sh_error_dt_all[,interaction := factor(interaction,levels=c("no","some","more","numerous","many"))]

vS_error_dt_all[model_type == "gam", interaction := gsub("gam_","",model_name)]
vS_error_dt_all[model_type == "lm", interaction := gsub("lm_","",model_name)]
vS_error_dt_all[!is.na(interaction), interaction := gsub("_interactions","",interaction)]
vS_error_dt_all[,interaction := factor(interaction,levels=c("no","some","more","numerous","many"))]


p1 <- ggplot(sh_error_dt_all[model_type=="gam"], aes(x=estimator, y=abs(error), fill=estimator)) +
  geom_boxplot(outliers=FALSE) +
  facet_grid(interaction~rho,labeller=label_both) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  coord_flip()+
  ggtitle("Absolute Shapley value errors for GAM models with varying interaction and feature correlation with Gaussian data distribution")

p2 <- ggplot(sh_error_dt_all[model_type=="lm"], aes(x=estimator, y=abs(error), fill=estimator)) +
  geom_boxplot(outliers=FALSE) +
  facet_grid(interaction~rho,labeller=label_both) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  coord_flip()+
  ggtitle("Absolute Shapley value errors for LM models with varying interaction and feature correlation with Gaussian data distribution")


p3 <- ggplot(vS_error_dt_all[model_type=="gam"], aes(x=estimator, y=abs(error), fill=estimator)) +
  geom_boxplot(outliers=FALSE) +
  facet_grid(interaction~rho,labeller=label_both) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  coord_flip()+
  ggtitle("Absolute v(S) errors for GAM models with varying interaction and feature correlation with Gaussian data distribution")

p4 <- ggplot(vS_error_dt_all[model_type=="lm"], aes(x=estimator, y=abs(error), fill=estimator)) +
  geom_boxplot(outliers=FALSE) +
  facet_grid(interaction~rho,labeller=label_both) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  coord_flip()+
  ggtitle("Absolute v(S) errors for LM models with varying interaction and feature correlation with Gaussian data distribution")


p5 <- ggplot(est_time_secs_all, aes(x=estimator, y=time_secs, fill=estimator)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  coord_flip()+
  ggtitle("Timing across estimators for all models and data features correlation with Gaussian data distribution")



ggsave("inst/simtest/results/plots/sh_error_gam.pdf", p1, width=16, height=10)
ggsave("inst/simtest/results/plots/sh_error_lm.pdf", p2, width=16, height=10)
ggsave("inst/simtest/results/plots/vS_error_gam.pdf", p3, width=16, height=10)
ggsave("inst/simtest/results/plots/vS_error_lm.pdf", p4, width=16, height=10)
ggsave("inst/simtest/results/plots/est_time_secs.pdf", p5, width=16, height=10)

### Now generate the same plots, but with only arf and ctree
sh_error_dt_all_arf <- sh_error_dt_all[estimator %in% c("arf","ctree")]
vS_error_dt_all_arf <- vS_error_dt_all[estimator %in% c("arf","ctree")]
est_time_secs_all_arf <- est_time_secs_all[estimator %in% c("arf","ctree")]

pp1 <- ggplot(sh_error_dt_all_arf[model_type=="gam"], aes(x=estimator, y=abs(error), fill=estimator)) +
  geom_boxplot(outliers=FALSE) +
  facet_grid(interaction~rho,labeller=label_both) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  coord_flip()+
  ggtitle("Absolute Shapley value errors for GAM models with varying interaction and feature correlation with Gaussian data distribution")

pp2 <- ggplot(sh_error_dt_all_arf[model_type=="lm"], aes(x=estimator, y=abs(error), fill=estimator)) +
  geom_boxplot(outliers=FALSE) +
  facet_grid(interaction~rho,labeller=label_both) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  coord_flip()+
  ggtitle("Absolute Shapley value errors for LM models with varying interaction and feature correlation with Gaussian data distribution")

pp3 <- ggplot(vS_error_dt_all_arf[model_type=="gam"], aes(x=estimator, y=abs(error), fill=estimator)) +
  geom_boxplot(outliers=FALSE) +
  facet_grid(interaction~rho,labeller=label_both) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  coord_flip()+
  ggtitle("Absolute v(S) errors for GAM models with varying interaction and feature correlation with Gaussian data distribution")

pp4 <- ggplot(vS_error_dt_all_arf[model_type=="lm"], aes(x=estimator, y=abs(error), fill=estimator)) +
  geom_boxplot(outliers=FALSE) +
  facet_grid(interaction~rho,labeller=label_both) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  coord_flip()+
  ggtitle("Absolute v(S) errors for LM models with varying interaction and feature correlation with Gaussian data distribution")


pp5 <- ggplot(est_time_secs_all_arf, aes(x=estimator, y=time_secs, fill=estimator)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  coord_flip()+
  ggtitle("Timing across estimators for all models and data features correlation with Gaussian data distribution")

ggsave("inst/simtest/results/plots/sh_error_gam_arf_ctree.pdf", pp1, width=16, height=10)
ggsave("inst/simtest/results/plots/sh_error_lm_arf_ctree.pdf", pp2, width=16, height=10)
ggsave("inst/simtest/results/plots/vS_error_gam_arf_ctree.pdf", pp3, width=16, height=10)
ggsave("inst/simtest/results/plots/vS_error_lm_arf_ctree.pdf", pp4, width=16, height=10)
ggsave("inst/simtest/results/plots/est_time_secs_arf_ctree.pdf", pp5, width=16, height=10)




