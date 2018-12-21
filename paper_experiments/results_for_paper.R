
#### Results for paper

rm(list=ls())

library(xtable)
library(data.table)
library(ggplot2)

res_folder <- "paper_experiments/res/"

all_res_dim3_file <- "all_results_dim3_with_AICc_per_testobs.csv"
all_red_dim10_file <- "Experiment_5_PiecewiseConstant_XGBoost_GenHyp10dim_heavytails___lCrWn.csv"

#### Creating figure 1:

dat3 <- fread(paste0(res_folder,all_res_dim3_file))



dat3_ex_1 <- dat3[true_model=="Linear"& fitted_model=="Linear" & variables =="Gaussian",]
ggplot(data = dat3_ex_1, aes(x=rho,y=absmean_total,color=rn))+
    geom_line() +
    scale_y_continuous(trans='log10') +
    labs(y="Absolute error (log scale)",
         title = "Results experiment 1",
         color = "Method") +
    xlim(0,1) +
    theme(legend.position = "bottom")
ggsave(file.path(res_folder,"tables_and_figures_for_paper","paper_figure_dim_3_ex_1.pdf"),width = 8, height = 5)


res_dat3_ex_1_rho01 <- as.list(dat3_ex_1[rho==0.1,absmean_total])
names(res_dat3_ex_1_rho01) <- dat3_ex_1[rho==0.1,rn]
mod_dat3_ex_1_rho01 <- unique(subset(dat3_ex_1[rho==0.1,],select=c("true_model","fitted_model","variables","rho")))
tab_dat3_ex_1_rho01 <- cbind(mod_dat3_ex_1_rho01,as.data.table(res_dat3_ex_1_rho01))

res_dat3_ex_1_rho07 <- as.list(dat3_ex_1[rho==0.7,absmean_total])
names(res_dat3_ex_1_rho07) <- dat3_ex_1[rho==0.7,rn]
mod_dat3_ex_1_rho07 <- unique(subset(dat3_ex_1[rho==0.7,],select=c("true_model","fitted_model","variables","rho")))
tab_dat3_ex_1_rho07 <- cbind(mod_dat3_ex_1_rho07,as.data.table(res_dat3_ex_1_rho07))


dat3_ex_3 <- dat3[true_model=="PiecewiseConstant"& fitted_model=="XGBoost" & variables =="Gaussian" & nTrain==2000,]
ggplot(data = dat3_ex_3, aes(x=rho,y=absmean_total,color=rn))+
    geom_line() +
    scale_y_continuous(trans='log10') +
    labs(y="Absolute error (log scale)",
         title = "Results experiment 3",
         color = "Method") +
    xlim(0,1) +
    theme(legend.position = "bottom")
ggsave(file.path(res_folder,"tables_and_figures_for_paper","paper_figure_dim_3_ex_3.pdf"),width = 8, height = 5)

res_dat3_ex_3_rho01 <- as.list(dat3_ex_3[rho==0.1,absmean_total])
names(res_dat3_ex_3_rho01) <- dat3_ex_3[rho==0.1,rn]
mod_dat3_ex_3_rho01 <- unique(subset(dat3_ex_3[rho==0.1,],select=c("true_model","fitted_model","variables","rho")))
tab_dat3_ex_3_rho01 <- cbind(mod_dat3_ex_3_rho01,as.data.table(res_dat3_ex_3_rho01))

res_dat3_ex_3_rho07 <- as.list(dat3_ex_3[rho==0.3,absmean_total])
names(res_dat3_ex_3_rho07) <- dat3_ex_3[rho==0.3,rn]
mod_dat3_ex_3_rho07 <- unique(subset(dat3_ex_3[rho==0.3,],select=c("true_model","fitted_model","variables","rho")))
tab_dat3_ex_3_rho07 <- cbind(mod_dat3_ex_3_rho07,as.data.table(res_dat3_ex_3_rho07))

tab_dat3 <- rbind(tab_dat3_ex_1_rho01,tab_dat3_ex_1_rho07,tab_dat3_ex_3_rho01,tab_dat3_ex_3_rho07,use.names=T,fill=T)

xtab_dat3 <- xtable(tab_dat3,
                    caption = "",
                    label = "tab:result_table_dim3",
                    digits = 3)

print.xtable(xtab_dat3,
             include.rownames = F,
             include.colnames = F,
             only.contents = T,
             hline.after = c(0,nrow(xtab_dat3)),
             file = file.path(res_folder,"tables_and_figures_for_paper",paste0("paper_table_dim_3.tex")))


##### Dim=10 ####

dat10 <- fread(paste0(res_folder,all_red_dim10_file))

dat10_ex_5 <- subset(dat10,select = colnames(dat10)[!grepl("h_X_",colnames(dat10))]) # Just removing the h's for now. Don't need them

res_dat10_ex_5 <- as.list(dat10_ex_5[,absmean_total])
names(res_dat10_ex_5) <- dat10_ex_5[,rn]

mod_dat10_ex_5 <- unique(subset(dat10_ex_5,select=c("true_model","fitted_model","variables")))

tab_dat10_ex_5 <- cbind(mod_dat10_ex_5,as.data.table(res_dat10_ex_5))




#### Creating joint result table for dim 10:

xtab_dat10_ex_5 <- xtable(tab_dat10_ex_5,
               caption = "",
               label = "tab:result_table",
               digits = 3)


print.xtable(xtab_dat10_ex_5,
             include.rownames = F,
             include.colnames = F,
             only.contents = T,
             hline.after = c(0,nrow(xtab_dat10_ex_5)),
             file = file.path(res_folder,"tables_and_figures_for_paper",paste0("paper_table_dim_10.tex")))



