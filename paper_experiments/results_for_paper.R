
#### Results for paper

rm(list=ls())

library(xtable)
library(data.table)
library(ggplot2)
library(gridExtra)
library(grid)

res_folder <- "paper_experiments/res/"

all_res_experiment_A_dim3 <- "all_results_experiment_A_dim_3_Linear_Linear_Gaussian.csv"
all_res_experiment_B_dim3 <- "all_results_experiment_B_dim_3_PiecewiseConstant_XGBoost_Gaussian.csv"
all_res_experiment_C_dim3 <- "all_results_experiment_C_dim_3_Linear_Linear_GenHyp.csv"
all_res_experiment_D_dim3 <- "all_results_experiment_D_dim_3_PiecewiseConstant_XGBoost_GenHyp.csv"
all_res_experiment_E_dim3 <- "all_results_experiment_E_dim_3_Linear_Linear_Gaussianmix.csv"
all_res_experiment_F_dim3 <- "all_results_experiment_F_dim_3_PiecewiseConstant_XGBoost_Gaussianmix.csv"

all_res_experiment_A_dim10 <- "all_results_experiment_A_dim_10_Linear_Linear_Gaussian.csv"
all_res_experiment_B_dim10 <- "all_results_experiment_B_dim_10_PiecewiseConstant_XGBoost_Gaussian.csv"
all_res_experiment_C_dim10 <- "all_results_experiment_C_dim_10_Linear_Linear_GenHyp.csv"
all_res_experiment_D_dim10 <- "all_results_experiment_D_dim_10_PiecewiseConstant_XGBoost_GenHyp.csv"
all_res_experiment_E_dim10 <- "all_results_experiment_E_dim_10_Linear_Linear_Gaussianmix.csv"
all_res_experiment_F_dim10 <- "all_results_experiment_F_dim_10_PiecewiseConstant_XGBoost_Gaussianmix.csv"


cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


### Testign #####

dat <- fread(paste0(res_folder,all_res_experiment_B_dim3))

# A+B+C+E+F dim 3 is done

dat[,sum(nTest),by=.(rho,rn)]

#########
# Help function

grid_arrange_shared_legend <-
    function(...,
             ncol = length(list(...)),
             nrow = 1,
             position = c("bottom", "right"),
             title = "Test") {

        plots <- list(...)
        position <- match.arg(position)
        g <-
            ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
        legend <- g[[which(sapply(g, function(x)
            x$name) == "guide-box")]]
        lheight <- sum(legend$height)
        lwidth <- sum(legend$width)
        gl <- lapply(plots, function(x)
            x + theme(legend.position = "none"))
        gl <- c(gl, ncol = ncol, nrow = nrow)

        combined <- switch(
            position,
            "bottom" = arrangeGrob(
                do.call(arrangeGrob, gl),
                legend,
                ncol = 1,
                heights = unit.c(unit(1, "npc") - lheight, lheight)
            ),
            "right" = arrangeGrob(
                do.call(arrangeGrob, gl),
                legend,
                ncol = 2,
                widths = unit.c(unit(1, "npc") - lwidth, lwidth)
            )
        )
        #combined
        grid.arrange(combined,top = textGrob(title,gp=gpar(fontsize=14)))
    }

#####

#### Creating figure, experiment A dim 3 ####

ex_letters <- c("A","B","C","D","E","F")
ex_dims <- c(3,10)

for (letter in ex_letters){
    for(dim0 in ex_dims){

        if(letter%in%c("A","B")){
            var = "rho"
        }
        if(letter%in%c("C","D")){
            var = "beta.scale"
        }
        if(letter%in%c("E","F")){
            var = "mu.scale"
        }

        file = paste0(res_folder,get(paste0("all_res_experiment_",letter,"_dim",dim0)))
        if (file.exists(file)){
            dat <- fread(file)

            dat[,avg_absrelmean_total := mean(absrelmean_total),by=.(get(var),rn)]
            dat[,avg_absmean_total := mean(absmean_total),by=.(get(var),rn)]
            dat[,avg_skillscoremean_total := mean(skillscoremean_total),by=.(get(var),rn)]

            g1 <- ggplot(data = dat, aes(x=get(var),y=avg_absmean_total,color=rn))+
                geom_line() +
                scale_y_continuous(trans='log10') +
                labs(y="Absolute error (log scale)",
                     x=var,
                     #  title = "Results experiment A dim 3",
                     color = "Method") +
                xlim(range(dat[,get(var)])) +
                scale_colour_manual(values=cbbPalette) +
                theme_light() +
                theme(legend.position = "bottom")

            g2 <- ggplot(data = dat, aes(x=get(var),y=avg_skillscoremean_total,color=rn))+
                geom_line() +
                labs(y="Skill score relative to independence",
                     x=var,
                     #    title = "Results experiment A dim 3",
                     color = "Method") +
                xlim(range(dat[,get(var)])) + ylim(-0.5,1) +
                scale_colour_manual(values=cbbPalette) +
                theme_light() +
                theme(legend.position = "bottom")

            #grid.arrange(g1,g2,ncol=2)
            fig=grid_arrange_shared_legend(g1, g2,title = paste0("Results experiment ",letter," dim ",dim0))
            ggsave(file.path(res_folder,"tables_and_figures_for_paper",paste0("paper_figure_ex_",letter,"_dim_",dim0,".pdf")),fig,width = 10, height = 5)

            print(paste0("Produced pdf figure for ",file))
        }
    }
}











################################# old ##########







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



