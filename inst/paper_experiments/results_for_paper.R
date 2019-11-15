
#### Results for paper

rm(list=ls())

library(xtable)
library(data.table)
library(ggplot2)
library(gridExtra)
library(grid)

res_folder <- "paper_experiments/res/"
output_folder <- "/home/jullum/nr/project_stat/BigInsight/Projects/Explanations/Doc/MJ/GIT/Fig"
#output_folder <- "paper_experiments/res/tables_and_figures_for_paper"

all_res_experiment_A_dim3 <- "all_results_experiment_A_dim_3_Linear_Linear_Gaussian.csv"
all_res_experiment_D_dim3 <- "all_results_experiment_B_dim_3_PiecewiseConstant_XGBoost_Gaussian.csv"
all_res_experiment_B_dim3 <- "all_results_experiment_C_dim_3_Linear_Linear_GenHyp.csv"
all_res_experiment_E_dim3 <- "all_results_experiment_D_dim_3_PiecewiseConstant_XGBoost_GenHyp.csv"
all_res_experiment_C_dim3 <- "all_results_experiment_E_dim_3_Linear_Linear_Gaussianmix.csv"
all_res_experiment_F_dim3 <- "all_results_experiment_F_dim_3_PiecewiseConstant_XGBoost_Gaussianmix.csv"

#all_res_experiment_A_dim10 <- "all_results_experiment_A_dim_10_Linear_Linear_Gaussian.csv"
#all_res_experiment_B_dim10 <- "all_results_experiment_B_dim_10_PiecewiseConstant_XGBoost_Gaussian.csv"
all_res_experiment_G_dim10 <- "all_results_experiment_A_dim_10_Linear_Linear_Gaussian_hacked.csv"
all_res_experiment_H_dim10 <- "all_results_experiment_B_dim_10_PiecewiseConstant_XGBoost_Gaussian_hacked.csv"
all_res_experiment_C_dim10 <- "all_results_experiment_C_dim_10_Linear_Linear_GenHyp.csv"
all_res_experiment_D_dim10 <- "all_results_experiment_D_dim_10_PiecewiseConstant_XGBoost_GenHyp.csv"
all_res_experiment_E_dim10 <- "all_results_experiment_E_dim_10_Linear_Linear_Gaussianmix.csv"
all_res_experiment_F_dim10 <- "all_results_experiment_F_dim_10_PiecewiseConstant_XGBoost_Gaussianmix.csv"


cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


### Testign #####


dat <- fread(aa)

dat[,avg_absrelmean_total := mean(absrelmean_total),by=.(rn)]
dat[,avg_absmean_total := mean(absmean_total),by=.(rn)]
dat[,avg_skillscoremean_total := mean(skillscoremean_total),by=.(rn)]

dat[,.(absmean=mean(avg_absmean_total),skillscore=mean(avg_skillscoremean_total)),by=rn]

aa <- all_res_experiment_G_dim10#"paper_experiments/res/all_results_experiment_D_dim_10_PiecewiseConstant_XGBoost_GenHyp.csv"


dat <- fread(paste0(res_folder,aa))

# A+B+C+E+F dim 3 is done
setkey(dat,rho)

#dat <- dat[rho>=0.5,]
#fwrite(dat,file = aa)
dat[,sum(nTest),by=rho]



dat[rho==0.3,sum(nTest),by=.(this.seed)]

dat[this.seed %in% c(1238,1239),sum(nTest),by=.(rn)]

unique(dat[this.seed==1239,rho])

#1238 + 0.98 for A
#1239 + 0.98 for A
#1239 + 0.90 for A

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

ex_letters <- c("A","B","C","D","E","F","G","H")
ex_dims <- c(3,10)

ex_letter_dims_comb <- data.frame(rbind(c("A",3,"rho","Gaussian","Linear"),
                             c("B",3,"beta.scale","Generalized Hyperbolic","Linear"),
                             c("C",3,"mu.scale","Gaussian mixture","Linear"),
                             c("D",3,"rho","Gaussian","Piecewise constant"),
                             c("E",3,"beta.scale","Generalized Hyperbolic","Piecewise constant"),
                             c("F",3,"mu.scale","Gaussian mixture","Piecewise constant"),
                             c("G",10,"rho","Gaussian","Linear"),
                             c("H",10,"rho","Gaussian","Piecewise constant")))
ex_letter_dims_comb$X6 <- c(expression(rho),
                            expression(kappa),
                            expression(gamma),
                            expression(rho),
                            expression(kappa),
                            expression(gamma),
                            expression(rho),
                            expression(rho))

newnamesDT <- data.table(rn=c("empirical_sigma.01","comb_sigma.01","comb_AIC_each_k","Gaussian","copula","empirical_AIC_each_k","empirical_AIC_full","comb_Gaussian_AIC_each_k","comb_Gaussian_sigma.01","empirical_independence","treeSHAP"),
                         new_rn=c("empirical-0.1","empirical-0.1+copula","empirical-AICc-approx+copula","Gaussian","copula","empirical-AICc-approx","empirical-AICc-exact","empirical-AICc-approx+Gaussian","empirical-0.1+Gaussian","original","TreeSHAP"))

cols <-  c("orange","green2","green2","blue3","deepskyblue","orange","orange","green4","green4","black","red")
#    c("black", "red" ,"orange",,"orange", "deepskyblue", "blue4", "green4","green4", "yellow2", "yellow2")
 #   c(brewer.pal(n=9,name="Set1"),"#000000","#000000")
linetypes <- c(3,1,2,1,1,2,1,2,1,1,1)

#c(rep(1,9),2,6)
#sizes <- c(rep(0.5,9),1,1)

names(cols) = names(linetypes) = newnamesDT$new_rn

for(aa in 1:nrow(ex_letter_dims_comb)){
    letter = ex_letter_dims_comb[aa,1]
    dim0 = ex_letter_dims_comb[aa,2]
    var = as.character(ex_letter_dims_comb[aa,3])
    main.input.1 = ex_letter_dims_comb[aa,4]
    main.input.2 = ex_letter_dims_comb[aa,5]
    xlab = ex_letter_dims_comb[aa,6]

    #
    # if(letter%in%c("A","B")){
    #     var = "rho"
    #     xlab = expression(rho)
    #     main.input.1 = "Gaussian"
    # }
    # if(letter%in%c("C","D")){
    #     var = "beta.scale"
    #     xlab = expression(kappa)
    #     main.input.1 = "Generalized Hyperbolic"
    # }
    # if(letter%in%c("E","F")){
    #     var = "mu.scale"
    #     xlab = expression(gamma)
    #     main.input.1 = "Gaussian mixture"
    # }
    #
    # if(letter %in% c("A","C","E")){
    #     main.input.2 <- "Linear"
    # }
    #
    # if(letter %in% c("B","D","F")){
    #     main.input.2 <- "Piecewise constant"
    # }
    #


    file = paste0(res_folder,get(paste0("all_res_experiment_",letter,"_dim",dim0)))
    if (file.exists(file)){
        dat <- fread(file)

        dat <- dat[rn!="comb_independence",]

        dat <- merge(dat,newnamesDT,by = "rn",all.x = T)

        dat[,avg_absrelmean_total := mean(absrelmean_total),by=.(get(var),rn)]
        dat[,avg_absmean_total := mean(absmean_total),by=.(get(var),rn)]
        dat[,avg_skillscoremean_total := mean(skillscoremean_total),by=.(get(var),rn)]

        #           if(dim0==3){
        #                if(length(unique(dat$rn))==7){
        #                    linetype_val <- c(1,1,1,2,1,1,6)
        #                } else {
        #                    linetype_val <- c(1,1,1,2,1,1)
        #                }#
        #
        #            } else {#
        #
        #                if(length(unique(dat$rn))==8){
        linetype_val <- c(1,1,1,2,1,1,1,6)
    } else {
        linetype_val <- c(1,1,1,2,1,1,1)
    }
    #            }

    g1 <- ggplot(data = dat, aes(x=get(var),y=avg_absmean_total,color=new_rn,linetype=new_rn))+
        geom_line() +
        scale_y_continuous(trans='log10') +
        labs(y="MAE (log scale)",
             x=xlab) +
        #  title = "Results experiment A dim 3",
        # color = "Method")
        xlim(range(dat[,get(var)])) +
        scale_colour_manual(name="",values = cols) +
        scale_linetype_manual(name="", values=linetypes) +
           theme_light() +
        theme(legend.position = "bottom")

    g2 <- ggplot(data = dat, aes(x=get(var),y=avg_skillscoremean_total,color=new_rn,linetype=new_rn))+
        geom_line() +
        labs(y="Skill score (MAE)",
             x=xlab) +
        #  title = "Results experiment A dim 3",
        # color = "Method")
        xlim(range(dat[,get(var)])) +
        scale_colour_manual(name="",values = cols) +
        scale_linetype_manual(name="", values=linetypes) +
            theme_light() +
        theme(legend.position = "bottom")

    #grid.arrange(g1,g2,ncol=2)
    fig=grid_arrange_shared_legend(g1, g2,title =paste0("Results experiment ",letter,"\n","Sampling model: ",main.input.2,", ","feature distribution: ",main.input.1,", ","dimension: ",dim0))
    ggsave(file.path(output_folder,paste0("paper_figure_ex_",letter,"_dim_",dim0,".pdf")),fig,width = 10, height = 5)

    print(paste0("Produced pdf figure for ",file))
}



##### Figure with piecewise constant effects ####

source("paper_scripts/paper_helper_funcs.R")

xx <- seq(-5,5,0.01)
df1 <- data.frame(x = xx)
df1$y <- stepwiseConstant_fun1(df1$x)
df1$Function <- "fun_1"

df2 <- data.frame(x = xx)
df2$y <- stepwiseConstant_fun2(df2$x)
df2$Function <-"fun_2"

df3 <- data.frame(x = xx)
df3$y <- stepwiseConstant_fun3(df3$x)
df3$Function <- "fun_3"

df <- rbind(df1,df2,df3)


ggplot(data = df,mapping = aes(x=x,y=y)) + geom_line(aes(linetype=Function,color=Function),size = 1) +
    theme_light() + labs(title="Piecewise constant functions") #+
    # scale_color_manual(values = c("1" = "red",
    #                               "2" = "blue",
    #                               "3" = "green"),
    #                               labels = c("1" = expression(fun_1),
    #                                          "2" = expression(fun_2),
    #                                          "3" = expression(fun_3)))
    #
    #
    # #+ xlim(c(-5,5))

ggsave(file.path(output_folder,"piecewise_const_funcs.pdf"),width = 7, height = 4)

#plot(x,stepwiseConstant_fun1(x*0.5),type="l",ylim=c(-3,3),lwd=3)
#lines(x,stepwiseConstant_fun2(x*0.5),col=2)
#lines(x,stepwiseConstant_fun3(x*0.5),col=3)


#### Looking at the real data experiments

aa = "paper_experiments/res/all_results_experiment_Real_dim_28_Unknown_XGBoost_Unknown.csv"
datreal <- fread(aa)
XYtest <-   fread("/nr/project/stat/BFFGB18/LIME/lime/R/test6.csv")
colnames(datreal)[-c(1,2)] <- c("intercept",colnames(XYtest)[-c(1,2)])

bb = "paper_experiments/res/all_results_experiment_Real_dim_28_Unknown_XGBoost_Unknown_newnames.csv"

fwrite(x = datreal,file = bb)

datres <- datreal[,lapply(.SD,mean),by=Method,.SDcols=colnames(datreal)[-c(1,2)]]

library(fields)
image.plot(as.matrix(datres[,-c(1,2)]))

sign(as.matrix(datres[,-c(1,2)]))


###### Plott histogram of real data

XYtrain <- fread("/nr/project/stat/BFFGB18/LIME/lime/R/train6.csv")
nn <- c(2,6,9,12,15,18,21,24,27)+1
XYtrain.sub <- subset(XYtrain,select = nn)

colnames(XYtrain.sub) <- substr(colnames(XYtrain.sub),start=1,stop=nchar(colnames(XYtrain.sub))-4)
colnames(XYtrain.sub)[grepl("_correct",colnames(XYtrain.sub))] <- substr(colnames(XYtrain.sub)[grepl("_correct",colnames(XYtrain.sub))],
                                                                         start=1,
                                                                         stop=nchar(colnames(XYtrain.sub)[grepl("_correct",colnames(XYtrain.sub))])-8)


XYtrain.sub.melt <- melt(XYtrain.sub)

ggplot(XYtrain.sub.melt,aes(x=value)) +
    geom_histogram(aes(y=..density..),binwidth = 0.25) +
    facet_wrap(~variable,ncol=3)+
    xlim(c(-2,5)) +
    ylim(c(0,4)) +
    theme_light() +
#    theme(strip.background =element_rect(fill="white"))+
    theme(strip.text = element_text(colour = 'black'))
ggsave(file.path(output_folder,"real_example_histograms.pdf"),width = 6, height = 6)



sub <- aa <- list()
for(i in 1:9){
    sub[[i]] <- subset(XYtrain.sub,select = i)
    colnames(sub[[i]]) <- "var"
    aa[[i]] <- ggplot(sub[[i]],aes(x=var)) +
        geom_histogram(aes(y=..density..),bins=30) +
        xlim(quantile(unlist(sub[[i]]),c(0.01,0.99))) +
        xlab(colnames(XYtrain.sub)[i]) +
        ylab(ifelse(i %in% c(1,4,7),"density","")) +
        theme_light()
}

library(gridExtra)

pdf(file.path(output_folder,"real_example_histograms_2.pdf"),width = 7, height = 7)
grid.arrange(grobs=aa,ncol=3)
dev.off()




#######

tmp <- read.table("/nr/home/kjersti/all_results.csv",sep=",",header=T)
mat1 <- NULL
mat6 <- NULL
for(i in 1:1281)
{
    foo <- t(tmp[which(tmp[,2]==i),-c(1,2,3)])
    mat1 <- cbind(mat1,foo[,1])
    mat6 <- cbind(mat6,foo[,6])
}

dat1 <- as.data.table(t(mat1[c(3,9,19,27),]))
dat6 <- as.data.table(t(mat6[c(3,9,19,27),]))
nam <- substr(colnames(dat1),start=1,stop=nchar(colnames(dat1))-4)
nam[grepl("_correct",nam)] <- substr(nam[grepl("_correct",nam)],
                                                                         start=1,
                                                                         stop=nchar(nam[grepl("_correct",nam)])-8)
colnames(dat1) = colnames(dat6) =nam

melt.dat1 <- melt(dat1)

melt.dat6 <- melt(dat6)

melts <- cbind(melt.dat1,melt.dat6$value)
colnames(melts)[2:3] = c("original","CombGaussian")

library(ggplot2)
ggplot(melts,aes(x=original,y=CombGaussian)) +
    geom_point(size=1) +
    geom_abline(slope=1,color=2)  +
    facet_wrap(~variable,ncol=2)+
    theme_light() +
    #    theme(strip.background =element_rect(fill="white"))+
    theme(strip.text = element_text(colour = 'black')) +
    ylab("empirical-0.1-Gaussian") + xlim(c(-0.07,0.1)) + ylim(c(-0.07,0.11))
ggsave(file.path(output_folder,"Shapley_value_plot_real_example.pdf"),width = 6, height = 6)

#
#
#
# train6 <- read.table("/nr/project/stat/BFFGB18/LIME/lime/R/train6.csv",sep=";",header=TRUE)
#
#
# > par(mfrow=c(3,3))
# > for(i in nn)
#     > hist(train6[,i],main=colnames(train6)[i],xlab="Value",prob=T)
# >
#
#
#
#     > par(mfrow=c(2,2))
# >
#     > for(i in c(3,9,19,27))
#         > {
#             >  plot(mat1[i,],mat6[i,],xlab="Standard",ylab="Comb-Gaussian")
#             >  #lines(mat6[i,],mat6[i,],lty=3)
#                 >  lines(seq(-0.04,0.09,0.01),seq(-0.04,0.09,0.01),lty=3)
#             >  title(colnames(tmp)[i+3])
#             > }
#
#



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



