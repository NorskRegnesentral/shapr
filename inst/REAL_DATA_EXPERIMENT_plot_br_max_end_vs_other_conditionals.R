library(data.table)
library(shapr)

resfolder <- "/disk/home/jullum/Prosjekter_lokalt/tmp/SHAPR_SIMRES/final"

figfolder <- "/nr/project_stat/BigInsight/Projects/Explanations/Kode/Kode paper 1/figs"
dir.create(figfolder)


explainer<- readRDS(paste0(resfolder,"/REAL_DATA_EXPERIMENT_RERUN_EXPLAINER_mod.rds"))

Shapley.approx <- readRDS(file.path(resfolder,"REAL_DATA_EXPERIMENT_RERUN_SHAPR_specific_br_max.rds"))

#Shapley.approx_1 <- readRDS(paste0(resfolder,"/REAL_DATA_EXPERIMENT_RERUN_SHAPR_random_IDs_",1,"_.rds"))
#Shapley.approx_2 <- readRDS(paste0(resfolder,"/REAL_DATA_EXPERIMENT_RERUN_SHAPR_random_IDs_",2,"_.rds"))
#Shapley.approx_3 <- readRDS(paste0(resfolder,"/REAL_DATA_EXPERIMENT_RERUN_SHAPR_random_IDs_",3,"_.rds"))


features <- names(explainer$x_train)



this_S <- which(features=="br_max_end")

#cor(explainer$x_train[,..these_vars],explainer$x_train[,br_max_end],method="kendall")



this_id_combination <- explainer$X[n_features==length(this_S)][features==this_S,id_combination]

samps_DT <- rbind(cbind(Shapley.approx$comb_Gaussian_sigma.01$samples_dt[id_combination %in% this_id_combination],method="empirical-0.1+Gaussian"),
                    cbind(Shapley.approx$independence$samples_dt[id_combination %in% this_id_combination],method="original"))
#samps_DT[,id:=NULL]

features <- names(samps_DT)[-ncol(samps_DT)+(0:4)]
features <- gsub("_end","",features)
features <- gsub("_correct","",features)
names(samps_DT)[-ncol(samps_DT)+(0:4)] <- features



melt_samps_DT <- melt(data = samps_DT,id.vars = c("id","method","p_hat","w","id_combination","br_max"))


these_vars <- c("br_mean","br_min","br_std","inn_mean",
                "inn_std_mean","kk_min","sum_std_mean","sk_std_mean","kk_std_mean")

these_vars2 <- c("br_mean","br_std","kk_min","sk_std_mean")
these_vars3 <- c("br_mean","br_std")

# samps_DT_1 <- rbind(cbind(Shapley.approx_1$comb_Gaussian_sigma.01$samples_dt[id_combination %in% this_id_combination],method="empirical−0.1+Gaussian"),
#                   cbind(Shapley.approx_1$independence$samples_dt[id_combination %in% this_id_combination],method="original"))
# merge_dt_1 <- data.table(id=1:length(Shapley.approx_1$these_IDs),ID=Shapley.approx_1$these_IDs)
#
# samps_DT_1 <- samps_DT_1[merge_dt_1,on="id"]
# samps_DT_1[,id:=NULL]
# setnames(samps_DT_1,"ID","id")
#
# samps_DT_2 <- rbind(cbind(Shapley.approx_2$comb_Gaussian_sigma.01$samples_dt[id_combination %in% this_id_combination],method="empirical−0.1+Gaussian"),
#                     cbind(Shapley.approx_2$independence$samples_dt[id_combination %in% this_id_combination],method="original"))
# merge_dt_2 <- data.table(id=1:length(Shapley.approx_2$these_IDs),ID=Shapley.approx_2$these_IDs)
#
# samps_DT_2 <- samps_DT_2[merge_dt_2,on="id"]
# samps_DT_2[,id:=NULL]
# setnames(samps_DT_2,"ID","id")
#
#
# samps_DT_3 <- samps_DT_3[merge_dt_3,on="id"]
# samps_DT_3[,id:=NULL]
# setnames(samps_DT_3,"ID","id")
#
# samps_DT_3 <- rbind(cbind(Shapley.approx_3$comb_Gaussian_sigma.01$samples_dt[id_combination %in% this_id_combination],method="empirical−0.1+Gaussian"),
#                     cbind(Shapley.approx_3$independence$samples_dt[id_combination %in% this_id_combination],method="original"))
# merge_dt_3 <- data.table(id=1:length(Shapley.approx_3$these_IDs),ID=Shapley.approx_3$these_IDs)
# merge_dt_3 <- merge_dt_3[!duplicated(ID)]
#
# samps_DT_3 <- samps_DT_3[merge_dt_3,on="id"]
# samps_DT_3[,id:=NULL]
# setnames(samps_DT_3,"ID","id")
#
#
# melt_samps_DT <- melt(data = rbind(samps_DT_1,samps_DT_2,samps_DT_3),id.vars = c("id","method","p_hat","w","id_combination","br_max_end"))
setnames(explainer$x_train,names(explainer$x_train),features)

melt_x_train <- melt(data = explainer$x_train,id.vars="br_max")

# TODO here:
# 1. add other individuals which have more different values of br_max_end
# 2. find a better method for adding the points being used by empirical and empgauss


# Change order of
melt_samps_DT[,method:=as.factor(method)]
melt_samps_DT[,method:=factor(method,levels=levels(method)[2:1])]
#melt_samps_DT[,method:=relevel(method,"original")]

setorder(melt_samps_DT,id,method,variable,w)

melt_samps_DT[,w:=w/sum(w),by=.(id,method,variable)]
melt_samps_DT[,wsum:=cumsum(w),by=.(id,method,variable)]



melt_samps_DT[,max(wsum),by=.(id,method,variable)]

melt_samps_DT[wsum>=0.05 & variable %in% these_vars3,.N,by=.(br_max,method,variable)]


library(ggplot2)
# gg0 <- ggplot(melt_x_train[variable %in% these_vars2],
#               aes(y=br_max_end,x=value))+
# #              aes(x=sign(br_max_end)*sqrt(abs(br_max_end)),y=value))+
#   geom_point(alpha=0.1,size=1)+
#   facet_wrap(vars(variable),scales="free")+
#   scale_x_continuous(trans="pseudo_log")+
#   scale_y_continuous(trans="pseudo_log")+
#   xlab("other variable")+
#   ylab("br_max")
# gg1 <- gg0 + geom_point(data=melt_samps_DT[variable %in% these_vars2 & br_max_end==0],aes(color=method,alpha=w),stroke=0,size=2)+
#   scale_alpha(guide = 'none',range = c(0.5,1))+
# #  ggtitle("90% most important samples for S=br_max vs other variables")+
#   ggtitle("Samples for S=br_max vs other variables")+
#   theme(legend.position = "bottom")
# gg1
# ggsave(file.path(figfolder,"plot_br_max_vs_others.png"),width = 6,height=6)


melt_samps_DT2 <- copy(melt_samps_DT)
melt_samps_DT2[method=="original",br_max:=br_max+0.1]
melt_samps_DT2[method=="empirical−0.1+Gaussian",br_max:=br_max-0.1]


gg02 <- ggplot(melt_x_train[variable %in% these_vars3],
              aes(y=br_max,x=value))+
  #              aes(x=sign(br_max_end)*sqrt(abs(br_max_end)),y=value))+
  geom_point(alpha=1,size=1,stroke=0)+
  facet_wrap(vars(variable),scales="free_x")+
  scale_x_continuous(trans="pseudo_log")+
  scale_y_continuous(trans="pseudo_log")+
  xlab("br_mean                                                                                          br_std")+
  ylab("br_max")
gg12 <- gg02 + geom_point(data=melt_samps_DT2[variable %in% these_vars3 & br_max>=-0.2 & br_max<=0.2],aes(color=method),stroke=0,size=1.5)+
#  scale_alpha(guide = 'none',range = c(0.1,1))+
  ggtitle("Samples for S=br_max vs other variables")+
  theme(legend.position = "bottom",
    strip.background = element_blank(),
    strip.text.x = element_blank())
gg12
ggsave(file.path(figfolder,"plot_br_max_vs_others_final.pdf"),width = 9,height=6)


#
#
# ggplot(melt_x_train[variable %in% these_vars])+
#   geom_point(aes(x=br_max_end,y=value))+
#   geom_segment(data=melt_samps_DT[variable %in% these_vars],aes(x=br_max_end-1,xend=br_max_end+1,y=value,yend=value,col=method),alpha=2)+
#   facet_wrap(vars(variable),scales="free")
#
# XYtest <-   fread("/nr/project/stat/BFFGB18/LIME/lime/R/test6.csv")
#
# hist(XYtest$br_max_end)
#
