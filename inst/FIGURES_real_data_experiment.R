
library(data.table)
library(shapr)
library(ggplot2)

figfolder <- "/nr/project_stat/BigInsight/Projects/Explanations/Kode/Kode paper 1/figs"
dir.create(figfolder)


DT <- data.table::fread("/nr/project_stat/BigInsight/Projects/Explanations/Kode/Shapley_Anders/all_results_experiment_Real_dim_28_Unknown_XGBoost_Unknown_newnames.csv")

plot_ids <- c(1,29)
DT[Method=="empirical_independence",Method:="original"]
DT[Method=="comb_sigma.01",Method:="empirical-0.1+Gaussian"]

methods <- c("original","empirical-0.1+Gaussian")
features <- names(DT)[-(1:3)]
features <- gsub("_end","",features)
features <- gsub("_correct","",features)
features <- gsub("std_mean","cv",features)

names(DT)[-(1:3)] <- features


DT <- DT[Method %in% methods]

DT[test_no %in% plot_ids]
DT[,pred:=rowSums(.SD),.SDcols=c(features,"intercept")]

DT2 <- DT[,lapply(.SD,function(x)abs(diff(x))),by=.(test_no),.SDcols=features]
DT3 <- DT[,lapply(.SD,function(x)-diff(sign(x))+1),by=.(test_no),.SDcols=features]
DT4 <- DT[,lapply(.SD,function(x)abs(diff(x))*(-diff(sign(x))+1)),by=.(test_no),.SDcols=features]

for (i in DT4[,test_no]){
  smallest0 <- min(DT4[test_no==i,..features])
  DT4[test_no==i,smallest:=smallest0]

  this_smallest0 = features[which(smallest0==DT4[test_no==i,..features])]
  DT4[test_no==i,this_smallest:=this_smallest0]
}
setkey(DT4,smallest)
head(DT4[,],10)

DT4[]

alt_plot_ids <- c(860,1279,37,1719,1792)

plot_ids2 = c(1119,860)#c(1119,40)
DT[test_no %in% head(DT4[,test_no],10),.(test_no,pred)]

melt_DT <- melt(DT,id.vars = c("Method","test_no","pred"))

cols <- c("empirical-0.1+Gaussian" = "black", "original" = "white")


lims <- range(melt_DT[test_no %in% plot_ids2,value])

vec <- paste0("Individual ",c("B","A"),": Probability of default = ",round(c(DT[Method=="original" & test_no == plot_ids2[1],pred],DT[Method=="original" & test_no == plot_ids2[2],pred]),3))
names(vec) <- plot_ids2

plot_data <- copy(melt_DT[test_no %in% plot_ids2 & variable!="intercept"])

plot_data[,pos_neg:=sign(value)]

plot_data[,variable:=factor(variable,levels=rev(features))]

gg2 <- ggplot(data=plot_data, aes(x=variable,y=value,fill=factor(Method))) +
  geom_bar(position=position_dodge2(reverse = T,padding = 0),stat="identity",width=0.6,color="black") +
  coord_flip() +
  scale_fill_manual(values=cols) +
  labs(fill = "Method",y="Shapley value",x="Feature") +
  #  ylim(min(melt_dt$Shapley_value)-0.05,max(melt_dt$Shapley_value)) +
#  scale_y_continuous(breaks = scales::extended_breaks(8),limits = lims) +
  # guides(fill=FALSE)
  theme(legend.position = "bottom") +
#  guides(color=F) +
#  guides(fill = guide_legend(override.aes = list(color = "black")),
#         color=F) +
  facet_wrap(vars(test_no),ncol = 2,labeller = labeller(test_no=as_labeller(vec)))+
  #ggtitle("Shapley values for two examples")+
  geom_hline(yintercept = 0)
gg2
ggsave(file.path(figfolder,paste0("Shapley_bar_plot_ID_",paste0(plot_ids2,collapse="_"),"new.pdf")),width = 9,height=9)


