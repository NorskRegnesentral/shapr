require(cluster)
require(maptree)
require(corrplot)
require(rockchalk)
require(pcaPP)

## Draw rectangles on the correlation matrix graph.
corrRect.hclust <- function(
  corr,
  k = 2,
  col = "yellow",
  lwd = 6) {
  n <- nrow(corr)
  tree <- hclust(as.dist(1 - abs(corr)))
  hc <- cutree(tree, k = k)
  clustab <- table(hc)[unique(hc[tree$order])]
  cu <- c(0, cumsum(clustab))

  rect(cu[-(k + 1)] + 0.5,
       n - cu[-(k + 1)] + 0.5,
       cu[-1] + 0.5,
       n - cu[-1] + 0.5,
       border = col, lwd = lwd)
}


## Random correlation matrix
set.seed(11100110)
d = 60
corMat = cor(mvrnorm(n=5,rep(0,d),lazyCor(X=0.2,d=d)))
## Add two independent variables
corMat = rbind(corMat,rep(0,d))
corMat = cbind(corMat,c(rep(0,d),1))
corMat = rbind(corMat,rep(0,d+1))
corMat = cbind(corMat,c(rep(0,d+1),1))

## Dimnames are needed
dimnames(corMat) = list(
  c(paste(1:(d+2))),
  c(paste(1:(d+2)))
)

## Plot correlation matrix
corrplot(corMat, method = "square")
## Histogram of correlations
hist(corMat[upper.tri(corMat)],main="Distribution of correlations",xlab="",xlim=c(-1,1),nclass=30,col="brown")

## Use 1 - correlation as distance matrix
dissimilarity = 1 - abs(corMat)
distance = as.dist(dissimilarity)

## Hierarchical clustering
cluster = hclust(distance)

## Plot dendrogram
plot(cluster, main="Dissimilarity = 1 - |Correlation|", xlab="")

## Find optimal number of clusters
optimalK = kgs(cluster, distance, maxclus = d-10, alpha = 1)
plot(names (optimalK), optimalK, xlab="# of clusters", ylab="penalty",type="b",col="green",lwd=4,pch=19)
K = as.numeric(names(optimalK)[which(optimalK==min(optimalK))])

## Display variables with optimal cluster number
clusterK = cutree(cluster,k=K)
clusterList = list()
for(k in 1:K){
  clusterList[[k]] = which(clusterK==k)
}

print(clusterList)

## Plot correlation matrix, ordered in clusters
ord = cluster$order
corrplot(corMat[ord,ord], method = "square")
## Add cluster rectangles
corrRect.hclust(corMat,k=K)

## DNB data
trainData = read.table("/nr/project/stat/BigInsight/Projects/Explanations/Data/train6.csv", sep = ";", header = TRUE)
## Remove response
trainData = trainData[,-1]
## Kendall's tau
corMatDNB = cor.fk(trainData)

## Simplify labels
rown = unlist(strsplit(dimnames(corMatDNB)[[1]],"_end"))
rown = unlist(strsplit(rown,"_correct"))
dimnames(corMatDNB) = list(rown,rown)

## Plot correlation matrix
corrplot(corMatDNB, method = "square")
## Histogram of correlations
hist(corMatDNB[upper.tri(corMatDNB)],main="Distribution of Kendall's tau for DNB data",xlab="",xlim=c(-1,1),nclass=30,col="brown")

## Use 1 - correlation as distance matrix
dissimilarityDNB = 1 - abs(corMatDNB)
distanceDNB = as.dist(dissimilarityDNB)

## Hierarchical clustering
clusterDNB = hclust(distanceDNB)

## Plot dendrogram
plot(clusterDNB, main="Dissimilarity = 1 - |Kendall's tau|", xlab="")

## Find optimal number of clusters
optimalKDNB = kgs(clusterDNB, distanceDNB, maxclus = d-10, alpha = 0.1)
plot(names(optimalKDNB), optimalKDNB, xlab="# of clusters", ylab="penalty",type="b",col="green",lwd=4,pch=19)
KDNB = as.numeric(names(optimalKDNB)[which(optimalKDNB==min(optimalKDNB))])

## Display variables with optimal cluster number
clusterKDNB = cutree(clusterDNB,k=KDNB)
clusterListDNB = list()
for(k in 1:KDNB){
  clusterListDNB[[k]] = which(clusterKDNB==k)
}

print(clusterListDNB)

## Plot correlation matrix, ordered in clusters
ordDNB = clusterDNB$order
corrplot(corMatDNB[ordDNB,ordDNB], method = "square")
## Add cluster rectangles
corrRect.hclust(corMatDNB,k=KDNB)
## Add correlation
corrplot(corMatDNB[ordDNB,ordDNB], method = "number",number.cex = 0.7)
## Add cluster rectangles
corrRect.hclust(corMatDNB,k=KDNB)

## Read Shapley results
sr = read.table("/nr/project/stat/BigInsight/Projects/Explanations/Kode/Shapley_Anders/all_results_experiment_Real_dim_28_Unknown_XGBoost_Unknown_newnames.csv",header=TRUE,sep=",")
g1 = c("tr_mean_end","tr_max_end","tr_std_end")
g2 = c("br_std_end","sum_std_end","inn_mean_end","inn_max_end","inn_std_end")
g3 = c("br_std_mean_correct_end")
g4 = c("sum_std_mean_correct_end")
g5 = c("br_min_end","br_mean_end","br_max_end")
g6 = c("sum_min_end","sum_mean_end","sum_max_end")
g7 = c("sk_std_end","sk_min_end","sk_mean_end","sk_max_end")
g8 = c("inn_std_mean_correct_end")
g9 = c("tr_std_mean_correct_end")
g10 = c("sk_std_mean_correct_end")
g11 = c("kk_min_end","kk_mean_end","kk_max_end")
g12 = c("kk_std_end","kk_std_mean_correct_end")

sort.c = sort(sr[(sr[,"Method"]=="comb_Gaussian_sigma.01"),2],index.return=TRUE)
sort.i = sort(sr[(sr[,"Method"]=="empirical_independence"),2],index.return=TRUE)

cs = sr[(sr[,"Method"]=="comb_Gaussian_sigma.01"),1:31][sort.c$ix[1:1100],]
cs[c(1, 29),]

is = sr[(sr[,"Method"]=="empirical_independence"),1:31][sort.i$ix[1:1100],]
is[c(1, 29),]
is[c(1, 29),3:12]

# m.intercept = mean(cs[,"intercept"])
# m.g1 = mean(rowSums(cs[,g1]))
# m.g2 = mean(rowSums(cs[,g2]))
# m.g3 = mean((cs[,g3]))
# m.g4 = mean((cs[,g4]))
# m.g5 = mean(rowSums(cs[,g5]))
# m.g6 = mean(rowSums(cs[,g6]))
# m.g7 = mean(rowSums(cs[,g7]))
# m.g8 = mean((cs[,g8]))
# m.g9 = mean((cs[,g9]))
# m.g10 = mean((cs[,g10]))
# m.g11 = mean(rowSums(cs[,g11]))
# m.g12 = mean(rowSums(cs[,g12]))

## k=1,705,29
#k=1
folder <- "/nr/project/stat/BigInsight/Projects/Explanations/Doc/JMLR_V3-Kjersti/Fig/"
height <- 10
width <- 4.5

for (k in c(1,29,705)){

  j <- which(c(1,29,705)==k)
  ## Sum of phi's for best model
  m.intercept = (cs[k,"intercept"])
  m.g1 = (sum(cs[k,g1]))
  m.g2 = (sum(cs[k,g2]))
  m.g3 = ((cs[k,g3]))
  m.g4 = ((cs[k,g4]))
  m.g5 = (sum(cs[k,g5]))
  m.g6 = (sum(cs[k,g6]))
  m.g7 = (sum(cs[k,g7]))
  m.g8 = ((cs[k,g8]))
  m.g9 = ((cs[k,g9]))
  m.g10 = ((cs[k,g10]))
  m.g11 = (sum(cs[k,g11]))
  m.g12 = (sum(cs[k,g12]))

  ## Sum of phi's for independent model
  i.intercept = (is[k,"intercept"])
  i.g1 = (sum(is[k,g1]))
  i.g2 = (sum(is[k,g2]))
  i.g3 = ((is[k,g3]))
  i.g4 = ((is[k,g4]))
  i.g5 = (sum(is[k,g5]))
  i.g6 = (sum(is[k,g6]))
  i.g7 = (sum(is[k,g7]))
  i.g8 = ((is[k,g8]))
  i.g9 = ((is[k,g9]))
  i.g10 = ((is[k,g10]))
  i.g11 = (sum(is[k,g11]))
  i.g12 = (sum(is[k,g12]))

  vec.g = c(m.g1,
            m.g2,
            m.g3,
            m.g4,
            m.g5,
            m.g6,
            m.g7,
            m.g8,
            m.g9,
            m.g10,
            m.g11,
            m.g12)

  vec.gi = c(i.g1,
             i.g2,
             i.g3,
             i.g4,
             i.g5,
             i.g6,
             i.g7,
             i.g8,
             i.g9,
             i.g10,
             i.g11,
             i.g12)

  names.g = paste("g",1:12,sep="")

  m.sort = sort(abs(vec.g),decreasing = TRUE, index.return=TRUE)
  i.sort = sort(abs(vec.gi),decreasing = TRUE, index.return=TRUE)

  # op <- par(mar = c(5,5,4,2) + 0.1)
  # pdf(file = paste0(folder,"Real_data_exmpale_ranking_ex_",j,".pdf"),width = 9,height=10)
  # plot(c(m.intercept,cumsum(vec.g[m.sort$ix])+m.intercept),
  #      ylim=c(0,max(c(m.intercept,cumsum(vec.g[m.sort$ix])+m.intercept))+0.05),
  #      axes=FALSE,xlim=c(0,13),cex=1.5,pch=15,cex.lab=1.5,xlab="group ranking",
  #      main=paste0("Example 1: Probability of default = ",round(m.intercept + sum(vec.g),3)),cex.main=1.5,
  #      ylab="")
  # title(ylab="probability", cex.lab = 1.5,line = 3.7)
  # points(2:13,c(cumsum(vec.gi[i.sort$ix])+i.intercept),pch=1,cex=1.8)
  # axis(2,las=1,cex.axis=1.5)
  # axis(1,2:13,lab=c(1:12),cex.axis=1.5)
  # text(1,m.intercept-0.03,"intercept",cex=1.5)
  # text(2:13,c(pmin(cumsum(vec.g[m.sort$ix])+m.intercept,cumsum(vec.gi[i.sort$ix])+i.intercept))-0.03,c(names.g[m.sort$ix]),
  #      cex=1.5,col=c(rainbow(13,start=0.2)[m.sort$ix+1]),font=1)
  # text(2:13,c(pmax(cumsum(vec.g[m.sort$ix])+m.intercept,cumsum(vec.gi[i.sort$ix])+i.intercept))+0.03,c(names.g[i.sort$ix]),
  #      cex=1.5,col=rainbow(13,start=0.2)[i.sort$ix+1],font=3)
  # dev.off()
  g.sum = cumsum(vec.g[m.sort$ix])+m.intercept
  i.sum = cumsum(vec.gi[i.sort$ix])+i.intercept


  #### Horizontal Barplot by MJ ####

  library(data.table)
  library(ggplot2)
  library(ggrepel)

  dt <- data.table::data.table(rbind(vec.g,vec.gi))
  colnames(dt) = paste0("g",1:(ncol(dt)))
  setcolorder(dt,ncol(dt):1)
  dt[,method:=c("empirical-0.1+Gaussian","original")]
  setcolorder(dt,neworder="method")
  melt_dt <- melt(dt,id.vars = "method",variable.name = "group",value.name = "Shapley_value")
  melt_dt[,rank:=frank(-abs(Shapley_value)),by=method]
  setorder(melt_dt,rank)
  melt_dt[,cumsum_no_intercept:=cumsum(Shapley_value),by=method]
  melt_dt[,cumsum_with_intercept:=cumsum_no_intercept+m.intercept,by=method]

  intercept_dt <-  data.table(method=c("empirical-0.1+Gaussian","original"),
                              group="intercept",
                              cumsum_with_intercept=m.intercept,
                              rank=0)
  melt_dt <- rbind(melt_dt,intercept_dt,fill=T)

  ylim.max <- max(melt_dt$cumsum_with_intercept)+0.1
  intercept = melt_dt[group=="intercept",cumsum_with_intercept][1]

  library(grid)
  gg1 <- ggplot(data=melt_dt[group!="intercept",], aes(x=rank,y=cumsum_with_intercept,shape=factor(method))) +
    scale_shape_manual(values=c(15,0))+
    geom_point(size=3,stroke=1)+
    geom_point(aes(x=0, y=0.1),size=4,color="black") +
    geom_label_repel(aes(label = group,fill = factor(group),color=factor(method)),
#                     color = 'black',
                     box.padding=0.5,
                     point.padding=0.5,
                     size = 3.5,
                     show.legend=F,
                     force=2,
                     direction = "both",
                     ylim = c(0,ylim.max),
                     segment.color = "black"
    ) +
    ylim(0,ylim.max)+
    guides(color=FALSE,fill=FALSE) +
    labs(shape="Method",y="Probability",x="Group ranking") +
    scale_x_continuous(breaks=1:12,limits=c(0,12),minor_breaks=NULL) +
    geom_label(aes(label="Intercept",x=0.6,y=intercept-0.05),color="black") +
    scale_color_manual(values = rep(c("black","white"),each=1)) +
        #theme(legend.position = "bottom") +
    ggtitle(paste0("Example ",j,": Probability of default = ",round(m.intercept + sum(vec.g),3))) +
    guides(fill=FALSE,shape=FALSE)

  #
  # geom_label_repel(aes(label = "Intercept",x=0,y=intercept),
  #                 color="black",
  #                 size = 3.5,
  #                 inherit.aes = F)
  #
  #
  # theme(legend.position = "bottom")

  cols <- c("empirical-0.1+Gaussian" = "black", "original" = "white")
  gg2 <- ggplot(data=melt_dt[group!="intercept",], aes(x=group,y=Shapley_value,fill=factor(method))) +
    geom_bar(position=position_dodge2(reverse = T,padding = 0),stat="identity",color="black",width=0.8) +
    coord_flip() +
    scale_fill_manual(values=cols) +
    labs(fill = "Method",y="Shapley value",x="Group") +
    #  ylim(min(melt_dt$Shapley_value)-0.05,max(melt_dt$Shapley_value)) +
    scale_y_continuous(breaks = scales::extended_breaks(8),limits = c(min(melt_dt$Shapley_value)-0.02,max(melt_dt$Shapley_value))) +
   # guides(fill=FALSE)
   theme(legend.position = "bottom")

  library(gridExtra)

  pdf(file = paste0(folder,"Real_data_example_comb_",j,".pdf"),width = width,height=height)
  fig <- grid.arrange(gg1,gg2,ncol=1)
  dev.off()
  #fig
  print(j)
}

#ggsave(filename = paste0(folder,"Real_data_example_comb_",k,".pdf"),width = 7,height=10)



