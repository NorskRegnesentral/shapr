library(xgboost)
library(shapr)
library(data.table)
library(MASS)
library(lqmm) ## to check if Sigma is positive definite
library(rapportools) # for testing booleans
library(ggplot2)


## ------------- some functions ----------------------
check_for_cont <- function(col_ind, data){
  return(!is.factor(data[, col_ind]))
}

check_for_col_NA <- function(col_ind, data){
  sum_NA <- sum(is.na(data[, col_ind]))
  if(sum_NA > 0) return(FALSE)
  else return(TRUE)
}

check_for_row_NA <- function(row_ind, data){
  sum_NA <- sum(is.na(data[row_ind, ]))
  if(sum_NA > 0) return(FALSE)
  else return(TRUE)
}

check_for_NA2 <- function(X){
  # ifelse(X == -7, NA, ifelse(X == -8, NA, ifelse(X == -9, NA, X)))
  ifelse(X == -9, NA, X)
}

find_id <- function(data, Value1, Value2, Value3, Value4){
  data[ExternalRiskEstimate == Value1 & MSinceOldestTradeOpen == Value2 & MSinceMostRecentTradeOpen == Value3 & AverageMInFile == Value4, "Id"]

}

find_row <- function(data, Value1, Value2, Value3, Value4){
  data[ExternalRiskEstimate == Value1 & MSinceOldestTradeOpen == Value2 & MSinceMostRecentTradeOpen == Value3 & AverageMInFile == Value4]

}
## -----------------------

data <- read.table(file = "/nr/project/stat/BigInsight/Projects/Explanations/Data/FICO_HELOC_dataset_v1.csv", sep = ",", header = TRUE, stringsAsFactors = TRUE )
data <- data.table(data)
nrow(data) # 10459
data[, Id := 1:nrow(data)]

demo_id_1 = find_id(data, Value1 = 61, Value2 = 49, Value3 = 19, Value4 = 29)
demo_id_2 = find_id(data, Value1 = 59, Value2 = 131, Value3 = 7, Value4 = 81)
demo_id_3 = find_id(data, Value1 = 92, Value2 = 372, Value3 = 10, Value4 = 176)

test_ids = unlist(c(demo_id_1,demo_id_2,demo_id_3,491,1017,4806,3770,5624))
test_preds_duke = c(0.952,0.895,0.049,0.888,0.594,0.696,0.332,0.241)

test_data0 = data.table(Id =test_ids,pred_duke=test_preds_duke)
setkey(test_data0)



data0 <- copy(data)
cols <- colnames(data0)
data0 <- data0[, RiskPerformance := NULL][ , (cols) := lapply(.SD, FUN = check_for_NA2), .SDcols = cols]

data2 <- data[complete.cases(cbind(data[,"RiskPerformance"], data0)), ]

dim(data0)[1] - dim(data2)[1] # 598 have -9 everywhere

data2[, MaxDelqEver := as.factor(MaxDelqEver)]
data2[, MaxDelq2PublicRecLast12M := as.factor(MaxDelq2PublicRecLast12M)]

cat_var <- c("MaxDelq2PublicRecLast12M","MaxDelqEver")
all_var <- c("ExternalRiskEstimate",
             "MSinceOldestTradeOpen", "MSinceMostRecentTradeOpen", "AverageMInFile",
             "NumSatisfactoryTrades",
             "NumTrades60Ever2DerogPubRec", "NumTrades90Ever2DerogPubRec","NumTotalTrades","NumTradesOpeninLast12M",
             "PercentTradesNeverDelq", "MSinceMostRecentDelq","MaxDelq2PublicRecLast12M", "MaxDelqEver",
             "PercentInstallTrades", "NetFractionInstallBurden","NumInstallTradesWBalance",
             "MSinceMostRecentInqexcl7days", "NumInqLast6M", "NumInqLast6Mexcl7days",
             "NetFractionRevolvingBurden","NumRevolvingTradesWBalance",
             "NumBank2NatlTradesWHighUtilization",
             "PercentTradesWBalance")
cont_var = all_var[!(all_var %in% cat_var)]

groups = c(1,3,1,4,4,3,3,2,1,1)



cont_data = data2[,..all_var]
cont_data[,MaxDelqEver:=as.numeric(MaxDelqEver)]
cont_data[,MaxDelq2PublicRecLast12M:=as.numeric(MaxDelq2PublicRecLast12M)]
cont_data[,MaxDelqEver:=NA]
cont_data[,MaxDelq2PublicRecLast12M:=NA]


cat_data = data2[,..cat_var]



cor_cont = cor(cont_data)


# Using cramersV for the only categorical features
library(questionr)
cat_tab = table(cat_data)
cat_corr0 = cramer.v(cat_tab)
#cor_cat = matrix(c(1,cat_corr0,cat_corr0,1),ncol=2)

#cor_cont[which(all_var %in% cat_var),which(all_var %in% cat_var)] = cor_cat

#cor_all = cor_cont


library(corrplot)
pdf(file="/nr/project/stat/BigInsight/Projects/Explanations/Data/correlation_figure.pdf",width = 9,height=9)
corrplot(cor_all,diag=F)
corrRect(groups,col=3,lwd=2)
dev.off()

# May use correlation ratio for dependence between cont and cat.
#install.packages("DiscriMiner")
library(DiscriMiner)
#corRatio(variable, group)

all_data = data2[,..all_var]

corcatvec_1 = corcatvec_2 = rep(NA,length(all_var))
for (i in 1:ncol(all_data)){
  cont = unlist(all_data[,..i])
  if(is.factor(cont)){
    corcatvec_1[i] = NA
  } else {
    corcatvec_1[i] = corRatio(cont,unlist(cat_data[,1]))
  }
}
for (i in 1:ncol(all_data)){
  cont = unlist(all_data[,..i])
  if(is.factor(cont)){
    corcatvec_2[i] = NA
  } else {
    corcatvec_2[i] = corRatio(cont,unlist(cat_data[,2]))
  }
}


cor_catcont = cbind(corcatvec_1,corcatvec_2)
cor_catcont[12,] = c(1,cat_corr0)
cor_catcont[13,] = c(cat_corr0,1)

cor_all = cor_cont
cor_all[,12:13] = cor_catcont
cor_all[12:13,] = t(cor_catcont)

eps = 0.5
library(corrplot)
pdf(file="/nr/project/stat/BigInsight/Projects/Explanations/Data/correlation_figure_new.pdf",width = 9,height=9)
corrplot(cor_all,diag=F)
corrRect(groups,col=3,lwd=3)
rect(1-eps,11-eps,ncol(cor_all)+1-eps,13-eps,col=NA,border=6,lwd=1.8)
rect(12-eps,1-eps,14-eps,ncol(cor_all)+1-eps,col=NA,border=6,lwd=1.8)
dev.off()
