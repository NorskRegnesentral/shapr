library(xgboost)
library(shapr)
library(data.table)
library(MASS)
library(lqmm) ## to check if Sigma is positive definite
library(rapportools) # for testing booleans
library(ggplot2)
library(xtable)
library(reshape2)

load(file = "/nr/project/stat/BigInsight/Projects/Explanations/Data/FICO_explanations_cv_monotone_ctree.RData")
ctree_dt <- copy(explanation_cv_monotone)


load(file = "/nr/project/stat/BigInsight/Projects/Explanations/Data/FICO_explanations_cv_monotone_indep.RData")
ind_dt <- copy(explanation_cv_monotone_ind)


data <- cbind(c(paste0("ctree", 1:3), paste0("ind", 1:3)), rbind(ctree_dt$dt, ind_dt$dt))
data <- data.table(data)
setnames(data, "V1", "name")

data2 <- melt(data, id.vars = c("name"))
data3 <- reshape(data2, idvar = "variable", timevar = "name", direction = "wide")
View(data3)
setcolorder(data3, c("variable", "value.ctree1", "value.ind1", "value.ctree2", "value.ind2", "value.ctree3", "value.ind3"))

cols <- names(data3)[-1]
data4 <- data3[, lapply(.SD, function(x) -1 * abs(x)), .SDcols = cols]
data5 <- cbind(data3$variable, data4)
data6 <- data5[, lapply(.SD, rank), .SDcols = cols]
colnames(data6) <- c("ctree1-rank", "ind1-rank", "ctree2-rank", "ind2-rank", "ctree3-rank", "ind3-rank")
data7 <- cbind(data5, data6)
setcolorder(data7, c("V1",
                     "value.ctree1", "ctree1-rank", "value.ind1", "ind1-rank",
                     "value.ctree2", "ctree2-rank", "value.ind2", "ind2-rank",
                     "value.ctree3", "ctree3-rank", "value.ind3", "ind3-rank"))



## Now grouping
data[, ExternalRiskEstimate := ExternalRiskEstimate]
data[, TradeOpenTime := MSinceOldestTradeOpen + MSinceMostRecentTradeOpen + AverageMInFile]
data[, NumSatisfactoryTrades := NumSatisfactoryTrades]
data[, TradeFrequency := NumTrades60Ever2DerogPubRec + NumTrades90Ever2DerogPubRec + NumTotalTrades +
             NumTradesOpeninLast12M]
data[, Delinquency := PercentTradesNeverDelq + MSinceMostRecentDelq + MaxDelq2PublicRecLast12M +
             MaxDelqEver]
data[, Installment := PercentInstallTrades + NetFractionInstallBurden + NumInstallTradesWBalance]
data[, Inquiry := MSinceMostRecentInqexcl7days + NumInqLast6M + NumInqLast6Mexcl7days]
data[, RevolvingBalance := NetFractionRevolvingBurden + NumRevolvingTradesWBalance]
data[, Utilization := NumBank2NatlTradesWHighUtilization]
data[, TradeWBalance := PercentTradesWBalance]

feature_groups <- c("ExternalRiskEstimate", "TradeOpenTime",  "NumSatisfactoryTrades", "TradeFrequency",
                    "Delinquency", "Installment", "Inquiry", "RevolvingBalance", "Utilization", "TradeWBalance")
data <- data[, ..feature_groups]

data[, name := c(paste0("ctree", 1:3), paste0("ind", 1:3))]

data2 <- melt(data, id.vars = c("name"))
data3 <- reshape(data2, idvar = "variable", timevar = "name", direction = "wide")
setcolorder(data3, c("variable", "value.ctree1", "value.ind1", "value.ctree2", "value.ind2", "value.ctree3", "value.ind3"))

cols <- names(data3)[-1]
data4 <- data3[, lapply(.SD, function(x) -1 * abs(x)), .SDcols = cols]
data5 <- cbind(data3$variable, data4)
data6 <- data5[, lapply(.SD, rank), .SDcols = cols]
colnames(data6) <- c("ctree1-rank", "ind1-rank", "ctree2-rank", "ind2-rank", "ctree3-rank", "ind3-rank")
data7 <- cbind(data5, data6)
setcolorder(data7, c("V1",
                     "value.ctree1", "ctree1-rank", "value.ind1", "ind1-rank",
                     "value.ctree2", "ctree2-rank", "value.ind2", "ind2-rank",
                     "value.ctree3", "ctree3-rank", "value.ind3", "ind3-rank"))

## Now with  Duke's data
value.Duke1 <- c(1.305, 1.291, 1.175, 0.161, 1.973, 0.659, 1.913, 1.507, 0.495, 0.157)
value.Duke2 <- c(1.074, 0.409, 1.175, 0.201, 1.667, 0.745, 2.115, 1.171, 0.495, 0.157)
value.Duke3 <- c(1.074, 0.165, 1.032, 0.137, 1.341, 0.609, 1.733, 1.088, 0.724, 0.130)

data8 <- cbind(data7, value.Duke1)
data9 <- cbind(data8, value.Duke2)
data10 <- cbind(data9, value.Duke3)

cols <- c("value.Duke1", "value.Duke2", "value.Duke3")

data11 <- data10[, lapply(.SD, function(x) -1 * x), .SDcols = cols]
data11_2 <- data11[, lapply(.SD, rank), .SDcols = cols]
names(data11_2) <- c("Duke1-rank", "Duke2-rank", "Duke3-rank")
data12 <- cbind(data10, data11_2)

setcolorder(data12, c("V1",
                     "value.ctree1", "ctree1-rank", "value.ind1", "ind1-rank", "value.Duke1", "Duke1-rank",
                     "value.ctree2", "ctree2-rank", "value.ind2", "ind2-rank", "value.Duke2", "Duke2-rank",
                     "value.ctree3", "ctree3-rank", "value.ind3", "ind3-rank", "value.Duke3", "Duke3-rank"))
View(data12)
