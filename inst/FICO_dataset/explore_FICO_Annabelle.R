library(xgboost)
library(shapr)
library(data.table)
library(MASS)
library(lqmm) ## to check if Sigma is positive definite
library(rapportools) # for testing booleans
library(ggplot2)
library(xtable)
library(reshape2)

load(file = "/nr/project/stat/BigInsight/Projects/Explanations/Data/FICO_explanations_cv_regular_ctree_2.RData")
ctree_dt <- copy(explanation_cv_regular)
load(file = "/nr/project/stat/BigInsight/Projects/Explanations/Data/FICO_explanations_cv_regular_ctree.RData")
ctree_dt2 <- copy(explanation_cv_regular)

load(file = "/nr/project/stat/BigInsight/Projects/Explanations/Data/FICO_explanations_cv_regular_indep_demo123.RData")
ind_dt <- copy(explanation_cv_regular_ind)


# load(file = "/nr/project/stat/BigInsight/Projects/Explanations/Data/FICO_explanations_cv_monotone_indep_2.RData")
# ind_dt <- copy(explanation_cv_monotone_ind)
# load(file = "/nr/project/stat/BigInsight/Projects/Explanations/Data/FICO_explanations_cv_monotone_indep.RData")
# ind_dt2 <- copy(explanation_cv_monotone_ind)
# data <- cbind(c(paste0("ctree", 1:3), paste0("ind", 1:3)), rbind(ctree_dt$dt[2,], ctree_dt2$dt[1,], ctree_dt$dt[3,],
#                                                                  ind_dt$dt[2,], ind_dt2$dt[1,], ind_dt$dt[3,]))
data <- cbind(c(paste0("ctree", 1:3), paste0("ind", 1:3)), rbind(ctree_dt$dt[2,], ctree_dt2$dt[1,], ctree_dt$dt[3,],
                                                                 ind_dt$dt))



# OLD
# load(file = "/nr/project/stat/BigInsight/Projects/Explanations/Data/FICO_explanations_cv_monotone_ctree.RData")
# ctree_dt <- copy(explanation_cv_monotone)
# load(file = "/nr/project/stat/BigInsight/Projects/Explanations/Data/FICO_explanations_cv_monotone_indep.RData")
# ind_dt <- copy(explanation_cv_monotone_ind)
# data <- cbind(c(paste0("ctree", 1:3), paste0("ind", 1:3)), rbind(ctree_dt$dt, ind_dt$dt))



data <- data.table(data)
setnames(data, "V1", "name")
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
View(data7)

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

View(data7)

## Now with  Duke's data
value.Duke1 <- c(1.305, 1.947, 1.686, 0.217, 1.973, 0.772, 1.733, 1.203, 0.495, 0.157)
value.Duke2 <- c(1.305, 1.291, 1.175, 0.161, 1.973, 0.659, 1.913, 1.507, 0.495, 0.157)
value.Duke3 <- c(0.319, 0.866, 1.032, 0.137, 0.946, 0.524, 0.512, 0.586, 0.495, 0.112)

data8 <- cbind(data7, value.Duke1)
data9 <- cbind(data8, value.Duke2)
data10 <- cbind(data9, value.Duke3)

cols1 <- c("value.Duke1", "value.Duke2")
cols2 <- c("value.Duke3")

data11 <- data10[, lapply(.SD, function(x) -1 * x), .SDcols = cols1]
data11_2 <- data11[, lapply(.SD, rank), .SDcols = cols1]
names(data11_2) <- c("Duke1-rank", "Duke2-rank")

data11_1 <- data10[, lapply(.SD, rank), .SDcols = cols2]
names(data11_1) <- c("Duke3-rank")

data12 <- cbind(data10, data11_2, data11_1)

setcolorder(data12, c("V1",
                     "value.ctree1", "ctree1-rank", "value.ind1", "ind1-rank", "value.Duke1", "Duke1-rank",
                     "value.ctree2", "ctree2-rank", "value.ind2", "ind2-rank", "value.Duke2", "Duke2-rank",
                     "value.ctree3", "ctree3-rank", "value.ind3", "ind3-rank", "value.Duke3", "Duke3-rank"))

View(data12)

cols <- c("V1",
          "ctree1-rank",  "ind1-rank",  "Duke1-rank",
          "ctree2-rank", "ind2-rank",  "Duke2-rank",
          "ctree3-rank", "ind3-rank", "Duke3-rank")




data13 <- data12[, ..cols]

print(xtable(data13, digits = 0), include.rownames = FALSE)



## Now with  Duke's data - but changing the third points estimate
value.Duke1 <- c(1.305, 1.947, 1.686, 0.217, 1.973, 0.772, 1.733, 1.203, 0.495, 0.157)
value.Duke2 <- c(1.305, 1.291, 1.175, 0.161, 1.973, 0.659, 1.913, 1.507, 0.495, 0.157)


risk3 <- c(0.2, 0.351, 0.454, 0.382, 0.383, 0.446, 0.171, 0.312, 0.442, 0.524)
weights3 <- c(1.593, 2.468, 2.273, 0.358, 2.470, 1.175, 2.994, 1.877, 1.119, 0.214)
value.Duke3 <- (1/risk3) * weights3



data8 <- cbind(data7, value.Duke1)
data9 <- cbind(data8, value.Duke2)
data10 <- cbind(data9, value.Duke3)

cols1 <- c("value.Duke1", "value.Duke2")
cols2 <- c("value.Duke3")

data11 <- data10[, lapply(.SD, function(x) -1 * x), .SDcols = cols1]
data11_2 <- data11[, lapply(.SD, rank), .SDcols = cols1]
names(data11_2) <- c("Duke1-rank", "Duke2-rank")

data11_1 <- data.table(value.Duke3)
data11_12 <- data11_1[, lapply(.SD, function(x) -1 * x), .SDcols = cols2]
data11_13 <- data11_12[, lapply(.SD, rank), .SDcols = cols2]
names(data11_13) <- c("Duke3-rank")

data12 <- cbind(data10, data11_2, data11_13)

setcolorder(data12, c("V1",
                      "value.ctree1", "ctree1-rank", "value.ind1", "ind1-rank", "value.Duke1", "Duke1-rank",
                      "value.ctree2", "ctree2-rank", "value.ind2", "ind2-rank", "value.Duke2", "Duke2-rank",
                      "value.ctree3", "ctree3-rank", "value.ind3", "ind3-rank", "value.Duke3", "Duke3-rank"))




cols <- c("V1",
           "ctree1-rank",  "ind1-rank",  "Duke1-rank",
           "ctree2-rank", "ind2-rank",  "Duke2-rank",
          "ctree3-rank", "ind3-rank", "Duke3-rank")




data13 <- data12[, ..cols]

print(xtable(data13, digits = 0), include.rownames = FALSE)

