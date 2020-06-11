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
#
load(file = "/nr/project/stat/BigInsight/Projects/Explanations/Data/FICO_explanations_cv_regular_indep_demo123.RData")
ind_dt <- copy(explanation_cv_regular_ind)


data <- cbind(c(1:3, 1:3),   c("ctree", "ind"), rbind(ctree_dt$dt[2,], ctree_dt2$dt[1,], ctree_dt$dt[3,],
                                                                 ind_dt$dt))

data <- data.table(data)
setnames(data, c("V1", "V2"), c("id", "name"))
data2 <- melt(data, id.vars = c("id", "name"))
data3 <- dcast(data2, id+variable~name, value.var = 'value')

ggplot(data = data3, aes(x = ctree, y = ind)) + geom_point() + facet_wrap(~ variable)

#print(xtable(data13, digits = 0), include.rownames = FALSE)

# new data as of June 10th

load("/nr/project/stat/BigInsight/Projects/Explanations/Data/FICO_explanations_cv_regular_ctree_100testobs_chunk_1.RData")
ctree_dt_1 <- copy(explanation_cv_regular)
load("/nr/project/stat/BigInsight/Projects/Explanations/Data/FICO_explanations_cv_regular_ctree_100testobs_chunk_2.RData")
ctree_dt_2 <- copy(explanation_cv_regular)


load("/nr/project/stat/BigInsight/Projects/Explanations/Data/FICO_explanations_cv_regular_indep_100testobs_chunk_1.RData")
ind_dt_1 <- copy(explanation_cv_regular_ind)
load("/nr/project/stat/BigInsight/Projects/Explanations/Data/FICO_explanations_cv_regular_indep_100testobs_chunk_2.RData")
ind_dt_2 <- copy(explanation_cv_regular_ind)

num_chunks = 2

data <- cbind(c(1:(10*num_chunks), 1:(10*num_chunks)), c(rep("ctree", (10*num_chunks)), rep("ind", (10*num_chunks))),
              rbind(ctree_dt_1$dt, ctree_dt_2$dt, ind_dt_1$dt, ind_dt_2$dt))

data <- data.table(data)
setnames(data, c("V1", "V2"), c("id", "name"))
data2 <- melt(data, id.vars = c("id", "name"))

data2_without_none <-  data2[variable != 'none']

data3 <- dcast(data2_without_none, id+variable~name, value.var = 'value')



ggplot(data = data3, aes(x = ctree, y = ind)) + geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  #coord_fixed() +
  #geom_smooth(aes(group=variable), method="lm", se=FALSE) +
  facet_wrap(~ variable)  # scales = "free"
  #theme(aspect.ratio=1)


