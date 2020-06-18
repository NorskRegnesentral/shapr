library(xgboost)
library(shapr)
library(data.table)
library(MASS)
library(lqmm) ## to check if Sigma is positive definite
library(rapportools) # for testing booleans
library(ggplot2)
library(xtable)
library(reshape2)

## THIS SCRIPT WAS USED FOR THE SECOND SUBMISSION OF THE ARTICLE
## TO MAKE THE SECOND FIGURE OF SHAPLEY VALUES ESIMATED USING CTREE VS INDEPENDENCE


# new data as of June 16th
dt_ctree <- readRDS("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/data/trees/dt_ctree.rds")
dt_ind <- readRDS("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/data/trees/dt_ind.rds")

data <- cbind(c(1:100, 1:100),
              c(rep("ctree", 100), rep("ind", 100)),
              rbind(dt_ctree, dt_ind))

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


data4 <- data.table(data3)[(variable == 'MaxDelq2PublicRecLast12M') | (variable == 'ExternalRiskEstimate') | (variable == 'MSinceMostRecentInqexcl7days') | (variable == 'NumInqLast6Mexcl7days') ,]
data4[, col := ifelse(id == 14, 1, ifelse(id == 60, 2, ifelse(id == 70, 3, 4)))]

ggplot() + geom_point(data = data4[col == 1], aes(x = ctree, y = ind), color = "red", size = 2) +
  geom_point(data = data4[col == 2], aes(x = ctree, y = ind), color = "green", size = 2) +
  geom_point(data = data4[col == 3], aes(x = ctree, y = ind), color = "blue", size = 2) +
  geom_point(data = data4[col == 4], aes(x = ctree, y = ind), alpha = 0.1) +
  geom_abline(intercept = 0, slope = 1, col = 'red') +
  facet_wrap(~ variable, scales = "free") +
  xlab('ctree') + ylab('independence') + theme_bw() + scale_color_manual(values = c("red", "green", "blue", "black")) +
  theme(legend.title = element_blank())


ggplot() + geom_point(data = data4[col == 1], aes(x = ctree, y = ind), color = "red", size = 2) +
  geom_point(data = data4[col == 2], aes(x = ctree, y = ind), color = "green", size = 2) +
  geom_point(data = data4[col == 3], aes(x = ctree, y = ind), color = "blue", size = 2) +
  geom_point(data = data4[col == 4], aes(x = ctree, y = ind), alpha = 0.1) +
  geom_abline(intercept = 0, slope = 1, col = 'red') +
  facet_wrap(~ variable) +
  xlab('ctree') + ylab('independence') + theme_bw() + scale_color_manual(values = c("red", "green", "blue", "black")) +
  theme(legend.title = element_blank())

ggsave(
  '/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/figures/FICO/four_features.png',
  plot = last_plot(),
  device = 'png',
  units = "cm",
  dpi = 300,
  limitsize = TRUE,
)

ggsave(
  '/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/ctree-paper/figures/four_features.png',
  plot = last_plot(),
  device = 'png',
  units = "cm",
  width = 11,
  height = 11,
  dpi = 300,
  limitsize = TRUE,
)


