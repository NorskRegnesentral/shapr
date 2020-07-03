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

ggsave(
  '/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/figures/FICO/all_features.png',
  plot = last_plot(),
  device = 'png',
  units = "cm",
  dpi = 300,
  width = 50,
  height = 50,
  limitsize = TRUE,
)


#NumInqLast6Mexcl7days
#PercentTradesWBalance
x = sample(1:100, 3)
print(x)
# 39 different for NumTrades90Ever2DerogPubRec
# 86 different for NumTrades90Ever2DerogPubRec and  ExternalRiskEstimate

data4 <- data.table(data3)[(variable == 'MaxDelq2PublicRecLast12M') | (variable == 'ExternalRiskEstimate') | (variable == 'MSinceMostRecentInqexcl7days') | (variable == 'NumTrades90Ever2DerogPubRec') ,]
# data4[, col := ifelse(id == x[1], 1, ifelse(id == x[2], 2, ifelse(id == x[3], 3, 4)))]

# FINAL
data4[, col := ifelse(id == 63, 1, ifelse(id == 60, 2, ifelse(id == 86, 3, 4)))]

## FREE AXES
# ggplot() + geom_point(data = data4[col == 1], aes(x = ctree, y = ind), color = "red", size = 2) +
#   geom_point(data = data4[col == 2], aes(x = ctree, y = ind), color = "green", size = 2) +
#   geom_point(data = data4[col == 3], aes(x = ctree, y = ind), color = "blue", size = 2) +
#   geom_point(data = data4[col == 4], aes(x = ctree, y = ind), alpha = 0.1) +
#   geom_abline(intercept = 0, slope = 1, col = 'red') +
#   facet_wrap(~ variable, scales = "free") +
#   xlab('ctree') + ylab('independence') + theme_bw() + scale_color_manual(values = c("red", "green", "blue", "black")) +
#   theme(legend.title = element_blank())


data4[, variable := droplevels(variable)]
factor(data4$variable, levels = c( "MaxDelq2PublicRecLast12M", "ExternalRiskEstimate","MSinceMostRecentInqexcl7days","NumTrades90Ever2DerogPubRec"))
data4[, variable2 := factor(data4$variable, levels = c( "MaxDelq2PublicRecLast12M", "ExternalRiskEstimate","MSinceMostRecentInqexcl7days","NumTrades90Ever2DerogPubRec"))]

ggplot() +
  geom_point(data = data4[col == 4], aes(x = ctree, y = ind), alpha = 0.1) +
  geom_point(data = data4[col == 1], aes(x = ctree, y = ind), color = "red", size = 2) +
  geom_point(data = data4[col == 2], aes(x = ctree, y = ind), color = "green", size = 2) +
  geom_point(data = data4[col == 3], aes(x = ctree, y = ind), color = "blue", size = 2) +
  geom_abline(intercept = 0, slope = 1, col = 'red') +
  facet_wrap(~ variable2) +
  xlab('Shapley values estimated using ctree') +
  ylab('Shapley values estimated using independence') +
  theme_bw() + scale_color_manual(values = c("red", "green", "blue", "black")) +
  theme(legend.title = element_blank(),
        strip.text = element_text(size = 8))


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

ggsave(
  '/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/ctree-paper/CD_make_paper/Final submission Redelmeier/figures/four_features.pdf',
  plot = last_plot(),
  device = 'pdf',
  units = "cm",
  width = 11,
  height = 11,
  dpi = 300,
  limitsize = TRUE,
)

