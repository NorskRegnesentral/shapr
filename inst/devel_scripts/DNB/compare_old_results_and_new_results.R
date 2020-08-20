library(shapr)
library(data.table)
library(mvtnorm)
library(condMVNorm)
library(stringi)
library(xgboost)
library(GIGrvg)
library(ghyp)
library(gridExtra)
library(ggplot2)
library(ggrepel)
library(grid)

source.local <- ifelse(exists("source.local"),source.local,FALSE)
these_run_ind = 62:75
this.seed <- 12345

# source("paper_scripts/paper_helper_funcs.R", local = source.local) # Helper functions these experiments (mainly computing the true Shapley values)
# source("paper_experiments/source_specifying_seed_and_filenames.R", local = source.local) # Setting random or fixed seed and filenames.

#### Loading data ####
XYtrain <- fread("/nr/project/stat/BFFGB18/LIME/lime/R/train6.csv")
XYtest <- fread("/nr/project/stat/BFFGB18/LIME/lime/R/test6.csv")

XYtrain[, V1 := NULL]
XYtest[, V1 := NULL]

setnames(XYtrain, "default", "y")
setnames(XYtest, "default", "y")

# Testing, reducing the dimension of the data
XYtest <- XYtest
XYtrain <- XYtrain

nTrain <- nrow(XYtrain)
nTest <- nrow(XYtest)

Xtrain <- copy(XYtrain)
Xtest <- copy(XYtest)

Xtrain[, y := NULL]
Xtest[,y := NULL]

xgb.train <- xgb.DMatrix(data = as.matrix(XYtrain[, -"y"]),
                         label = XYtrain[, y])
xgb.test <- xgb.DMatrix(data = as.matrix(XYtest[, -"y"]),
                        label = XYtest[, y])

params <- list(eta = 0.1,
               objective = "binary:logistic",
               eval_metric = "auc",
               tree_method="hist")


tt <- proc.time()
model <- xgb.train(data = xgb.train,
                   params = params,
                   nrounds = 50,
                   print_every_n = 10,
                   ntread = 5,
                   watchlist = list(train = xgb.train,
                                    test = xgb.test),
                   verbose = 1)
print(proc.time() -tt)

d <- 1:nrow(Xtest)
run_list <- split(d, ceiling(seq_along(d)/21))

current.Xtest <- NULL
for(run_ind in 1:92){
  set.seed(this.seed)
  current.Xtest <- rbind(current.Xtest, copy(Xtest[run_list[[run_ind]], ]))
}

x_test_from_paper <- current.Xtest[c(1,29), ]


explainer <- shapr(Xtrain, model, n_combinations = 10000)
pred_zero = mean(XYtrain$y)

tt <- proc.time()
explanation_emp_0.1_gauss <- explain(x_test_from_paper,
                                     explainer,
                                     approach = c(rep("empirical", 3),
                                                  rep("gaussian", dim(x_test_from_paper)[2] - 3)),
                                     prediction_zero = pred_zero)
print(proc.time() - tt)

tt <- proc.time()
explanation_ind <- explain(x_test_from_paper,
                           explainer,
                           approach = "empirical",
                           type = "independence",
                           prediction_zero = pred_zero)
print(proc.time() - tt)

# First compare the individual Shapley values
## Read Shapley results
sr = read.table("/nr/project/stat/BigInsight/Projects/Explanations/Kode/Shapley_Anders/all_results_experiment_Real_dim_28_Unknown_XGBoost_Unknown_newnames.csv",
                header = TRUE, sep = ",")
sort.c = sort(sr[(sr[,"Method"] == "comb_Gaussian_sigma.01"), 2], index.return = TRUE)
sort.i = sort(sr[(sr[,"Method"] == "empirical_independence"), 2], index.return = TRUE)


## EMPIRICAL + GAUSSIAN
cs = sr[(sr[, "Method"] == "comb_Gaussian_sigma.01"), 3:31][sort.c$ix[1:1100], ]
cs[c(1, 29), 1:11]
cs_dt <- data.table(cs)
cs_dt_1 <- cs_dt[c(1)]
cs_dt_29 <- cs_dt[c(29)]

cs_dt_1[,id := "old1"][, intercept := NULL]
cs_dt_29[,id := "old29"][, intercept := NULL]

melt_cs_1 <- melt(cs_dt_1, id.vars = "id", variable.name = "feature", value.name = "Shapley_value")
melt_cs_29 <- melt(cs_dt_29, id.vars = "id", variable.name = "feature", value.name = "Shapley_value")

cs_new <- data.table(explanation_emp_0.1_gauss$dt)
cs_new_1 <- cs_new[c(1)]
cs_new_1[, id := "new1"][, none := NULL]
cs_new_29 <- cs_new[c(2)]
cs_new_29[, id :="new29"][, none := NULL]


melt_cs_new_1 <- melt(cs_new_1, id.vars = "id", variable.name = "feature", value.name = "Shapley_value")
melt_cs_new_29 <- melt(cs_new_29, id.vars = "id", variable.name = "feature", value.name = "Shapley_value")

all_Shapley_values_1 <- rbind(melt_cs_1, melt_cs_new_1)

cols <- c("old1" = "black", "new1" = "white")
gg1 <- ggplot(data = all_Shapley_values_1, aes(x = feature, y = Shapley_value, fill = factor(id))) +
  geom_bar(position=position_dodge2(reverse = T, padding = 0),
           stat = "identity",
           color = "black",
           width = 0.8) +
  coord_flip() +
  scale_fill_manual(values = cols) +
  labs(fill = "Method", y = "Shapley value", x = "Feature", title = "Emp+Gaussian, id = 1") +
  scale_y_continuous(breaks = scales::extended_breaks(8),
                     limits = c(min(all_Shapley_values_1$Shapley_value)-0.02, max(all_Shapley_values_1$Shapley_value))) +
  theme(legend.position = "bottom")


all_Shapley_values_29 <- rbind(melt_cs_29, melt_cs_new_29)

cols <- c("old29" = "black", "new29" = "white")
gg1 <- ggplot(data = all_Shapley_values_29, aes(x = feature, y = Shapley_value, fill = factor(id))) +
  geom_bar(position=position_dodge2(reverse = T, padding = 0),
           stat = "identity",
           color = "black",
           width = 0.8) +
  coord_flip() +
  scale_fill_manual(values = cols) +
  labs(fill = "Method", y = "Shapley value", x = "Feature", title = "Emp+Gaussian, id = 29") +
  scale_y_continuous(breaks = scales::extended_breaks(8),
                     limits = c(min(all_Shapley_values_29$Shapley_value)-0.02, max(all_Shapley_values_29$Shapley_value))) +
  theme(legend.position = "bottom")


## EMPIRICAL INDEPENDENCE
is = sr[(sr[, "Method"] == "empirical_independence"), 3:31][sort.i$ix[1:1100], ]
is[c(1, 29), 1:11]

is_dt <- data.table(is)
is_dt_1 <- is_dt[c(1)]
is_dt_29 <- is_dt[c(29)]

is_dt_1[,id := "old1"][, intercept := NULL]
is_dt_29[,id := "old29"][, intercept := NULL]

melt_is_1 <- melt(is_dt_1, id.vars = "id", variable.name = "feature", value.name = "Shapley_value")
melt_is_29 <- melt(is_dt_29, id.vars = "id", variable.name = "feature", value.name = "Shapley_value")

is_new <- data.table(explanation_ind$dt)
is_new_1 <- is_new[c(1)]
is_new_1[, id := "new1"][, none := NULL]
is_new_29 <- is_new[c(2)]
is_new_29[, id :="new29"][, none := NULL]


melt_is_new_1 <- melt(is_new_1, id.vars = "id", variable.name = "feature", value.name = "Shapley_value")
melt_is_new_29 <- melt(is_new_29, id.vars = "id", variable.name = "feature", value.name = "Shapley_value")

all_Shapley_values_1 <- rbind(melt_is_1, melt_is_new_1)

cols <- c("old1" = "black", "new1" = "white")
gg2 <- ggplot(data = all_Shapley_values_1, aes(x = feature, y = Shapley_value, fill = factor(id))) +
  geom_bar(position=position_dodge2(reverse = T, padding = 0),
           stat = "identity",
           color = "black",
           width = 0.8) +
  coord_flip() +
  scale_fill_manual(values = cols) +
  labs(fill = "Method", y = "Shapley value", x = "Feature", title = "empirical independence, id = 1") +
  scale_y_continuous(breaks = scales::extended_breaks(8),
                     limits = c(min(all_Shapley_values_1$Shapley_value)-0.02, max(all_Shapley_values_1$Shapley_value))) +
  theme(legend.position = "bottom")



all_Shapley_values_29 <- rbind(melt_cs_29, melt_cs_new_29)

cols <- c("old29" = "black", "new29" = "white")
gg1 <- ggplot(data = all_Shapley_values_29, aes(x = feature, y = Shapley_value, fill = factor(id))) +
  geom_bar(position=position_dodge2(reverse = T, padding = 0),
           stat = "identity",
           color = "black",
           width = 0.8) +
  coord_flip() +
  scale_fill_manual(values = cols) +
  labs(fill = "Method", y = "Shapley value", x = "Feature", title = "empirical independence, id = 29") +
  scale_y_continuous(breaks = scales::extended_breaks(8),
                     limits = c(min(all_Shapley_values_29$Shapley_value)-0.02, max(all_Shapley_values_29$Shapley_value))) +
  theme(legend.position = "bottom")




## Then compare the grouped version

g1 = c("tr_mean_end", "tr_max_end","tr_std_end")
g2 = c("br_std_end", "sum_std_end","inn_mean_end","inn_max_end","inn_std_end")
g3 = c("br_std_mean_correct_end")
g4 = c("sum_std_mean_correct_end")
g5 = c("br_min_end", "br_mean_end","br_max_end")
g6 = c("sum_min_end", "sum_mean_end","sum_max_end")
g7 = c("sk_std_end", "sk_min_end","sk_mean_end","sk_max_end")
g8 = c("inn_std_mean_correct_end")
g9 = c("tr_std_mean_correct_end")
g10 = c("sk_std_mean_correct_end")
g11 = c("kk_min_end", "kk_mean_end","kk_max_end")
g12 = c("kk_std_end", "kk_std_mean_correct_end")

group1_names = list(g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11, g12)

# Prepare the data for explanation
explainer_grouped <- shapr(Xtrain, model, group = group1_names)

explanation_emp_0.1_gauss_grouped <- explain(x_test_from_paper,
                                             explainer_grouped,
                                             approach = c(rep("empirical", 3),
                                                          rep("gaussian", dim(x_test_from_paper)[2] - 3)),
                                             prediction_zero = pred_zero)


explanation_ind_grouped <- explain(x_test_from_paper,
                                   explainer_grouped,
                                   approach = "empirical",
                                   type = "independence",
                                   prediction_zero = pred_zero)

#plot(explanation_ind_grouped)

# method_name = 'empirical-0.1+Gaussian' or 'original
plot_function <- function(method_name, k){

  j <- which(k == c(1, 29))

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

  names.g = paste("group",1:12,sep="")

  m.sort = sort(abs(vec.g),decreasing = TRUE, index.return=TRUE)
  i.sort = sort(abs(vec.gi),decreasing = TRUE, index.return=TRUE)
  g.sum = cumsum(vec.g[m.sort$ix])+m.intercept
  i.sum = cumsum(vec.gi[i.sort$ix])+i.intercept

  dt <- data.table::data.table(rbind(vec.g,vec.gi))
  colnames(dt) = paste0("group",1:(ncol(dt)))
  setcolorder(dt, ncol(dt):1)
  dt[,method:=c("empirical-0.1+Gaussian","original")]
  setcolorder(dt,neworder="method")
  melt_dt <- melt(dt,id.vars = "method", variable.name = "group",value.name = "Shapley_value")
  melt_dt[,rank:=frank(-abs(Shapley_value)),by=method]
  setorder(melt_dt,rank)
  melt_dt[,cumsum_no_intercept:=cumsum(Shapley_value),by=method]
  melt_dt[,cumsum_with_intercept:=cumsum_no_intercept+m.intercept,by=method]

  intercept_dt <-  data.table(method=c("empirical-0.1+Gaussian","original"),
                              group="intercept",
                              cumsum_with_intercept=m.intercept,
                              rank=0)
  melt_dt <- rbind(melt_dt,intercept_dt,fill=T)
  X1 <- copy(melt_dt[method == method_name][group != 'intercept'][order(-group)])
  X1[, id := paste0('old')]

  cols0 <- c("group", "Shapley_value", "id")
  X1_ <- X1[,..cols0]
  X1_ <- setcolorder(X1_, c("id", "group", "Shapley_value"))


  ## new
  if(method_name == 'empirical-0.1+Gaussian'){
    X_new <- data.table(explanation_emp_0.1_gauss_grouped$dt)
  } else{
    X_new <- data.table(explanation_ind_grouped$dt)
  }


  X_new_1 <- X_new[j,]

  X_new_1[, id := paste0("new")][, none := NULL]

  X_new_1 <- copy(melt(X_new_1, id.var = 'id', variable.name = "group", value.name = "Shapley_value"))
  X_new_1[order(id)]

  data1 <- rbind(X1_, X_new_1)

  cols <- c("old" = "black", "new" = "white")
  gg_grouped1 <- ggplot(data=data1, aes(x = group, y = Shapley_value, fill = factor(id))) +
    geom_bar(position=position_dodge2(reverse = T, padding = 0),
             stat = "identity",
             color = "black",
             width = 0.8) +
    coord_flip() +
    scale_fill_manual(values = cols) +
    labs(fill = "Method", y = "Shapley value", x = "Group", title = paste0("method = ", method_name, " ID = ", k)) +
    scale_y_continuous(breaks = scales::extended_breaks(8),
                       limits = c(min(data1$Shapley_value)-0.02, max(data1$Shapley_value))) +
    theme(legend.position = "bottom")

  return(gg_grouped1)

}

gg0 <- plot_function('empirical-0.1+Gaussian', k = 1)
gg0


ggsave(
  "/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/grouped_shapley/emp_gaus_id_1.pdf",
  plot = last_plot(),
  device = "pdf",
  path = NULL,
  scale = 1,
  width = 10,
  height = 10,
  units = "cm",
  dpi = 300,
  limitsize = TRUE
)

gg0 <- plot_function('empirical-0.1+Gaussian', k = 29)
gg0

ggsave(
  "/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/grouped_shapley/emp_gaus_id_29.pdf",
  plot = last_plot(),
  device = "pdf",
  path = NULL,
  scale = 1,
  width = 10,
  height = 10,
  units = "cm",
  dpi = 300,
  limitsize = TRUE
)

gg1 <- plot_function('original', k = 1)
gg1

ggsave(
  "/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/grouped_shapley/emp_ind_id_1.pdf",
  plot = last_plot(),
  device = "pdf",
  path = NULL,
  scale = 1,
  width = 10,
  height = 10,
  units = "cm",
  dpi = 300,
  limitsize = TRUE
)

gg1 <- plot_function('original', k = 29)
gg1

ggsave(
  "/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/grouped_shapley/emp_ind_id_29.pdf",
  plot = last_plot(),
  device = "pdf",
  path = NULL,
  scale = 1,
  width = 10,
  height = 10,
  units = "cm",
  dpi = 300,
  limitsize = TRUE
)

