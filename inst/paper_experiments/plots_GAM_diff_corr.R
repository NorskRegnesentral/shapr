library(shapr)
library(data.table)
library(ggplot2)

MAD <- function(pre_grouped, post_grouped){
  abs(pre_grouped - post_grouped)
}
MDR <- function(ranking_pre_grouped, ranking_post_grouped){
  abs(ranking_pre_grouped - ranking_post_grouped)
}

groupA_shapley = fread("inst/paper_experiments/results/AR-groupA_Shapley_values_GAM_diff_corr.csv")

# remove any test tries
groupA_shapley = groupA_shapley[No_test_obs == 100]
groupA_shapley[, .N, by = c("correlation", "model_type")][order(model_type)]

all_shapley = fread("inst/paper_experiments/results/AR-groupA-All_Shapley_values_GAM_diff_corr.csv")
all_shapley = all_shapley[No_test_obs == 100]
all_shapley[, .N, by = c("correlation", "model_type")][order(model_type)]

# Group A
groupA <- list(group1 = 1:4,
               group2 = 5:8,
               group3 = 9:10)
groupA = lapply(groupA, function(x){paste0("feat_", 1:10, "_") [x]})
groupA_names = copy(names(groupA))
rank_group_namesA = paste0(groupA_names, "_rank")

results = list()
for(exper in c("AR-groupA-experiment_gam_diff_corr")  ){
  for(corr in c(0, 0.1, 0.3, 0.7, 0.9)){ # , 0.1, 0.3, 0.7, 0.9

    group_exp = groupA_shapley[model_type == exper][correlation == corr]
    group_exp_u = group_exp[, tail(.SD, 1), by = .(test_id, model_type, correlation)]

    all_exp = all_shapley[model_type == exper][correlation == corr]
    all_exp_u = all_exp[, tail(.SD, 1), by = .(test_id, model_type, correlation)]

    explanation_base = data.table(all_exp_u)
    explanation_base[, group1 := rowSums(.SD), .SDcols = groupA[[1]]]
    explanation_base[, group2 := rowSums(.SD), .SDcols = groupA[[2]]]
    explanation_base[, group3 := rowSums(.SD), .SDcols = groupA[[3]]]

    explanation_mat_post = as.matrix(explanation_base[, ..groupA_names])
    explanation_ranking_post = t(apply(-explanation_mat_post, FUN = rank, 1))
    colnames(explanation_ranking_post) = rank_group_namesA
    explanation_base = cbind(explanation_base, explanation_ranking_post)

    pre_grouped = group_exp_u[, ..groupA_names]

    mean_pre_grouped = apply(pre_grouped, 2, mean)
    sd_pre_grouped2 = apply(pre_grouped, 2, sd)
    sd_pre_grouped3 = mean(apply(pre_grouped, 2, sd))

    pre_grouped_stand = sweep(pre_grouped, 2, mean_pre_grouped, "-")
    pre_grouped_stand2 = sweep(pre_grouped_stand, 2, sd_pre_grouped2, "/")
    pre_grouped_stand3 = pre_grouped_stand / sd_pre_grouped3
    #
    pre_grouped_rank = group_exp_u[, ..rank_group_namesA]

    post_grouped = explanation_base[, ..groupA_names]
    post_grouped_stand = sweep(post_grouped, 2, mean_pre_grouped, "-")
    post_grouped_stand2 = sweep(post_grouped_stand, 2, sd_pre_grouped2, "/")
    post_grouped_stand3 = post_grouped_stand / sd_pre_grouped3
    post_grouped_rank = explanation_base[, ..rank_group_namesA]


    MAD0 = apply(MAD(pre_grouped, post_grouped), 1, mean)

    MAD1 = apply(MAD(pre_grouped_stand2, post_grouped_stand2), 1, mean)

    MAD2 = apply(MAD(pre_grouped_stand3, post_grouped_stand3), 1, mean)

    MDR0 = apply(MDR(pre_grouped_rank, post_grouped_rank), 1, mean)

    results[[length(results) + 1]] = data.frame(absolute_difference = MAD1, absolute_difference_rank = MDR0, correlation = corr, experiment = exper)

  }
}

results_all = rbindlist(results)

results_all$experiment = factor(results_all$experiment)
results_all$correlation = factor(results_all$correlation)
results_all$absolute_difference_log = log(results_all$absolute_difference)

results_groupA = results_all


ggplot(results_all, aes(y = absolute_difference, x = correlation, col = experiment)) + geom_boxplot() +
  stat_summary(fun = mean, geom="point", aes(group = experiment), position = position_dodge(.8),
               color = "black", size = 3) +
  labs(y = "Mean-per-obs(abs(Pre-group - Post-group))", x = "correlations between groups") +
  ggtitle("GAM models with 10 features, 3 groups, diff correlations within and between groups")


#### GROUP B

groupB_shapley = fread("inst/paper_experiments/results/MJ-groupB_Shapley_values_GAM_diff_corr.csv")

# remove any test tries
groupB_shapley = groupB_shapley[No_test_obs == 100]
groupB_shapley[, .N, by = c("correlation", "model_type")][order(model_type)]

all_shapley = fread("inst/paper_experiments/results/MJ-groupB-All_Shapley_values_GAM_diff_corr.csv")
all_shapley = all_shapley[No_test_obs == 100]
all_shapley[, .N, by = c("correlation", "model_type")][order(model_type)]

groupB <- list(group1 = 1:2,
               group2 = 3:4,
               group3 = 5:6,
               group4 = 7:8,
               group5 = 9:10)
groupB = lapply(groupB, function(x){paste0("feat_", 1:10, "_") [x]})
groupB_names = copy(names(groupB))
rank_group_namesB = paste0(groupB_names, "_rank")


results = list()
for(exper in c("MJ-groupB-experiment_gam_diff_corr")  ){
  for(corr in c(0, 0.1, 0.3, 0.7, 0.9)){ # ,

    group_exp = groupB_shapley[correlation == corr]
    group_exp_u = group_exp[, tail(.SD, 1), by = .(test_id, model_type, correlation)]

    all_exp = all_shapley[correlation == corr]
    all_exp_u = all_exp[, tail(.SD, 1), by = .(test_id, model_type, correlation)]

    explanation_base = data.table(all_exp_u)
    explanation_base[, group1 := rowSums(.SD), .SDcols = groupB[[1]]]
    explanation_base[, group2 := rowSums(.SD), .SDcols = groupB[[2]]]
    explanation_base[, group3 := rowSums(.SD), .SDcols = groupB[[3]]]
    explanation_base[, group4 := rowSums(.SD), .SDcols = groupB[[4]]]
    explanation_base[, group5 := rowSums(.SD), .SDcols = groupB[[5]]]

    explanation_mat_post = as.matrix(explanation_base[, ..groupB_names])
    explanation_ranking_post = t(apply(-explanation_mat_post, FUN = rank, 1))
    colnames(explanation_ranking_post) = rank_group_namesB
    explanation_base = cbind(explanation_base, explanation_ranking_post)

    pre_grouped = group_exp_u[, ..groupB_names]

    mean_pre_grouped = apply(pre_grouped, 2, mean)
    sd_pre_grouped2 = apply(pre_grouped, 2, sd)
    sd_pre_grouped3 = mean(apply(pre_grouped, 2, sd))

    pre_grouped_stand = sweep(pre_grouped, 2, mean_pre_grouped, "-")
    pre_grouped_stand2 = sweep(pre_grouped_stand, 2, sd_pre_grouped2, "/")
    pre_grouped_stand3 = pre_grouped_stand / sd_pre_grouped3
    #
    pre_grouped_rank = group_exp_u[, ..rank_group_namesB]

    post_grouped = explanation_base[, ..groupB_names]
    post_grouped_stand = sweep(post_grouped, 2, mean_pre_grouped, "-")
    post_grouped_stand2 = sweep(post_grouped_stand, 2, sd_pre_grouped2, "/")
    post_grouped_stand3 = post_grouped_stand / sd_pre_grouped3
    post_grouped_rank = explanation_base[, ..rank_group_namesB]


    MAD0 = apply(MAD(pre_grouped, post_grouped), 1, mean)

    MAD1 = apply(MAD(pre_grouped_stand2, post_grouped_stand2), 1, mean)

    MAD2 = apply(MAD(pre_grouped_stand3, post_grouped_stand3), 1, mean)

    MDR0 = apply(MDR(pre_grouped_rank, post_grouped_rank), 1, mean)

    results[[length(results) + 1]] = data.frame(absolute_difference = MAD1, absolute_difference_rank = MDR0, correlation = corr, experiment = exper)

  }
}

results_all = rbindlist(results)

results_all$experiment = factor(results_all$experiment)
results_all$correlation = factor(results_all$correlation)
results_all$absolute_difference_log = log(results_all$absolute_difference)

ggplot(results_all, aes(y = absolute_difference, x = correlation, col = experiment)) + geom_boxplot() +
  stat_summary(fun = mean, geom="point", aes(group = experiment), position = position_dodge(.8),
               color = "black", size = 3) +
  labs(y = "Mean-per-obs(abs(Pre-group - Post-group))", x = "correlation between groups") +
  ggtitle("GAM models with 10 features, 5 groups")

ggplot(results_all, aes(y = absolute_difference_rank, x = correlation, col = experiment)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom="point", aes(group = experiment), position = position_dodge(.8),
               color = "black", size = 3) +
  labs(y = "Mean-per-obs(abs(Pre-group_rank - Post-group_rank))", x = "correlation between groups") +
  ggtitle("GAM models with 10 continuous features, 5 groups")


results_groupB = results_all

results_AB = rbind(results_groupA, results_groupB)

results_AB[experiment == "AR-groupA-experiment_gam_diff_corr", experiment := "group A"]
results_AB[experiment == "MJ-groupB-experiment_gam_diff_corr", experiment := "group B"]
setnames(results_AB, "experiment", "grouping")

p3 <- ggplot(results_AB, aes(y = absolute_difference, x = correlation, col = grouping)) + geom_boxplot() +
  stat_summary(fun = mean, geom = "point", aes(group = grouping), position = position_dodge(.8),
               color = "black", size = 3) +
  labs(y = "Mean-per-obs(abs(Pre-group - Post-group))", x = "correlation between features of different groups")
# ggtitle("lm models with 10 features, 5 groups, diff correlations within and between groups") +
# theme(legend.position = "none")
# ggsave(
#   "exper3-GAM-groupAB.png",
#   plot = p3,
#   device = 'png',
#   path = 'inst/paper_experiments/figures/',
#   scale = 1,
#   width = 13,
#   height = 7,
#   units = "cm"
# )

size = 7
p3 <- ggplot(results_AB, aes(y = absolute_difference, x = correlation, col = grouping)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom="point", aes(group = grouping), position = position_dodge(.8),
               color = "black", size = 1) +
  labs(y = "Mean-per-obs(abs(Pre-group - Post-group))", x = "correlation between all features") +
  ggplot2::theme(
    legend.text = element_text(size = size),
    legend.title = element_text(size = size),
    axis.text = element_text(size = size),
    axis.text.y = element_text(size = size),
    axis.title = element_text(size = size),
    strip.text = element_text(size = size)
  )

results_GAM = results_AB
results_GAM[, model := "GAM"]
################################ lm #####################################

groupA_shapley = fread("inst/paper_experiments/results/AR-groupA_Shapley_values_lm_diff_corr.csv")

# remove any test tries
groupA_shapley = groupA_shapley[No_test_obs == 100]
groupA_shapley[, .N, by = c("correlation", "model_type")][order(model_type)]

all_shapley = fread("inst/paper_experiments/results/AR-groupA_All_Shapley_values_lm_diff_corr.csv")
all_shapley = all_shapley[No_test_obs == 100]
all_shapley[, .N, by = c("correlation", "model_type")][order(model_type)]

# Group A
groupA <- list(group1 = 1:4,
               group2 = 5:8,
               group3 = 9:10)
groupA = lapply(groupA, function(x){paste0("feat_", 1:10, "_") [x]})
groupA_names = copy(names(groupA))
rank_group_namesA = paste0(groupA_names, "_rank")

results = list()
for(exper in c("AR-groupA-experiment_lm_diff_corr")  ){
  for(corr in c(0, 0.1, 0.3, 0.7, 0.9)){

    group_exp = groupA_shapley[correlation == corr]
    group_exp_u = group_exp[, tail(.SD, 1), by = .(test_id, model_type, correlation)]

    all_exp = all_shapley[correlation == corr]
    all_exp_u = all_exp[, tail(.SD, 1), by = .(test_id, model_type, correlation)]

    explanation_base = data.table(all_exp_u)
    explanation_base[, group1 := rowSums(.SD), .SDcols = groupA[[1]]]
    explanation_base[, group2 := rowSums(.SD), .SDcols = groupA[[2]]]
    explanation_base[, group3 := rowSums(.SD), .SDcols = groupA[[3]]]

    explanation_mat_post = as.matrix(explanation_base[, ..groupA_names])
    explanation_ranking_post = t(apply(-explanation_mat_post, FUN = rank, 1))
    colnames(explanation_ranking_post) = rank_group_namesA
    explanation_base = cbind(explanation_base, explanation_ranking_post)

    pre_grouped = group_exp_u[, ..groupA_names]

    mean_pre_grouped = apply(pre_grouped, 2, mean)
    sd_pre_grouped2 = apply(pre_grouped, 2, sd)
    sd_pre_grouped3 = mean(apply(pre_grouped, 2, sd))

    pre_grouped_stand = sweep(pre_grouped, 2, mean_pre_grouped, "-")
    pre_grouped_stand2 = sweep(pre_grouped_stand, 2, sd_pre_grouped2, "/")
    pre_grouped_stand3 = pre_grouped_stand / sd_pre_grouped3
    #
    pre_grouped_rank = group_exp_u[, ..rank_group_namesA]

    post_grouped = explanation_base[, ..groupA_names]
    post_grouped_stand = sweep(post_grouped, 2, mean_pre_grouped, "-")
    post_grouped_stand2 = sweep(post_grouped_stand, 2, sd_pre_grouped2, "/")
    post_grouped_stand3 = post_grouped_stand / sd_pre_grouped3
    post_grouped_rank = explanation_base[, ..rank_group_namesA]


    MAD0 = apply(MAD(pre_grouped, post_grouped), 1, mean)

    MAD1 = apply(MAD(pre_grouped_stand2, post_grouped_stand2), 1, mean)

    MAD2 = apply(MAD(pre_grouped_stand3, post_grouped_stand3), 1, mean)

    MDR0 = apply(MDR(pre_grouped_rank, post_grouped_rank), 1, mean)

    results[[length(results) + 1]] = data.frame(absolute_difference = MAD1, absolute_difference_rank = MDR0, correlation = corr, experiment = exper)

  }
}

results_all = rbindlist(results)

results_all$experiment = factor(results_all$experiment)
results_all$correlation = factor(results_all$correlation)
results_all$absolute_difference_log = log(results_all$absolute_difference)

results_groupA = results_all

#### GROUP B

MAD <- function(pre_grouped, post_grouped){
  abs(pre_grouped - post_grouped)
}
MDR <- function(ranking_pre_grouped, ranking_post_grouped){
  abs(ranking_pre_grouped - ranking_post_grouped)
}

groupB_shapley = fread("inst/paper_experiments/results/AR-groupB_Shapley_values_lm_diff_corr.csv")

# remove any test tries
groupB_shapley = groupB_shapley[No_test_obs == 100]
groupB_shapley[, .N, by = c("correlation", "model_type")][order(model_type)]

all_shapley = fread("inst/paper_experiments/results/AR-groupB_All_Shapley_values_lm_diff_corr.csv")

groupB <- list(group1 = 1:2,
               group2 = 3:4,
               group3 = 5:6,
               group4 = 7:8,
               group5 = 9:10)
groupB = lapply(groupB, function(x){paste0("feat_", 1:10, "_") [x]})
groupB_names = copy(names(groupB))
rank_group_namesB = paste0(groupB_names, "_rank")


results = list()
for(corr in c(0, 0.1, 0.3, 0.7, 0.9)){

  group_exp = groupB_shapley[correlation == corr]
  group_exp_u = group_exp[, tail(.SD, 1), by = .(test_id, model_type, correlation)]

  all_exp = all_shapley[correlation == corr]
  all_exp_u = all_exp[, tail(.SD, 1), by = .(test_id, model_type, correlation)]

  explanation_base = data.table(all_exp_u)
  explanation_base[, group1 := rowSums(.SD), .SDcols = groupB[[1]]]
  explanation_base[, group2 := rowSums(.SD), .SDcols = groupB[[2]]]
  explanation_base[, group3 := rowSums(.SD), .SDcols = groupB[[3]]]
  explanation_base[, group4 := rowSums(.SD), .SDcols = groupB[[4]]]
  explanation_base[, group5 := rowSums(.SD), .SDcols = groupB[[5]]]

  explanation_mat_post = as.matrix(explanation_base[, ..groupB_names])
  explanation_ranking_post = t(apply(-explanation_mat_post, FUN = rank, 1))
  colnames(explanation_ranking_post) = rank_group_namesB
  explanation_base = cbind(explanation_base, explanation_ranking_post)

  pre_grouped = group_exp_u[, ..groupB_names]

  mean_pre_grouped = apply(pre_grouped, 2, mean)
  sd_pre_grouped2 = apply(pre_grouped, 2, sd)
  sd_pre_grouped3 = mean(apply(pre_grouped, 2, sd))

  pre_grouped_stand = sweep(pre_grouped, 2, mean_pre_grouped, "-")
  pre_grouped_stand2 = sweep(pre_grouped_stand, 2, sd_pre_grouped2, "/")
  pre_grouped_stand3 = pre_grouped_stand / sd_pre_grouped3
  #
  pre_grouped_rank = group_exp_u[, ..rank_group_namesB]

  post_grouped = explanation_base[, ..groupB_names]
  post_grouped_stand = sweep(post_grouped, 2, mean_pre_grouped, "-")
  post_grouped_stand2 = sweep(post_grouped_stand, 2, sd_pre_grouped2, "/")
  post_grouped_stand3 = post_grouped_stand / sd_pre_grouped3
  post_grouped_rank = explanation_base[, ..rank_group_namesB]


  MAD0 = apply(MAD(pre_grouped, post_grouped), 1, mean)

  MAD1 = apply(MAD(pre_grouped_stand2, post_grouped_stand2), 1, mean)

  MAD2 = apply(MAD(pre_grouped_stand3, post_grouped_stand3), 1, mean)

  MDR0 = apply(MDR(pre_grouped_rank, post_grouped_rank), 1, mean)

  results[[length(results) + 1]] = data.frame(absolute_difference = MAD1, absolute_difference_rank = MDR0, correlation = corr, experiment = "AR-groupB-experiment_lm_diff_corr")

}

results_all = rbindlist(results)

results_all$experiment = factor(results_all$experiment)
results_all$correlation = factor(results_all$correlation)
results_all$absolute_difference_log = log(results_all$absolute_difference)

results_groupB = results_all

results_AB = rbind(results_groupA, results_groupB)

results_AB[experiment == "AR-groupA-experiment_lm_diff_corr", experiment := "group A"]
results_AB[experiment == "AR-groupB-experiment_lm_diff_corr", experiment := "group B"]
setnames(results_AB, "experiment", "grouping")

results_lm = results_AB
results_lm[, model := "lm_2"]
results_GAM[, model := "GAM_2"]

results = rbind(results_lm, results_GAM)

scaleFUN <- function(x) sprintf("%.3f", x)

size = 7
theme_set(theme_bw())
p4 <- ggplot(results, aes(y = absolute_difference, x = correlation, col = grouping)) +
  geom_boxplot() + scale_y_log10() + # + scale_y_continuous(trans="log", labels=scaleFUN) +
  stat_summary(fun = mean, geom="point", aes(group = grouping), position = position_dodge(.8),
               color = "black", size = 1) +
  labs(y = "Mean Absolute Deviation for Individual i", x = "Correlation Across Features in Different Groups") +
  facet_wrap(~model) + #  scales = "free_y"
  ggplot2::theme(
    legend.text = element_text(size = size),
    legend.title = element_text(size = size),
    axis.text = element_text(size = size),
    axis.text.y = element_text(size = size),
    axis.title = element_text(size = size),
    strip.text = element_text(size = size)
  )

ggsave(
  "exper3-lm-GAM-groupAB-log10-scale.png",
  plot = p4,
  device = 'png',
  path = 'inst/paper_experiments/figures/',
  scale = 1,
  width = 17.4,
  height = 6.5,
  units = "cm"
)
