library(shapr)
library(data.table)
library(ggplot2)

MAD <- function(pre_grouped, post_grouped){
  abs(pre_grouped - post_grouped)
}
MDR <- function(ranking_pre_grouped, ranking_post_grouped){
  abs(ranking_pre_grouped - ranking_post_grouped)
}

# Half way through I started saving the gam results with another name :/
groupA_shapley = fread("inst/paper_experiments/results/finished-results/group1_Shapley_values_GAM.csv")
groupA2_shapley = fread("inst/paper_experiments/results/finished-results/groupA_Shapley_values_GAM.csv")
# groupA2_shapley[, .N, by = c("correlation", "model_type")][order(model_type)]

groupA_shapley = rbind(groupA_shapley, groupA2_shapley)

# remove any test tries
groupA_shapley = groupA_shapley[No_test_obs == 100]
groupA_shapley[, .N, by = c("correlation", "model_type")][order(model_type)]

all_shapley = fread("inst/paper_experiments/results/finished-results/All_Shapley_values_GAM.csv")
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
for(exper in c("experiment_gam1", "experiment_gam2", "experiment_gam3")  ){
  for(corr in c(0, 0.1, 0.3, 0.7, 0.9)){

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

ggplot(results_all, aes(y = absolute_difference_log, x = correlation, col = experiment)) + geom_boxplot() +
  stat_summary(fun = mean, geom="point", aes(group = experiment), position = position_dodge(.8),
               color = "black", size = 3) +
  labs(y = "log of Mean-per-obs(abs(Pre-group - Post-group))") +
  ggtitle("GAM models with 10 features, 3 groups") + ylim(-7.5, 2)

ggplot(results_all, aes(y = absolute_difference_rank, x = correlation, col = experiment)) + geom_boxplot() +
  stat_summary(fun = mean, geom="point", aes(group = experiment), position = position_dodge(.8),
               color = "black", size = 3) +
  labs(y = "Mean absolute deviation for individual i") +
  ggtitle("GAM models with 10 continuous features, 3 groups")


results_all[experiment == "experiment_gam1", experiment := "GAM_1"]
results_all[experiment == "experiment_gam2", experiment := "GAM_2"]
results_all[experiment == "experiment_gam3", experiment := "GAM_3"]
setnames(results_all, "experiment", "model")

results_allA = results_all


p1 <- ggplot(results_all, aes(y = absolute_difference_log, x = correlation, col = model)) + geom_boxplot() +
  stat_summary(fun = mean, geom="point", aes(group = model), position = position_dodge(.8),
               color = "black", size = 3) +  ylim(-7.5, 2) +
  labs(y = "log of Mean absolute deviation for individual i", x = "correlation between all features")

# ggsave(
#   "exper2-GAM-groupA.png",
#   plot = p1,
#   device = 'png',
#   path = 'inst/paper_experiments/figures/',
#   scale = 1,
#   width = 13,
#   height = 7,
#   units = "cm"
# )



#### GROUP B

# Half way through I started saving the gam results with another name :/
groupB_shapley = fread("inst/paper_experiments/results/finished-results/group2_Shapley_values_GAM.csv")
groupB2_shapley = fread("inst/paper_experiments/results/finished-results/groupB_Shapley_values_GAM.csv")
# groupB2_shapley[, .N, by = c("correlation", "model_type")][order(model_type)]

groupB_shapley = rbind(groupB_shapley, groupB2_shapley)

# remove any test tries
groupB_shapley = groupB_shapley[No_test_obs == 100]
groupB_shapley[, .N, by = c("correlation", "model_type")][order(model_type)]

all_shapley = fread("inst/paper_experiments/results/finished-results/All_Shapley_values_GAM.csv")
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
for(exper in c("experiment_gam1", "experiment_gam2", "experiment_gam3")  ){
  for(corr in c(0, 0.1, 0.3, 0.7, 0.9)){

    group_exp = groupB_shapley[model_type == exper][correlation == corr]
    group_exp_u = group_exp[, tail(.SD, 1), by = .(test_id, model_type, correlation)]

    all_exp = all_shapley[model_type == exper][correlation == corr]
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

ggplot(results_all, aes(y = absolute_difference_log, x = correlation, col = experiment)) + geom_boxplot() +
  stat_summary(fun = mean, geom="point", aes(group = experiment), position = position_dodge(.8),
               color = "black", size = 3) +
  labs(y = "log of Mean-per-obs(abs(Pre-group - Post-group))") +
  ggtitle("GAM models with 10 features, 5 groups") + ylim(-7.5, 2)

ggplot(results_all, aes(y = absolute_difference_rank, x = correlation, col = experiment)) + geom_boxplot() +
  stat_summary(fun = mean, geom="point", aes(group = experiment), position = position_dodge(.8),
               color = "black", size = 3) +
  labs(y = "Mean-per-obs(abs(Pre-group_rank - Post-group_rank))") +
  ggtitle("GAM models with 10 continuous features, 5 groups")

results_all[experiment == "experiment_gam1", experiment := "GAM_1"]
results_all[experiment == "experiment_gam2", experiment := "GAM_2"]
results_all[experiment == "experiment_gam3", experiment := "GAM_3"]
setnames(results_all, "experiment", "model")

results_allB = results_all

p2 <- ggplot(results_all, aes(y = absolute_difference_log, x = correlation, col = model)) + geom_boxplot() +
  stat_summary(fun = mean, geom="point", aes(group = model), position = position_dodge(.8),
               color = "black", size = 3) + ylim(-7.5, 2) +
  labs(y = "log of Mean absolute deviation for individual i", x = "correlation between all features")

# ggsave(
#   "exper2-GAM-groupB.png",
#   plot = p1,
#   device = 'png',
#   path = 'inst/paper_experiments/figures/',
#   scale = 1,
#   width = 13,
#   height = 7,
#   units = "cm"
# )


results_allA[, grouping := "group A"]
results_allB[, grouping := "group B"]

results = rbind(results_allA, results_allB)
size = 7

scaleFUN <- function(x) sprintf("%.3f", x)

theme_set(theme_bw())
p3 <- ggplot(results, aes(y = absolute_difference, x = correlation, col = model)) +
  geom_boxplot() + scale_y_log10() + #+scale_y_continuous(trans="log", labels=scaleFUN) +
  stat_summary(fun = mean, geom="point", aes(group = model), position = position_dodge(.8),
               color = "black", size = 1) +
  labs(y = "Mean Absolute Deviation for Individual i", x = "Correlation Across All Feature Pairs") +
  facet_wrap(~grouping) +
  ggplot2::theme(
    legend.text = element_text(size = size),
    legend.title = element_text(size = size),
    axis.text = element_text(size = size),
    axis.text.y = element_text(size = size),
    axis.title = element_text(size = size),
    strip.text = element_text(size = size)
  )

ggsave(
  "exper2-GAM-groupAB-log10-scale.png",
  plot = p3,
  device = 'png',
  path = 'inst/paper_experiments/figures/',
  scale = 1,
  width = 17.4,
  height = 6.5,
  units = "cm"
)
