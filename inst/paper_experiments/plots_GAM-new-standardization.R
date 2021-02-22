# This script was made Feb 18th. The new standardization was suggested by Kjersti. Instead of standardizing the Shapley values, we standardize the response.
# We don't subtract by the mean because this will be cancelled out in the MAD calculation. We divide, instead, by the standard deviation of the response for the
# test observations.
# This is to make Figure 3 in the paper.

library(shapr)
library(data.table)
library(ggplot2)

MAD <- function(pre_grouped, post_grouped){
  abs(pre_grouped - post_grouped)
}

# Half way through I started saving the gam results with another name :/
groupA_shapley = fread("inst/paper_experiments/results/finished-results/group1_Shapley_values_GAM.csv")
groupA2_shapley = fread("inst/paper_experiments/results/finished-results/groupA_Shapley_values_GAM.csv")

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
    group_exp_u[, response := none + group1 + group2 + group3]
    # 6.193040
    # 2: 6.543539
    # 3: 4.185911
    # 4: 4.258351
    # 5: 6.074558
    # 6: 5.866909
    pre_grouped = group_exp_u[, ..groupA_names]
    sd_pre_grouped = sd(group_exp_u$response) # 1.416 - this is from 1-general-experiment-function.R
    # sd_pre_grouped2 = apply(pre_grouped, 2, sd) # old sd
    pre_grouped_stand = sweep(pre_grouped, 2, sd_pre_grouped, "/")
    #
    all_exp = all_shapley[model_type == exper][correlation == corr]
    all_exp_u = all_exp[, tail(.SD, 1), by = .(test_id, model_type, correlation)]

    explanation_base = data.table(all_exp_u)
    explanation_base[, group1 := rowSums(.SD), .SDcols = groupA[[1]]]
    explanation_base[, group2 := rowSums(.SD), .SDcols = groupA[[2]]]
    explanation_base[, group3 := rowSums(.SD), .SDcols = groupA[[3]]]

    post_grouped = explanation_base[, ..groupA_names]
    post_grouped_stand = sweep(post_grouped, 2, sd_pre_grouped, "/")

    MAD0 = apply(MAD(pre_grouped_stand, post_grouped_stand), 1, mean)

    results[[length(results) + 1]] = data.frame(absolute_difference = MAD0, correlation = corr, experiment = exper)

  }
}

results_all = rbindlist(results)

results_all$experiment = factor(results_all$experiment)
results_all$correlation = factor(results_all$correlation)
results_all$absolute_difference_log = log(results_all$absolute_difference)

results_all[experiment == "experiment_gam1", experiment := "GAM_1"]
results_all[experiment == "experiment_gam2", experiment := "GAM_2"]
results_all[experiment == "experiment_gam3", experiment := "GAM_3"]
setnames(results_all, "experiment", "model")

results_allA = results_all

#### GROUP B

# Half way through I started saving the gam results with another name :/
groupB_shapley = fread("inst/paper_experiments/results/finished-results/group2_Shapley_values_GAM.csv")
groupB2_shapley = fread("inst/paper_experiments/results/finished-results/groupB_Shapley_values_GAM.csv")

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

results = list()
for(exper in c("experiment_gam1", "experiment_gam2", "experiment_gam3")  ){
  for(corr in c(0, 0.1, 0.3, 0.7, 0.9)){

    group_exp = groupB_shapley[model_type == exper][correlation == corr]
    group_exp_u = group_exp[, tail(.SD, 1), by = .(test_id, model_type, correlation)]
    group_exp_u[, response := none + group1 + group2 + group3 + group4 + group5]

    sd_pre_grouped = sd(group_exp_u$response)
    # sd_pre_grouped2 = apply(pre_grouped, 2, sd) # old
    pre_grouped = group_exp_u[, ..groupB_names]
    pre_grouped_stand = sweep(pre_grouped, 2, sd_pre_grouped, "/")

    all_exp = all_shapley[model_type == exper][correlation == corr]
    all_exp_u = all_exp[, tail(.SD, 1), by = .(test_id, model_type, correlation)]

    explanation_base = data.table(all_exp_u)
    explanation_base[, group1 := rowSums(.SD), .SDcols = groupB[[1]]]
    explanation_base[, group2 := rowSums(.SD), .SDcols = groupB[[2]]]
    explanation_base[, group3 := rowSums(.SD), .SDcols = groupB[[3]]]
    explanation_base[, group4 := rowSums(.SD), .SDcols = groupB[[4]]]
    explanation_base[, group5 := rowSums(.SD), .SDcols = groupB[[5]]]

    post_grouped = explanation_base[, ..groupB_names]
    post_grouped_stand = sweep(post_grouped, 2, sd_pre_grouped, "/")

    MAD0 = apply(MAD(pre_grouped_stand, post_grouped_stand), 1, mean)

    results[[length(results) + 1]] = data.frame(absolute_difference = MAD0, correlation = corr, experiment = exper)

  }
}

results_all = rbindlist(results)

results_all$experiment = factor(results_all$experiment)
results_all$correlation = factor(results_all$correlation)
results_all$absolute_difference_log = log(results_all$absolute_difference)

results_all[experiment == "experiment_gam1", experiment := "GAM_1"]
results_all[experiment == "experiment_gam2", experiment := "GAM_2"]
results_all[experiment == "experiment_gam3", experiment := "GAM_3"]
setnames(results_all, "experiment", "model")

results_allB = results_all

results_allA[, grouping := "Grouping A"]
results_allB[, grouping := "Grouping B"]

results = rbind(results_allA, results_allB)
size = 7
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
  "exper2-GAM-groupAB-log10-standardization-from-response.png",
  plot = p3,
  device = 'png',
  path = 'inst/paper_experiments/figures/',
  scale = 1,
  width = 17.4,
  height = 6.5,
  units = "cm"
)
