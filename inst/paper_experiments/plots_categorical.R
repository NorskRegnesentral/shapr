library(shapr)
library(data.table)
library(ggplot2)

MAD <- function(pre_grouped, post_grouped){
  abs(pre_grouped - post_grouped)
}
MDR <- function(ranking_pre_grouped, ranking_post_grouped){
  abs(ranking_pre_grouped - ranking_post_grouped)
}

all_shapley = fread("inst/paper_experiments/results/All_Shapley_values_categorical_lm_new_response.csv")
all_shapley[, .N, by = c("correlation", "model_type")][order(model_type)]

groupA_shapley = fread("inst/paper_experiments/results/groupA_Shapley_values_categorical_new_response.csv")
groupA_shapley[, .N, by = c("correlation", "model_type")][order(model_type)]

groupB_shapley = fread("inst/paper_experiments/results/groupB_Shapley_values_categorical_new_response.csv")
groupB_shapley[, .N, by = c("correlation", "model_type")][order(model_type)]

# remove any test tries
groupA_shapley = groupA_shapley[No_test_obs == 100]
all_shapley = all_shapley[No_test_obs == 100]
all_shapley[, .N, by = correlation]

# Group A
groupA <- list(group1 = 1:4,
               group2 = 5:8,
               group3 = 9:10)
groupA = lapply(groupA, function(x){paste0("feat_", 1:10, "_") [x]})
groupA_names = copy(names(groupA))
rank_group_namesA = paste0(groupA_names, "_rank")


results = list()
for(exper in c("experiment_lm1", "experiment_lm2", "experiment_lm3")  ){
  for(corr in unique(groupA_shapley[model_type == exper][["correlation"]])){ # , 0.1, 0.3, 0.7, 0.9
    
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

ggplot(results_all, aes(y = absolute_difference_rank, x = correlation, col = experiment)) + geom_boxplot() + 
  stat_summary(fun = mean, geom="point", aes(group = experiment), position = position_dodge(.8),
               color = "black", size = 3) +
  labs(y = "Mean-per-obs(abs(Pre-group - Post-group))") + 
  ggtitle("lm models with 10 categorical features, 3 levels, 3 groups")


#### GROUP B

all_shapley = fread("inst/paper_experiments/results/All_Shapley_values_categorical_lm_new_response.csv")
groupB_shapley = fread("inst/paper_experiments/results/groupB_Shapley_values_categorical_new_response.csv")

# remove any test tries
groupB_shapley = groupB_shapley[No_test_obs == 100]
all_shapley = all_shapley[No_test_obs == 100]
all_shapley[, .N, by = correlation]

# Group B
groupB <- list(group1 = 1:2,
               group2 = 3:4,
               group3 = 5:6,
               group4 = 7:8,
               group5 = 9:10)
groupB = lapply(groupB, function(x){paste0("feat_", 1:10, "_") [x]})
groupB_names = copy(names(groupB))
rank_group_namesB = paste0(groupB_names, "_rank")


results = list()
for(exper in c("experiment_lm1", "experiment_lm2", "experiment_lm3")  ){
  for(corr in unique(groupB_shapley[model_type == exper][["correlation"]])){
    
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
    
    results[[length(results) + 1]] = data.frame(absolute_difference = MAD1,absolute_difference_rank = MDR0, correlation = corr, experiment = exper)
    
  }
}

results_all = rbindlist(results)

results_all$experiment = factor(results_all$experiment)
results_all$correlation = factor(results_all$correlation)
results_all$absolute_difference_log = log(results_all$absolute_difference)

ggplot(results_all, aes(y = absolute_difference, x = correlation, col = experiment)) + geom_boxplot() + 
  stat_summary(fun = mean, geom="point", aes(group = experiment), position = position_dodge(.8),
               color = "black", size = 3) +
  labs(y = "Mean-per-obs(abs(Pre-group - Post-group))") + 
  ggtitle("lm models with 10 categorical features, 3 levels, 5 groups") + ylim(0, 0.1)

ggplot(results_all, aes(y = absolute_difference_rank, x = correlation, col = experiment)) + geom_boxplot() + 
  stat_summary(fun = mean, geom="point", aes(group = experiment), position = position_dodge(.8),
               color = "black", size = 3) +
  labs(y = "Mean-per-obs(abs(Pre-group_rank - Post-group_rank))") + 
  ggtitle("lm models with 10 categorical features, 3 levels, 5 groups")

