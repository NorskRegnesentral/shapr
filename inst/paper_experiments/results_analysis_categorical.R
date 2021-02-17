library(data.table)

MAD <- function(pre_grouped, post_grouped, weights){
  mean(colSums((abs(pre_grouped - post_grouped)) * weights))
}
MDR <- function(ranking_pre_grouped, ranking_post_grouped, weights){
  mean(colSums((abs(ranking_pre_grouped - ranking_post_grouped)) * weights))
}

all_shapley = fread("inst/paper_experiments/results/All_Shapley_values_categorical_lm_new.csv")
groupA_shapley = fread("inst/paper_experiments/results/groupA_Shapley_values_categorical_new.csv")
groupB_shapley = fread("inst/paper_experiments/results/groupB_Shapley_values_categorical_new.csv")

# remove any test tries
groupA_shapley = groupA_shapley[No_test_obs == 100]; nrow(groupA_shapley)
groupB_shapley = groupB_shapley[No_test_obs == 100]; nrow(groupB_shapley)
all_shapley = all_shapley[No_test_obs == 100][model_type == 'experiment_lm1']
all_shapley[, .N, by = "correlation"]

# Group 1
groupA <- list(group1 = 1:4,
               group2 = 5:8,
               group3 = 9:10)
groupA = lapply(groupA, function(x){paste0("feat_", 1:10, "_") [x]})
groupA_names = names(groupA)
rank_groupA_names = paste0(groupA_names, "_rank")

exper = "experiment_lm1"
corr = 0
results = list()
for(exper in c("experiment_lm1")  ){
  for(corr in c(0, 0.1, 0.3, 0.7, 0.9)){
    
    group_exp = groupA_shapley[model_type == exper][correlation == corr]
    group_exp_u = group_exp[, tail(.SD, 1), by = .(test_id, model_type, correlation)]
    
    # 51 & 86
    
    all_exp = all_shapley[model_type == exper][correlation == corr]
    all_exp_u = all_exp[, tail(.SD, 1), by = .(test_id, model_type, correlation)]
    
    explanation_base = data.table(all_exp_u)
    explanation_base[, group1 := rowSums(.SD), .SDcols = groupA[[1]]]
    explanation_base[, group2 := rowSums(.SD), .SDcols = groupA[[2]]]
    explanation_base[, group3 := rowSums(.SD), .SDcols = groupA[[3]]]
    
    explanation_mat_post = as.matrix(explanation_base[, ..groupA_names])
    explanation_ranking_post = t(apply(-explanation_mat_post, FUN = rank, 1))
    colnames(explanation_ranking_post) = rank_groupA_names
    explanation_base = cbind(explanation_base, explanation_ranking_post)
    
    pre_grouped = group_exp_u[, ..groupA_names]
    
    mean_pre_grouped = apply(pre_grouped, 2, mean)
    sd_pre_grouped2 = apply(pre_grouped, 2, sd)
    sd_pre_grouped3 = mean(apply(pre_grouped, 2, sd))
    
    pre_grouped_stand = sweep(pre_grouped, 2, mean_pre_grouped, "-")
    pre_grouped_stand2 = sweep(pre_grouped_stand, 2, sd_pre_grouped2, "/")
    pre_grouped_stand3 = pre_grouped_stand / sd_pre_grouped3
    # 
    pre_grouped_rank = group_exp_u[, ..rank_groupA_names]
    
    post_grouped = explanation_base[, ..groupA_names]
    post_grouped_stand = sweep(post_grouped, 2, mean_pre_grouped, "-")
    post_grouped_stand2 = sweep(post_grouped_stand, 2, sd_pre_grouped2, "/")
    post_grouped_stand3 = post_grouped_stand / sd_pre_grouped3
    post_grouped_rank = explanation_base[, ..rank_groupA_names]
    
    weights = group_exp_u$joint_prob / sum(group_exp_u$joint_prob)
    
    MAD0 = MAD(pre_grouped, post_grouped, weights = weights)
    
    MAD1 = MAD(pre_grouped_stand2, post_grouped_stand2, weights = weights)
    
    MAD2 = MAD(pre_grouped_stand3, post_grouped_stand3, weights = weights)
    
    MDR0 = MDR(pre_grouped_rank, post_grouped_rank, weights = weights)
    
    results[[length(results) + 1]] = data.frame(exper, corr, MAD_no_stand = MAD0, MAD_stand = MAD1, MAD_avg_sd_stand = MAD2, MDR = MDR0)
    
  }
}

rbindlist(results)


# Group B

all_shapley = fread("inst/paper_experiments/results/All_Shapley_values_categorical_lm_new.csv")
groupB_shapley = fread("inst/paper_experiments/results/groupB_Shapley_values_categorical_new.csv")

# remove any test tries
groupB_shapley = groupB_shapley[No_test_obs == 100]
all_shapley = all_shapley[No_test_obs == 100][model_type == "experiment_lm1"]
all_shapley[, .N, by = correlation]

groupB <- list(group1 = 1:2,
               group2 = 3:4,
               group3 = 5:6,
               group4 = 7:8,
               group5 = 9:10)
groupB = lapply(groupB, function(x){paste0("feat_", 1:10, "_") [x]})
groupB_names = copy(names(groupB))
rank_groupB_names = paste0(groupB_names, "_rank")

results = list()
for(exper in c("experiment_lm1")  ){
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
    colnames(explanation_ranking_post) = rank_groupB_names
    explanation_base = cbind(explanation_base, explanation_ranking_post)
    
    pre_grouped = group_exp_u[, ..groupB_names]
    
    mean_pre_grouped = apply(pre_grouped, 2, mean)
    sd_pre_grouped2 = apply(pre_grouped, 2, sd)
    sd_pre_grouped3 = mean(apply(pre_grouped, 2, sd))
    
    pre_grouped_stand = sweep(pre_grouped, 2, mean_pre_grouped, "-")
    pre_grouped_stand2 = sweep(pre_grouped_stand, 2, sd_pre_grouped2, "/")
    pre_grouped_stand3 = pre_grouped_stand / sd_pre_grouped3
    #
    pre_grouped_rank = group_exp_u[, ..rank_groupB_names]
    
    post_grouped = explanation_base[, ..groupB_names]
    post_grouped_stand = sweep(post_grouped, 2, mean_pre_grouped, "-")
    post_grouped_stand2 = sweep(post_grouped_stand, 2, sd_pre_grouped2, "/")
    post_grouped_stand3 = post_grouped_stand / sd_pre_grouped3
    post_grouped_rank = explanation_base[, ..rank_groupB_names]
    
    
    weights = group_exp_u$joint_prob / sum(group_exp_u$joint_prob)
    
    MAD0 = MAD(pre_grouped, post_grouped, weights = weights)
    
    MAD1 = MAD(pre_grouped_stand2, post_grouped_stand2, weights = weights)
    
    MAD2 = MAD(pre_grouped_stand3, post_grouped_stand3, weights = weights)
    
    MDR0 = MDR(pre_grouped_rank, post_grouped_rank, weights = weights)
    
    results[[length(results) + 1]] = data.frame(exper, corr, MAD_no_stand = MAD0, MAD_stand = MAD1, MAD_avg_sd_stand = MAD2, MDR = MDR0)
    
  }
}

rbindlist(results)
