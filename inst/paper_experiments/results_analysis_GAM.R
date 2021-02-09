library(shapr)
library(data.table)
library(ggplot2)

MAD <- function(pre_grouped, post_grouped, weights){
  mean(colSums((abs(pre_grouped - post_grouped)) * weights))
}
MDR <- function(ranking_pre_grouped, ranking_post_grouped, weights){
  mean(colSums((abs(ranking_pre_grouped - ranking_post_grouped)) * weights))
}

group1_shapley = fread("inst/paper_experiments/results/group1_Shapley_values_GAM.csv")
group2_shapley = fread("inst/paper_experiments/results/group2_Shapley_values_GAM.csv")
all_shapley = fread("inst/paper_experiments/results/All_Shapley_values_GAM.csv")

# remove any test tries
group1_shapley = group1_shapley[No_test_obs == 100]
group2_shapley = group2_shapley[No_test_obs == 100]
all_shapley = all_shapley[No_test_obs == 100]

# Group1

# experiment 1 - AR
# group_exp1 = group1[model_type == 'experiment_gam1']
# group_exp1[, .N, by = c("test_id", "model_type",  "correlation")][order(N)]
# group_exp1_u = group_exp1[, tail(.SD, 1), by = .(test_id, model_type, correlation)]
# nrow(group_exp1_u)
#
# all_exp1 = all_shapley[model_type == 'experiment_gam1']
# all_exp1[, .N, by = c("test_id", "correlation")][order(N)]
# all_exp1_u = all_exp1[, tail(.SD, 1), by = .(test_id, model_type, correlation)]
# nrow(all_exp1_u)
#
# # experiment 2 - AR
# group_exp2 = group1[model_type == 'experiment_gam2']
# group_exp2[, .N, by = c("test_id", "correlation")][order(N)]
# group_exp2_u = group_exp2[, tail(.SD, 1), by = .(test_id, model_type, correlation)]
# nrow(group_exp2_u)
#
# all_exp2 = all_shapley[model_type == 'experiment_gam2']
# all_exp2[, .N, by = c("test_id", "correlation")]
# all_exp2_u = all_exp2[, tail(.SD, 1), by = .(test_id, model_type, correlation)]
# nrow(all_exp2_u)
#
# # experiment 3 - MJ
# group_exp3 = group1[model_type == 'experiment_gam3']
# group_exp3[, .N, by = c("test_id", "correlation")][order(N)]
# group_exp3_u = group_exp3[, tail(.SD, 1), by = .(test_id, model_type, correlation)]
# nrow(group_exp3_u)
#
# all_exp3 = all_shapley[model_type == 'experiment_gam3']
# all_exp3[, .N, by = c("test_id", "correlation")][order(N)]
# all_exp3_u = all_exp3[, tail(.SD, 1), by = .(test_id, model_type, correlation)]
# nrow(all_exp3_u)


# Group 1
group1 <- list(group1 = 1:4,
               group2 = 5:8,
               group3 = 9:10)
group1 = lapply(group1, function(x){paste0("feat_", 1:10, "_") [x]})
group1_names = copy(names(group1))
rank_group_names1 = paste0(group1_names, "_rank")

results = list()

# exper = "experiment_gam1"

for(exper in c("experiment_gam1", "experiment_gam2", "experiment_gam3")  ){
  for(corr in unique(all_shapley$correlation)){

    group_exp = group1_shapley[model_type == exper][correlation == corr]
    group_exp_u = group_exp[, tail(.SD, 1), by = .(test_id, model_type, correlation)]

    all_exp = all_shapley[model_type == exper][correlation == corr]
    all_exp_u = all_exp[, tail(.SD, 1), by = .(test_id, model_type, correlation)]

    explanation_base = data.table(all_exp_u)
    explanation_base[, group1 := rowSums(.SD), .SDcols = group1[[1]]]
    explanation_base[, group2 := rowSums(.SD), .SDcols = group1[[2]]]
    explanation_base[, group3 := rowSums(.SD), .SDcols = group1[[3]]]

    explanation_mat_post = as.matrix(explanation_base[, ..group1_names])
    explanation_ranking_post = t(apply(-explanation_mat_post, FUN = rank, 1))
    colnames(explanation_ranking_post) = rank_group_names1
    explanation_base = cbind(explanation_base, explanation_ranking_post)

    pre_grouped = group_exp_u[, ..group1_names]

    mean_pre_grouped = matrix(apply(pre_grouped, 2, mean), ncol = ncol(pre_grouped), nrow = nrow(pre_grouped), byrow = T)
    sd_pre_grouped = apply(pre_grouped, 2, sd) # matrix(apply(pre_grouped, 2, sd), ncol = ncol(pre_grouped), nrow = nrow(pre_grouped), byrow = T)

    pre_grouped_stand = (pre_grouped - mean_pre_grouped) / sd_pre_grouped
    pre_grouped_rank = group_exp_u[, ..rank_group_names1]

    post_grouped = explanation_base[, ..group1_names]
    post_grouped_stand = (post_grouped - mean_pre_grouped) / sd_pre_grouped
    post_grouped_rank = explanation_base[, ..rank_group_names1]

    # print(paste0("Group: 1 ", "GAM type: ", exper, " Corr: ", corr))

    MAD0 = (MAD(pre_grouped_stand, post_grouped_stand, weights = 1))
    MDR0 = (MDR(pre_grouped_rank, post_grouped_rank, weights = 1))


    results[[length(results) + 1]] = data.frame(exper, corr, MAD0, MDR0)

    print("---")
  }
}

rbindlist(results)
#
#
# results = data.table(experiment = c(rep("GAM_1", 10), rep("GAM_2", 10), rep("GAM_3", 10)),
#                      rho = rep(c(0, 0.1, 0.3, 0.7, 0.9), 6),
#                      group = rep(c(rep("G1", 5), rep("G2", 5)), 6))
#
# corr_vec = c(0, 0.1, 0.3, 0.7, 0.9)
#
# results = data.table(experiment = c(rep("GAM_1", 10), rep("GAM_2", 10), rep("GAM_3", 10)),
#                      rho = rep(c(0, 0.1, 0.3, 0.7, 0.9), 6),
#                      group = rep(c(rep("G1", length(corr_vec)), rep("G2", length(corr_vec))), 6))
