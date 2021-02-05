library(shapr)
library(data.table)
library(ggplot2)

A = fread("inst/paper_experiments/results/results_groupA.csv")
B = fread("inst/paper_experiments/results/results_groupB.csv")

# 4 rows per correlation, test_id, and experiment
# one for each combination of pre_grouped and standardized

# Grouping A
# experiment 1 - no interactions
A_sub = A[A[, (standardized == 1) & (model_type == "experiment1")]]

pre_grouped = A_sub[A_sub[, (pre_grouped == 1) ]]
post_grouped = A_sub[A_sub[, (pre_grouped == 0)]]

A_g1 = data.frame(cbind(pre_group1 = pre_grouped$group1, post_group1 = post_grouped$group1, correlation = pre_grouped$correlation))
A_g1$correlation = factor(A_g1$correlation)

ggplot(data = A_g1) + geom_point(aes(x = pre_group1, y = post_group1, col = correlation)) +
  geom_abline(slope = 1, intercept = 0) + facet_wrap( ~ correlation)

# All experiments
A_sub = A[A[, (standardized == 1)]]
A_sub$correlation = factor(A_sub$correlation)

ggplot(data = A_sub) + geom_point(aes(x = correlation, y = MDR)) + facet_wrap( ~ model_type)

#
A_sub = A[A[, (standardized == 1)]]

groups = c("group1", "group2", "group3")
pre_grouped = A_sub[A_sub[, (pre_grouped == 1) ]]
post_grouped = A_sub[A_sub[, (pre_grouped == 0)]]

abs_pre_minus_post = apply(abs(pre_grouped[, ..groups] - post_grouped[, ..groups]), 1, sum)



A_g1 = data.frame(pre_group1 = pre_grouped$group1,
                  pre_group2 = pre_grouped$group2,
                  pre_group3 = pre_grouped$group3,
                  post_group1 = post_grouped$group1,
                  post_group2 = post_grouped$group2,
                  post_group3 = post_grouped$group3,
                  correlation = pre_grouped$correlation,
                  experiment = pre_grouped$model_type,
                  abs_pre_minus_post = abs_pre_minus_post)

A_g1$correlation = factor(A_g1$correlation)

ggplot(A_g1, aes(x = correlation, y = abs_pre_minus_post)) + geom_boxplot() + facet_wrap(~experiment) +
  ggtitle(paste("Grouping A, ",  expression(sum( abs(pre[i] - post[i] ), i = 1, n)))) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 6, color="red", fill="red")

# Grouping B
B_sub = B[B[, (standardized == 1)]]

groups = c("group1", "group2", "group3", "group4", "group5")
pre_grouped = B_sub[B_sub[, (pre_grouped == 1) ]]
post_grouped = B_sub[B_sub[, (pre_grouped == 0)]]

abs_pre_minus_post = apply(abs(pre_grouped[, ..groups] - post_grouped[, ..groups]), 1, sum)



B_g1 = data.frame(pre_group1 = pre_grouped$group1,
                  pre_group2 = pre_grouped$group2,
                  pre_group3 = pre_grouped$group3,
                  pre_group4 = pre_grouped$group4,
                  pre_group5 = pre_grouped$group5,
                  post_group1 = post_grouped$group1,
                  post_group2 = post_grouped$group2,
                  post_group3 = post_grouped$group3,
                  post_group4 = post_grouped$group4,
                  post_group5 = post_grouped$group5,
                  correlation = pre_grouped$correlation,
                  experiment = pre_grouped$model_type,
                  abs_pre_minus_post = abs_pre_minus_post)

B_g1$correlation = factor(B_g1$correlation)

ggplot(B_g1, aes(x = correlation, y = abs_pre_minus_post)) + geom_boxplot() + facet_wrap(~experiment) +
  ggtitle(paste("Grouping B, ",  expression(sum( abs(pre[i] - post[i] ), i = 1, n)))) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 6, color="red", fill="red")
