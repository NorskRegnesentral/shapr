# old results but re-simulated
# be careful, this is not saved as a .rds
# to run this:
# open up simulation_dim3.R
# set parameters_list = parameters_list[[2]]
# then open simulation_true_shapley_withdatatable_diff_alphas.R
# run all the lines until the methods
# only run 'empirical_nsamples1000'


head(explanation_list[['empirical_nsamples1000']]$x_test)
parameters_list$N_shapley
parameters_list$corr

explanation_list[['empirical_nsamples1000']]$dt_sum


# new new
MAE(true_shapley, explanation_list[['empirical_nsamples1000']]$dt_sum)

# new old
MAE(true_shapley, data[[i]]$methods$empirical$dt_sum)

# old old
MAE(data[[i]]$true_shapley, data[[i]]$methods$empirical$dt_sum)

# START HERE
# old results
results_old <- readRDS("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/higher_dimensions/8_02_20_dim_3/8_02_20_results_dim_3.rds")
results_old[results_old$MAE_methods_names == 'empirical',]

# this is the old code
data <- readRDS("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/higher_dimensions/8_02_20_dim_3/8_02_20_results_5_dim_3.rds")

i <- 5 # # 4 = corr 0.80

data[[i]]$parameters$N_training
data[[i]]$parameters$N_testing
data[[i]]$parameters$N_shapley
data[[i]]$parameters$corr

# just checking the simulated and old data are the same #
# round(data[[i]]$methods$empirical$dt_sum, 5) == round(explanation_list[['empirical_nsamples1000']]$dt_sum, 5)
# head(data[[i]]$methods$empirical$dt_sum)
# head(explanation_list[['empirical_nsamples1000']]$dt_sum)
# data[[i]]$methods$empirical_ind$x_test == explanation_list[['empirical_nsamples1000']]$x_test
##

head(data[[i]]$methods$empirical$x_test)
# head(x_test)

# dt <- data.table(data[[i]]$methods$empirical$x_test)
dt <- data.table(data[[i]]$methods$gaussian$x_test)

dt[, id := 1:nrow(dt)]
dt_order <- dt[order(feat_1_2, feat_1_3, feat_2_2, feat_2_3, feat_3_2, feat_3_3 ),]

# for i = 1, 2, 3 when we call unique, we get 27 rows - this is to be expected
# but for i = 4, we only get 26 rows??
# unique(dt, by = c("feat_1_2", "feat_1_3", "feat_2_2", "feat_2_3", "feat_3_2", "feat_3_3"))


# dt_sum_empirical <- data.table(data[[i]]$methods$empirical$dt_sum)
dt_sum_gaussian <- data.table(data[[i]]$methods$gaussian$dt_sum)

# cols <- c("feat_1_2", "feat_1_3", "feat_2_2", "feat_2_3", "feat_3_2", "feat_3_3")
cols <- c("feat_1_", "feat_2_", "feat_3_")

## IF YOU DON'T ROUND YOU GET WEIRD ROUNDING ERRORS WHEN YOU DO UNIQUE LATER
# dt_sum_empirical[,(cols) := round(.SD, 5), .SDcols = cols]
# dt_sum_empirical[, id := 1:nrow(dt_sum_empirical)]

dt_sum_gaussian[,(cols) := round(.SD, 5), .SDcols = cols]
dt_sum_gaussian[, id := 1:nrow(dt_sum_gaussian)]

# then we merge with all combinations
# dt_merge <- merge(dt, dt_sum_empirical, by = "id", sort = TRUE)
dt_merge <- merge(dt, dt_sum_gaussian, by = "id", sort = TRUE)

# the strange this is that we get 45 unique rows - we should still only get 27
# hypothesis: probably a rounding error
# for Gaussian: impossible to do this - you still get 1000 rows!
dt_merge_unique <- unique(dt_merge, by = c("feat_1_", "feat_2_", "feat_3_"))

dt_trueShapley <- data.table(data[[i]]$true_shapley)
trueShapley_unique <- unique(dt_trueShapley[, id := 1:nrow(dt_trueShapley)],  by = c("feat_1_", "feat_2_", "feat_3_"))

# Try to calculate the MAE - you need to round above to get 27 rows for the "estimated Shapleys"
# first way:
mean(apply(abs(dt_merge_unique[, c("feat_1_", "feat_2_", "feat_3_")] - trueShapley_unique[, c("feat_1_", "feat_2_", "feat_3_")]), 1, mean))

# i = 1, empirical = 0.02966
# i = 2, empirical = 0.03479
# i = 3, empirical = 0.04351
# i = 4, empirical = 0.067
# i = 5, empirical = 0.0919

# alternate way
trueShapley_merge <- merge(trueShapley_unique, dt_merge, by = "id")
mean(apply(abs(trueShapley_merge[, c("feat_1_.x", "feat_2_.x", "feat_3_.x")] - trueShapley_merge[, c("feat_1_.y", "feat_2_.y", "feat_3_.y")]), 2, mean))
# i = 1, empirical =  0.02966492
# i = 2, empirical =  0.03479
# i = 3, empirical = 0.0435
# i = 4, empirical = 0.06652

# i = 1, gaussian = 0.0264
# i = 2, gaussian =  0.0314
# i = 3, gaussian = 0.0424
# i = 4, gaussian = 0.0807
# i = 5, gaussian = 0.0808











# new data
# new <- readRDS("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/paper_simulations/19_02_20_6kbYr_dim_3/19_02_20_6kbYr_dim_3_results_4.rds")
new <-  readRDS("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/paper_simulations/24_02_20_KKuq1_dim_3/24_02_20_KKuq1_dim_3_results_5.rds")

i = 4
y <- new[[i]]$true_shapley
head(y)

x <- new[[i]]$methods$gaussian_nsamples1000$dt_sum
head(x)

mean(apply(abs(y[, c("feat_1_", "feat_2_", "feat_3_")] - x[, c("feat_1_", "feat_2_", "feat_3_")]), 2, mean))
# i = 1
# i = 2, empirical = 0.03
# i = 3, empirical = 0.046
# i = 4, empirical = 0.0660
# i = 5, empirical = 0.109

# i = 1, gaussian = 0.031
# i = 2, gaussian = 0.0278
# i = 3, gaussian = 0.04657
# i = 4, gaussian = 0.0919
# i = 5, gaussian = 0.132


MAE(new[[i]][['true_shapley']], new[[i]][['methods']][['empirical']]$dt_sum)

new1 <- readRDS("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/paper_simulations/19_02_20_6kbYr_results.rds")
new1[new1[, MAE_methods_names == "empirical"],]
# i = 1, empirical = 0.030
# i = 2, empirical = 0.03641617
# i = 3, empirical = 0.04629697
# i = 4, empirical = 0.06603988
