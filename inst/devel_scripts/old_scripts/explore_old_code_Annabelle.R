# From February 27th

# Old results
results_old <- readRDS("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/MAE/27_02_20_1HvEj_dim3_nbcat3/27_02_20_1HvEj_dim3_nbcat3_results.rds")
results_old[results_old$MAE_methods_names == 'empirical',]

data_old <- readRDS("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/MAE/27_02_20_1HvEj_dim3_nbcat3/27_02_20_1HvEj_dim3_nbcat3_results_5.rds")

METHOD <- 'empirical_ind'

# METHOD <- 'gaussian_nsamples1000'
# METHOD <- 'empirical'

method_old <- data.table(data_old[[1]][['methods']][[METHOD]]$x_test)
method_old[, id_old := 1:nrow(method_old)]
# unique(method_old, by = c("feat_1_2", "feat_1_3", "feat_2_2", "feat_2_3", "feat_3_2", "feat_3_3"))


dt_sum <- data.table(data_old[[1]]$methods[[METHOD]]$dt_sum) #
dt_sum[, id_old := 1:nrow(dt_sum)]

dt_merge <- merge(method_old, dt_sum, by = "id_old", sort = TRUE)
setnames(dt_merge, "none", "none_method")
setnames(dt_merge, "feat_1_", "feat_1_method")
setnames(dt_merge, "feat_2_", "feat_2_method")
setnames(dt_merge, "feat_3_", "feat_3_method")

dt_trueShapley <- data.table(data_old[[1]]$true_shapley)
dt_trueShapley[, id_old := 1:nrow(dt_trueShapley)]
setnames(dt_trueShapley, "none", "none_true")
setnames(dt_trueShapley, "feat_1_", "feat_1_true")
setnames(dt_trueShapley, "feat_2_", "feat_2_true")
setnames(dt_trueShapley, "feat_3_", "feat_3_true")


dt_merge0 <- merge(dt_merge, dt_trueShapley, by = "id_old", sort = TRUE)
dt_merge0_order_old <- dt_merge0[order(feat_1_2, feat_1_3, feat_2_2, feat_2_3, feat_3_2, feat_3_3), ]
dt_merge0_order_old[, id := .GRP, by = .(feat_1_2, feat_1_3, feat_2_2, feat_2_3, feat_3_2, feat_3_3)]


# New results
results_new <- readRDS("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/paper_simulations/27_02_20_YK2ng_dim3_nbcat3/27_02_20_YK2ng_dim3_nbcat3_results.rds")

data_new <- readRDS("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/paper_simulations/27_02_20_YK2ng_dim3_nbcat3/27_02_20_YK2ng_dim3_nbcat3_rho_0.9.rds")

method_new <- data.table(data_new[[1]]$methods[[METHOD]]$x_test)
method_new[, id_new := 1:nrow(method_new)]


dt_sum <- data.table(data_new[[1]]$methods[[METHOD]]$dt_sum)
dt_sum[, id_new := 1:nrow(dt_sum)]

dt_merge <- merge(method_new, dt_sum, by = "id_new", sort = TRUE)
setnames(dt_merge, "none", "none_method")
setnames(dt_merge, "feat_1_", "feat_1_method")
setnames(dt_merge, "feat_2_", "feat_2_method")
setnames(dt_merge, "feat_3_", "feat_3_method")

dt_trueShapley <- data.table(data_new[[1]]$true_shapley)
dt_trueShapley[, id_new := 1:nrow(dt_trueShapley)]
setnames(dt_trueShapley, "none", "none_true")
setnames(dt_trueShapley, "feat_1_", "feat_1_true")
setnames(dt_trueShapley, "feat_2_", "feat_2_true")
setnames(dt_trueShapley, "feat_3_", "feat_3_true")


dt_merge0 <- merge(dt_merge, dt_trueShapley, by = "id_new", sort = TRUE)
dt_merge0_order_new <- dt_merge0[order(feat_1_2, feat_1_3, feat_2_2, feat_2_3, feat_3_2, feat_3_3), ]
dt_merge0_order_new[, id := .GRP, by = .(feat_1_2, feat_1_3, feat_2_2, feat_2_3, feat_3_2, feat_3_3)]


unique(dt_merge0_order_old, by = "id")
dt_merge0_order_new
















# OLD CHECKING
# old results
results_old <- readRDS("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/higher_dimensions/8_02_20_dim_3/8_02_20_results_dim_3.rds")
results_old[results_old$MAE_methods_names == 'empirical',]

# this is the old code
data <- readRDS("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/higher_dimensions/8_02_20_dim_3/8_02_20_results_5_dim_3.rds")

i <- 5 # # 4 = corr 0.80
data[[i]]$methods$empirical$x_test
# head(x_test)

dt <- data.table(data[[i]]$methods$empirical$x_test)
# dt <- data.table(data[[i]]$methods$gaussian$x_test)

dt[, id := 1:nrow(dt)]
dt_order <- dt[order(feat_1_2, feat_1_3, feat_2_2, feat_2_3, feat_3_2, feat_3_3 ),]

unique(dt, by = c("feat_1_2", "feat_1_3", "feat_2_2", "feat_2_3", "feat_3_2", "feat_3_3"))


dt_sum_empirical <- data.table(data[[i]]$methods$empirical$dt_sum)
# dt_sum_gaussian <- data.table(data[[i]]$methods$gaussian$dt_sum)

# cols <- c("feat_1_2", "feat_1_3", "feat_2_2", "feat_2_3", "feat_3_2", "feat_3_3")
cols <- c("feat_1_", "feat_2_", "feat_3_")

## IF YOU DON'T ROUND YOU GET WEIRD ROUNDING ERRORS WHEN YOU DO UNIQUE LATER
dt_sum_empirical[,(cols) := round(.SD, 5), .SDcols = cols]
dt_sum_empirical[, id := 1:nrow(dt_sum_empirical)]

# dt_sum_gaussian[,(cols) := round(.SD, 5), .SDcols = cols]
# dt_sum_gaussian[, id := 1:nrow(dt_sum_gaussian)]

# then we merge with all combinations
dt_merge <- merge(dt, dt_sum_empirical, by = "id", sort = TRUE)
# dt_merge <- merge(dt, dt_sum_gaussian, by = "id", sort = TRUE)

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
# i = 5, empirical = 0.09

# i = 1, gaussian = 0.0264
# i = 2, gaussian =  0.0314
# i = 3, gaussian = 0.0424
# i = 4, gaussian = 0.0807
# i = 5, gaussian = 0.0808



# new <- readRDS("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/paper_simulations/19_02_20_6kbYr_dim_3/19_02_20_6kbYr_dim_3_results_4.rds")
new <-  readRDS("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/paper_simulations/24_02_20_KKuq1_dim_3/24_02_20_KKuq1_dim_3_results_5.rds")

i = 2
y <- new[[i]]$true_shapley
head(y)
x <- new[[i]]$methods$empirical$dt_sum
# x <- new[[i]]$methods$gaussian_nsamples1000$dt_sum
head(x)

mean(apply(abs(y[, c("feat_1_", "feat_2_", "feat_3_")] - x[, c("feat_1_", "feat_2_", "feat_3_")]), 2, mean))
# i = 1, empirical = 0.0302 ----------- 0.03084548
# i = 2, empirical = 0.0364 ----------- 0.02766051
# i = 3, empirical = 0.046 ---------- 0.03724191
# i = 4, empirical = 0.0660 --------- 0.04193204
# i = 5, empirical = 0.109 ---------- 0.04305615

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
