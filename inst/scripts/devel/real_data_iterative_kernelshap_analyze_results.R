library(data.table)
shapley_reweighting_strategy = "none"

# load(paste0("/nr/project/stat/BigInsight/Projects/Explanations/EffektivShapley/Frida/simuleringsresultater/gmc_data/iterative_kernelshap_lingauss_p12", shapley_reweighting_strategy, ".RData"))

sim_results_folder = "/nr/project/stat/BigInsight/Projects/Explanations/EffektivShapley/Frida/simuleringsresultater/german_credit/"

exact_vals = fread(paste0(sim_results_folder,"exact_shapley_values_", shapley_reweighting_strategy, ".csv"))
# names(exact_vals) <- c("phi0", paste0("VV",1:12))
iterative_vals = fread(paste0(sim_results_folder,"iterative_shapley_values_", shapley_reweighting_strategy, ".csv"))
approx_vals = fread(paste0(sim_results_folder,"approx_shapley_values_", shapley_reweighting_strategy, ".csv"))

bias_vec <- colMeans(exact_vals - iterative_vals)
rmse_vec <- sqrt(colMeans((exact_vals - iterative_vals)^2))
mae_vec <- colMeans(abs(exact_vals - iterative_vals))

bias_vec_approx <- colMeans(exact_vals - approx_vals)
rmse_vec_approx <- sqrt(colMeans((exact_vals - approx_vals)^2))
mae_vec_approx <- colMeans(abs(exact_vals - approx_vals))

library(ggplot2)

# Create a data frame for the bar plot

# MAE
df <- data.frame(matrix(0, length(mae_vec)*2, 3))
colnames(df) <- c("MAE", "approach", "features")
# rownames(df) <- names(exact_vals)
df[1:length(exact_vals), 1] <- mae_vec_approx
df[1:length(exact_vals), 2] <- rep("approx", length(exact_vals))
df[(length(exact_vals)+1):nrow(df), 1] <- mae_vec
df[(length(exact_vals)+1):nrow(df), 2] <- rep("iterative", length(exact_vals))
df[, 3] <- rep(names(exact_vals), 2)

# Create the bar plot using ggplot
p <- ggplot(df, aes(x = features, y = MAE, fill = approach)) +
  geom_col(position = "dodge")
ggsave(paste(sim_results_folder, "mae_comparison.png"), plot = p)


# RMSE
df <- data.frame(matrix(0, length(rmse_vec)*2, 3))
colnames(df) <- c("RMSE", "approach", "features")
# rownames(df) <- names(exact_vals)
df[1:length(exact_vals), 1] <- rmse_vec_approx
df[1:length(exact_vals), 2] <- rep("approx", length(exact_vals))
df[(length(exact_vals)+1):nrow(df), 1] <- rmse_vec
df[(length(exact_vals)+1):nrow(df), 2] <- rep("iterative", length(exact_vals))
df[, 3] <- rep(names(exact_vals), 2)

# Create the bar plot using ggplot
p <- ggplot(df, aes(x = features, y = RMSE, fill = approach)) +
  geom_col(position = "dodge")
ggsave(paste(sim_results_folder, "rmse_comparison.png"), plot = p)


# Bias
df <- data.frame(matrix(0, length(bias_vec)*2, 3))
colnames(df) <- c("abs_bias", "approach", "features")
# rownames(df) <- names(exact_vals)
df[1:length(exact_vals), 1] <- abs(bias_vec_approx)
df[1:length(exact_vals), 2] <- rep("approx", length(exact_vals))
df[(length(exact_vals)+1):nrow(df), 1] <- abs(bias_vec)
df[(length(exact_vals)+1):nrow(df), 2] <- rep("iterative", length(exact_vals))
df[, 3] <- rep(names(exact_vals), 2)

# Create the bar plot using ggplot
p <- ggplot(df, aes(x = features, y = abs_bias, fill = approach)) +
  geom_col(position = "dodge")
ggsave(paste(sim_results_folder, "bias_comparison.png"), plot = p)

# Number of sample used
runcomps_list = fread(paste0(sim_results_folder, "runcomps_list_", shapley_reweighting_strategy, ".txt"))

df = data.frame(matrix(0, length(runcomps_list), 1))
colnames(df) <- c("n_rows")
df$n_rows <- as.numeric(runcomps_list)

p <- ggplot(df, aes(n_rows)) +
  geom_histogram(bins = 100)
ggsave(paste0(sim_results_folder, "n_rows.png"), plot = p)

#### Just looking at the largest predictions

preds <- rowSums(exact_vals)

these <- head(order(-preds),10)

preds[these]-rowSums(iterative_vals)[these]

bias_vec <- colMeans(exact_vals[these] - iterative_vals[these])
rmse_vec <- sqrt(colMeans((exact_vals[these] - iterative_vals[these])^2))
mae_vec <- colMeans(abs(exact_vals[these] - iterative_vals[these]))

bias_vec_approx <- colMeans(exact_vals[these] - approx_vals[these])
rmse_vec_approx <- sqrt(colMeans((exact_vals[these] - approx_vals[these])^2))
mae_vec_approx <- colMeans(abs(exact_vals[these] - approx_vals[these]))

# list with run object for all variables
run_obj_list = readRDS(paste0(sim_results_folder, "run_obj_list_", shapley_reweighting_strategy, ".RDS"))
attributes(run_obj_list[[1]])
run_obj_list[[1]]$kshap_it_est_dt[length(run_obj_list[[1]]$kshap_it_est_dt)]

# Number of variables dropped in final Shapley value estimate
n_dropped = list()
for (i in 1:length(run_obj_list)) {
    kshap_it_est_dt = run_obj_list[[i]]$kshap_it_est_dt
    n_dropped[[i]] = sum(is.na(kshap_it_est_dt[nrow(kshap_it_est_dt)]))
}

df = data.frame(matrix(0, length(n_dropped), 1))
colnames(df) <- c("n_var_dropped")
df$n_var_dropped <- as.numeric(n_dropped)

p <- ggplot(df, aes(n_var_dropped)) +
    geom_histogram()
ggsave(paste0(sim_results_folder, "n_var_dropped.png"), plot = p)


# Error versus number of variables dropped
df = data.frame(matrix(0, length(n_dropped), 2))
colnames(df) <- c("n_var_dropped", "mae")
df$n_var_dropped <- as.numeric(n_dropped)
mae = rowSums(abs(exact_vals - iterative_vals))
df$mae <- mae

p <- ggplot(df, aes(n_var_dropped, mae)) +
    geom_point()
ggsave(paste0(sim_results_folder, "mae_vs_n_var_dropped.png"), plot = p)

# Error versus number of rows
df = data.frame(matrix(0, length(runcomps_list), 2))
colnames(df) <- c("n_rows", "mae")
df$n_rows <- as.numeric(runcomps_list)
mae = rowSums(abs(exact_vals - iterative_vals))
df$mae <- mae

p <- ggplot(df, aes(n_rows, mae)) +
    geom_point()
ggsave(paste0(sim_results_folder, "mae_vs_n_rows.png"), plot = p)




##########################################################################################################################################################
######################## HOW MANY ROWS ARE NEEDED FOR NORMAL KERNELSHAP TO YIELD SAME ERROE AS ITERATIVE VERSION ##########################################

sim_results_folder = "/nr/project/stat/BigInsight/Projects/Explanations/EffektivShapley/Frida/simuleringsresultater/adult_v2_fix_n_comb/"
run_obj_list = readRDS(paste0(sim_results_folder, "run_obj_list_", shapley_reweighting_strategy, ".RDS"))

n_features = ncol(run_obj_list[[1]]$kshap_final) - 1
final_est = matrix(0, nrow = length(run_obj_list), ncol = n_features)

for (i in 1:length(run_obj_list)) {
    final_est[i,] = as.numeric(run_obj_list[[i]]$kshap_final[1,1:n_features])
}
colnames(final_est) <- colnames(run_obj_list[[1]]$kshap_final)[1:n_features]
approx_vals = final_est

exact_vals = fread(paste0(sim_results_folder,"exact_shapley_values_", shapley_reweighting_strategy, ".csv"))
# names(exact_vals) <- c("phi0", paste0("VV",1:12))
iterative_vals = fread(paste0(sim_results_folder,"iterative/iterative_shapley_values_", shapley_reweighting_strategy, ".csv"))
# approx_vals = fread(paste0(sim_results_folder,"approx_shapley_values_", shapley_reweighting_strategy, ".csv"))

bias_vec <- colMeans(exact_vals - iterative_vals)
rmse_vec <- sqrt(colMeans((exact_vals - iterative_vals)^2))
mae_vec <- colMeans(abs(exact_vals - iterative_vals))

bias_vec_approx <- colMeans(exact_vals - approx_vals)
rmse_vec_approx <- sqrt(colMeans((exact_vals - approx_vals)^2))
mae_vec_approx <- colMeans(abs(exact_vals - approx_vals))

library(ggplot2)

# Create a data frame for the bar plot

# MAE
df <- data.frame(matrix(0, length(mae_vec)*2, 3))
colnames(df) <- c("MAE", "approach", "features")
# rownames(df) <- names(exact_vals)
df[1:length(exact_vals), 1] <- mae_vec_approx
df[1:length(exact_vals), 2] <- rep("approx", length(exact_vals))
df[(length(exact_vals)+1):nrow(df), 1] <- mae_vec
df[(length(exact_vals)+1):nrow(df), 2] <- rep("iterative", length(exact_vals))
df[, 3] <- rep(names(exact_vals), 2)

# Create the bar plot using ggplot
p <- ggplot(df, aes(x = features, y = MAE, fill = approach)) +
  geom_col(position = "dodge")
ggsave(paste(sim_results_folder, "mae_comparison.png"), plot = p)


# RMSE
df <- data.frame(matrix(0, length(rmse_vec)*2, 3))
colnames(df) <- c("RMSE", "approach", "features")
# rownames(df) <- names(exact_vals)
df[1:length(exact_vals), 1] <- rmse_vec_approx
df[1:length(exact_vals), 2] <- rep("approx", length(exact_vals))
df[(length(exact_vals)+1):nrow(df), 1] <- rmse_vec
df[(length(exact_vals)+1):nrow(df), 2] <- rep("iterative", length(exact_vals))
df[, 3] <- rep(names(exact_vals), 2)

# Create the bar plot using ggplot
p <- ggplot(df, aes(x = features, y = RMSE, fill = approach)) +
  geom_col(position = "dodge")
ggsave(paste(sim_results_folder, "rmse_comparison.png"), plot = p)


# Bias
df <- data.frame(matrix(0, length(bias_vec)*2, 3))
colnames(df) <- c("abs_bias", "approach", "features")
# rownames(df) <- names(exact_vals)
df[1:length(exact_vals), 1] <- abs(bias_vec_approx)
df[1:length(exact_vals), 2] <- rep("approx", length(exact_vals))
df[(length(exact_vals)+1):nrow(df), 1] <- abs(bias_vec)
df[(length(exact_vals)+1):nrow(df), 2] <- rep("iterative", length(exact_vals))
df[, 3] <- rep(names(exact_vals), 2)

# Create the bar plot using ggplot
p <- ggplot(df, aes(x = features, y = abs_bias, fill = approach)) +
  geom_col(position = "dodge")
ggsave(paste(sim_results_folder, "bias_comparison.png"), plot = p)



############## NBNBNBN!!! ##############
# Something in the code below here is not working.
# The results below are contradictory to those above.
# There must be some kind of error in the code below.
############## NBNBNBN!!! ##############

# Number of sample used
runcomps_list = fread(paste0(sim_results_folder, "runcomps_list_", shapley_reweighting_strategy, ".txt"))

df = data.frame(matrix(0, length(runcomps_list), 1))
colnames(df) <- c("n_rows")
df$n_rows <- as.numeric(runcomps_list)

p <- ggplot(df, aes(n_rows)) +
  geom_histogram(bins = 100)
ggsave(paste0(sim_results_folder, "n_rows.png"), plot = p)

dim(run_obj_list[[1]]$kshap_it_est_dt)

mae_approx = 0
for (i in 1:length(run_obj_list)) {
# i = 5
  est = run_obj_list[[i]]$kshap_it_est_dt[,2:15]
  mae = matrix(0, nrow(est), ncol(est))
  for (j in 1:nrow(est)){
    mae[j, ] = as.numeric(abs(est[j,] - exact_vals[i]))
  }
  mae = rowMeans(mae)
  # mae = apply(est, 1, function(x) abs(x - exact_vals[i]))
  # mae = matrix(unlist(mae), nrow = nrow(est), byrow = TRUE)
  # mae = rowMeans(mae)
  mae_approx = mae_approx + mae
}
mae_approx = mae_approx / length(run_obj_list)
mae = mae_approx
n_rows_iterative = as.numeric(fread(paste0(sim_results_folder, "/iterative/runcomps_list_", shapley_reweighting_strategy, ".txt")))
iterative = fread(paste0(sim_results_folder, "/iterative/iterative_shapley_values_", shapley_reweighting_strategy, ".csv"))
mae_iterative = rowMeans(abs(exact_vals - iterative))

# Plot MAE with ggplot
df <- data.frame(mae = mae)
p <- ggplot(df, aes(x =(seq_along(mae) - 1)*10+48, y = mae)) +
  geom_line() +
  labs(x = "Index", y = "MAE") +
  geom_point(aes(x = mean(n_rows_iterative[i]), y = mean(mae_iterative[1]), colour = "red"))

ggsave(paste0(sim_results_folder, "mae_vs_index.png"), plot = p)


