library(data.table)
shapley_reweighting_strategy = "none"

load(paste0("/nr/samba/user/fsaase/Documents/effektiv_shapley_output/iterative_kernelshap_lingauss_p12", shapley_reweighting_strategy, ".RData"))

sim_results_folder = "../effektiv_shapley_output/"


exact_vals = fread(paste0(sim_results_folder,"exact_shapley_values", shapley_reweighting_strategy, ".csv"))
names(exact_vals) <- c("phi0", paste0("VV",1:12))
iterative_vals = fread(paste0(sim_results_folder,"iterative_shapley_values", shapley_reweighting_strategy, ".csv"))
approx_vals = fread(paste0(sim_results_folder,"approx_shapley_values", shapley_reweighting_strategy, ".csv"))

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
runcomps_list

df = data.frame(matrix(0, length(runcomps_list), 1))
colnames(df) <- c("n_rows")
df$n_rows <- as.numeric(runcomps_list)

p <- ggplot(df, aes(n_rows)) +
    geom_histogram()
ggsave(paste0(sim_results_folder, "n_rows.png"), plot = p)

