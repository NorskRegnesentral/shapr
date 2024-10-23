# This file build on Pull Request https://github.com/NorskRegnesentral/shapr/pull/273
# This file does not run on the iterative version.
# The point of the file was to replicate the plot values that Heskes obtained in their implementation
# to validate my implementation.

# Set to true in order to save plots in the main folder
save_plots <- FALSE


# Sina plot -------------------------------------------------------------------------------------------------------
#' Make a sina plot of the Shapley values computed using shapr.
#'
#' @param explanation shapr list containing an explanation produced by shapr::explain.
#'
#' @return ggplot2 object containing the sina plot.
#' @export
#'
#' @import tidyr
#' @import shapr
#' @import ggplot2
#' @import ggforce
#'
#' @importFrom dplyr `%>%`
#'
#' @examples
#' # set parameters and random seed
#' set.seed(2020)
#' N <- 1000
#' m <- 4
#' sds <- runif(4, 0.5, 1.5)
#' pars <- runif(7, -1, 1)
#'
#' # Create data from a structural equation model
#' X_1 <- rnorm(N, sd = sds[1])
#' Z <- rnorm(N, 1)
#' X_2 <- X_1 * pars[1] + Z * pars[2] + rnorm(N, sd = sds[2])
#' X_3 <- X_1 * pars[3] + Z * pars[4] + rnorm(N, sd = sds[3])
#' Y <- X_1 * pars[5] + X_2 * pars[6] + X_3 * pars[7] + rnorm(N, sd = sds[4])
#'
#' # collecting data
#' mu_A <- rep(0, m)
#' X_A <- cbind(X_1, X_2, X_3)
#' dat_A <- cbind(X_A, Y)
#' cov_A <- cov(dat_A)
#'
#' model <- lm(Y ~ . + 0 , data = as.data.frame(dat_A))
#' explainer <- shapr::shapr(X_A, model)
#' y_mean <- mean(Y)
#'
#' explanation_classic <- shapr::explain(
#'   dat_A,
#'   approach = "gaussian",
#'   explainer = explainer,
#'   phi0 = y_mean
#' )
#' sina_plot(explanation_classic)
#'
#' explanation_causal <- shapr::explain(
#'   dat_A,
#'   approach = "causal",
#'   explainer = explainer,
#'   phi0 = y_mean,
#'   ordering = list(1, c(2, 3))
#' )
#' sina_plot(explanation_causal)
#'
#' @seealso \link[SHAPforxgboost]{shap.plot.summary}
#'
#' @details Function adapted from \link[SHAPforxgboost]{shap.plot.summary}.
#' Copyright © 2020 - Yang Liu & Allan Just
#'
sina_plot <- function(explanation, seed = 123) {
  set.seed(seed)

  shapley_values_est <- explanation$shapley_values_est[, -"none", drop = FALSE]
  X_values <- explanation$internal$data$x_explain

  # If we are doing group Shapley, then we compute the mean feature value for each group for each explicand
  if (explanation$internal$parameters$is_groupwise) {
    feature_groups = explanation$internal$parameters$group
    X_values <- X_values[, lapply(feature_groups, function(cols) rowMeans(.SD[, .SD, .SDcols = cols], na.rm = TRUE))]
    #setnames(X_values, names(X_values), paste0(names(X_values), "_mean")) # Rename columns to reflect mean calculations
  }

  data_long <- X_values %>%
    tidyr::pivot_longer(everything()) %>%
    dplyr::bind_cols(
      explanation$shapley_values_est %>%
        dplyr::select(-none) %>%
        tidyr::pivot_longer(everything()) %>%
        dplyr::select(-name) %>%
        dplyr::rename(shap = value)) %>%
    dplyr::mutate(name = factor(name, levels = rev(names(explanation$shapley_values_est)))) %>%
    dplyr::group_by(name) %>%
    dplyr::arrange(name) %>%
    dplyr::mutate(mean_value = mean(value)) %>%
    dplyr::mutate(std_value = (value - min(value)) / (max(value) - min(value)))

  x_bound <- max(abs(max(data_long$shap)), abs(min(data_long$shap)))

  ggplot2::ggplot(data = data_long) +
    ggplot2::coord_flip(ylim = c(-x_bound, x_bound)) +
    ggplot2::geom_hline(yintercept = 0) +
    ggforce::geom_sina(
      ggplot2::aes(x = name, y = shap, color = std_value),
      method = "counts", maxwidth = 0.7, alpha = 0.7
    ) +
    ggplot2::theme_minimal() + ggplot2::theme(
      axis.line.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(),
      legend.position = "top",
      legend.title = ggplot2::element_text(size = 16), legend.text = ggplot2::element_text(size = 14),
      axis.title.y = ggplot2::element_text(size = 16), axis.text.y = ggplot2::element_text(size = 14),
      axis.title.x = ggplot2::element_text(size = 16, vjust = -1), axis.text.x = ggplot2::element_text(size = 14)
    ) +
    ggplot2::scale_color_gradient(
      low = "dark green"  , high = "sandybrown" ,
      breaks = c(0, 1), labels = c(" Low", "High "),
      guide = ggplot2::guide_colorbar(barwidth = 12, barheight = 0.3)
    ) +
    ggplot2::labs(y = "Causal Shapley value (impact on model output)",
                  x = "", color = "Scaled feature value  ")
}


# 0 - Load Packages and Source Files --------------------------------------
library(tidyverse)
library(data.table)
library(xgboost)
library(ggpubr)
library(shapr)
library(ggplot2)
library(grid)
library(gridExtra)

if (save_plots && !dir.exists("figures")) dir.create("figures")

# 1 - Prepare and Plot Data -----------------------------------------------
# Can also download the data set from the source https://archive.ics.uci.edu/dataset/275/bike+sharing+dataset
# temp <- tempfile()
# download.file("https://archive.ics.uci.edu/static/public/275/bike+sharing+dataset.zip", temp)
# bike <- read.csv(unz(temp, "day.csv"))
# unlink(temp)


bike <- read.csv("inst/extdata/day.csv")
# Difference in days, which takes DST into account
bike$trend <- as.numeric(difftime(bike$dteday, bike$dteday[1], units = "days"))
# bike$trend <- as.integer(difftime(bike$dteday, min(as.Date(bike$dteday)))+1)/24
bike$cosyear <- cospi(bike$trend/365*2)
bike$sinyear <- sinpi(bike$trend/365*2)
# Unnormalize variables (see data set information in link above)
bike$temp <- bike$temp * (39 - (-8)) + (-8)
bike$atemp <- bike$atemp * (50 - (-16)) + (-16)
bike$windspeed <- 67 * bike$windspeed
bike$hum <- 100 * bike$hum

bike_plot <- ggplot(bike, aes(x = trend, y = cnt, color = temp)) +
  geom_point(size = 0.75) + scale_color_gradient(low = "blue", high = "red") +
  labs(colour = "temp") +
  xlab( "Days since 1 January 2011") + ylab("Number of bikes rented") +
  theme_minimal() +
  theme(legend.position = "right", legend.title = element_text(size = 10))

if (save_plots) {
  ggsave("figures/bike_rental_plot.pdf", bike_plot, width = 4.5, height = 2)
} else {
  print(bike_plot)
}

x_var <- c("trend", "cosyear", "sinyear", "temp", "atemp", "windspeed", "hum")
y_var <- "cnt"

# NOTE: Encountered RNG reproducibility issues across different systems,
# so we saved the training-test split.
# set.seed(2013)
# train_index <- caret::createDataPartition(bike$cnt, p = .8, list = FALSE, times = 1)
train_index <- readRDS("inst/extdata/train_index.rds")

# Training data
x_train <- as.matrix(bike[train_index, x_var])
y_train_nc <- as.matrix(bike[train_index, y_var]) # not centered
y_train <- y_train_nc - mean(y_train_nc)

# Test data
x_explain <- as.matrix(bike[-train_index, x_var])
y_explain_nc <- as.matrix(bike[-train_index, y_var]) # not centered
y_explain <- y_explain_nc - mean(y_train_nc)

# Fit an XGBoost model to the training data
model <- xgboost(
  data = x_train,
  label = y_train,
  nround = 100,
  verbose = FALSE
)
# caret::RMSE(y_explain, predict(model, x_explain))
sqrt(mean((predict(model, x_explain) - y_explain)^2))
phi0 <- mean(y_train)

message("1. Prepared and plotted data, trained XGBoost model")

# 2 - Compute Shapley Values ----------------------------------------------
progressr::handlers("cli")
explanation_gaussian_time = system.time({
  explanation_gaussian <-
    progressr::with_progress({
      explain(
        model = model,
        x_train = x_train,
        x_explain = x_explain,
        approach = "gaussian",
        phi0 = phi0,
        asymmetric = FALSE,
        causal_ordering = list(1:7),
        confounding = FALSE,
        seed = 2020,
        n_samples = 50,
        keep_samp_for_vS = FALSE
      )
    })
})

saveRDS(list(explanation_asymmetric = explanation_asymmetric,
             time = explanation_asymmetric_time),
        "~/CauSHAPley/inst/extdata/explanation_asymmetric_Olsen.RDS")


## a. We compute the causal symmetric Shapley values on a given partial order (see paper) ####
message("2a. Computing and plotting causal Shapley values")
progressr::handlers("cli")
explanation_causal_time = system.time({
  explanation_causal <-
    progressr::with_progress({
      explain(
        model = model,
        x_train = x_train,
        x_explain = x_explain,
        approach = "gaussian",
        phi0 = phi0,
        asymmetric = FALSE,
        causal_ordering = list(1, c(2, 3), c(4:7)),
        confounding = c(FALSE, TRUE, FALSE),
        seed = 2020,
        n_samples = 50,
        keep_samp_for_vS = FALSE,
        verbose = 2,
      )
    })
})

set.seed(123)
sina_causal <- sina_plot(explanation_causal)
sina_causal

# save limits of sina_causal plot for comparing against marginal and asymmetric
ylim_causal <- sina_causal$coordinates$limits$y

sina_causal = sina_causal +
  coord_flip(ylim = ylim_causal) +
  ylab("Causal Shapley value (impact on model output)")

sina_causal

saveRDS(list(explanation = explanation_causal,
             time = explanation_causal_time,
             plot = sina_causal,
             version = "Causal Shapley values"),
        "inst/extdata/explanation_causal_Olsen.RDS")

if (save_plots) {
  ggsave("figures/sina_plot_causal.pdf", sina_causal, height = 6.5, width = 6.5)
} else {
  print(sina_causal)
}


## b. For computing marginal Shapley values, we assume one component with confounding ####
message("2b. Computing and plotting marginal Shapley values")
explanation_marginal_time = system.time({
  explanation_marginal <-
    progressr::with_progress({
      explain(
        model = model,
        x_train = x_train,
        x_explain = x_explain,
        approach = "independence",
        phi0 = phi0,
        asymmetric = FALSE,
        causal_ordering = list(1:7),
        confounding = FALSE,
        seed = 2020,
        n_samples = 5000,
        keep_samp_for_vS = FALSE
      )
    })
})

set.seed(123)
sina_marginal <- sina_plot(explanation_marginal) +
  coord_flip(ylim = ylim_causal) +
  ylab("Marginal Shapley value (impact on model output)")

sina_marginal

saveRDS(list(explanation = explanation_marginal,
             time = explanation_marginal_time,
             plot = sina_marginal,
             version = "Marginal Shapley values"),
        "~/CauSHAPley/inst/extdata/explanation_marginal_Olsen.RDS")



if (save_plots) {
  ggsave("figures/sina_plot_marginal.pdf", sina_marginal, height = 6.5, width = 6.5)
} else {
  print(sina_marginal)
}




## c. Finally, we compute the asymmetric Shapley values for the same partial order ####
message("2c. Computing and plotting asymmetric conditional Shapley values")

progressr::handlers("cli")
explanation_asymmetric_time = system.time({
  explanation_asymmetric <-
    progressr::with_progress({
      explain(
        model = model,
        x_train = x_train,
        x_explain = x_explain,
        approach = "gaussian",
        phi0 = phi0,
        asymmetric = TRUE,
        causal_ordering = list(1, c(2, 3), c(4:7)),
        confounding = FALSE,
        seed = 2020,
        n_samples = 10000,
        keep_samp_for_vS = FALSE
      )
    })
})
set.seed(123)
sina_asymmetric <- sina_plot(explanation_asymmetric) +
  coord_flip(ylim = ylim_causal) +
  ylab("Asymmetric conditional Shapley value (impact on model output)")

sina_asymmetric

saveRDS(list(explanation = explanation_asymmetric,
             time = explanation_asymmetric_time,
             plot = sina_asymmetric,
             version = "Asymmetric conditional Shapley values"),
        "~/CauSHAPley/inst/extdata/explanation_asymmetric_Olsen.RDS")

if (save_plots) {
  ggsave("figures/sina_plot_asymmetric.pdf", sina_asymmetric, height = 6.5, width = 6.5)
} else {
  print(sina_asymmetric)
}





## d. Asymmetric causal Shapley values (very similar to the conditional ones) ####
message("2d. Computing and plotting asymmetric conditional Shapley values")

progressr::handlers("cli")
explanation_asymmetric_causal_time = system.time({
  explanation_asymmetric_causal <-
    progressr::with_progress({
      explain(
        model = model,
        x_train = x_train,
        x_explain = x_explain,
        approach = "gaussian",
        phi0 = phi0,
        asymmetric = TRUE,
        causal_ordering = list(1, c(2, 3), c(4:7)),
        confounding = c(FALSE, TRUE, FALSE),
        seed = 2020,
        n_samples = 10000,
        keep_samp_for_vS = FALSE
      )
    })
})

set.seed(123)
sina_asymmetric_causal <- sina_plot(explanation_asymmetric_causal) +
  coord_flip(ylim = ylim_causal) +
  ylab("Asymmetric causal Shapley value (impact on model output)")

sina_asymmetric_causal

saveRDS(list(explanation = explanation_asymmetric_causal,
             time = explanation_asymmetric_causal_time,
             plot = sina_asymmetric_causal,
             version = "Asymmetric causal Shapley values"),
        "~/CauSHAPley/inst/extdata/explanation_asymmetric_causal_Olsen.RDS")


if (save_plots) {
  ggsave("figures/sina_plot_asymmetric_causal.pdf", sina_asymmetric_causal, height = 6.5, width = 6.5)
} else {
  print(sina_asymmetric_causal)
}




# 2.5 Compare with old implementation ----
save_explanation_causal = readRDS("~/CauSHAPley/inst/extdata/explanation_causal.RDS")
save_explanation_marginal = readRDS("~/CauSHAPley/inst/extdata/explanation_marginal.RDS")
save_explanation_asymmetric = readRDS("~/CauSHAPley/inst/extdata/explanation_asymmetric.RDS")
save_explanation_asymmetric_causal = readRDS("~/CauSHAPley/inst/extdata/explanation_asymmetric_causal.RDS")

save_explanation_causal_Olsen = readRDS("~/CauSHAPley/inst/extdata/explanation_causal_Olsen.RDS")
save_explanation_marginal_Olsen = readRDS("~/CauSHAPley/inst/extdata/explanation_marginal_Olsen.RDS")
save_explanation_asymmetric_Olsen = readRDS("~/CauSHAPley/inst/extdata/explanation_asymmetric_Olsen.RDS")
save_explanation_asymmetric_causal_Olsen = readRDS("~/CauSHAPley/inst/extdata/explanation_asymmetric_causal_Olsen.RDS")

explanation_causal = save_explanation_causal_Olsen$explanation
explanation_marginal = save_explanation_marginal_Olsen$explanation
explanation_asymmetric = save_explanation_asymmetric_Olsen$explanation
explanation_asymmetric_causal = save_explanation_asymmetric_causal_Olsen$explanation

gridExtra::grid.arrange(save_explanation_causal$plot + ggplot2::ggtitle("Heskes et al. (2020):"),
                        save_explanation_causal_Olsen$plot + ggplot2::ggtitle("SHAPR:"),
                        top = grid::textGrob("Causal Shapley values",
                                             gp = grid::gpar(fontsize=18,font=8)))

# Will be a difference as we use marginal independence and they us marginal Gaussian
gridExtra::grid.arrange(save_explanation_marginal$plot + ggplot2::ggtitle("Heskes et al. (2020):"),
                        save_explanation_marginal_Olsen$plot + ggplot2::ggtitle("SHAPR:"),
                        top = grid::textGrob("Marginal Shapley values",
                                             gp = grid::gpar(fontsize=18,font=8)))

gridExtra::grid.arrange(save_explanation_asymmetric$plot + ggplot2::ggtitle("Heskes et al. (2020):"),
                        save_explanation_asymmetric_Olsen$plot + ggplot2::ggtitle("SHAPR:"),
                        top = grid::textGrob("Asymmetric conditional Shapley values",
                                             gp = grid::gpar(fontsize=18,font=8)))

gridExtra::grid.arrange(save_explanation_asymmetric_causal$plot + ggplot2::ggtitle("Heskes et al. (2020):"),
                        save_explanation_asymmetric_causal_Olsen$plot + ggplot2::ggtitle("SHAPR:"),
                        top = grid::textGrob("Asymmetric causal Shapley values",
                                             gp = grid::gpar(fontsize=18,font=8)))




# 3 - Shapley value scatter plots (Figure 3) ------------------------------
message("3. Producing scatter plots comparing marginal and causal Shapley values on the test set")
sv_correlation_df <- data.frame(
  temp = x_explain[, "temp"],
  sv_marg_cosyear = explanation_marginal$shapley_values_est$cosyear,
  sv_caus_cosyear = explanation_causal$shapley_values_est$cosyear,
  sv_marg_temp = explanation_marginal$shapley_values_est$temp,
  sv_caus_temp = explanation_causal$shapley_values_est$temp
)



scatterplot_topleft <-
  ggplot(sv_correlation_df, aes(x = sv_marg_temp, y = sv_marg_cosyear, color = temp)) +
  geom_point(size = 1)+xlab("MargSV temp")+ylab( "MargSV cosyear")+
  scale_x_continuous(limits = c(-1500, 1000), breaks = c(-1000, 0, 1000)) +
  scale_y_continuous(limits = c(-500, 500), breaks = c(-500, 0, 500))  +
  scale_color_gradient(low="blue", high="red") +
  theme_minimal() +
  theme(text = element_text(size = 12),
        axis.text.x = element_blank(), axis.text.y = element_text(size = 12),
        axis.ticks.x = element_blank(), axis.title.x = element_blank())

scatterplot_topright <-
  ggplot(sv_correlation_df, aes(x = sv_caus_cosyear, y = sv_marg_cosyear, color = temp)) +
  geom_point(size = 1) + scale_color_gradient(low="blue", high="red") +
  xlab("CauSV cosyear") + ylab("MargSV cosyear") +
  scale_x_continuous(limits = c(-1500, 1000), breaks = c(-1000, 0, 1000)) +
  scale_y_continuous(limits = c(-500, 500), breaks = c(-500, 0, 500)) +
  theme_minimal() +
  theme(text = element_text(size=12), axis.title.x = element_blank(), axis.title.y=element_blank(),
        axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank())

scatterplot_bottomleft <-
  ggplot(sv_correlation_df, aes(x = sv_marg_temp, y = sv_caus_temp, color = temp)) +
  geom_point(size = 1) + scale_color_gradient(low="blue", high="red") +
  ylab( "CauSV temp") + xlab("MargSV temp") +
  scale_x_continuous(limits = c(-1500, 1000), breaks = c(-1000, 0, 1000)) +
  scale_y_continuous(limits = c(-1000, 1000), breaks = c(-500, 0, 500))  +
  theme_minimal() +
  theme(text = element_text(size=12),
        axis.text.x = element_text(size=12), axis.text.y = element_text(size=12))

scatterplot_bottomright <-
  ggplot(sv_correlation_df, aes(x = sv_caus_cosyear, y = sv_caus_temp, color = temp)) +
  geom_point(size = 1) + ylab("CauSV temp") + xlab( "CauSV cosyear") +
  scale_x_continuous(limits = c(-1500, 1000), breaks = c(-1000, 0, 1000)) +
  scale_y_continuous(limits = c(-1000, 1000), breaks = c(-500, 0, 500))  +
  scale_color_gradient(low="blue", high="red")+
  theme_minimal() +
  theme(text = element_text(size=12), axis.text.x=element_text(size=12),
        axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())

grid_top <- gridExtra::grid.arrange(scatterplot_topleft, scatterplot_topright, ncol = 2)
grid_bottom <- gridExtra::grid.arrange(scatterplot_bottomleft, scatterplot_bottomright, legend = "none")

grid_top <- ggpubr::ggarrange(scatterplot_topleft, scatterplot_topright, legend = "none")
grid_bottom <- ggpubr::ggarrange(scatterplot_bottomleft, scatterplot_bottomright, legend = "none")

bike_plot <- ggplot(bike, aes(x = trend, y = cnt, color = temp)) +
  geom_point(size = 0.75) + scale_color_gradient(low = "blue", high = "red") +
  labs(colour = "temp") +
  xlab( "Days since 1 January 2011") + ylab("Number of bikes rented") +
  theme_minimal() +
  theme(legend.position = "right", legend.title = element_text(size = 10))

p1 = ggpubr::ggarrange(scatterplot_topleft,
                  scatterplot_topright,
                  scatterplot_bottomleft,
                  scatterplot_bottomright,
                  legend = "none")

ggpubr::ggarrange(bike_plot, p1, nrow = 2, heights = c(1,2))

if (save_plots) {
  ggsave("figures/scatter_plots_top.pdf", grid_top, width = 5, height = 1)
  ggsave("figures/scatter_plots_bottom.pdf", grid_bottom, width = 5, height = 2)
} else {
  print(ggpubr::ggarrange(grid_top, grid_bottom, nrow = 2))
}


# 4 - Shapley value bar plots (Figure 4) ----------------------------------
message("4. Producing bar plots comparing marginal, causal, and asymmetric conditional Shapley values")

# Get test set index for two data points with similar temperature
# 1. 2012-10-09 (October)
# 2. 2012-12-03 (December)
features = c("cosyear", "temp")
dates = c("2012-10-09", "2012-12-03")
dates_idx = sapply(dates, function(data) which(as.integer(row.names(x_explain)) == which(bike$dteday == data)))
# predicted values for the two points
# predict(model, x_explain)[dates_idx] + mean(y_train_nc)

explanations = list("Marginal" = explanation_marginal, "Causal" = explanation_causal)
explanations_extracted = data.table::rbindlist(lapply(seq_along(explanations), function(idx) {
  explanations[[idx]]$shapley_values_est[dates_idx, ..features][, `:=` (Date = dates, type = names(explanations)[idx])]
}))

dt_all = data.table::melt(explanations_extracted, id.vars = c("Date", "type"), variable.name = "feature")
bar_plots <- ggplot(dt_all, aes(x = feature, y = value, group = interaction(Date, feature),
                                fill = Date, label = round(value, 2))) +
  geom_col(position = "dodge") +
  theme_classic() + ylab("Shapley value") +
  facet_wrap(vars(type)) + theme(axis.title.x = element_blank()) +
  scale_fill_manual(values = c('indianred4', 'ivory4')) +
  theme(legend.position.inside = c(0.75, 0.25), axis.title = element_text(size = 20),
        legend.title = element_text(size = 16), legend.text = element_text(size = 14),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),
        strip.text.x = element_text(size = 14))


if (save_plots) {
  ggsave("figures/bar_plots.pdf", bar_plots, width = 6, height = 3)
} else {
  print(bar_plots)
}


plot_SV_several_approaches(explanations, index_explicands = dates_idx, only_these_features = features, facet_ncol = 1,
                           facet_scales = "free_y")



# 5 - Other approaches  -------------------------------------------------------------------------------------------
approaches = c("independence", "empirical", "gaussian", "copula", "ctree", "vaeac")
n_samples_list = list("independence" = 1000,
                      "empirical" = 1000,
                      "gaussian" = 1000,
                      "copula" = 1000,
                      "ctree" = 1000,
                      "vaeac" = 1000)
explanation_list = list()

for (approach_idx in seq_along(approaches)) {

}





progressr::handlers("cli")
explanation_asymmetric_causal_gaussian_time = system.time({
  explanation_asymmetric_causal_gaussian <-
    progressr::with_progress({
      explain(
        model = model,
        x_train = x_train,
        x_explain = x_explain,
        approach = "gaussian",
        phi0 = phi0,
        asymmetric = TRUE,
        causal_ordering = list(1, c(2, 3), c(4:7)),
        confounding = c(FALSE, TRUE, FALSE),
        seed = 2020,
        n_samples = 1000,
        keep_samp_for_vS = FALSE
      )
    })
})

progressr::handlers("cli")
explanation_asymmetric_causal_copula_time = system.time({
  explanation_asymmetric_causal_copula <-
    #progressr::with_progress({
    explain(
      model = model,
      x_train = x_train,
      x_explain = x_explain,
      approach = "copula",
      phi0 = phi0,
      asymmetric = TRUE,
      causal_ordering = list(1, c(2, 3), c(4:7)),
      confounding = c(FALSE, TRUE, FALSE),
      seed = 2020,
      n_samples = 1000,
      keep_samp_for_vS = FALSE
    )
  #})
})

progressr::handlers("cli")
explanation_asymmetric_causal_ctree_time = system.time({
  explanation_asymmetric_causal_ctree <-
    progressr::with_progress({
      explain(
        model = model,
        x_train = x_train,
        x_explain = x_explain,
        approach = "ctree",
        phi0 = phi0,
        asymmetric = TRUE,
        causal_ordering = list(1, c(2, 3), c(4:7)),
        confounding = c(FALSE, TRUE, FALSE),
        seed = 2020,
        n_samples = 500,
        keep_samp_for_vS = FALSE
      )
    })
})


progressr::handlers("cli")
explanation_asymmetric_causal_independence_time = system.time({
  explanation_asymmetric_causal_independence <-
    #progressr::with_progress({
    explain(
      model = model,
      x_train = x_train,
      x_explain = x_explain,
      approach = "independence",
      phi0 = phi0,
      asymmetric = TRUE,
      causal_ordering = list(1, c(2, 3), c(4:7)),
      confounding = c(FALSE, TRUE, FALSE),
      seed = 2020,
      n_samples = 1000,
      keep_samp_for_vS = FALSE
    )
  #})
})

progressr::handlers("cli")
explanation_asymmetric_causal_empirical_time = system.time({
  explanation_asymmetric_causal_empirical <-
    #progressr::with_progress({
    explain(
      model = model,
      x_train = x_train,
      x_explain = x_explain,
      approach = "empirical",
      phi0 = phi0,
      asymmetric = TRUE,
      causal_ordering = list(1, c(2, 3), c(4:7)),
      confounding = c(FALSE, TRUE, FALSE),
      seed = 2020,
      n_samples = 1000,
      keep_samp_for_vS = FALSE
    )
  #})
})

progressr::handlers("cli")
explanation_asymmetric_causal_vaeac_time = system.time({
  explanation_asymmetric_causal_vaeac <-
    #progressr::with_progress({
    explain(
      model = model,
      x_train = x_train,
      x_explain = x_explain,
      approach = "vaeac",
      phi0 = phi0,
      asymmetric = TRUE,
      causal_ordering = list(1, c(2, 3), c(4:7)),
      confounding = c(FALSE, TRUE, FALSE),
      seed = 2020,
      n_samples = 1000,
      keep_samp_for_vS = FALSE,
      verbose = 2
    )
  #})
})

sina_plot(explanation_asymmetric_causal_independence)
sina_plot(explanation_asymmetric_causal_empirical)
sina_plot(explanation_asymmetric_causal_gaussian)
sina_plot(explanation_asymmetric_causal_copula)
sina_plot(explanation_asymmetric_causal_ctree)
sina_plot(explanation_asymmetric_causal_vaeac)















# 6 - Sampled n_combinations --------------------------------------------------------------------------------------
explanation_asymmetric_all_gaussian2 <-
  progressr::with_progress({
    explain(
      model = model,
      x_train = x_train,
      x_explain = x_explain,
      approach = "gaussian",
      phi0 = phi0,
      asymmetric = TRUE,
      causal_ordering = list(1, c(2, 3), c(4:7)),
      confounding = FALSE,
      seed = 2020,
      n_samples = 1000,
      n_combinations = 10,
      keep_samp_for_vS = FALSE,
      n_batches = 1
    )
  })

explanation_asymmetric_all_gaussian$shapley_values_est - explanation_asymmetric_all_gaussian2$shapley_values_est


explanation_asymmetric_all_gaussian$MSEv
explanation_asymmetric_all_gaussian2$MSEv

sina_plot(explanation_asymmetric_all_gaussian)
sina_plot(explanation_asymmetric_all_gaussian2)


explanation_asymmetric_gaussian <-
  progressr::with_progress({
    explain(
      model = model,
      x_train = x_train,
      x_explain = x_explain,
      approach = "gaussian",
      phi0 = phi0,
      asymmetric = TRUE,
      causal_ordering = list(1, c(2, 3), c(4:7)),
      confounding = FALSE,
      seed = 2020,
      n_samples = 1000,
      keep_samp_for_vS = FALSE,
      n_combinations = 10
    )
  })



explanation_asymmetric_causal_gaussian
explanation_asymmetric_causal_gaussian




explanation_causal_time = system.time({
  explanation_causal <-
    progressr::with_progress({
      explain(
        model = model,
        x_train = x_train,
        x_explain = x_explain,
        approach = "gaussian",
        phi0 = phi0,
        asymmetric = TRUE,
        causal_ordering = list(1, c(2, 3), c(4:7)),
        confounding = c(FALSE, TRUE, FALSE),
        seed = 2020,
        n_samples = 5000,
        keep_samp_for_vS = FALSE,
        verbose = 2,
      )
    })
})


explanation_causal_time_sampled = system.time({
  explanation_causal_sampled <-
    progressr::with_progress({
      explain(
        model = model,
        x_train = x_train,
        x_explain = x_explain,
        approach = "gaussian",
        phi0 = phi0,
        asymmetric = TRUE,
        causal_ordering = list(1, c(2, 3), c(4:7)),
        confounding = c(FALSE, TRUE, FALSE),
        seed = 2020,
        n_samples = 5000,
        n_combinations = 10,
        keep_samp_for_vS = FALSE
      )
    })
})

explanation_causal_time
explanation_causal_time_sampled

sina_plot(explanation_causal)
sina_plot(explanation_causal_sampled)




# 7 - Group -------------------------------------------------------------------------------------------------------
# It makes sense to group the "temp" and "atemp" due to their high correlation
cor(x_train[,4], x_train[,5])
plot(x_train[,4], x_train[,5])
pairs(x_train)

group_list <- list(
  trend = "trend",
  cosyear = "cosyear",
  sinyear = "sinyear",
  temp_group = c("temp", "atemp"),
  windspeed = "windspeed",
  hum = "hum")
causal_ordering = list("trend", c("cosyear", "sinyear"), c("temp_group", "windspeed", "hum"))
causal_ordering = list(1, 2:3, 4:6) # Equivalent to using the names (verified)
confounding = c(FALSE, TRUE, FALSE)
asymmetric = TRUE

progressr::handlers("cli")
explanation_group_asymmetric_causal_time = system.time({
  explanation_group_asymmetric_causal <-
    progressr::with_progress({
      explain(
        model = model,
        x_train = x_train,
        x_explain = x_explain,
        approach = "gaussian",
        phi0 = phi0,
        asymmetric = TRUE,
        causal_ordering = list(1, 2:3, 4:6),
        confounding = c(FALSE, TRUE, FALSE),
        group = group_list,
        seed = 2020,
        n_samples = 1000
      )
    })
})

explanation_group_asymmetric_causal$shapley_values_est
sina_plot(explanation_group_asymmetric_causal)

# Now we compute the group Shapley values based on only half of the coalitions
explanation_group_asymmetric_causal_sampled_time = system.time({
  explanation_group_asymmetric_causal_sampled <-
    progressr::with_progress({
      explain(
        model = model,
        x_train = x_train,
        x_explain = x_explain,
        approach = "gaussian",
        phi0 = phi0,
        asymmetric = TRUE,
        causal_ordering = list(1, 2:3, 4:6),
        confounding = confounding,
        group = group_list,
        n_combinations = explanation_group_asymmetric_causal$internal$parameters$n_combinations_causal_max/2 + 1,
        seed = 2020,
        n_samples = 1000
      )
    })
})


# Now we compute the group symmetric causal Shapley values
explanation_group_symmetric_causal_time = system.time({
  explanation_group_symmetric_causal <-
    progressr::with_progress({
      explain(
        model = model,
        x_train = x_train,
        x_explain = x_explain,
        approach = "gaussian",
        phi0 = phi0,
        asymmetric = FALSE,
        causal_ordering = list(1, 2:3, 4:6), #FORTSETT HER MED Å ENDRE OG SE HVA SOM KRÆSJER
        confounding = confounding,
        group = group_list,
        seed = 2020,
        n_samples = 1000
      )
    })
})

explanation_group_symmetric_causal_sampled_time = system.time({
  explanation_group_symmetric_causal_sampled <-
    progressr::with_progress({
      explain(
        model = model,
        x_train = x_train,
        x_explain = x_explain,
        approach = "gaussian",
        phi0 = phi0,
        asymmetric = FALSE,
        causal_ordering = causal_ordering,
        confounding = confounding,
        group = group_list,
        n_combinations = 30,
        seed = 2020,
        n_samples = 1000
      )
    })
})

# Symmetric Conditional
progressr::handlers("cli")
explanation_group_symmetric_conditional_time = system.time({
  explanation_group_symmetric_conditional <-
    progressr::with_progress({
      explain(
        model = model,
        x_train = x_train,
        x_explain = x_explain,
        approach = "gaussian",
        phi0 = phi0,
        asymmetric = FALSE,
        causal_ordering = NULL,
        confounding = FALSE,
        group = group_list,
        seed = 2020,
        n_samples = 1000
      )
    })
})

explanation_group_symmetric_conditional_sampled_time = system.time({
  explanation_group_symmetric_conditional_sampled <-
    progressr::with_progress({
      explain(
        model = model,
        x_train = x_train,
        x_explain = x_explain,
        approach = "gaussian",
        phi0 = phi0,
        asymmetric = FALSE,
        causal_ordering = NULL,
        confounding = FALSE,
        group = group_list,
        n_combinations = 30,
        seed = 2020,
        n_samples = 1000
      )
    })
})

explanation_group_asymmetric_conditional_time = system.time({
  explanation_group_asymmetric_conditional <-
    progressr::with_progress({
      explain(
        model = model,
        x_train = x_train,
        x_explain = x_explain,
        approach = "gaussian",
        phi0 = phi0,
        asymmetric = TRUE,
        causal_ordering = list(seq_along(group_list)),
        confounding = FALSE,
        group = group_list,
        seed = 2020,
        n_samples = 1000
      )
    })
})
explanation_group_asymmetric_conditional$internal$objects$X

explanation_group_asymmetric_causal_time = system.time({
  explanation_group_asymmetric_causal <-
    progressr::with_progress({
      explain(
        model = model,
        x_train = x_train,
        x_explain = x_explain,
        approach = "gaussian",
        phi0 = phi0,
        asymmetric = TRUE,
        causal_ordering = causal_ordering,
        confounding = c(FALSE, TRUE, FALSE),
        group = group_list,
        seed = 2020,
        n_samples = 1000
      )
    })
})
explanation_group_asymmetric_causal$internal$objects$X

explanation_group_asymmetric_conditional$internal$objects$S_causal_strings
explanation_group_asymmetric_causal$internal$objects$S_causal_strings
all.equal(explanation_group_asymmetric_causal$internal$objects$S_causal_strings,
          explanation_group_asymmetric_conditional$internal$objects$S_causal_strings)

explanation_group_asymmetric_conditional_sampled_time = system.time({
  explanation_group_asymmetric_conditional_sampled <-
    progressr::with_progress({
      explain(
        model = model,
        x_train = x_train,
        x_explain = x_explain,
        approach = "gaussian",
        phi0 = phi0,
        asymmetric = TRUE,
        causal_ordering = causal_ordering,
        confounding = FALSE,
        n_combinations = 7,
        group = group_list,
        seed = 2020,
        n_samples = 1000
      )
    })
})


sina_plot(explanation_asymmetric_causal)
sina_plot(explanation_group_asymmetric_causal)
sina_plot(explanation_group_asymmetric_causal_sampled)

n_index_x_explain = 6
index_x_explain = order(y_explain)[seq(1, length(y_explain), length.out = n_index_x_explain)]
plot(explanation_group_asymmetric_causal, index_x_explain = index_x_explain)
plot(explanation_group_asymmetric_causal_sampled, index_x_explain = index_x_explain)

plot(explanation_asymmetric_causal, plot_type = "beeswarm")


plot_SV_several_approaches(list(feature = explanation_asymmetric_causal),
                           index_explicands = index_x_explain)
plot_SV_several_approaches(list(exact = explanation_group_asymmetric_causal,
                                non_exact = explanation_group_asymmetric_causal_sampled),
                           index_explicands = index_x_explain,
                           include_group_feature_means = TRUE)

plot_SV_several_approaches(
  list(
    GrAsymCau_exact = explanation_group_asymmetric_causal,
    GrAsymCau_non_exact = explanation_group_asymmetric_causal_sampled,
    GrSymCau_exact = explanation_group_symmetric_causal,
    GrSymCau_non_exact = explanation_group_symmetric_causal_sampled,
    GrAsymCon_exact = explanation_group_asymmetric_conditional,
    GrAsymCon_non_exact = explanation_group_asymmetric_conditional_sampled,
    GrSymCon_exact = explanation_group_symmetric_conditional,
    GrSymCon_non_exact = explanation_group_symmetric_conditional_sampled
  ),
  index_explicands = index_x_explain,
  brewer_palette = "Paired",
  include_group_feature_means = FALSE)
