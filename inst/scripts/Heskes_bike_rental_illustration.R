# Set to true in order to save plots in the main folder
save_plots <- FALSE

# 0 - Load Packages and Source Files --------------------------------------
library(tidyverse)
library(data.table)
library(xgboost)
library(ggpubr)
library(shapr)
library(ggplot2)
library(grid)
library(gridExtra)

# For sina plotting capabilities
source("~/PhD/Paper3/shapr/R/tmp/sina.R")

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
prediction_zero <- mean(y_train)

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
        prediction_zero = prediction_zero,
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
        prediction_zero = prediction_zero,
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
        prediction_zero = prediction_zero,
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
        prediction_zero = prediction_zero,
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
        prediction_zero = prediction_zero,
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
  valtemp = x_explain[, "temp"],
  sv_marg_cosyear = explanation_marginal$shapley_values$cosyear,
  sv_caus_cosyear = explanation_causal$shapley_values$cosyear,
  sv_marg_temp = explanation_marginal$shapley_values$temp,
  sv_caus_temp = explanation_causal$shapley_values$temp
)

scatterplot_topleft <-
  ggplot(sv_correlation_df, aes(x = sv_marg_temp, y = sv_marg_cosyear, color = valtemp)) +
  geom_point(size = 1)+xlab("MSV temp")+ylab( "MSV cosyear")+
  scale_x_continuous(limits = c(-1500, 1000), breaks = c(-1000, 0, 1000)) +
  scale_y_continuous(limits = c(-500, 500), breaks = c(-500, 0, 500))  +
  scale_color_gradient(low="blue", high="red") +
  theme_minimal() +
  theme(text = element_text(size = 12),
        axis.text.x = element_blank(), axis.text.y = element_text(size = 12),
        axis.ticks.x = element_blank(), axis.title.x = element_blank())

scatterplot_topright <-
  ggplot(sv_correlation_df, aes(x = sv_caus_cosyear, y = sv_marg_cosyear, color = valtemp)) +
  geom_point(size = 1) + scale_color_gradient(low="blue", high="red") +
  xlab("CSV cosyear") + ylab("MSV cosyear") +
  scale_x_continuous(limits = c(-1500, 1000), breaks = c(-1000, 0, 1000)) +
  scale_y_continuous(limits = c(-500, 500), breaks = c(-500, 0, 500)) +
  theme_minimal() +
  theme(text = element_text(size=12), axis.title.x = element_blank(), axis.title.y=element_blank(),
        axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank())

scatterplot_bottomleft <-
  ggplot(sv_correlation_df, aes(x = sv_marg_temp, y = sv_caus_temp, color = valtemp)) +
  geom_point(size = 1) + scale_color_gradient(low="blue", high="red") +
  ylab( "CSV temp") + xlab("MSV temp") +
  scale_x_continuous(limits = c(-1500, 1000), breaks = c(-1000, 0, 1000)) +
  scale_y_continuous(limits = c(-1000, 1000), breaks = c(-500, 0, 500))  +
  theme_minimal() +
  theme(text = element_text(size=12),
        axis.text.x = element_text(size=12), axis.text.y = element_text(size=12))

scatterplot_bottomright <-
  ggplot(sv_correlation_df, aes(x = sv_caus_cosyear, y = sv_caus_temp, color = valtemp)) +
  geom_point(size = 1) + ylab("CSV temp") + xlab( "CSV cosyear") +
  scale_x_continuous(limits = c(-1500, 1000), breaks = c(-1000, 0, 1000)) +
  scale_y_continuous(limits = c(-1000, 1000), breaks = c(-500, 0, 500))  +
  scale_color_gradient(low="blue", high="red")+
  theme_minimal() +
  theme(text = element_text(size=12), axis.text.x=element_text(size=12),
        axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())

grid_top <- ggarrange(scatterplot_topleft, scatterplot_topright, legend = "none")
grid_bottom <- ggarrange(scatterplot_bottomleft, scatterplot_bottomright, legend = "none")

if (save_plots) {
  ggsave("figures/scatter_plots_top.pdf", grid_top, width = 5, height = 1)
  ggsave("figures/scatter_plots_bottom.pdf", grid_bottom, width = 5, height = 2)
} else {
  print(ggarrange(grid_top, grid_bottom, nrow = 2))
}


# 4 - Shapley value bar plots (Figure 4) ----------------------------------
message("4. Producing bar plots comparing marginal, causal, and asymmetric conditional Shapley values")

# Get test set index for two data points with similar temperature
# 1. 2012-10-09 (October)
# 2. 2012-12-03 (December)

october <- which(as.integer(row.names(x_explain)) == which(bike$dteday == "2012-10-09"))
december <- which(as.integer(row.names(x_explain)) == which(bike$dteday == "2012-12-03"))

# predicted values for the two points
# predict(model, x_explain)[c(october, december)] + mean(y_train_nc)

dt_marginal <- explanation_marginal$shapley_values %>%
  dplyr::slice(c(october, december)) %>%
  select(cosyear, temp) %>%
  mutate(date = c("2012-10-09", "2012-12-03"), type = 'Marginal')

dt_causal <- explanation_causal$shapley_values %>%
  dplyr::slice(c(october, december)) %>%
  select(cosyear, temp) %>%
  mutate(date = c("2012-10-09", "2012-12-03"), type = 'Causal')

dt_asymmetric <- explanation_asymmetric$shapley_values %>%
  dplyr::slice(c(october, december)) %>%
  select(cosyear, temp) %>%
  mutate(date = c("2012-10-09", "2012-12-03"), type = 'Asymmetric')

dt_all <- dt_marginal %>% pivot_longer(c(cosyear, temp)) %>%
  rbind(dt_causal %>% pivot_longer(c(cosyear, temp))) %>%
  rbind(dt_asymmetric %>% pivot_longer(c(cosyear, temp)))

bar_plots <- ggplot(dt_all, aes(x = name, y = value, group = interaction(date, name),
                                fill = date, label = round(value, 2))) +
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
        prediction_zero = prediction_zero,
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
      prediction_zero = prediction_zero,
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
        prediction_zero = prediction_zero,
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
      prediction_zero = prediction_zero,
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
      prediction_zero = prediction_zero,
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
      prediction_zero = prediction_zero,
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
      prediction_zero = prediction_zero,
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

explanation_asymmetric_all_gaussian$shapley_values - explanation_asymmetric_all_gaussian2$shapley_values


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
      prediction_zero = prediction_zero,
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
        prediction_zero = prediction_zero,
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
        prediction_zero = prediction_zero,
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
        prediction_zero = prediction_zero,
        asymmetric = TRUE,
        causal_ordering = list(1, 2:3, 4:6),
        confounding = c(FALSE, TRUE, FALSE),
        group = group_list,
        seed = 2020,
        n_samples = 1000
      )
    })
})

explanation_group_asymmetric_causal$shapley_values
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
        prediction_zero = prediction_zero,
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
        prediction_zero = prediction_zero,
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
        prediction_zero = prediction_zero,
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
        prediction_zero = prediction_zero,
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
        prediction_zero = prediction_zero,
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
        prediction_zero = prediction_zero,
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
        prediction_zero = prediction_zero,
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
        prediction_zero = prediction_zero,
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
                           groupwise_feature_means = TRUE)

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
  groupwise_feature_means = FALSE)
