# Libraries
# library(ggplot2)
# require(GGally)
# library(ggpubr)
# library(gridExtra)

# Libraries
library(xgboost)
library(shapr)

# Download and set up the data as done in Heskes et al. (2020)
temp <- tempfile()
download.file("https://archive.ics.uci.edu/static/public/275/bike+sharing+dataset.zip", temp)
bike <- read.csv(unz(temp, "day.csv"))
unlink(temp)
# Difference in days, which takes DST into account
bike$trend <- as.numeric(difftime(bike$dteday, bike$dteday[1], units = "days"))
bike$cosyear <- cospi(bike$trend / 365 * 2)
bike$sinyear <- sinpi(bike$trend / 365 * 2)
# Unnormalize variables (see data set information in link above)
bike$temp <- bike$temp * (39 - (-8)) + (-8)
bike$atemp <- bike$atemp * (50 - (-16)) + (-16)
bike$windspeed <- 67 * bike$windspeed
bike$hum <- 100 * bike$hum

# Define the features and the response variable
x_var <- c("trend", "cosyear", "sinyear", "temp", "atemp", "windspeed", "hum")
y_var <- "cnt"

# Training-test split. 80% training and 20% test
set.seed(123)
train_index <- sample(x = nrow(bike), size = round(0.8*nrow(bike)))

# Training data
x_train <- as.matrix(bike[train_index, x_var])
y_train_nc <- as.matrix(bike[train_index, y_var]) # not centered
y_train <- y_train_nc - mean(y_train_nc)

# Test/explicand data
x_explain <- as.matrix(bike[-train_index, x_var])
y_explain_nc <- as.matrix(bike[-train_index, y_var]) # not centered
y_explain <- y_explain_nc - mean(y_train_nc)

# Fit an XGBoost model to the training data
model <- xgboost::xgboost(data = x_train, label = y_train, nround = 100, verbose = FALSE)

# Compute the phi0
prediction_zero <- mean(y_train)

# Specify the causal ordering and confounding
causal_ordering <- list("trend", c("cosyear", "sinyear"), c("temp", "atemp", "windspeed", "hum"))
confounding <- c(FALSE, TRUE, FALSE)

# Symmetric causal Shapley values: change asymmetric, causal_ordering, and confounding for other versions
explanation_sym_cau <- explain(
  model = model,
  x_train = x_train,
  x_explain = x_explain,
  prediction_zero = prediction_zero,
  n_MC_samples = 100, # Just for speed
  approach = "gaussian",
  asymmetric = FALSE,
  paired_shap_sampling = TRUE, # Paired sampling is default, but must be FALSE for asymmetric SV
  causal_ordering = causal_ordering,
  confounding = confounding
)


# Symmetric Shapley values
explanation_sym_con <- explain(
  model = model,
  x_train = x_train,
  x_explain = x_explain,
  approach = "gaussian",
  prediction_zero = prediction_zero,
  n_MC_samples = 1000,
  verbose = c("basic", "progress", "convergence", "shapley", "vS_details")
  # asymmetric = FALSE, # Default value (TRUE will give the same since `causal_ordering = NULL`)
  # causal_ordering = NULL, # Default value
  # confounding = NULL # Default value
)

# Asymmetric Shapley values
explanation_asym_con <- explain(
  model = model,
  x_train = x_train,
  x_explain = x_explain,
  prediction_zero = prediction_zero,
  n_MC_samples = 1000,
  approach = "gaussian",
  paired_shap_sampling = FALSE,
  asymmetric = TRUE,
  causal_ordering = causal_ordering,
  confounding = NULL, # Default value,
  verbose = c("basic", "progress", "convergence", "shapley", "vS_details")
)

# Asymmetric causal Shapley values
explanation_asym_cau <- explain(
  model = model,
  x_train = x_train,
  x_explain = x_explain,
  prediction_zero = prediction_zero,
  n_MC_samples = 1000,
  approach = "gaussian",
  paired_shap_sampling = FALSE,
  asymmetric = TRUE,
  causal_ordering = causal_ordering,
  confounding = confounding
)

# Symmetric marginal Shapley values
explanation_sym_marg <- explain(
  model = model,
  x_train = x_train,
  x_explain = x_explain,
  prediction_zero = prediction_zero,
  n_MC_samples = 1000,
  approach = "gaussian",
  asymmetric = FALSE,
  causal_ordering = list(1:7),
  confounding = TRUE
)

# Asymmetric marginal Shapley values
explanation_asym_marg <- explain(
  model = model,
  x_train = x_train,
  x_explain = x_explain,
  prediction_zero = prediction_zero,
  n_MC_samples = 1000,
  approach = "gaussian",
  paired_shap_sampling = FALSE,
  asymmetric = TRUE,
  causal_ordering = list(1:7),
  confounding = TRUE
)


# Combine the explanations
explanation_list = list("Symmetric conditional" = explanation_sym_con,
                        "Asymmetric conditional" = explanation_asym_con,
                        "Symmetric causal" = explanation_sym_cau,
                        "Asymmetric causal" = explanation_asym_cau,
                        "Symmetric marginal" = explanation_sym_marg,
                        "Asymmetric marginal" = explanation_asym_marg)

# Make the beeswarm plots
grobs <- lapply(seq(length(explanation_list)), function(explanation_idx) {
  gg <- plot(explanation_list[[explanation_idx]], plot_type = "beeswarm") +
    ggplot2::ggtitle(gsub("_", " ", names(explanation_list)[[explanation_idx]]))
  # ggplot2::ggtitle(tools::toTitleCase(gsub("_", " ", names(explanation_list)[[explanation_idx]])))

  # Flip the order such that the features comes in the right order
  gg <- gg +
    ggplot2::scale_x_discrete(limits = rev(levels(gg$data$variable)[levels(gg$data$variable) != "none"]))
})

# Get the limits
ylim <- sapply(grobs, function(grob) ggplot2::ggplot_build(grob)$layout$panel_scales_y[[1]]$range$range)
ylim <- c(min(ylim), max(ylim))

# Update the limits
grobs <- suppressMessages(lapply(grobs, function(grob) grob + ggplot2::coord_flip(ylim = ylim)))

# THE PLOT IN THE PAPER
fig_few2 = ggpubr::ggarrange(grobs[[2]], grobs[[3]], grobs[[5]],
                             ncol=3, nrow=1, common.legend = TRUE, legend="right")
ggsave(filename = "/Users/larsolsen/Downloads/Paper5_example_fig_fewer_other.png",
       plot = fig_few2,
       scale = 0.85,
       width = 14,
       height = 4)


# OTHER PLOTS
# All 6 versions
fig = ggpubr::ggarrange(grobs[[1]], grobs[[3]], grobs[[5]], grobs[[2]], grobs[[4]], grobs[[6]],
                        ncol=3, nrow=2, common.legend = TRUE, legend="right")

ggsave(filename = "/Users/larsolsen/Downloads/Paper5_example_fig.png", plot = fig,
       scale = 0.85,
       width = 14,
       height = 6)

# Only 3 of them
fig_few = ggpubr::ggarrange(grobs[[1]], grobs[[2]], grobs[[3]],
                            ncol=3, nrow=1, common.legend = TRUE, legend="right")
ggsave(filename = "/Users/larsolsen/Downloads/Paper5_example_fig_fewer.png",
       plot = fig_few,
       scale = 0.85,
       width = 14,
       height = 4)

# Other four
fig_few3 = ggpubr::ggarrange(grobs[[1]], grobs[[3]], grobs[[2]], grobs[[5]],
                             ncol=2, nrow=2, common.legend = TRUE, legend="right")
ggsave(filename = "/Users/larsolsen/Downloads/Paper5_example_fig_fewer_other_other_2.png",
       plot = fig_few3,
       scale = 0.85,
       width = 15,
       height = 6)
