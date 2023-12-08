# Setup example ---------------------------------------------------------------------------------------------------
# Load necessary libraries
library(xgboost)
library(data.table)
library(shapr)
library(ggplot2)

# Get the data
data("airquality")
data <- data.table::as.data.table(airquality)
data <- data[complete.cases(data), ]

#' Define the features and the response
x_var <- c("Solar.R", "Wind", "Temp", "Month")
y_var <- "Ozone"

# Split data into test and training data set
ind_x_explain <- 1:25
x_train <- data[-ind_x_explain, ..x_var]
y_train <- data[-ind_x_explain, get(y_var)]
x_explain <- data[ind_x_explain, ..x_var]

# Fitting a basic xgboost model to the training data
model <- xgboost::xgboost(
  data = as.matrix(x_train),
  label = y_train,
  nround = 20,
  verbose = FALSE
)

# Specifying the phi_0, i.e. the expected prediction without any features
prediction_zero <- mean(y_train)

# Independence approach
explanation_independence <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = "independence",
  prediction_zero = prediction_zero,
  n_samples = 1e2
)

# Empirical approach
explanation_empirical <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = "empirical",
  prediction_zero = prediction_zero,
  n_samples = 1e2
)

# Gaussian 1e1 approach
explanation_gaussian_1e1 <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = "gaussian",
  prediction_zero = prediction_zero,
  n_samples = 1e1
)

# Gaussian 1e2 approach
explanation_gaussian_1e2 <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = "gaussian",
  prediction_zero = prediction_zero,
  n_samples = 1e2
)

# ctree approach
explanation_ctree <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = "ctree",
  prediction_zero = prediction_zero,
  n_samples = 1e2
)

# Combined approach
explanation_combined <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = c("gaussian", "independence", "ctree"),
  prediction_zero = prediction_zero,
  n_samples = 1e2
)

# Create a list of explanations without names
explanation_list_unnamed <- list(
  explanation_independence,
  explanation_empirical,
  explanation_gaussian_1e1,
  explanation_gaussian_1e2,
  explanation_ctree,
  explanation_combined
)

# Create a list of explanations with names
explanation_list_named <- list(
  "Ind." = explanation_independence,
  "Emp." = explanation_empirical,
  "Gaus. 1e1" = explanation_gaussian_1e1,
  "Gaus. 1e2" = explanation_gaussian_1e2,
  "Ctree" = explanation_ctree,
  "Combined" = explanation_combined
)



# Plots -----------------------------------------------------------------------------------------------------------
# Create the default MSEv plot
MSEv_figure <- plot_MSEv_eval_crit(explanation_list_named)
MSEv_figure

# For long method names, one can rotate them or put them on different lines (or both)
MSEv_figure + ggplot2::guides(x = ggplot2::guide_axis(angle = 45))
MSEv_figure + ggplot2::guides(x = ggplot2::guide_axis(n.dodge = 2))

# The function sets default names based on the used approach when an unnamed list is provided
plot_MSEv_eval_crit(explanation_list_unnamed) + ggplot2::guides(x = ggplot2::guide_axis(angle = 45))

# Can move the legend around or simply remove it
MSEv_figure +
  ggplot2::theme(legend.position = "bottom") +
  ggplot2::guides(fill = ggplot2::guide_legend(nrow = 2, ncol = 3))
MSEv_figure + ggplot2::theme(legend.position = "none")

# Change the size of the title or simply remove it
MSEv_figure + ggplot2::theme(plot.title = ggplot2::element_text(size = 10))
MSEv_figure + ggplot2::labs(title = NULL)

# Change the theme and color scheme
MSEv_figure + ggplot2::theme_minimal() +
  ggplot2::scale_fill_brewer(palette = "Paired")

# Can add the height of the bars as text. Remove the error bars.
bar_text_n_decimals <- 1
MSEv_figure_wo_CI <- plot_MSEv_eval_crit(explanation_list_named, CI_level = NULL)
MSEv_figure_wo_CI +
  ggplot2::geom_text(
    ggplot2::aes(label = sprintf(
      paste("%.", sprintf("%d", bar_text_n_decimals), "f", sep = ""),
      round(MSEv, bar_text_n_decimals)
    )),
    vjust = 1.75,
    hjust = NA,
    color = "black",
    position = ggplot2::position_dodge(0.9),
    size = 5
  )

# Rotate the plot
MSEv_figure +
  ggplot2::scale_x_discrete(limits = rev(levels(MSEv_figure$data$Method))) +
  ggplot2::coord_flip()

# All of these can be combined
MSEv_figure_wo_CI +
  ggplot2::scale_x_discrete(limits = rev(levels(MSEv_figure_wo_CI$data$Method))) +
  ggplot2::coord_flip() +
  ggplot2::scale_fill_discrete() + #' Default ggplot2 palette
  ggplot2::theme_minimal() + #' This must be set before the other theme call
  ggplot2::theme(
    plot.title = ggplot2::element_text(size = 10),
    legend.position = "bottom"
  ) +
  ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, ncol = 6)) +
  ggplot2::geom_text(
    ggplot2::aes(label = sprintf(
      paste("%.", sprintf("%d", bar_text_n_decimals), "f", sep = ""),
      round(MSEv, bar_text_n_decimals)
    )),
    vjust = NA, # These must be changed for different figure sizes
    hjust = 1.15, # These must be changed for different figure sizes
    color = "black",
    position = ggplot2::position_dodge(0.9),
    size = 5
  )

# or with the CI
MSEv_figure +
  ggplot2::scale_x_discrete(limits = rev(levels(MSEv_figure$data$Method))) +
  ggplot2::coord_flip() +
  ggplot2::scale_fill_discrete() + #' Default ggplot2 palette
  ggplot2::theme_minimal() + #' This must be set before the other theme call
  ggplot2::theme(
    plot.title = ggplot2::element_text(size = 10),
    legend.position = "bottom"
  ) +
  ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, ncol = 6)) +
  ggplot2::geom_text(
    ggplot2::aes(label = sprintf(
      paste("%.", sprintf("%d", bar_text_n_decimals), "f", sep = ""),
      round(MSEv, bar_text_n_decimals)
    )),
    vjust = -1, # These must be changed for different figure sizes
    hjust = 1.15, # These must be changed for different figure sizes
    color = "black",
    position = ggplot2::position_dodge(0.9),
    size = 5
  )



# Can also create plots where we look at the MSEv criterion averaged only over the combinations or observations.
# Note that we can also alter the design of these plots as we did above.
MSEv_figures <- plot_MSEv_eval_crit(
  explanation_list_named,
  plot_type = c("overall", "comb", "explicand"))
MSEv_figures$MSEv_bar
MSEv_figures$MSEv_combination_bar
MSEv_figures$MSEv_explicand_bar

# When there are many combinations or observations, then it can be easier to look at line plots
MSEv_figures$MSEv_combination_line_point
MSEv_figures$MSEv_explicand_line_point

# We can specify which test observations or combinations to plot
plot_MSEv_eval_crit(explanation_list_named,
                    plot_type = "explicand",
  index_x_explain = c(1, 3:4, 6)
)$MSEv_explicand_bar
plot_MSEv_eval_crit(explanation_list_named,
                    plot_type = "comb",
  id_combination = c(3, 4, 9, 13:15)
)$MSEv_combination_bar


# To rotate the combination plot, we need to alter the order of the methods to get them in the same order as before
MSEv_combination <- plot_MSEv_eval_crit(
  explanation_list_named,
  plot_type = "comb",
  id_combination = c(3, 4, 9, 13:15)
)$MSEv_combination_bar
MSEv_combination$data$Method <- factor(MSEv_combination$data$Method, levels = rev(levels(MSEv_combination$data$Method)))
MSEv_combination +
  ggplot2::scale_x_discrete(limits = rev(unique(MSEv_combination$data$id_combination))) +
  ggplot2::scale_fill_discrete(breaks = rev(levels(MSEv_combination$data$Method)), direction = -1) +
  ggplot2::coord_flip()


# Rotate and with text, but without CI
MSEv_combination_wo_CI <- plot_MSEv_eval_crit(
  explanation_list_named,
  plot_type = "comb",
  id_combination = c(3, 4, 9, 13:15),
  CI_level = NULL
)$MSEv_combination_bar
MSEv_combination_wo_CI$data$Method <- factor(MSEv_combination_wo_CI$data$Method,
  levels = rev(levels(MSEv_combination_wo_CI$data$Method))
)
MSEv_combination_wo_CI +
  ggplot2::scale_x_discrete(limits = rev(unique(MSEv_combination_wo_CI$data$id_combination))) +
  ggplot2::scale_fill_brewer(
    breaks = rev(levels(MSEv_combination_wo_CI$data$Method)),
    palette = "Paired",
    direction = -1
  ) +
  ggplot2::coord_flip() +
  ggplot2::theme_minimal() + #' This must be set before the other theme call
  ggplot2::theme(
    plot.title = ggplot2::element_text(size = 10),
    legend.position = "bottom"
  ) +
  ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, ncol = 6)) +
  ggplot2::geom_text(
    ggplot2::aes(
      label = sprintf(
        paste("%.", sprintf("%d", bar_text_n_decimals), "f", sep = ""),
        round(MSEv, bar_text_n_decimals)
      ),
      group = Method
    ),
    hjust = 1.2,
    vjust = NA,
    color = "white",
    position = ggplot2::position_dodge(MSEv_combination_wo_CI$layers[[1]]$geom_params$width),
    size = 3
  )

# Check for same combinations ------------------------------------------------------------------------------------
explanation_gaussian_seed_1 <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = "gaussian",
  prediction_zero = prediction_zero,
  n_samples = 10,
  n_combinations = 10,
  seed = 1
)

explanation_gaussian_seed_1_V2 <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = "gaussian",
  prediction_zero = prediction_zero,
  n_samples = 10,
  n_combinations = 10,
  seed = 1
)

explanation_gaussian_seed_2 <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = "gaussian",
  prediction_zero = prediction_zero,
  n_samples = 10,
  n_combinations = 10,
  seed = 2
)

explanation_gaussian_seed_3 <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = "gaussian",
  prediction_zero = prediction_zero,
  n_samples = 10,
  n_combinations = 10,
  seed = 3
)

# Explanations based on different combinations
explanation_gaussian_seed_1$internal$objects$X$features
explanation_gaussian_seed_2$internal$objects$X$features
explanation_gaussian_seed_3$internal$objects$X$features

# Will give an error due to different combinations
plot_MSEv_eval_crit(list(
  "Seed1" = explanation_gaussian_seed_1,
  "Seed1_V2" = explanation_gaussian_seed_1_V2,
  "Seed2" = explanation_gaussian_seed_2,
  "Seed3" = explanation_gaussian_seed_3
))



# Different explicands --------------------------------------------------------------------------------------------
explanation_gaussian_all <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = "gaussian",
  prediction_zero = prediction_zero,
  n_samples = 10
)

explanation_gaussian_only_5 <- explain(
  model = model,
  x_explain = x_explain[1:5, ],
  x_train = x_train,
  approach = "gaussian",
  prediction_zero = prediction_zero,
  n_samples = 10
)

# Will give an error due to different explicands
plot_MSEv_eval_crit(list(
  "All_explicands" = explanation_gaussian_all,
  "Five_explicands" = explanation_gaussian_only_5
))


# Different feature names ----------------------------------------------------------------------------------------------
explanation_gaussian <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = "gaussian",
  prediction_zero = prediction_zero,
  n_samples = 10
)

explanation_gaussian_copy <- copy(explanation_gaussian_all)
colnames(explanation_gaussian_copy$shapley_values) <- rev(colnames(explanation_gaussian_copy$shapley_values))

# Will give an error due to different feature names
plot_MSEv_eval_crit(list(
  "Original" = explanation_gaussian,
  "Reversed_feature_names" = explanation_gaussian_copy
))



# Missing MSEv ----------------------------------------------------------------------------------------------------
explanation_gaussian <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = "gaussian",
  prediction_zero = prediction_zero,
  n_samples = 10
)

explanation_gaussian_copy <- copy(explanation_gaussian_all)
explanation_gaussian_copy$MSEv <- NULL

# Will give an error due to missing MSEv
plot_MSEv_eval_crit(list(
  "Original" = explanation_gaussian,
  "Missing_MSEv" = explanation_gaussian_copy
))
