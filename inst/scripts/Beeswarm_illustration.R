# Functions -------------------------------------------------------------------------------------------------------
plot_shapr <- function(x,
                       plot_type = "bar",
                       digits = 3,
                       index_x_explain = NULL,
                       top_k_features = NULL,
                       col = NULL, # first increasing color, then decreasing color
                       bar_plot_phi0 = TRUE,
                       bar_plot_order = "largest_first",
                       scatter_features = NULL,
                       scatter_hist = TRUE,
                       ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is not installed. Please run install.packages('ggplot2')")
  }
  if (!(plot_type %in% c("bar", "waterfall", "scatter", "beeswarm"))) {
    stop(paste(plot_type, "is an invalid plot type. Try plot_type='bar', plot_type='waterfall',
               plot_type='scatter', or plot_type='beeswarm'."))
  }
  if (!(bar_plot_order %in% c("largest_first", "smallest_first", "original"))) {
    stop(paste(bar_plot_order, "is an invalid plot order. Try bar_plot_order='largest_first',
               bar_plot_order='smallest_first' or bar_plot_order='original'."))
  }

  if (is.null(index_x_explain)) index_x_explain <- seq(x$internal$parameters$n_explain)
  if (is.null(top_k_features)) top_k_features <- x$internal$parameters$n_features + 1

  is_groupwise <- x$internal$parameters$is_groupwise

  # melting Kshap
  shap_names <- colnames(x$shapley_values_est)[-1]
  dt_shap <- round(data.table::copy(x$shapley_values_est), digits = digits)
  dt_shap[, id := .I]
  dt_shap_long <- data.table::melt(dt_shap, id.vars = "id", value.name = "phi")
  dt_shap_long[, sign := factor(sign(phi), levels = c(1, -1), labels = c("Increases", "Decreases"))]

  # Converting and melting Xtest
  if (!is_groupwise) {
    desc_mat <- trimws(format(x$internal$data$x_explain, digits = digits))
    for (i in seq_len(ncol(desc_mat))) {
      desc_mat[, i] <- paste0(shap_names[i], " = ", desc_mat[, i])
    }
  } else {
    desc_mat <- trimws(format(x$shapley_values_est[, -1], digits = digits))
    for (i in seq_len(ncol(desc_mat))) {
      desc_mat[, i] <- paste0(shap_names[i])
    }
  }

  dt_desc <- data.table::as.data.table(cbind(none = "none", desc_mat))
  dt_desc_long <- data.table::melt(dt_desc[, id := .I], id.vars = "id", value.name = "description")

  # Data table for plotting
  dt_plot <- merge(dt_shap_long, dt_desc_long)

  # Adding the predictions
  dt_pred <- data.table::data.table(id = dt_shap$id, pred = x$pred_explain)
  dt_plot <- merge(dt_plot, dt_pred, by = "id")

  # Adding header for each individual plot
  dt_plot[, header := paste0("id: ", id, ", pred = ", format(pred, digits = digits + 1))]

  if (plot_type == "scatter" || plot_type == "beeswarm") {
    # Add feature values to data table
    dt_feature_vals <- data.table::copy(x$internal$data$x_explain)
    dt_feature_vals <- as.data.table(cbind(none = NA, dt_feature_vals))
    dt_feature_vals[, id := .I]

    # Deal with numeric and factor variables separately
    factor_features <- dt_feature_vals[, sapply(.SD, function(x) is.factor(x) | is.character(x)), .SDcols = shap_names]
    factor_features <- shap_names[factor_features]

    dt_feature_vals_long <- suppressWarnings(data.table::melt(dt_feature_vals,
                                                              id.vars = "id",
                                                              value.name = "feature_value"
    ))
    # this gives a warning because none-values are NA...
    dt_plot <- merge(dt_plot, dt_feature_vals_long, by = c("id", "variable"))
  }

  return(list(dt_plot = dt_plot,
              col = col,
              index_x_explain = index_x_explain, x = x, factor_features = factor_features))
}


make_beeswarm_plot_old <- function(dt_plot, col, index_x_explain, x, factor_cols) {
  if (!requireNamespace("ggbeeswarm", quietly = TRUE)) {
    stop("geom_beeswarm is not installed. Please run install.packages('ggbeeswarm')")
  }

  if (is.null(col)) {
    col <- c("#F8766D", "yellow", "#00BA38")
  }
  if (!(length(col) %in% c(2, 3))) {
    stop("'col' must be of length 2 or 3 when making beeswarm plot.")
  }

  dt_plot <- dt_plot[variable != "none"]

  # Deal with factor variables
  process_data <- shapr:::process_factor_data(dt_plot, factor_cols)
  dt_plot <- process_data$dt_plot

  dt_train <- data.table::copy(x$internal$data$x_train)
  dt_train <- suppressWarnings( # suppress warnings for coercion from int to double or to factor
    data.table::melt(dt_train[, id := .I], id.vars = "id", value.name = "feature_value")
  )
  dt_train <- shapr:::process_factor_data(dt_train, factor_cols)$dt_plot
  dt_train[, `:=`(max = max(feature_value), min = min(feature_value)), by = variable]
  dt_train <- dt_train[, .(variable, max, min)]
  dt_train <- unique(dt_train)
  dt_plot <- merge(dt_plot, dt_train, by = "variable")

  # scale obs. features value to their distance from min. feature value relative to the distance
  # between min. and max. feature value in order to have a global color bar indicating magnitude
  # of obs. feature value.
  # The feature values are scaled wrt the training data
  dt_plot[feature_value <= max & feature_value >= min,
          feature_value_scaled := (feature_value - min) / (max - min),
          by = variable
  ]
  dt_plot[feature_value > max, feature_value_scaled := 1]
  dt_plot[feature_value < min, feature_value_scaled := 0]

  # make sure features with only one value are also scaled
  dt_plot[is.nan(feature_value_scaled), feature_value_scaled := 0.5, by = variable]

  # Only plot the desired observations
  dt_plot <- dt_plot[id %in% index_x_explain]

  # For factor variables, we want one line per factor level
  # Give them a NA feature value to make the color grey
  dt_plot[type == "factor", variable := description]
  dt_plot[type == "factor", feature_value_scaled := NA]

  gg <- ggplot2::ggplot(dt_plot, ggplot2::aes(x = variable, y = phi, color = feature_value_scaled)) +
    ggplot2::geom_hline(yintercept = 0, color = "grey70", linewidth = 0.5) +
    ggbeeswarm::geom_beeswarm(priority = "random", cex = 0.4) +
    # the cex-parameter doesnt generalize well, should use corral but not available yet....
    ggplot2::coord_flip() +
    ggplot2::theme_classic() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_line(colour = "grey90", linetype = "dashed")) +
    ggplot2::labs(x = "", y = "Shapley value") +
    ggplot2::guides(color = ggplot2::guide_colourbar(
      ticks = FALSE,
      barwidth = 0.5, barheight = 10
    ))

  if (length(col) == 3) { # check is col-parameter is the default
    gg <- gg +
      ggplot2::scale_color_gradient2(
        low = col[3], mid = col[2], high = col[1],
        midpoint = 0.5,
        breaks = c(0, 1),
        limits = c(0, 1),
        labels = c("Low", "High"),
        name = "Feature \n value"
      )
  } else if (length(col) == 2) { # allow user to specify three colors
    gg <- gg +
      ggplot2::scale_color_gradient(
        low = col[2],
        high = col[1],
        breaks = c(0, 1),
        limits = c(0, 1),
        labels = c("Low", "High"),
        name = "Feature \n value"
      )
  }

  return(gg)
}

make_beeswarm_plot_new_cex <- function(dt_plot, col, index_x_explain, x, factor_cols) {
  if (!requireNamespace("ggbeeswarm", quietly = TRUE)) {
    stop("geom_beeswarm is not installed. Please run install.packages('ggbeeswarm')")
  }

  if (is.null(col)) {
    col <- c("#F8766D", "yellow", "#00BA38")
  }
  if (!(length(col) %in% c(2, 3))) {
    stop("'col' must be of length 2 or 3 when making beeswarm plot.")
  }

  dt_plot <- dt_plot[variable != "none"]

  # Deal with factor variables
  process_data <- shapr:::process_factor_data(dt_plot, factor_cols)
  dt_plot <- process_data$dt_plot

  dt_train <- data.table::copy(x$internal$data$x_train)
  dt_train <- suppressWarnings( # suppress warnings for coercion from int to double or to factor
    data.table::melt(dt_train[, id := .I], id.vars = "id", value.name = "feature_value")
  )
  dt_train <- shapr:::process_factor_data(dt_train, factor_cols)$dt_plot
  dt_train[, `:=`(max = max(feature_value), min = min(feature_value)), by = variable]
  dt_train <- dt_train[, .(variable, max, min)]
  dt_train <- unique(dt_train)
  dt_plot <- merge(dt_plot, dt_train, by = "variable")

  # scale obs. features value to their distance from min. feature value relative to the distance
  # between min. and max. feature value in order to have a global color bar indicating magnitude
  # of obs. feature value.
  # The feature values are scaled wrt the training data
  dt_plot[feature_value <= max & feature_value >= min,
          feature_value_scaled := (feature_value - min) / (max - min),
          by = variable
  ]
  dt_plot[feature_value > max, feature_value_scaled := 1]
  dt_plot[feature_value < min, feature_value_scaled := 0]

  # make sure features with only one value are also scaled
  dt_plot[is.nan(feature_value_scaled), feature_value_scaled := 0.5, by = variable]

  # Only plot the desired observations
  dt_plot <- dt_plot[id %in% index_x_explain]

  # For factor variables, we want one line per factor level
  # Give them a NA feature value to make the color grey
  dt_plot[type == "factor", variable := description]
  dt_plot[type == "factor", feature_value_scaled := NA]

  gg <- ggplot2::ggplot(dt_plot, ggplot2::aes(x = variable, y = phi, color = feature_value_scaled)) +
    ggplot2::geom_hline(yintercept = 0, color = "grey70", linewidth = 0.5) +
    ggbeeswarm::geom_beeswarm(priority = "random", cex = 1 / length(index_x_explain)^(1/4)) +
    # the cex-parameter doesnt generalize well, should use corral but not available yet....
    ggplot2::coord_flip() +
    ggplot2::theme_classic() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_line(colour = "grey90", linetype = "dashed")) +
    ggplot2::labs(x = "", y = "Shapley value") +
    ggplot2::guides(color = ggplot2::guide_colourbar(
      ticks = FALSE,
      barwidth = 0.5, barheight = 10
    ))

  if (length(col) == 3) { # check is col-parameter is the default
    gg <- gg +
      ggplot2::scale_color_gradient2(
        low = col[3], mid = col[2], high = col[1],
        midpoint = 0.5,
        breaks = c(0, 1),
        limits = c(0, 1),
        labels = c("Low", "High"),
        name = "Feature \n value"
      )
  } else if (length(col) == 2) { # allow user to specify three colors
    gg <- gg +
      ggplot2::scale_color_gradient(
        low = col[2],
        high = col[1],
        breaks = c(0, 1),
        limits = c(0, 1),
        labels = c("Low", "High"),
        name = "Feature \n value"
      )
  }

  return(gg)
}

make_beeswarm_plot_new <- function(dt_plot, col, index_x_explain, x, factor_cols,
                                   corral.method = "swarm",
                                   corral.corral = "wrap",
                                   corral.priority = "random",
                                   corral.width = 0.75,
                                   corral.cex = 0.75) {
  if (!requireNamespace("ggbeeswarm", quietly = TRUE)) {
    stop("geom_beeswarm is not installed. Please run install.packages('ggbeeswarm')")
  }

  if (is.null(col)) {
    col <- c("#F8766D", "yellow", "#00BA38")
  }
  if (!(length(col) %in% c(2, 3))) {
    stop("'col' must be of length 2 or 3 when making beeswarm plot.")
  }

  dt_plot <- dt_plot[variable != "none"]

  # Deal with factor variables
  process_data <- shapr:::process_factor_data(dt_plot, factor_cols)
  dt_plot <- process_data$dt_plot

  dt_train <- data.table::copy(x$internal$data$x_train)
  dt_train <- suppressWarnings( # suppress warnings for coercion from int to double or to factor
    data.table::melt(dt_train[, id := .I], id.vars = "id", value.name = "feature_value")
  )
  dt_train <- shapr:::process_factor_data(dt_train, factor_cols)$dt_plot
  dt_train[, `:=`(max = max(feature_value), min = min(feature_value)), by = variable]
  dt_train <- dt_train[, .(variable, max, min)]
  dt_train <- unique(dt_train)
  dt_plot <- merge(dt_plot, dt_train, by = "variable")

  # scale obs. features value to their distance from min. feature value relative to the distance
  # between min. and max. feature value in order to have a global color bar indicating magnitude
  # of obs. feature value.
  # The feature values are scaled wrt the training data
  dt_plot[feature_value <= max & feature_value >= min,
          feature_value_scaled := (feature_value - min) / (max - min),
          by = variable
  ]
  dt_plot[feature_value > max, feature_value_scaled := 1]
  dt_plot[feature_value < min, feature_value_scaled := 0]

  # make sure features with only one value are also scaled
  dt_plot[is.nan(feature_value_scaled), feature_value_scaled := 0.5, by = variable]

  # Only plot the desired observations
  dt_plot <- dt_plot[id %in% index_x_explain]

  # For factor variables, we want one line per factor level
  # Give them a NA feature value to make the color grey
  dt_plot[type == "factor", variable := description]
  dt_plot[type == "factor", feature_value_scaled := NA]

  gg <- ggplot2::ggplot(dt_plot, ggplot2::aes(x = variable, y = phi, color = feature_value_scaled)) +
    ggplot2::geom_hline(yintercept = 0, color = "grey70", linewidth = 0.5) +
    ggbeeswarm::geom_beeswarm(method = corral.method,
                              corral = corral.corral,
                              priority = corral.priority,
                              corral.width = corral.width,
                              cex = corral.cex) +
    ggplot2::coord_flip() +
    ggplot2::theme_classic() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_line(colour = "grey90", linetype = "dashed")) +
    ggplot2::labs(x = "", y = "Shapley value") +
    ggplot2::guides(color = ggplot2::guide_colourbar(
      ticks = FALSE,
      barwidth = 0.5, barheight = 10
    ))

  if (length(col) == 3) { # check is col-parameter is the default
    gg <- gg +
      ggplot2::scale_color_gradient2(
        low = col[3], mid = col[2], high = col[1],
        midpoint = 0.5,
        breaks = c(0, 1),
        limits = c(0, 1),
        labels = c("Low", "High"),
        name = "Feature \n value"
      )
  } else if (length(col) == 2) { # allow user to specify three colors
    gg <- gg +
      ggplot2::scale_color_gradient(
        low = col[2],
        high = col[1],
        breaks = c(0, 1),
        limits = c(0, 1),
        labels = c("Low", "High"),
        name = "Feature \n value"
      )
  }

  return(gg)
}

make_beeswarm_plot_paper3 <- function(dt_plot, col, index_x_explain, x, factor_cols) {
  if (!requireNamespace("ggbeeswarm", quietly = TRUE)) {
    stop("geom_beeswarm is not installed. Please run install.packages('ggbeeswarm')")
  }

  if (is.null(col)) {
    col <- c("#F8766D", "yellow", "#00BA38")
  }
  if (!(length(col) %in% c(2, 3))) {
    stop("'col' must be of length 2 or 3 when making beeswarm plot.")
  }

  dt_plot <- dt_plot[variable != "none"]

  # Deal with factor variables
  process_data <- shapr:::process_factor_data(dt_plot, factor_cols)
  dt_plot <- process_data$dt_plot

  dt_train <- data.table::copy(x$internal$data$x_train)
  dt_train <- suppressWarnings( # suppress warnings for coercion from int to double or to factor
    data.table::melt(dt_train[, id := .I], id.vars = "id", value.name = "feature_value")
  )
  dt_train <- shapr:::process_factor_data(dt_train, factor_cols)$dt_plot
  dt_train[, `:=`(max = max(feature_value), min = min(feature_value)), by = variable]
  dt_train <- dt_train[, .(variable, max, min)]
  dt_train <- unique(dt_train)
  dt_plot <- merge(dt_plot, dt_train, by = "variable")

  # scale obs. features value to their distance from min. feature value relative to the distance
  # between min. and max. feature value in order to have a global color bar indicating magnitude
  # of obs. feature value.
  # The feature values are scaled wrt the training data
  dt_plot[feature_value <= max & feature_value >= min,
          feature_value_scaled := (feature_value - min) / (max - min),
          by = variable
  ]
  dt_plot[feature_value > max, feature_value_scaled := 1]
  dt_plot[feature_value < min, feature_value_scaled := 0]

  # make sure features with only one value are also scaled
  dt_plot[is.nan(feature_value_scaled), feature_value_scaled := 0.5, by = variable]

  # Only plot the desired observations
  dt_plot <- dt_plot[id %in% index_x_explain]

  # For factor variables, we want one line per factor level
  # Give them a NA feature value to make the color grey
  dt_plot[type == "factor", variable := description]
  dt_plot[type == "factor", feature_value_scaled := NA]

  gg <- ggplot2::ggplot(dt_plot, ggplot2::aes(x = variable, y = phi, color = feature_value_scaled)) +
    ggplot2::geom_hline(yintercept = 0, color = "grey60", linewidth = 0.5) +
    #ggbeeswarm::geom_beeswarm(priority = "random", cex = 0.1) +
    ggbeeswarm::geom_beeswarm(corral = "wrap", priority = "random", corral.width = 0.75) +
    # the cex-parameter doesnt generalize well, should use corral but not available yet....
    ggplot2::coord_flip() +
    #ggplot2::theme_classic() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_line(colour = "grey75", linetype = "dashed")) +
    ggplot2::labs(x = "", y = "Shapley value") +
    ggplot2::guides(color = ggplot2::guide_colourbar(
      ticks = FALSE,
      #barwidth = 0.5, barheight = 10
      barwidth = 10, barheight = 0.5
    ))

  if (length(col) == 3) { # check is col-parameter is the default
    gg <- gg +
      ggplot2::scale_color_gradient2(
        low = col[3], mid = col[2], high = col[1],
        midpoint = 0.5,
        breaks = c(0, 1),
        limits = c(0, 1),
        labels = c("       Low", "High       "),
        name = "Feature value: "
      ) +
      theme(legend.position = 'bottom') +
      guides(fill = guide_legend(nrow = 1))
  } else if (length(col) == 2) { # allow user to specify three colors
    gg <- gg +
      ggplot2::scale_color_gradient(
        low = col[2],
        high = col[1],
        breaks = c(0, 1),
        limits = c(0, 1),
        labels = c("Low", "High"),
        name = "Feature \n value"
      )
  }

  return(gg)
}

# Run code from here ----------------------------------------------------------------------------------------------
# Load necessary library
library(shapr)
library(xgboost)
library(data.table)
library(MASS)
library(ggplot2)
library(ggpubr)

# Parameters
M <- 10  # Number of dimensions
N_train <- 1000  # Number of training observations
N_explain <- 5000  # Number of test observations
mu <- rep(0, M)  # Mean vector, for example, a zero vector
rho <- 0.5  # Correlation coefficient (must be between -1 and 1)
beta = matrix(c(1, -2, 2, 0.5, 1.5, 0.25, 0.75, -0.5, 1, -2)[1:M])

# Construct the equi-correlation matrix
cov_matrix <- matrix(rho, nrow = M, ncol = M)
diag(cov_matrix) <- 1  # Set diagonal to 1

# Generate N observations from the multivariate normal distribution
set.seed(123)  # Set seed for reproducibility
x_train <- mvrnorm(N_train, mu, cov_matrix)
x_explain <- mvrnorm(N_explain, mu, cov_matrix)

y_train <- x_train %*% beta + rnorm(N_train, sd = 1)
y_explain <- x_explain %*% beta + rnorm(N_explain, sd = 1)

x_train = as.data.table(x_train)
x_explain = as.data.table(x_explain)

# Fitting a basic xgboost model to the training data
model <- xgboost::xgboost(
  data = as.matrix(x_train),
  label = y_train,
  nround = 20,
  verbose = FALSE
)

# Specifying the phi_0, i.e. the expected prediction without any features
p0 <- mean(y_train)

# Computing the actual Shapley values with kernelSHAP accounting for feature dependence using
# the empirical (conditional) distribution approach with bandwidth parameter sigma = 0.1 (default)
explanation <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = "gaussian",
  prediction_zero = p0,
  max_n_coalitions = 10, # Do not need precise Shapley values to illustrate the behaviour of beeswarm plot
  n_MC_samples = 10 # Do not need precise Shapley values to illustrate the behaviour of beeswarm plot
)

# Get the objects needed to make the beeswarm plot
tmp_list = plot_shapr(explanation, plot_type = "beeswarm")


## Plots -----------------------------------------------------------------------------------------------------------
# Make the old and new beeswarm plot
list_figures = lapply(c(50, 100, 1000, 5000), function(N_explain_plot) {
  # Old version have problem with runaway points: see https://github.com/eclarke/ggbeeswarm?tab=readme-ov-file#corral-runaway-points
  gg_old <- make_beeswarm_plot_old(dt_plot = tmp_list$dt_plot,
                                   col = tmp_list$col,
                                   index_x_explain = tmp_list$index_x_explain[seq(N_explain_plot)],
                                   x = tmp_list$x,
                                   factor_cols = tmp_list$factor_features)

  gg_new_cex <- make_beeswarm_plot_new_cex(dt_plot = tmp_list$dt_plot,
                                           col = tmp_list$col,
                                           index_x_explain = tmp_list$index_x_explain[seq(N_explain_plot)],
                                           x = tmp_list$x,
                                           factor_cols = tmp_list$factor_features)

  gg_new <- make_beeswarm_plot_new(dt_plot = tmp_list$dt_plot,
                                   col = tmp_list$col,
                                   index_x_explain = tmp_list$index_x_explain[seq(N_explain_plot)],
                                   x = tmp_list$x,
                                   factor_cols = tmp_list$factor_features,
                                   corral.corral  = "wrap", # Default. Other options: "none" (default in geom_beeswarm), "gutter", "random", "omit"
                                   corral.method = "swarm", # Default (and default in geom_beeswarm). Other options: "compactswarm", "hex", "square", "center
                                   corral.priority = "random", # Default . Other options: "ascending" (default in geom_beeswarm), "descending", "density"
                                   corral.width = 0.75, # Default. 0.9 is default in geom_beeswarm
                                   corral.cex = 0.75) # Default. 1 is default in geom_beeswarm

  gg_paper3 <- make_beeswarm_plot_paper3(dt_plot = tmp_list$dt_plot,
                                         col = tmp_list$col,
                                         index_x_explain = tmp_list$index_x_explain[seq(N_explain_plot)],
                                         x = tmp_list$x,
                                         factor_cols = tmp_list$factor_features)
  return(ggpubr::ggarrange(gg_old, gg_new_cex, gg_new, gg_paper3, labels = c("Old", "New_cex", "New", "Paper3"), nrow = 1, vjust = 2))
})


# 50
list_figures[[1]]

# 100
list_figures[[2]]

# 1000
list_figures[[3]]

# 5000
list_figures[[4]]

# Plot them together
ggpubr::ggarrange(list_figures[[1]], list_figures[[2]], list_figures[[3]], list_figures[[4]], labels = c(50, 100, 1000, 5000), ncol = 1, vjust = 1)
