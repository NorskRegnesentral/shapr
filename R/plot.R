#' Plot of the Shapley value explanations
#'
#' @description Plots the individual prediction explanations.
#'
#' @param x An `shapr` object.
#'  The output from [explain()].
#' @param plot_type Character.
#'  Specifies the type of plot to produce.
#'  `"bar"` (the default) gives a regular horizontal bar plot of the Shapley value magnitudes.
#'  `"waterfall"` gives a waterfall plot indicating the changes in the prediction score due to each features
#'  contribution (their Shapley values).
#'  `"scatter"` plots the feature values on the x-axis and Shapley values on the y-axis, as well as
#'  (optionally) a background scatter_hist showing the distribution of the feature data.
#'  `"beeswarm"` summarises the distribution of the Shapley values along the x-axis for all the features.
#'  Each point gives the shapley value of a given instance, where the points are colored by the feature value
#'  of that instance.
#' @param digits Integer.
#' Number of significant digits to use in the feature description.
#' Applicable for `plot_type` `"bar"` and `"waterfall"`
#' @param bar_plot_phi0 Logical.
#' Whether to include `phi0` in the plot for  `plot_type = "bar"`.
#' @param index_x_explain Integer vector.
#' Which of the test observations to plot. E.g. if you have
#' explained 10 observations using [explain()], you can generate a plot for the first 5
#' observations by setting `index_x_explain = 1:5`.
#' @param top_k_features Integer.
#' How many features to include in the plot.
#' E.g. if you have 15 features in your model you can plot the 5 most important features,
#' for each explanation, by setting `top_k_features = 1:5`.
#' Applicable for `plot_type` `"bar"` and `"waterfall"`
#' @param col Character vector (length depends on plot type).
#' The color codes (hex codes or other names understood by [ggplot2::ggplot()]) for positive and negative
#' Shapley values, respectively.
#' The default is `col=NULL`, plotting with the default colors respective to the plot type.
#' For `plot_type = "bar"` and `plot_type = "waterfall"`, the default is `c("#00BA38","#F8766D")`.
#' For `plot_type = "beeswarm"`, the default is `c("#F8766D","yellow","#00BA38")`.
#' For `plot_type = "scatter"`, the default is `"#619CFF"`.
#'
#' If you want to alter the colors i the plot, the length of the `col` vector depends on plot type.
#' For `plot_type = "bar"` or `plot_type = "waterfall"`, two colors should be provided, first for positive and
#' then for negative Shapley values.
#' For `plot_type = "beeswarm"`, either two or three colors can be given.
#' If two colors are given, then the first color determines the color that points with high feature values will have,
#' and the second determines the color of points with low feature values.
#' If three colors are given, then the first colors high feature values, the second colors mid-range feature values,
#' and the third colors low feature values.
#' For instance, `col = c("red", "yellow", "blue")` will make high values red, mid-range values yellow,
#' and low values blue.
#' For `plot_type = "scatter"`, a single color is to be given, which determines the color of the points on the
#' scatter plot.
#' @param bar_plot_order Character.
#' Specifies what order to plot the features with respect to the magnitude of the shapley values with
#' `plot_type = "bar"`:
#'  `"largest_first"` (the default) plots the features ordered from largest to smallest absolute Shapley value.
#'  `"smallest_first"` plots the features ordered from smallest to largest absolute Shapley value.
#'  `"original"` plots the features in the original order of the data table.
#' @param scatter_features Integer or character vector.
#' Only used for `plot_type = "scatter"`.
#' Specifies what features to include in (scatter) plot. Can be a numerical vector indicating feature index, or a
#' character vector, indicating the name(s) of the feature(s) to plot.
#' @param scatter_hist Logical.
#' Only used for `plot_type = "scatter"`.
#' Whether to include a scatter_hist indicating the distribution of the data when making the scatter plot. Note that the
#' bins are scaled so that when all the bins are stacked they fit the span of the y-axis of the plot.
#' @param ... Currently not used.
#'
#' @details See the examples below, or `vignette("understanding_shapr", package = "shapr")` for an examples of
#' how you should use the function.
#'
#' @return ggplot object with plots of the Shapley value explanations
#'
#' @export
#' @examples
#'
#' data("airquality")
#' airquality <- airquality[complete.cases(airquality), ]
#' x_var <- c("Solar.R", "Wind", "Temp", "Month")
#' y_var <- "Ozone"
#'
#' # Split data into test- and training data
#' data_train <- head(airquality, -50)
#' data_explain <- tail(airquality, 50)
#'
#' x_train <- data_train[, x_var]
#' x_explain <- data_explain[, x_var]
#'
#' # Fit a linear model
#' lm_formula <- as.formula(paste0(y_var, " ~ ", paste0(x_var, collapse = " + ")))
#' model <- lm(lm_formula, data = data_train)
#'
#' # Explain predictions
#' p <- mean(data_train[, y_var])
#'
#' # Empirical approach
#' x <- explain(
#'   model = model,
#'   x_explain = x_explain,
#'   x_train = x_train,
#'   approach = "empirical",
#'   prediction_zero = p,
#'   n_samples = 1e2
#' )
#'
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   # The default plotting option is a bar plot of the Shapley values
#'   # We draw bar plots for the first 4 observations
#'   plot(x, index_x_explain = 1:4)
#'
#'   # We can also make waterfall plots
#'   plot(x, plot_type = "waterfall", index_x_explain = 1:4)
#'   # And only showing the 2 features with largest contribution
#'   plot(x, plot_type = "waterfall", index_x_explain = 1:4, top_k_features = 2)
#'
#'   # Or scatter plots showing the distribution of the shapley values and feature values
#'   plot(x, plot_type = "scatter")
#'   # And only for a specific feature
#'   plot(x, plot_type = "scatter", scatter_features = "Temp")
#'
#'   # Or a beeswarm plot summarising the Shapley values and feature values for all features
#'   plot(x, plot_type = "beeswarm")
#'   plot(x, plot_type = "beeswarm", col = c("red", "black")) # we can change colors
#' }
#'
#' # Example of scatter and beeswarm plot with factor variables
#' airquality$Month_factor <- as.factor(month.abb[airquality$Month])
#' airquality <- airquality[complete.cases(airquality), ]
#' x_var <- c("Solar.R", "Wind", "Temp", "Month_factor")
#' y_var <- "Ozone"
#'
#' # Split data into test- and training data
#' data_train <- airquality
#' data_explain <- tail(airquality, 50)
#'
#' x_train <- data_train[, x_var]
#' x_explain <- data_explain[, x_var]
#'
#' # Fit a linear model
#' lm_formula <- as.formula(paste0(y_var, " ~ ", paste0(x_var, collapse = " + ")))
#' model <- lm(lm_formula, data = data_train)
#'
#' # Explain predictions
#' p <- mean(data_train[, y_var])
#'
#' # Empirical approach
#' x <- explain(
#'   model = model,
#'   x_explain = x_explain,
#'   x_train = x_train,
#'   approach = "ctree",
#'   prediction_zero = p,
#'   n_samples = 1e2
#' )
#'
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   plot(x, plot_type = "scatter")
#'   plot(x, plot_type = "beeswarm")
#' }
#'
#' @author Martin Jullum, Vilde Ung
plot.shapr <- function(x,
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
  shap_names <- colnames(x$shapley_values)[-1]
  dt_shap <- round(data.table::copy(x$shapley_values), digits = digits)
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
    desc_mat <- trimws(format(x$shapley_values[, -1], digits = digits))
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

  if (plot_type == "scatter") {
    # Only plot the desired observations
    dt_plot <- dt_plot[id %in% index_x_explain]
    gg <- make_scatter_plot(dt_plot, scatter_features, scatter_hist, col, factor_features)
  } else if (plot_type == "beeswarm") {
    gg <- make_beeswarm_plot(dt_plot, col, index_x_explain, x, factor_features)
  } else { # if bar or waterfall plot
    # Only plot the desired observations
    dt_plot <- dt_plot[id %in% index_x_explain]

    if (length(dt_plot[, unique(id)]) > 10) {
      stop("Too many observations to plot together! Try for instance setting index_x_explain = 1:10 so that the max.
           is not exceeded.")
    }

    dt_plot <- order_for_plot(dt_plot, x$internal$parameters$n_features, bar_plot_order, top_k_features)


    # compute start and end values for waterfall rectangles
    data.table::setorder(dt_plot, rank_waterfall)
    dt_plot[, end := cumsum(phi), by = id]
    expected <- x$internal$parameters$prediction_zero
    dt_plot[, start := c(expected, head(end, -1)), by = id]
    dt_plot[, phi_significant := format(phi, digits = digits), by = id]

    # helpers for labelling y-axis correctly
    if (bar_plot_order == "largest_first") {
      desc_labels <- dt_plot[variable != "none" & variable != "rest", description[order(abs(phi))]]
    } else if (bar_plot_order == "smallest_first") {
      desc_labels <- dt_plot[variable != "none" & variable != "rest", description[order(-abs(phi))]]
    } else if (bar_plot_order == "original") {
      desc_labels <- dt_plot[variable != "none" & variable != "rest", description[order(unique_label)]]
    }
    if (top_k_features < x$internal$parameters$n_features) { # if there is a "rest" of collapsed lower-rank features
      desc_labels <- c(
        paste(x$internal$parameters$n_features - top_k_features, "other features"),
        desc_labels
      )
    }
    if (!bar_plot_phi0 || plot_type == "waterfall") { # if none is not to be included in plot
      dt_plot <- dt_plot[variable != "none"]
    } else {
      desc_labels <- c(desc_labels, "None")
    }
    breaks <- levels(droplevels(dt_plot[, unique_label])) # removes -1 if no rest and 0 if no none in plot

    if (plot_type == "bar") {
      gg <- make_bar_plot(dt_plot, bar_plot_phi0, col, breaks, desc_labels)
    } else if (plot_type == "waterfall") {
      gg <- make_waterfall_plot(dt_plot, expected, col, digits, bar_plot_order, breaks, desc_labels)
    }
  }
  return(gg)
}

get_num_breaks <- function(dt_plot, feature_name) {
  n_feat_vals <- length(dt_plot[variable == feature_name, unique(feature_value)]) # number of unique points to plot
  type <- dt_plot[variable == feature_name, type][1]

  if (type == "numeric") {
    if (n_feat_vals > 500) {
      num_breaks <- 50
    } else if (n_feat_vals > 200) {
      num_breaks <- 20
    } else if (n_feat_vals > 100) {
      num_breaks <- 10
    } else {
      num_breaks <- min(5, n_feat_vals + 2)
    }
  } else { # If factor
    num_breaks <- n_feat_vals
  }

  return(num_breaks)
}


compute_scatter_hist_values <- function(dt_plot, scatter_features) {
  dt_scatter_hist_list <- list()
  for (feature_name in scatter_features) {
    num_breaks <- get_num_breaks(dt_plot, feature_name)

    x <- dt_plot[variable == feature_name, feature_value]

    if (min(x) == max(x)) {
      scatter_hist_object <- hist(x, breaks = 1, plot = FALSE)
      # scatter_hist_object$breaks = c(x[1] - .Machine$double.eps*10^10, x[1] + .Machine$double.eps*10^10)
      scatter_hist_object$breaks <- c(x[1] - 0.01, x[1] + 0.01)
    } else {
      step <- (max(x) - min(x)) / (num_breaks - 1)
      scatter_hist_object <- hist(x, breaks = seq(min(x) - step / 2, max(x) + step / 2, by = step), plot = FALSE)
    }

    y_max <- max(dt_plot[variable == feature_name, phi])
    y_min <- min(dt_plot[variable == feature_name, phi])
    y_tot <- ifelse(y_max == y_min, 0.1, y_max - y_min) # what if these happen to be the same...?

    count_tot <- sum(scatter_hist_object$count)
    count_scale <- y_tot / count_tot

    xvals <- scatter_hist_object$breaks
    x_start <- xvals[-length(xvals)]
    x_end <- xvals[-1]
    y_end <- count_scale * scatter_hist_object$count + y_min

    dt_bins <- data.table(
      x_start = x_start,
      x_end = x_end,
      y_end = y_end,
      y_start = y_min,
      variable = feature_name
    )

    dt_scatter_hist_list[[feature_name]] <- dt_bins
  }
  dt_scatter_hist <- data.table::rbindlist(dt_scatter_hist_list)

  return(dt_scatter_hist)
}

make_scatter_plot <- function(dt_plot, scatter_features, scatter_hist, col, factor_cols) {
  if (is.null(col)) {
    col <- "#619CFF"
  } else if (length(col) != 1) {
    stop("'col' must be of length 1 when making scatter plot.")
  }

  dt_plot <- dt_plot[variable != "none"]

  if (is.null(scatter_features)) {
    scatter_features <- unique(dt_plot[, variable])
  } else if (is.numeric(scatter_features)) {
    # i.e. plot first 4 features if scatter_features = 1:4
    scatter_features <- dt_plot[scatter_features, unique(variable)]
  } else if (is.character(scatter_features)) {
    if (any(!(scatter_features %in% unique(dt_plot[, variable])))) {
      stop("Some or all of the listed feature names in 'scatter_features' do not match the names in the data.")
    }
  }

  dt_plot <- dt_plot[variable %in% scatter_features]

  process_data <- process_factor_data(dt_plot, factor_cols)
  dt_plot <- process_data$dt_plot
  lookup <- process_data$lookup
  max_feature_value <- process_data$max_feature_value

  gg <- ggplot2::ggplot(dt_plot) +
    ggplot2::facet_wrap(~variable, scales = "free", labeller = "label_value")

  # compute bin values for scatter_hist

  dt_scatter_hist <- compute_scatter_hist_values(dt_plot, scatter_features)

  # Plot numeric features
  gg <- gg + ggplot2::geom_rect(
    data = dt_scatter_hist,
    ggplot2::aes(
      xmin = x_start, xmax = x_end,
      ymin = y_start, ymax = y_end
    ), fill = ifelse(scatter_hist, "grey85", NA), # NA if no scatter_hist==FALSE
    color = ifelse(scatter_hist, "grey80", NA) # NA if no scatter_hist==FALSE
  )

  gg <- gg + ggplot2::geom_point(ggplot2::aes(x = feature_value, y = phi), colour = col) +
    ggplot2::theme_classic(base_family = "sans") +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = ggplot2::element_text(hjust = 0.5),
      strip.background = ggplot2::element_rect(colour = "white", fill = "grey90"),
      panel.grid.major.y = ggplot2::element_line(colour = "grey90")
    ) +
    ggplot2::labs(
      x = "Feature values",
      y = "Shapley values"
    )

  # Function used by ggplot to map numerical values to the original factor values
  custom_label_func <- function(breaks) {
    breaks <- round(breaks, 3)
    labels <- as.character(breaks)

    factor_breaks <- which(breaks > max_feature_value)
    replace_these_breaks <- which(breaks %in% lookup$breaks)

    if (length(replace_these_breaks) > 0) {
      labels[replace_these_breaks] <- lookup$labels[match(labels[replace_these_breaks], lookup$breaks)]
    }
    if (!identical(factor_breaks, replace_these_breaks)) {
      hide_these_breaks <- factor_breaks[!(factor_breaks %in% replace_these_breaks)]
      labels[hide_these_breaks] <- ""
    }

    return(labels)
  }

  gg <- gg + ggplot2::scale_x_continuous(labels = custom_label_func)

  return(gg)
}

order_for_plot <- function(dt_plot, N_features, bar_plot_order, top_k_features) {
  if (bar_plot_order == "largest_first") {
    dt_plot[variable != "none", rank := data.table::frank(-abs(phi)), by = "id"]
  } else if (bar_plot_order == "smallest_first") {
    dt_plot[variable != "none", rank := data.table::frank(abs(phi)), by = "id"]
  } else if (bar_plot_order == "original") {
    dt_plot[variable != "none", rank := seq_along(phi), by = "id"]
  }
  dt_plot[variable == "none", rank := 0]

  # collapse phi-value for features that are not in top k features
  dt_plot[rank > top_k_features, phi := sum(phi), by = id]
  dt_plot[rank > top_k_features, variable := "rest", by = id]
  dt_plot[variable == "rest", rank := min(rank), by = id]
  dt_plot[variable == "rest", description := paste(N_features - top_k_features, "other features")]
  dt_plot[variable == "rest", sign := ifelse(phi < 0, "Decreases", "Increases")]
  dt_plot <- unique(dt_plot)

  # unique label for correct order when plotting multiple observations
  dt_plot[, unique_label := rev(seq_along(description))]
  dt_plot[variable == "none", unique_label := 0] # such that none is always at top of plot
  dt_plot[variable == "rest", unique_label := -1] # such that rest is always at bottom of plot
  if (bar_plot_order == "largest_first") {
    unique_levels <- c(-1, dt_plot[variable != "none" & variable != "rest", unique_label[order(abs(phi))]], 0)
  } else if (bar_plot_order == "smallest_first") {
    unique_levels <- c(-1, dt_plot[variable != "none" & variable != "rest", unique_label[order(-abs(phi))]], 0)
  } else if (bar_plot_order == "original") {
    unique_levels <- c(-1, rev(dt_plot[variable != "none" & variable != "rest", unique_label]), 0)
  }
  dt_plot[, unique_label := factor(unique_label, levels = unique_levels)]
  if (bar_plot_order == "largest_first") {
    dt_plot[variable != "none", rank_waterfall := data.table::frank(abs(phi)), by = "id"]
  } else if (bar_plot_order == "smallest_first") {
    dt_plot[variable != "none", rank_waterfall := data.table::frank(-abs(phi)), by = "id"]
  } else if (bar_plot_order == "original") {
    dt_plot[variable != "none", rank_waterfall := rev(seq_along(phi)), by = "id"]
  }
  dt_plot[variable == "none", rank_waterfall := 0]

  return(dt_plot)
}


#' Treat factors as numeric values
#'
#' Factors are given a numeric value above the highest numeric value in the data. The value of the different levels
#' are sorted by factor and then level.
#' @param dt data.table to plot
#' @param factor_cols Columns that are factors or character
#' @return A list of a lookup table with each factor and level and its numeric value, a data.table
#' very similar to the input data, but now with numeric values for factors, and the maximum feature value.
process_factor_data <- function(dt, factor_cols) {
  dt_plot_numeric <- dt[!variable %in% factor_cols]
  dt_plot_numeric[, feature_value := as.numeric(feature_value)]
  dt_plot_numeric[, type := "numeric"]

  dt_plot_factor <- dt[variable %in% factor_cols]
  dt_plot_factor[, type := "factor"]
  max_feature_value <- ifelse(nrow(dt_plot_numeric) > 0, ceiling(dt_plot_numeric[, max(feature_value)]) + 1, 0)
  data.table::setnames(dt_plot_factor, "feature_value", "feature_value_factor")
  data.table::setorderv(dt_plot_factor, c("variable", "feature_value_factor"))
  dt_plot_factor[, feature_value := .GRP + max_feature_value, variable]
  dt_plot_factor[, feature_value := feature_value + .GRP / 100, feature_value_factor]

  # A lookup table used later for matching numeric labels with the factor level
  lookup <- unique(dt_plot_factor[, .(feature_value_factor, feature_value)])
  data.table::setnames(lookup, c("feature_value_factor", "feature_value"), c("labels", "breaks"))
  dt_plot_numeric <- rbind(dt_plot_numeric, dt_plot_factor[, mget(names(dt_plot_numeric))])

  return(list(lookup = lookup, dt_plot = dt_plot_numeric, max_feature_value = max_feature_value))
}


make_beeswarm_plot <- function(dt_plot, col, index_x_explain, x, factor_cols) {
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
  process_data <- process_factor_data(dt_plot, factor_cols)
  dt_plot <- process_data$dt_plot

  dt_train <- data.table::copy(x$internal$data$x_train)
  dt_train <- suppressWarnings( # suppress warnings for coercion from int to double or to factor
    data.table::melt(dt_train[, id := .I], id.vars = "id", value.name = "feature_value")
  )
  dt_train <- process_factor_data(dt_train, factor_cols)$dt_plot
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

make_bar_plot <- function(dt_plot, bar_plot_phi0, col, breaks, desc_labels) {
  if (is.null(col)) {
    col <- c("#00BA38", "#F8766D")
  }
  if (length(col) != 2) {
    stop("'col' must be of length 2 when making bar plot.")
  }


  if (!(bar_plot_phi0)) {
    dt_plot <- dt_plot[variable != "none", ]
  }

  # bar plotting helpers
  dt_plot[, y_text_bar := ifelse(abs(phi) > max(abs(phi)) / 8, phi / 2, 0), by = id]
  # text placement depends on the direction of the largest bar, in order for text not to be clipped
  dt_plot[, positive := sign[which.max(abs(phi))] == "Increases", by = id]
  dt_plot[, hjust_text_bar := ifelse(abs(phi) > max(abs(phi)) / 8, 0.5, 1), by = id]
  dt_plot[positive == TRUE & y_text_bar == 0, hjust_text_bar := 0]
  dt_plot[positive == TRUE & y_text_bar == 0, y_text_bar := ifelse(phi > 0, phi, 0)]
  dt_plot[positive == FALSE & y_text_bar == 0, y_text_bar := ifelse(phi < 0, phi, 0)]

  dt_plot[, text_color_bar := ifelse(abs(phi) > max(abs(phi)) / 8, "white", ifelse(sign == "Increases",
    col[1], col[2]
  )), by = id]
  if (bar_plot_phi0) {
    text_color_bar <- dt_plot[, text_color_bar]
  } else {
    text_color_bar <- dt_plot[variable != "none", text_color_bar]
  }

  # make plot
  gg <- ggplot2::ggplot(dt_plot, ggplot2::aes(x = unique_label, fill = sign)) +
    ggplot2::facet_wrap(~header, scales = "free", labeller = "label_value", ncol = 2) +
    ggplot2::theme_classic(base_family = "sans") +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = ggplot2::element_text(hjust = 0.5),
      strip.background = ggplot2::element_rect(colour = "white", fill = "white")
    ) +
    ggplot2::scale_fill_manual(values = col, drop = TRUE) +
    ggplot2::scale_x_discrete(breaks = breaks, labels = desc_labels) +
    ggplot2::geom_col(ggplot2::aes(y = phi)) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      y = "Feature contribution",
      x = "Feature",
      fill = "",
      title = "Shapley value prediction explanation"
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = phi_significant,
        x = unique_label, y = y_text_bar,
        vjust = 0.5, hjust = hjust_text_bar
      ),
      size = 2.5, family = "sans", col = text_color_bar
    )

  return(gg)
}

make_waterfall_plot <- function(dt_plot,
                                expected,
                                col,
                                digits,
                                bar_plot_order,
                                breaks,
                                desc_labels) {
  if (is.null(col)) {
    col <- c("#00BA38", "#F8766D")
  }
  if (length(col) != 2) {
    stop("'col' must be of length 2 when making waterfall plot.")
  }

  # waterfall plotting helpers
  if (bar_plot_order == "largest_first" || bar_plot_order == "original") {
    dt_plot[, y_text := ifelse(abs(phi) < abs(min(start, end) - max(start, end)) / 8,
      ifelse(expected < pred, ifelse(end > start, end, start),
        ifelse(end < start, end, start)
      ),
      start + (end - start) / 2
    ), by = id]
  } else if (bar_plot_order == "smallest_first") {
    dt_plot[, y_text := ifelse(abs(phi) < abs(min(start, end) - max(start, end)) / 8,
      ifelse(expected > pred, ifelse(end > start, end, start),
        ifelse(end < start, end, start)
      ),
      start + (end - start) / 2
    ), by = id]
  }

  dt_plot[, text_color := ifelse(abs(phi) < abs(min(start, end) - max(start, end)) / 8,
    ifelse(sign == "Increases", col[1], col[2]),
    "white"
  ), by = id]
  text_color <- dt_plot[variable != "none", text_color]

  if (bar_plot_order == "largest_first" || bar_plot_order == "original") {
    dt_plot[, hjust_text := ifelse(abs(phi) < abs(min(start, end) - max(start, end)) / 8,
      ifelse(expected > pred, 1, 0), 0.5
    ), by = id]
  } else if (bar_plot_order == "smallest_first") {
    dt_plot[, hjust_text := ifelse(abs(phi) < abs(min(start, end) - max(start, end)) / 8,
      ifelse(expected > pred, 0, 1), 0.5
    ), by = id]
  }

  dt_plot[, arrow_color := ifelse(sign == "Increasing", col[1], col[2])]
  N_features <- max(dt_plot[, rank_waterfall])
  n_obs <- length(dt_plot[, unique(id)])
  dt_plot[, pred_label := paste0("italic(f(x)) == ", format(pred, digits = digits + 1))]
  dt_plot[, pred_x := N_features + 0.8]
  dt_plot[, phi0_label := paste0("~phi[0]==", format(expected, digits = digits + 1))]
  dt_plot[, phi0_x := 0]

  gg <- ggplot2::ggplot(dt_plot, ggplot2::aes(x = unique_label, fill = sign)) +
    ggplot2::facet_wrap(~header, scales = "free", labeller = "label_value", ncol = 2) +
    ggplot2::theme_classic(base_family = "sans") +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = ggplot2::element_text(hjust = 0.5),
      strip.background = ggplot2::element_rect(colour = "white", fill = "white")
    ) +
    ggplot2::scale_fill_manual(values = col, drop = TRUE) +
    ggplot2::scale_x_discrete(breaks = breaks, labels = desc_labels) +
    ggplot2::geom_segment(ggplot2::aes(x = -Inf, xend = max(rank_waterfall) + 0.8, y = pred, yend = pred),
      linetype = "dotted", col = "grey30", linewidth = 0.25
    ) +
    ggplot2::coord_flip(clip = "off", xlim = c(0.5, ifelse(N_features + N_features * 0.11 < N_features + 0.5,
      N_features + 0.5,
      N_features + N_features * 0.11
    ))) +
    ggplot2::labs(
      y = "Prediction",
      x = "Feature",
      fill = "",
      title = "Shapley value prediction explanation"
    ) +
    ggplot2::geom_rect(ggplot2::aes(xmin = rank_waterfall - 0.3, xmax = rank_waterfall + 0.3, ymin = end, ymax = start),
      show.legend = NA
    ) +
    ggplot2::geom_segment(
      x = -Inf, xend = 1.3, y = expected, yend = expected,
      linetype = "dotted", col = "grey30", linewidth = 0.25
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = phi_significant,
        x = rank_waterfall, y = y_text,
        vjust = 0.5, hjust = hjust_text
      ),
      size = 2.5, family = "sans", col = text_color
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = rank_waterfall + 0.45, xend = rank_waterfall + 0.45,
        y = start, yend = end, color = sign
      ),
      arrow = ggplot2::arrow(length = ggplot2::unit(0.03, "npc")), show.legend = FALSE
    ) +
    ggplot2::scale_color_manual(values = col) +
    ggplot2::geom_text(
      data = dt_plot[1:n_obs, ],
      ggplot2::aes(
        x = pred_x, y = pred, label = pred_label,
        vjust = 0, hjust = ifelse(pred > expected, 1, 0)
      ),
      parse = TRUE, family = "sans", col = "grey30", size = 2.5
    ) +
    ggplot2::geom_text(
      data = dt_plot[1:n_obs, ],
      ggplot2::aes(
        x = phi0_x, y = expected, label = phi0_label,
        vjust = 0, hjust = ifelse(pred < expected, 1, 0)
      ),
      parse = TRUE, family = "sans", col = "grey30", size = 2.5
    )

  return(gg)
}
