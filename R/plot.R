#' Plot of the Shapley Value Explanations
#'
#' @description Plots the individual prediction explanations.
#'
#' @param x An `shapr` object.
#'  The output from [explain()].
#' @param plot_type Character.
#'  Specifies the type of plot to produce.
#'  `"bar"` (the default) gives a regular horizontal bar plot of the Shapley value magnitudes.
#'  `"waterfall"` gives a waterfall plot indicating the changes in the prediction score due to each feature's
#'  contribution (their Shapley values).
#'  `"scatter"` plots the feature values on the x-axis and Shapley values on the y-axis, as well as
#'  (optionally) a background scatter_hist showing the distribution of the feature data.
#'  `"beeswarm"` summarizes the distribution of the Shapley values along the x-axis for all the features.
#'  Each point gives the Shapley value of a given instance, where the points are colored by the feature value
#'  of that instance.
#' @param digits Integer.
#' Number of significant digits to use in the feature description.
#' Applicable for `plot_type` `"bar"` and `"waterfall"`
#' @param print_ggplot Logical.
#' Whether to print the created `ggplot` object once it is returned.
#' The default is `TRUE` which ensures the plot is always displayed also in loops, functions, when sourcing a script,
#' and when assigning the output to a variable like `p <- plot.shapr(...)`.
#' See [ggplot2::print.ggplot()] for more details.
#' If you wish to further modify the returned `ggplot` object outside of `plot.shapr`,
#' we recommend setting `print_ggplot = FALSE` to avoid force printing.
#' See the examples for a practical use case.
#' @param bar_plot_phi0 Logical.
#' Whether to include `phi0` in the plot for  `plot_type = "bar"`.
#' @param index_x_explain Integer vector.
#' Which of the test observations to plot. For example, if you have
#' explained 10 observations using [explain()], you can generate a plot for the first five
#' observations by setting `index_x_explain = 1:5`.
#' Defaults to the first 10 observations.
#' @param top_k_features Integer.
#' How many features to include in the plot.
#' E.g. if you have 15 features in your model you can plot the 5 most important features,
#' for each explanation, by setting `top_k_features = 1:5`.
#' Applicable for `plot_type` `"bar"` and `"waterfall"`
#' @param col Character vector (where length depends on plot type).
#' The color codes (hex codes or other names understood by [ggplot2::ggplot()]) for positive and negative
#' Shapley values, respectively.
#' The default is `col=NULL`, plotting with the default colors respective to the plot type.
#' For `plot_type = "bar"` and `plot_type = "waterfall"`, the default is `c("#00BA38","#F8766D")`.
#' For `plot_type = "beeswarm"`, the default is `c("#F8766D","yellow","#00BA38")`.
#' For `plot_type = "scatter"`, the default is `"#619CFF"`.
#'
#' If you want to alter the colors in the plot, the length of the `col` vector depends on plot type.
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
#' Specifies what order to plot the features with respect to the magnitude of the Shapley values with
#' `plot_type = "bar"`:
#'  `"largest_first"` (the default) plots the features ordered from largest to smallest absolute Shapley value.
#'  `"smallest_first"` plots the features ordered from smallest to largest absolute Shapley value.
#'  `"original"` plots the features in the original order of the data table.
#' @param scatter_features Integer or character vector.
#' Only used for `plot_type = "scatter"`.
#' Specifies which features to include in the scatter plot. Can be a numerical vector indicating feature index, or a
#' character vector, indicating the name(s) of the feature(s) to plot.
#' @param scatter_hist Logical.
#' Only used for `plot_type = "scatter"`.
#' Whether to include a scatter_hist indicating the distribution of the data when making the scatter plot. Note
#' that the bins are scaled so that when all the bins are stacked they fit the span of the y-axis of the plot.
#' @param include_group_feature_means Logical.
#' Whether to include the average feature value in a group on the y-axis or not.
#' If `FALSE` (default), then no value is shown for the groups. If `TRUE`, then `shapr` includes the mean of the
#' features in each group.
#' @param beeswarm_cex Numeric.
#' The cex argument of [ggbeeswarm::geom_beeswarm()], controlling the spacing in the beeswarm plots.
#' @param ... Other arguments passed to underlying functions,
#' like [ggbeeswarm::geom_beeswarm()] for `plot_type = "beeswarm"`.
#'
#' @details See the examples below, or `vignette("general_usage", package = "shapr")` for examples of
#' how to use the function.
#'
#' @return ggplot object with plots of the Shapley value explanations
#'
#' @export
#' @examples
#' \donttest{
#' if (requireNamespace("party", quietly = TRUE)) {
#'   data("airquality")
#'   airquality <- airquality[complete.cases(airquality), ]
#'   x_var <- c("Solar.R", "Wind", "Temp", "Month")
#'   y_var <- "Ozone"
#'
#'   # Split data into test- and training data
#'   data_train <- head(airquality, -50)
#'   data_explain <- tail(airquality, 50)
#'
#'   x_train <- data_train[, x_var]
#'   x_explain <- data_explain[, x_var]
#'
#'   # Fit a linear model
#'   lm_formula <- as.formula(paste0(y_var, " ~ ", paste0(x_var, collapse = " + ")))
#'   model <- lm(lm_formula, data = data_train)
#'
#'   # Explain predictions
#'   p <- mean(data_train[, y_var])
#'
#'   # Empirical approach
#'   x <- explain(
#'     model = model,
#'     x_explain = x_explain,
#'     x_train = x_train,
#'     approach = "empirical",
#'     phi0 = p,
#'     n_MC_samples = 1e2
#'   )
#'
#'   if (requireNamespace(c("ggplot2", "ggbeeswarm"), quietly = TRUE)) {
#'     # The default plotting option is a bar plot of the Shapley values
#'     # We draw bar plots for the first 4 observations
#'     plot(x, index_x_explain = 1:4)
#'
#'     # We can also make waterfall plots
#'     plot(x, plot_type = "waterfall", index_x_explain = 1:4)
#'     # And only showing the two features with the largest contributions
#'     plot(x, plot_type = "waterfall", index_x_explain = 1:4, top_k_features = 2)
#'
#'     # Or scatter plots showing the distribution of the Shapley values and feature values
#'     plot(x, plot_type = "scatter")
#'     # And only for a specific feature
#'     plot(x, plot_type = "scatter", scatter_features = "Temp")
#'
#'     # Or a beeswarm plot summarising the Shapley values and feature values for all features
#'     plot(x, plot_type = "beeswarm")
#'     plot(x, plot_type = "beeswarm", col = c("red", "black")) # we can change colors
#'
#'     # Additional arguments can be passed to ggbeeswarm::geom_beeswarm() using the '...' argument.
#'     # For instance, sometimes the beeswarm plots overlap too much.
#'     # This can be fixed with the 'corral="wrap" argument.
#'     # See ?ggbeeswarm::geom_beeswarm for more information.
#'     plot(x, plot_type = "beeswarm", corral = "wrap")
#'   }
#'
#'   # Example of scatter and beeswarm plot with factor variables
#'   airquality$Month_factor <- as.factor(month.abb[airquality$Month])
#'   airquality <- airquality[complete.cases(airquality), ]
#'   x_var <- c("Solar.R", "Wind", "Temp", "Month_factor")
#'   y_var <- "Ozone"
#'
#'   # Split data into test- and training data
#'   data_train <- airquality
#'   data_explain <- tail(airquality, 50)
#'
#'   x_train <- data_train[, x_var]
#'   x_explain <- data_explain[, x_var]
#'
#'   # Fit a linear model
#'   lm_formula <- as.formula(paste0(y_var, " ~ ", paste0(x_var, collapse = " + ")))
#'   model <- lm(lm_formula, data = data_train)
#'
#'   # Explain predictions
#'   p <- mean(data_train[, y_var])
#'
#'   # Empirical approach
#'   x <- explain(
#'     model = model,
#'     x_explain = x_explain,
#'     x_train = x_train,
#'     approach = "ctree",
#'     phi0 = p,
#'     n_MC_samples = 1e2
#'   )
#'
#'   if (requireNamespace(c("ggplot2", "ggbeeswarm"), quietly = TRUE)) {
#'     plot(x, plot_type = "scatter")
#'     plot(x, plot_type = "beeswarm")
#'   }
#'
#'   # Example of further modification of the output from plot.shapr
#'   plt <- plot(x, index_x_explain = 1:4, print_ggplot = FALSE) # Storing without printing
#'
#'   # Displays the modified ggplot object
#'   plt +
#'     ggplot2::ggtitle("My custom title") +
#'     ggplot2::ylab("Variable influence") +
#'     ggplot2::xlab("Variable")
#' }
#' }
#'
#' @author Martin Jullum, Vilde Ung, Lars Henry Berge Olsen
plot.shapr <- function(x,
                       plot_type = "bar",
                       digits = 3,
                       print_ggplot = TRUE,
                       index_x_explain = 1:10,
                       top_k_features = NULL,
                       col = NULL,
                       bar_plot_phi0 = TRUE,
                       bar_plot_order = "largest_first",
                       scatter_features = NULL,
                       scatter_hist = TRUE,
                       include_group_feature_means = FALSE,
                       beeswarm_cex = 1 / length(index_x_explain)^(1 / 4),
                       ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    cli::cli_abort("ggplot2 is not installed. Please run {.run install.packages('ggplot2')}")
  }
  if (!(plot_type %in% c("bar", "waterfall", "scatter", "beeswarm"))) {
    cli::cli_abort(paste(plot_type, "is an invalid plot type. Try plot_type='bar', plot_type='waterfall',
               plot_type='scatter', or plot_type='beeswarm'."))
  }
  if (!(bar_plot_order %in% c("largest_first", "smallest_first", "original"))) {
    cli::cli_abort(paste(bar_plot_order, "is an invalid plot order. Try bar_plot_order='largest_first',
               bar_plot_order='smallest_first' or bar_plot_order='original'."))
  }

  # Remove the explain_id column
  x$shapley_values_est <- x$shapley_values_est[, -"explain_id"]

  if (is.null(index_x_explain)) index_x_explain <- seq(x$internal$parameters$n_explain)
  if (is.null(top_k_features)) top_k_features <- x$internal$parameters$n_features + 1
  if (length(beeswarm_cex) == 0) beeswarm_cex <- 1 / length(index_x_explain)^(1 / 4)

  # Inform user if only a subset of observations is being plotted
  n_explain <- x$internal$parameters$n_explain
  if (length(index_x_explain) < n_explain) {
    msg <- paste0("Showing ", length(index_x_explain), " of ", n_explain, " observations.")
    cli::cli_inform(c("i" = msg))
  }

  is_groupwise <- x$internal$parameters$is_groupwise

  # For group-wise Shapley values, we check if we are to take the mean over grouped features
  if (is_groupwise) {
    if (is.na(include_group_feature_means) ||
      !is.logical(include_group_feature_means) ||
      length(include_group_feature_means) > 1) {
      cli::cli_abort("`include_group_feature_means` must be single logical.")
    }
    if (!include_group_feature_means && plot_type %in% c("scatter", "beeswarm")) {
      cli::cli_abort(paste0(
        "`shapr` cannot make a `", plot_type, "` plot for group-wise Shapley values, as the plot needs a ",
        "single feature value for the whole group.\n",
        "For numerical data, the user can set `include_group_feature_means = TRUE` to use the mean of all ",
        "grouped features. The user should use this option cautiously to not misinterpret the explanations."
      ))
    }

    if (any(x$internal$objects$feature_specs$classes != "numeric")) {
      cli::cli_abort("`include_group_feature_means` cannot be `TRUE` for datasets with non-numerical features.")
    }

    # Take the mean over the grouped features and update the feature name to the group name
    x$internal$data$x_explain <-
      x$internal$data$x_explain[, lapply(
        x$internal$parameters$group,
        function(cols) rowMeans(.SD[, .SD, .SDcols = cols], na.rm = TRUE)
      )]

    x$internal$data$x_train <-
      x$internal$data$x_train[, lapply(
        x$internal$parameters$group,
        function(cols) rowMeans(.SD[, .SD, .SDcols = cols], na.rm = TRUE)
      )]
  }

  # melting Kshap
  shap_names <- x$internal$parameters$shap_names
  dt_shap <- signif(data.table::copy(x$shapley_values_est))
  dt_shap[, id := .I]
  dt_shap_long <- data.table::melt(dt_shap, id.vars = "id", value.name = "phi")
  dt_shap_long[, sign := factor(sign(phi), levels = c(1, -1), labels = c("Increases", "Decreases"))]

  # Converting and melting x_explain
  if (!is_groupwise || include_group_feature_means) {
    desc_mat <- trimws(format(x$internal$data$x_explain, digits = digits))
    for (i in seq_len(ncol(desc_mat))) {
      desc_mat[, i] <- paste0(shap_names[i], " = ", desc_mat[, i])
    }
  } else {
    desc_mat <- trimws(format(x$shapley_values_est[, -c("none")], digits = digits))
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
    gg <- make_beeswarm_plot(dt_plot,
      col,
      index_x_explain,
      x,
      factor_features,
      beeswarm_cex = beeswarm_cex,
      ...
    )
  } else { # if bar or waterfall plot
    # Only plot the desired observations
    dt_plot <- dt_plot[id %in% index_x_explain]

    if (length(dt_plot[, unique(id)]) > 10) {
      cli::cli_abort(
        c(
          "Too many observations to plot together!",
          "Try for instance setting index_x_explain = 1:10 so that the max.is not exceeded."
        )
      )
    }

    dt_plot <- order_for_plot(dt_plot, x$internal$parameters$n_features, bar_plot_order, top_k_features)


    # compute start and end values for waterfall rectangles
    data.table::setorder(dt_plot, rank_waterfall)
    dt_plot[, end := cumsum(phi), by = id]
    expected <- x$internal$parameters$phi0
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

  if (isTRUE(print_ggplot)) {
    return(print(gg)) # Return the figure with force display
  } else {
    return(gg) # Return the figure without force display
  }
}

get_n_breaks <- function(dt_plot, feature_name) {
  n_feat_vals <- length(dt_plot[variable == feature_name, unique(feature_value)]) # number of unique points to plot
  type <- dt_plot[variable == feature_name, type][1]

  if (type == "numeric") {
    if (n_feat_vals > 500) {
      n_breaks <- 50
    } else if (n_feat_vals > 200) {
      n_breaks <- 20
    } else if (n_feat_vals > 100) {
      n_breaks <- 10
    } else {
      n_breaks <- min(5, n_feat_vals + 2)
    }
  } else { # If factor
    n_breaks <- n_feat_vals
  }

  return(n_breaks)
}


compute_scatter_hist_values <- function(dt_plot, scatter_features) {
  dt_scatter_hist_list <- list()
  for (feature_name in scatter_features) {
    n_breaks <- get_n_breaks(dt_plot, feature_name)

    x <- dt_plot[variable == feature_name, feature_value]

    if (min(x) == max(x)) {
      scatter_hist_object <- hist(x, breaks = 1, plot = FALSE)
      # scatter_hist_object$breaks = c(x[1] - .Machine$double.eps*10^10, x[1] + .Machine$double.eps*10^10)
      scatter_hist_object$breaks <- c(x[1] - 0.01, x[1] + 0.01)
    } else {
      step <- (max(x) - min(x)) / (n_breaks - 1)
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
    cli::cli_abort("'col' must be of length 1 when making scatter plot.")
  }

  dt_plot <- dt_plot[variable != "none"]

  if (is.null(scatter_features)) {
    scatter_features <- unique(dt_plot[, variable])
  } else if (is.numeric(scatter_features)) {
    # i.e. plot first 4 features if scatter_features = 1:4
    scatter_features <- dt_plot[scatter_features, unique(variable)]
  } else if (is.character(scatter_features)) {
    if (any(!(scatter_features %in% unique(dt_plot[, variable])))) {
      cli::cli_abort(
        "Some or all of the listed feature names in 'scatter_features' do not match the names in the data."
      )
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
#' @keywords internal
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


make_beeswarm_plot <- function(dt_plot,
                               col,
                               index_x_explain,
                               x,
                               factor_cols,
                               beeswarm_cex,
                               ...) {
  if (!requireNamespace("ggbeeswarm", quietly = TRUE)) {
    cli::cli_abort("ggbeeswarm is not installed. Please run {.run install.packages('ggbeeswarm')}.")
  }

  if (is.null(col)) {
    col <- c("#F8766D", "yellow", "#00BA38")
  }
  if (!(length(col) %in% c(2, 3))) {
    cli::cli_abort("'col' must be of length 2 or 3 when making beeswarm plot.")
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
    ggbeeswarm::geom_beeswarm(priority = "random", cex = beeswarm_cex, ...) +
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
    cli::cli_abort("'col' must be of length 2 when making bar plot.")
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
    cli::cli_abort("'col' must be of length 2 when making waterfall plot.")
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


#' Plots of the MSEv Evaluation Criterion
#'
#' @description
#' Make plots to visualize and compare the MSEv evaluation criterion for a list of
#' [explain()] objects applied to the same data and model. The function creates
#' bar plots and line plots with points to illustrate the overall MSEv evaluation
#' criterion, but also for each observation/explicand and coalition by only averaging over
#' the coalitions and observations/explicands, respectively.
#'
#' @inheritParams plot.shapr
#'
#' @param explanation_list A list of [explain()] objects applied to the same data and model.
#' If the entries in the list are named, then the function use these names. Otherwise, they default to
#' the approach names (with integer suffix for duplicates) for the explanation objects in `explanation_list`.
#' @param id_coalition Integer vector. Which of the coalitions to plot.
#' E.g. if you used `n_coalitions = 16` in [explain()], you can generate a plot for the
#' first 5 coalitions and the 10th by setting `id_coalition = c(1:5, 10)`.
#' @param CI_level Positive numeric between zero and one. Default is `0.95` if the number of observations to explain is
#' larger than 20, otherwise `CI_level = NULL`, which removes the confidence intervals. The level of the approximate
#' confidence intervals for the overall MSEv and the MSEv_coalition. The confidence intervals are based on that
#' the MSEv scores are means over the observations/explicands, and that means are approximation normal. Since the
#' standard deviations are estimated, we use the quantile t from the T distribution with N_explicands - 1 degrees of
#' freedom corresponding to the provided level. Here, N_explicands is the number of observations/explicands.
#' MSEv +/- t*SD(MSEv)/sqrt(N_explicands). Note that the `explain()` function already scales the standard deviation by
#' sqrt(N_explicands), thus, the CI are MSEv \/- t*MSEv_sd, where the values MSEv and MSEv_sd are extracted from the
#' MSEv data.tables in the objects in the `explanation_list`.
#' @param geom_col_width Numeric. Bar width. By default, set to 90% of the [ggplot2::resolution()] of the data.
#' @param plot_type Character vector. The possible options are "overall" (default), "comb", and "explicand".
#' If `plot_type = "overall"`, then the plot (one bar plot) associated with the overall MSEv evaluation criterion
#' for each method is created, i.e., when averaging over both the coalitions and observations/explicands.
#' If `plot_type = "comb"`, then the plots (one line plot and one bar plot) associated with the MSEv evaluation
#' criterion for each coalition are created, i.e., when we only average over the observations/explicands.
#' If `plot_type = "explicand"`, then the plots (one line plot and one bar plot) associated with the MSEv evaluation
#' criterion for each observations/explicands are created, i.e., when we only average over the coalitions.
#' If `plot_type` is a vector of one or several of "overall", "comb", and "explicand", then the associated plots are
#' created.
#'
#' @details Note that in contrast to [plot.shapr()], [plot_MSEv_eval_crit()] always just returns the ggplot objects,
#' i.e. no force displaying through [ggplot2::print.ggplot()].
#'
#' @return Either a single [ggplot2::ggplot()] object of the MSEv criterion when `plot_type = "overall"`, or a list
#' of [ggplot2::ggplot()] objects based on the `plot_type` parameter.
#'
#'
#' @export
#' @examples
#' \donttest{
#' if (requireNamespace("xgboost", quietly = TRUE) && requireNamespace("ggplot2", quietly = TRUE)) {
#'   # Get the data
#'   data("airquality")
#'   data <- data.table::as.data.table(airquality)
#'   data <- data[complete.cases(data), ]
#'
#'   #' Define the features and the response
#'   x_var <- c("Solar.R", "Wind", "Temp", "Month")
#'   y_var <- "Ozone"
#'
#'   # Split data into test and training data set
#'   ind_x_explain <- 1:25
#'   x_train <- data[-ind_x_explain, ..x_var]
#'   y_train <- data[-ind_x_explain, get(y_var)]
#'   x_explain <- data[ind_x_explain, ..x_var]
#'
#'   # Fitting a basic xgboost model to the training data
#'   model <- xgboost::xgboost(
#'     x = x_train,
#'     y = y_train,
#'     nround = 20,
#'     verbosity = 0
#'   )
#'
#'   # Specifying the phi_0, i.e. the expected prediction without any features
#'   phi0 <- mean(y_train)
#'
#'   # Independence approach
#'   explanation_independence <- explain(
#'     model = model,
#'     x_explain = x_explain,
#'     x_train = x_train,
#'     approach = "independence",
#'     phi0 = phi0,
#'     n_MC_samples = 1e2
#'   )
#'
#'   # Gaussian 1e1 approach
#'   explanation_gaussian_1e1 <- explain(
#'     model = model,
#'     x_explain = x_explain,
#'     x_train = x_train,
#'     approach = "gaussian",
#'     phi0 = phi0,
#'     n_MC_samples = 1e1
#'   )
#'
#'   # Gaussian 1e2 approach
#'   explanation_gaussian_1e2 <- explain(
#'     model = model,
#'     x_explain = x_explain,
#'     x_train = x_train,
#'     approach = "gaussian",
#'     phi0 = phi0,
#'     n_MC_samples = 1e2
#'   )
#'
#'   # ctree approach
#'   explanation_ctree <- explain(
#'     model = model,
#'     x_explain = x_explain,
#'     x_train = x_train,
#'     approach = "ctree",
#'     phi0 = phi0,
#'     n_MC_samples = 1e2
#'   )
#'
#'   # Combined approach
#'   explanation_combined <- explain(
#'     model = model,
#'     x_explain = x_explain,
#'     x_train = x_train,
#'     approach = c("gaussian", "independence", "ctree"),
#'     phi0 = phi0,
#'     n_MC_samples = 1e2
#'   )
#'
#'   # Create a list of explanations with names
#'   explanation_list_named <- list(
#'     "Ind." = explanation_independence,
#'     "Gaus. 1e1" = explanation_gaussian_1e1,
#'     "Gaus. 1e2" = explanation_gaussian_1e2,
#'     "Ctree" = explanation_ctree,
#'     "Combined" = explanation_combined
#'   )
#'
#'   # Create the default MSEv plot where we average over both the coalitions and observations
#'   # with approximate 95% confidence intervals
#'   plot_MSEv_eval_crit(explanation_list_named, CI_level = 0.95, plot_type = "overall")
#'
#'   # Can also create plots of the MSEv criterion averaged only over the coalitions or observations.
#'   MSEv_figures <- plot_MSEv_eval_crit(explanation_list_named,
#'     CI_level = 0.95,
#'     plot_type = c("overall", "comb", "explicand")
#'   )
#'   MSEv_figures$MSEv_bar
#'   MSEv_figures$MSEv_coalition_bar
#'   MSEv_figures$MSEv_explicand_bar
#'
#'   # When there are many coalitions or observations, then it can be easier to look at line plots
#'   MSEv_figures$MSEv_coalition_line_point
#'   MSEv_figures$MSEv_explicand_line_point
#'
#'   # We can specify which observations or coalitions to plot
#'   plot_MSEv_eval_crit(explanation_list_named,
#'     plot_type = "explicand",
#'     index_x_explain = c(1, 3:4, 6),
#'     CI_level = 0.95
#'   )$MSEv_explicand_bar
#'   plot_MSEv_eval_crit(explanation_list_named,
#'     plot_type = "comb",
#'     id_coalition = c(3, 4, 9, 13:15),
#'     CI_level = 0.95
#'   )$MSEv_coalition_bar
#'
#'   # We can alter the figures if other palette schemes or design is wanted
#'   bar_text_n_decimals <- 1
#'   MSEv_figures$MSEv_bar +
#'     ggplot2::scale_x_discrete(limits = rev(levels(MSEv_figures$MSEv_bar$data$Method))) +
#'     ggplot2::coord_flip() +
#'     ggplot2::scale_fill_discrete() + #' Default ggplot2 palette
#'     ggplot2::theme_minimal() + #' This must be set before the other theme call
#'     ggplot2::theme(
#'       plot.title = ggplot2::element_text(size = 10),
#'       legend.position = "bottom"
#'     ) +
#'     ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, ncol = 6)) +
#'     ggplot2::geom_text(
#'       ggplot2::aes(label = sprintf(
#'         paste("%.", sprintf("%d", bar_text_n_decimals), "f", sep = ""),
#'         round(MSEv, bar_text_n_decimals)
#'       )),
#'       vjust = -1.1, # This value must be altered based on the plot dimension
#'       hjust = 1.1, # This value must be altered based on the plot dimension
#'       color = "black",
#'       position = ggplot2::position_dodge(0.9),
#'       size = 5
#'     )
#' }
#' }
#'
#' @author Lars Henry Berge Olsen
plot_MSEv_eval_crit <- function(explanation_list,
                                index_x_explain = 1:10,
                                id_coalition = NULL,
                                CI_level = if (length(explanation_list[[1]]$pred_explain) < 20) NULL else 0.95,
                                geom_col_width = 0.9,
                                plot_type = "overall") {
  # Setup and checks ----------------------------------------------------------------------------
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    cli::cli_abort("ggplot2 is not installed. Please run {.run install.packages('ggplot2')}.")
  }

  # Check for valid plot type argument
  unknown_plot_type <- plot_type[!(plot_type %in% c("overall", "comb", "explicand"))]
  if (length(unknown_plot_type) > 0) {
    cli::cli_abort(paste0(
      "The `plot_type` must be one (or several) of 'overall', 'comb', 'explicand'. ",
      "Do not recognise: '", paste(unknown_plot_type, collapse = "', '"), "'."
    ))
  }

  # Ensure that even a single explanation object is in a list
  if ("shapr" %in% class(explanation_list)) explanation_list <- list(explanation_list)

  # Name the elements in the explanation_list if no names have been provided
  if (is.null(names(explanation_list))) explanation_list <- MSEv_name_explanation_list(explanation_list)

  # Check valid CI_level value
  if (!is.null(CI_level) && (CI_level <= 0 || 1 <= CI_level)) {
    cli::cli_abort("the `CI_level` parameter must be strictly between zero and one.")
  }

  # Check that the explanation objects explain the same observations
  MSEv_check_explanation_list(explanation_list)

  # Get the number of observations and coalitions and the quantile of the T distribution
  iter <- length(explanation_list[[1]]$internal$iter_list)
  n_coalitions <- explanation_list[[1]]$internal$iter_list[[iter]]$n_coalitions

  n_explain <- explanation_list[[1]]$internal$parameters$n_explain
  tfrac <- if (is.null(CI_level)) NULL else qt((1 + CI_level) / 2, n_explain - 1)

  # Inform user if only a subset of observations is being plotted
  if (length(index_x_explain) < n_explain) {
    msg <- paste0("Showing ", length(index_x_explain), " of ", n_explain, " observations.")
    cli::cli_inform(c("i" = msg))
  }

  # Create data.tables of the MSEv values
  MSEv_dt_list <- MSEv_extract_MSEv_values(
    explanation_list = explanation_list,
    index_x_explain = index_x_explain,
    id_coalition = id_coalition
  )
  MSEv_dt <- MSEv_dt_list$MSEv
  MSEv_explicand_dt <- MSEv_dt_list$MSEv_explicand
  MSEv_coalition_dt <- MSEv_dt_list$MSEv_coalition

  # Warnings related to the approximate confidence intervals
  if (!is.null(CI_level)) {
    if (n_explain < 20) {
      msg <- paste0(
        "The approximate ", CI_level * 100, "% confidence intervals might be wide as they are only based on ",
        n_explain, " observations."
      )
      cli::cli_inform(c("i" = msg))
    }

    # Check for CI with negative values
    methods_with_negative_CI <- MSEv_dt[MSEv_sd > abs(tfrac) * MSEv, Method]
    if (length(methods_with_negative_CI) > 0) {
      msg1 <- paste0(
        "The method/methods '", paste(methods_with_negative_CI, collapse = "', '"), "' has/have ",
        "approximate ", CI_level * 100, "% confidence intervals with negative values, ",
        "which is not possible for the MSEv criterion."
      )
      msg2 <- paste0(
        "Check the `MSEv_explicand` plots for potential observational outliers ",
        "that causes the wide confidence intervals."
      )
      cli::cli_inform(c("i" = msg1, " " = msg2))
    }
  }

  # Plot ------------------------------------------------------------------------------------------------------------
  return_object <- list()

  if ("explicand" %in% plot_type) {
    # MSEv averaged over only the coalitions for each observation
    return_object <- c(
      return_object,
      make_MSEv_explicand_plots(
        MSEv_explicand_dt = MSEv_explicand_dt,
        n_coalitions = n_coalitions,
        geom_col_width = geom_col_width
      )
    )
  }

  if ("comb" %in% plot_type) {
    # MSEv averaged over only the observations for each coalitions
    return_object <- c(
      return_object,
      make_MSEv_coalition_plots(
        MSEv_coalition_dt = MSEv_coalition_dt,
        n_explain = n_explain,
        geom_col_width = geom_col_width,
        tfrac = tfrac
      )
    )
  }

  if ("overall" %in% plot_type) {
    # MSEv averaged over both the coalitions and observations
    return_object$MSEv_bar <- make_MSEv_bar_plot(
      MSEv_dt = MSEv_dt,
      n_coalitions = n_coalitions,
      n_explain = n_explain,
      geom_col_width = geom_col_width,
      tfrac = tfrac
    )
  }

  # Return ----------------------------------------------------------------------------------------------------------
  if (length(plot_type) == 1 && plot_type == "overall") {
    return_object <- return_object$MSEv_bar
  }

  return(return_object)
}

#' @keywords internal
#' @author Lars Henry Berge Olsen
MSEv_name_explanation_list <- function(explanation_list) {
  # Give names to the entries in the `explanation_list` based on their used approach.

  # Extract the approach names and paste in case of combined approaches.
  names <- sapply(
    explanation_list,
    function(explanation) paste(explanation$internal$parameters$approach, collapse = "_")
  )

  # Add integer suffix for non-unique names
  names <- make.unique(names, sep = "_")
  names(explanation_list) <- names

  msg1 <- "User provided an `explanation_list` without named explanation objects."
  msg2 <- "Use the approach names of the explanation objects as the names (with integer suffix for duplicates)."

  cli::cli_inform(c("i" = msg1, " " = msg2))

  return(explanation_list)
}

#' @keywords internal
#' @author Lars Henry Berge Olsen
MSEv_check_explanation_list <- function(explanation_list) {
  # Check that the explanation list is valid for plotting the MSEv evaluation criterion

  # All entries must be named
  if (any(names(explanation_list) == "")) cli::cli_abort("All the entries in `explanation_list` must be named.")

  # Check that all explanation objects use the same column names for the Shapley values
  if (length(unique(lapply(explanation_list, function(explanation) colnames(explanation$shapley_values_est)))) != 1) {
    cli::cli_abort("The Shapley value feature names are not identical in all objects in the `explanation_list`.")
  }

  # Check that all explanation objects use the same test observations
  entries_using_diff_x_explain <- sapply(explanation_list, function(explanation) {
    !identical(explanation_list[[1]]$internal$data$x_explain, explanation$internal$data$x_explain)
  })
  if (any(entries_using_diff_x_explain)) {
    methods_with_diff_comb_str <-
      paste(names(entries_using_diff_x_explain)[entries_using_diff_x_explain], collapse = "', '")
    cli::cli_abort(paste0(
      "The object/objects '", methods_with_diff_comb_str, "' in `explanation_list` has/have a different ",
      "`x_explain` than '", names(explanation_list)[1], "'. Cannot compare them."
    ))
  }

  # Check that no explanation object is missing the MSEv
  entries_missing_MSEv <- sapply(explanation_list, function(explanation) is.null(explanation$MSEv))
  if (any(entries_missing_MSEv)) {
    methods_without_MSEv_string <- paste(names(entries_missing_MSEv)[entries_missing_MSEv], collapse = "', '")
    cli::cli_abort(sprintf(
      "The object/objects '%s' in `explanation_list` is/are missing the `MSEv` list.",
      methods_without_MSEv_string
    ))
  }

  # Check that all explanation objects use the same coalitions
  entries_using_diff_combs <- sapply(explanation_list, function(explanation) {
    !identical(explanation_list[[1]]$internal$objects$X$features, explanation$internal$objects$X$features)
  })
  if (any(entries_using_diff_combs)) {
    methods_with_diff_comb_str <- paste(names(entries_using_diff_combs)[entries_using_diff_combs], collapse = "', '")
    cli::cli_abort(paste0(
      "The object/objects '", methods_with_diff_comb_str, "' in `explanation_list` uses/use different ",
      "coalitions than '", names(explanation_list)[1], "'. Cannot compare them."
    ))
  }
}

#' @keywords internal
#' @author Lars Henry Berge Olsen
MSEv_extract_MSEv_values <- function(explanation_list,
                                     index_x_explain = 1:10,
                                     id_coalition = NULL) {
  # Function that extract the MSEv values from the different explanations objects in explanation_list,
  # put the values in data.tables, and keep only the desired observations and coalitions.

  # The overall MSEv criterion
  MSEv <- rbindlist(lapply(explanation_list, function(explanation) explanation$MSEv$MSEv),
    use.names = TRUE, idcol = "Method"
  )
  MSEv$Method <- factor(MSEv$Method, levels = names(explanation_list))

  # The MSEv evaluation criterion for each explicand.
  MSEv_explicand <- rbindlist(lapply(explanation_list, function(explanation) explanation$MSEv$MSEv_explicand),
    use.names = TRUE, idcol = "Method"
  )
  MSEv_explicand$id <- factor(MSEv_explicand$id)
  MSEv_explicand$Method <- factor(MSEv_explicand$Method, levels = names(explanation_list))

  # The MSEv evaluation criterion for each coalition.
  MSEv_coalition <- rbindlist(lapply(explanation_list, function(explanation) explanation$MSEv$MSEv_coalition),
    use.names = TRUE, idcol = "Method"
  )
  MSEv_coalition$id_coalition <- factor(MSEv_coalition$id_coalition)
  MSEv_coalition$Method <- factor(MSEv_coalition$Method, levels = names(explanation_list))

  # Only keep the desired observations and coalitions
  if (!is.null(index_x_explain)) MSEv_explicand <- MSEv_explicand[id %in% index_x_explain]
  if (!is.null(id_coalition)) {
    id_coalition_aux <- id_coalition
    MSEv_coalition <- MSEv_coalition[id_coalition %in% id_coalition_aux]
  }

  return(list(MSEv = MSEv, MSEv_explicand = MSEv_explicand, MSEv_coalition = MSEv_coalition))
}

#' @keywords internal
#' @author Lars Henry Berge Olsen
make_MSEv_bar_plot <- function(MSEv_dt,
                               n_coalitions,
                               n_explain,
                               tfrac = NULL,
                               geom_col_width = 0.9) {
  MSEv_bar <-
    ggplot2::ggplot(MSEv_dt, ggplot2::aes(x = Method, y = MSEv, fill = Method)) +
    ggplot2::geom_col(
      width = geom_col_width,
      position = ggplot2::position_dodge(geom_col_width)
    ) +
    ggplot2::labs(
      x = "Method",
      y = bquote(MSE[v]),
      title = bquote(MSE[v] ~ "criterion averaged over the" ~ .(n_coalitions) ~
        "coalitions and" ~ .(n_explain) ~ "explicands")
    )

  if (!is.null(tfrac)) {
    CI_level <- 1 - 2 * (1 - pt(tfrac, n_explain - 1))

    MSEv_bar <- MSEv_bar +
      ggplot2::labs(title = bquote(MSE[v] ~ "criterion averaged over the" ~ .(n_coalitions) ~
        "coalitions and" ~ .(n_explain) ~ "explicands with" ~
        .(CI_level * 100) * "% CI")) +
      ggplot2::geom_errorbar(
        position = ggplot2::position_dodge(geom_col_width),
        width = 0.25,
        ggplot2::aes(
          ymin = MSEv - tfrac * MSEv_sd,
          ymax = MSEv + tfrac * MSEv_sd,
          group = Method
        )
      )
  }

  return(MSEv_bar)
}

#' @keywords internal
#' @author Lars Henry Berge Olsen
make_MSEv_explicand_plots <- function(MSEv_explicand_dt,
                                      n_coalitions,
                                      geom_col_width = 0.9) {
  MSEv_explicand_source <-
    ggplot2::ggplot(MSEv_explicand_dt, ggplot2::aes(x = id, y = MSEv)) +
    ggplot2::labs(
      x = "index_x_explain",
      y = bquote(MSE[v] ~ "(explicand)"),
      title = bquote(MSE[v] ~ "criterion averaged over the" ~ .(n_coalitions) ~
        "coalitions for each explicand")
    )

  MSEv_explicand_bar <-
    MSEv_explicand_source +
    ggplot2::geom_col(
      width = geom_col_width,
      position = ggplot2::position_dodge(geom_col_width),
      ggplot2::aes(fill = Method)
    )

  MSEv_explicand_line_point <-
    MSEv_explicand_source +
    ggplot2::aes(x = as.numeric(id)) +
    ggplot2::labs(x = "index_x_explain") +
    ggplot2::geom_point(ggplot2::aes(col = Method)) +
    ggplot2::geom_line(ggplot2::aes(group = Method, col = Method))

  return(list(
    MSEv_explicand_bar = MSEv_explicand_bar,
    MSEv_explicand_line_point = MSEv_explicand_line_point
  ))
}

#' @keywords internal
#' @author Lars Henry Berge Olsen
make_MSEv_coalition_plots <- function(MSEv_coalition_dt,
                                      n_explain,
                                      tfrac = NULL,
                                      geom_col_width = 0.9) {
  MSEv_coalition_source <-
    ggplot2::ggplot(MSEv_coalition_dt, ggplot2::aes(x = id_coalition, y = MSEv)) +
    ggplot2::labs(
      x = "id_coalition",
      y = bquote(MSE[v] ~ "(coalition)"),
      title = bquote(MSE[v] ~ "criterion averaged over the" ~ .(n_explain) ~
        "explicands for each coalition")
    )

  MSEv_coalition_bar <-
    MSEv_coalition_source +
    ggplot2::geom_col(
      width = geom_col_width,
      position = ggplot2::position_dodge(geom_col_width),
      ggplot2::aes(fill = Method)
    )

  if (!is.null(tfrac)) {
    CI_level <- 1 - 2 * (1 - pt(tfrac, n_explain - 1))

    MSEv_coalition_bar <-
      MSEv_coalition_bar +
      ggplot2::labs(title = bquote(MSE[v] ~ "criterion averaged over the" ~ .(n_explain) ~
        "explicands for each coalition with" ~ .(CI_level * 100) * "% CI")) +
      ggplot2::geom_errorbar(
        position = ggplot2::position_dodge(geom_col_width),
        width = 0.25,
        ggplot2::aes(
          ymin = MSEv - tfrac * MSEv_sd,
          ymax = MSEv + tfrac * MSEv_sd,
          group = Method
        )
      )
  }

  MSEv_coalition_line_point <-
    MSEv_coalition_source +
    ggplot2::aes(x = as.numeric(id_coalition)) +
    ggplot2::labs(x = "id_coalition") +
    ggplot2::geom_point(ggplot2::aes(col = Method)) +
    ggplot2::geom_line(ggplot2::aes(group = Method, col = Method))

  return(list(
    MSEv_coalition_bar = MSEv_coalition_bar,
    MSEv_coalition_line_point = MSEv_coalition_line_point
  ))
}

#' Shapley Value Bar Plots for Several Explanation Objects
#'
#' @description
#' Make plots to visualize and compare the estimated Shapley values for a list of
#' [shapr::explain()] objects applied to the same data and model. For group-wise Shapley values,
#' the features values plotted are the mean feature values for all features in each group.
#'
#' @inheritParams plot_MSEv_eval_crit
#' @inheritParams plot.shapr
#' @param index_explicands Integer vector. Which of the explicands (test observations) to plot.
#' E.g. if you have explained 10 observations using [shapr::explain()], you can generate a plot for the
#' first 5 observations/explicands and the 10th by setting `index_x_explain = c(1:5, 10)`.
#' The argument `index_explicands_sort` must be `FALSE` to plot the explicand
#' in the order specified in `index_x_explain`.
#' @param only_these_features String vector. Containing the names of the features which
#' are to be included in the bar plots.
#' @param plot_phi0 Boolean. If we are to include the \eqn{\phi_0} in the bar plots or not.
#' @param add_zero_line Boolean. If we are to add a black line for a feature contribution of 0.
#' @param brewer_palette String. Name of one of the color palettes from [RColorBrewer::RColorBrewer()].
#'  If `NULL`, then the function uses the default [ggplot2::ggplot()] color scheme.
#' The following palettes are available for use with these scales:
#' \describe{
#'    \item{Diverging}{BrBG, PiYG, PRGn, PuOr, RdBu, RdGy, RdYlBu, RdYlGn, Spectral}
#'    \item{Qualitative}{Accent, Dark2, Paired, Pastel1, Pastel2, Set1, Set2, Set3}
#'    \item{Sequential}{Blues, BuGn, BuPu, GnBu, Greens, Greys, Oranges,
#'      OrRd, PuBu, PuBuGn, PuRd, Purples, RdPu, Reds, YlGn, YlGnBu, YlOrBr, YlOrRd}
#' }
#' @param axis_labels_n_dodge Integer. The number of rows that
#' should be used to render the labels. This is useful for displaying labels that would otherwise overlap.
#' @param axis_labels_rotate_angle Numeric. The angle of the axis label, where 0 means horizontal, 45 means tilted,
#' and 90 means vertical. Compared to setting the angle in [ggplot2::theme()] / [ggplot2::element_text()], this also
#' uses some heuristics to automatically pick the `hjust` and `vjust` that you probably want.
#' @param horizontal_bars Boolean. Flip Cartesian coordinates so that horizontal becomes vertical,
#' and vertical, horizontal. This is primarily useful for converting geoms and statistics which display
#' y conditional on x, to x conditional on y. See [ggplot2::coord_flip()].
#' @param facet_scales Should scales be free ("`free`", the default), fixed ("`fixed`"), or free in one dimension
#' ("`free_x`", "`free_y`")? The user has to change the latter manually depending on the value of `horizontal_bars`.
#' @param facet_ncol  Integer. The number of columns in the facet grid. Default is `facet_ncol = 2`.
#' @param geom_col_width Numeric. Bar width. By default, set to 85% of the [ggplot2::resolution()] of the data.
#' @param include_group_feature_means Logical. Whether to include the average feature value in a group on the
#' y-axis or not. If `FALSE` (default), then no value is shown for the groups. If `TRUE`, then `shapr` includes
#' the mean of the features in each group.
#' @param index_explicands_sort Boolean. If `FALSE` (default), then `shapr` plots the explicands in the order
#' specified in `index_explicands`. If `TRUE`, then `shapr` sort the indices in increasing order based on their id.
#'
#' @return A [ggplot2::ggplot()] object.
#' @export
#'
#' @examples
#' \dontrun{
#' if (requireNamespace("xgboost", quietly = TRUE) && requireNamespace("ggplot2", quietly = TRUE)) {
#'   # Get the data
#'   data("airquality")
#'   data <- data.table::as.data.table(airquality)
#'   data <- data[complete.cases(data), ]
#'
#'   # Define the features and the response
#'   x_var <- c("Solar.R", "Wind", "Temp", "Month")
#'   y_var <- "Ozone"
#'
#'   # Split data into test and training data set
#'   ind_x_explain <- 1:12
#'   x_train <- data[-ind_x_explain, ..x_var]
#'   y_train <- data[-ind_x_explain, get(y_var)]
#'   x_explain <- data[ind_x_explain, ..x_var]
#'
#'   # Fitting a basic xgboost model to the training data
#'   model <- xgboost::xgboost(
#'     x = x_train,
#'     y = y_train,
#'     nround = 20,
#'     verbosity = 0
#'   )
#'
#'   # Specifying the phi_0, i.e. the expected prediction without any features
#'   phi0 <- mean(y_train)
#'
#'   # Independence approach
#'   explanation_independence <- explain(
#'     model = model,
#'     x_explain = x_explain,
#'     x_train = x_train,
#'     approach = "independence",
#'     phi0 = phi0,
#'     n_MC_samples = 1e2
#'   )
#'
#'   # Empirical approach
#'   explanation_empirical <- explain(
#'     model = model,
#'     x_explain = x_explain,
#'     x_train = x_train,
#'     approach = "empirical",
#'     phi0 = phi0,
#'     n_MC_samples = 1e2
#'   )
#'
#'   # Gaussian 1e1 approach
#'   explanation_gaussian_1e1 <- explain(
#'     model = model,
#'     x_explain = x_explain,
#'     x_train = x_train,
#'     approach = "gaussian",
#'     phi0 = phi0,
#'     n_MC_samples = 1e1
#'   )
#'
#'   # Gaussian 1e2 approach
#'   explanation_gaussian_1e2 <- explain(
#'     model = model,
#'     x_explain = x_explain,
#'     x_train = x_train,
#'     approach = "gaussian",
#'     phi0 = phi0,
#'     n_MC_samples = 1e2
#'   )
#'
#'   # Combined approach
#'   explanation_combined <- explain(
#'     model = model,
#'     x_explain = x_explain,
#'     x_train = x_train,
#'     approach = c("gaussian", "ctree", "empirical"),
#'     phi0 = phi0,
#'     n_MC_samples = 1e2
#'   )
#'
#'   # Create a list of explanations with names
#'   explanation_list <- list(
#'     "Ind." = explanation_independence,
#'     "Emp." = explanation_empirical,
#'     "Gaus. 1e1" = explanation_gaussian_1e1,
#'     "Gaus. 1e2" = explanation_gaussian_1e2,
#'     "Combined" = explanation_combined
#'   )
#'
#'   # The function uses the provided names.
#'   plot_SV_several_approaches(explanation_list)
#'
#'   # We can change the number of columns in the grid of plots and add other visual alterations
#'   # Set `print_ggplot = FALSE` to avoid force displaying the ggplot object before the modifications
#'   # outside plot_SV_several_approaches()
#'
#'   plot_SV_several_approaches(explanation_list,
#'     facet_ncol = 3,
#'     facet_scales = "free_y",
#'     add_zero_line = TRUE,
#'     digits = 2,
#'     brewer_palette = "Paired",
#'     geom_col_width = 0.6,
#'     print_ggplot = FALSE
#'   ) +
#'     ggplot2::theme_minimal() +
#'     ggplot2::theme(legend.position = "bottom", plot.title = ggplot2::element_text(size = 10))
#'
#'
#'   # We can specify which explicands to plot to get less chaotic plots and make the bars vertical
#'   plot_SV_several_approaches(explanation_list,
#'     index_explicands = c(1:2, 5, 10),
#'     horizontal_bars = FALSE,
#'     axis_labels_rotate_angle = 45
#'   )
#'
#'   # We can change the order of the features by specifying the
#'   # order using the `only_these_features` parameter.
#'   plot_SV_several_approaches(explanation_list,
#'     index_explicands = c(1:2, 5, 10),
#'     only_these_features = c("Temp", "Solar.R", "Month", "Wind")
#'   )
#'
#'   # We can also remove certain features if we are not interested in them
#'   # or want to focus on, e.g., two features. The function will give a
#'   # message to if the user specifies non-valid feature names.
#'   plot_SV_several_approaches(explanation_list,
#'     index_explicands = c(1:2, 5, 10),
#'     only_these_features = c("Temp", "Solar.R"),
#'     plot_phi0 = TRUE
#'   )
#' }
#' }
#'
#' @author Lars Henry Berge Olsen
plot_SV_several_approaches <- function(explanation_list,
                                       index_explicands = NULL,
                                       index_explicands_sort = FALSE,
                                       only_these_features = NULL,
                                       plot_phi0 = FALSE,
                                       digits = 4,
                                       print_ggplot = TRUE,
                                       add_zero_line = FALSE,
                                       axis_labels_n_dodge = NULL,
                                       axis_labels_rotate_angle = NULL,
                                       horizontal_bars = TRUE,
                                       facet_scales = "free",
                                       facet_ncol = 2,
                                       geom_col_width = 0.85,
                                       brewer_palette = NULL,
                                       include_group_feature_means = FALSE) {
  # Setup and checks ----------------------------------------------------------------------------
  # Check that ggplot2 is installed
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    cli::cli_abort("ggplot2 is not installed. Please run {.run install.packages('ggplot2')}.")
  }

  # Ensure that even a single explanation object is in a list
  if ("shapr" %in% class(explanation_list)) explanation_list <- list(explanation_list)

  # Name the elements in the explanation_list if no names have been provided
  if (is.null(names(explanation_list))) explanation_list <- MSEv_name_explanation_list(explanation_list)

  # All entries must be named
  if (any(names(explanation_list) == "")) cli::cli_abort("All the entries in `explanation_list` must be named.")

  # Check that the column names for the Shapley values are the same for all explanations in the `explanation_list`
  if (length(unique(lapply(explanation_list, function(explanation) colnames(explanation$shapley_values_est)))) != 1) {
    cli::cli_abort("The Shapley value feature names are not identical in all objects in the `explanation_list`.")
  }

  # Check that all explanation objects use the same test observations
  entries_using_diff_x_explain <- sapply(explanation_list, function(explanation) {
    !identical(explanation_list[[1]]$internal$data$x_explain, explanation$internal$data$x_explain)
  })
  if (any(entries_using_diff_x_explain)) {
    methods_with_diff_comb_str <-
      paste(names(entries_using_diff_x_explain)[entries_using_diff_x_explain], collapse = "', '")
    cli::cli_abort(paste0(
      "The object/objects '", methods_with_diff_comb_str, "' in `explanation_list` has/have a different ",
      "`x_explain` than '", names(explanation_list)[1], "'. Cannot compare them."
    ))
  }

  # Update the index_explicands to be all explicands if not specified
  if (is.null(index_explicands)) index_explicands <- seq(explanation_list[[1]]$internal$parameters$n_explain)


  # Creating data.tables --------------------------------------------------------------------------------------------
  # Update the `only_these_features` parameter vector based on `plot_phi0` or in case it is NULL
  only_these_features <- update_only_these_features(
    explanation_list = explanation_list,
    only_these_features = only_these_features,
    plot_phi0 = plot_phi0
  )

  # Create a variable storing the features to use excluding `none`
  only_these_features_wo_none <- only_these_features[only_these_features != "none"]

  # Create data.table of the Shapley values
  dt_Shapley_values <- extract_Shapley_values_dt(
    explanation_list = explanation_list,
    index_explicands = index_explicands,
    only_these_features = only_these_features
  )

  # Create data.table of feature descriptions
  dt_desc_long <- create_feature_descriptions_dt(
    explanation_list = explanation_list,
    only_these_features_wo_none = only_these_features_wo_none,
    index_explicands = index_explicands,
    horizontal_bars = horizontal_bars,
    digits = digits,
    include_group_feature_means = include_group_feature_means
  )

  # Set the explicands to the same order as they were given
  if (!index_explicands_sort) {
    dt_Shapley_values[, .id := factor(.id, levels = index_explicands, ordered = TRUE)]
    dt_desc_long[, .id := factor(.id, levels = index_explicands, ordered = TRUE)]
  }

  # Melt `dt_Shapley_values` and merge with `dt_desc_long` to create data.table ready to be plotted with ggplot2
  dt_Shapley_values_long <- create_Shapley_value_figure_dt(
    dt_Shapley_values = dt_Shapley_values,
    dt_desc_long = dt_desc_long,
    digits = digits,
    horizontal_bars = horizontal_bars
  )

  # Update the axis_labels parameters
  axis_labels_list <- update_axis_labels(
    axis_labels_rotate_angle = axis_labels_rotate_angle,
    axis_labels_n_dodge = axis_labels_n_dodge,
    horizontal_bars = horizontal_bars,
    length_of_longest_description = max(nchar(levels(dt_desc_long$.description)))
  )
  axis_labels_rotate_angle <- axis_labels_list[["axis_labels_rotate_angle"]]
  axis_labels_n_dodge <- axis_labels_list[["axis_labels_n_dodge"]]

  # Get the breaks and direction for the fill aesthetic
  breaks <- if (horizontal_bars) rev(levels(dt_Shapley_values_long$.method)) else levels(dt_Shapley_values_long$.method)
  direction <- if (horizontal_bars) -1 else 1

  # Plot --------------------------------------------------------------------------------
  figure <- ggplot2::ggplot(dt_Shapley_values_long, ggplot2::aes(x = .description, y = .phi))
  if (add_zero_line) figure <- figure + ggplot2::geom_abline(intercept = 0, slope = 0)
  figure <- figure +
    ggplot2::geom_col(
      width = geom_col_width,
      position = ggplot2::position_dodge(geom_col_width),
      ggplot2::aes(fill = .method)
    ) +
    ggplot2::facet_wrap(~.header, scales = facet_scales, labeller = "label_value", ncol = facet_ncol) +
    ggplot2::labs(
      x = "Feature and value",
      y = bquote("Feature contribution (Shapley value" ~ phi[j] * ")"),
      fill = "Method"
    ) +
    ggplot2::guides(x = ggplot2::guide_axis(n.dodge = axis_labels_n_dodge, angle = axis_labels_rotate_angle)) +
    ggplot2::labs(title = "Shapley value prediction explanation")
  if (is.null(brewer_palette)) {
    figure <- figure + ggplot2::scale_fill_discrete(
      breaks = breaks,
      direction = direction
    )
  }
  if (!is.null(brewer_palette)) {
    figure <- figure + ggplot2::scale_fill_brewer(
      breaks = breaks,
      direction = direction,
      palette = brewer_palette
    )
  }
  if (horizontal_bars) figure <- figure + ggplot2::coord_flip()

  if (isTRUE(print_ggplot)) {
    return(print(figure)) # Return the figure with force display
  } else {
    return(figure) # Return the figure without force display
  }
}


#' @keywords internal
#' @author Lars Henry Berge Olsen
update_only_these_features <- function(explanation_list,
                                       only_these_features,
                                       plot_phi0) {
  # Update the `only_these_features` parameter vector based on `plot_phi0` or in case it is NULL

  # Get the common feature names for all explanation objects (including `none`) and one without `none`
  feature_names_with_none <- colnames(explanation_list[[1]]$shapley_values_est)[-1]
  feature_names_without_none <- feature_names_with_none[feature_names_with_none != "none"]

  # Only keep the desired features/columns
  if (!is.null(only_these_features)) {
    # Check if user has provided a non-valid feature name, note that `none` is a valid feature name
    only_these_features_in_names <- only_these_features[only_these_features %in% feature_names_with_none]
    only_these_features_not_names <- only_these_features[!only_these_features %in% feature_names_with_none]

    # Give the user a warning if the user provided non-valid feature names
    if (length(only_these_features_not_names) > 0) {
      msg <- paste0(
        "User provided non-valid feature names in `only_these_features` (",
        paste0("'", only_these_features_not_names, "'", collapse = ", "),
        "). The function skips non-valid feature names."
      )
      cli::cli_inform(c("i" = msg))
    }

    # Stop if we have no valid feature names.
    if (length(only_these_features_in_names[only_these_features_in_names != "none"]) == 0) {
      cli::cli_abort(paste0(
        "The parameter `only_these_features` must contain at least one of: ",
        paste0("'", feature_names_without_none, "'", collapse = ", "),
        "."
      ))
    }

    # If user has specified `plot_phi0 = TRUE`, then we ensure that it is included in our variable
    if (plot_phi0) only_these_features_in_names <- unique(c("none", only_these_features_in_names))

    # Overwrite the `only_these_features` with `only_these_features_in_names` to remove non-valid input
    only_these_features <- only_these_features_in_names
  } else {
    # If user has specified `plot_phi0 = FALSE`, then we exclude the phi0/`none` from the feature names.
    only_these_features <- if (plot_phi0) feature_names_with_none else feature_names_without_none
  }

  return(only_these_features)
}

#' @keywords internal
#' @author Lars Henry Berge Olsen
extract_Shapley_values_dt <- function(explanation_list,
                                      index_explicands,
                                      only_these_features) {
  # Extract the Shapley values and combine them into a single data table.
  # We add an id column (`.id`) for the explicands and a column indicating the method (`.method`)
  dt_Shapley_values <- data.table::rbindlist(
    lapply(
      explanation_list,
      function(explanation) {
        data.table::copy(explanation$shapley_values_est)[, c(".id", ".pred") := list(.I, explanation$pred_explain)]
      }
    ),
    use.names = TRUE,
    idcol = ".method"
  )

  # Convert to factors
  dt_Shapley_values$.method <- factor(dt_Shapley_values$.method, levels = names(explanation_list), ordered = TRUE)

  # Set the keys and change the order of the columns
  data.table::setkeyv(dt_Shapley_values, c(".id", ".method"))
  data.table::setcolorder(dt_Shapley_values, c(".id", ".pred", ".method"))

  # Only keep the desired explicands
  if (!is.null(index_explicands)) dt_Shapley_values <- dt_Shapley_values[.id %in% index_explicands, ]

  # Give a small warning to the user if they have not specified the `index_explicands` and too many explicands
  if (length(index_explicands) > 12) {
    msg1 <- "It might be too many explicands to plot together in a nice fashion!"
    msg2 <- "Try for instance setting `index_explicands = 1:10` to limit the number of explicands."
    cli::cli_inform(c("i" = msg1, " " = msg2))
  }

  # Keep only the needed columns, and ensure that .id, .pred, and .method are included
  only_these_columns <- unique(c(".id", ".pred", ".method", only_these_features))
  dt_Shapley_values <- dt_Shapley_values[, only_these_columns, with = FALSE]

  return(dt_Shapley_values)
}


#' @keywords internal
#' @author Lars Henry Berge Olsen
update_axis_labels <- function(axis_labels_rotate_angle,
                               axis_labels_n_dodge,
                               horizontal_bars,
                               length_of_longest_description) {
  # User has provided neither `axis_labels_n_dodge` nor `axis_labels_rotate_angle`
  if (is.null(axis_labels_rotate_angle) && is.null(axis_labels_n_dodge)) {
    # Set default values
    axis_labels_rotate_angle <- 0
    axis_labels_n_dodge <- 1

    # Get the length of the longest description
    length_of_longest_description <- length_of_longest_description

    # If it is long, then we alter the default values set above and give message to user
    if (length_of_longest_description > 12 && !horizontal_bars) {
      msg1 <- paste0(
        "Long label names: consider specifying either `axis_labels_rotate_angle` or ",
        "`axis_labels_n_dodge`, to fix any potentially overlapping axis labels."
      )
      msg2 <- "The function sets `axis_labels_rotate_angle = 45` internally."

      cli::cli_inform(c("i" = msg1, " " = msg2))

      # Set it to rotate 45 degrees
      axis_labels_rotate_angle <- 45
    }
  }

  # User has specified `axis_labels_n_dodge` so set `axis_labels_rotate_angle` to default value
  if (is.null(axis_labels_rotate_angle)) axis_labels_rotate_angle <- 0

  # User has specified `axis_labels_rotate_angle` so set `axis_labels_n_dodge` to default value
  if (is.null(axis_labels_n_dodge)) axis_labels_n_dodge <- 1

  return(list(
    axis_labels_rotate_angle = axis_labels_rotate_angle,
    axis_labels_n_dodge = axis_labels_n_dodge
  ))
}


#' @keywords internal
#' @author Lars Henry Berge Olsen
create_feature_descriptions_dt <- function(explanation_list,
                                           only_these_features_wo_none,
                                           index_explicands,
                                           horizontal_bars,
                                           digits,
                                           include_group_feature_means) {
  # Check if are dealing with group-wise or feature-wise Shapley values
  if (explanation_list[[1]]$internal$parameters$is_groupwise) {
    # Group-wise Shapley values

    if (include_group_feature_means && any(explanation_list[[1]]$internal$objects$feature_specs$classes != "numeric")) {
      cli::cli_abort("`include_group_feature_means` cannot be `TRUE` for datasets with non-numerical features.")
    }

    # Get the relevant explicands
    x_explain <- explanation_list[[1]]$internal$data$x_explain[index_explicands]

    # Check if we are to compute the mean feature value within each group for each explicand
    if (include_group_feature_means) {
      feature_groups <- explanation_list[[1]]$internal$parameters$group
      x_explain <-
        x_explain[, lapply(feature_groups, function(cols) rowMeans(.SD[, .SD, .SDcols = cols], na.rm = TRUE))]

      # Extract only the relevant columns
      x_explain <- x_explain[, only_these_features_wo_none, with = FALSE]

      # Create the description matrix
      desc_mat <- trimws(format(x_explain, digits = digits))
      for (i in seq_len(ncol(desc_mat))) desc_mat[, i] <- paste0(colnames(desc_mat)[i], " = ", desc_mat[, i])
    } else {
      # Create the description matrix
      desc_mat <- matrix(rep(only_these_features_wo_none, each = nrow(x_explain)), nrow = nrow(x_explain))
      colnames(desc_mat) <- only_these_features_wo_none
    }
  } else {
    # Feature-wise Shapley values

    # Get the relevant explicands
    x_explain <-
      explanation_list[[1]]$internal$data$x_explain[index_explicands, only_these_features_wo_none, with = FALSE]

    # Create the description matrix
    desc_mat <- trimws(format(x_explain, digits = digits))
    for (i in seq_len(ncol(desc_mat))) desc_mat[, i] <- paste0(colnames(desc_mat)[i], " = ", desc_mat[, i])
  }

  # Converting and melting the explicands
  dt_desc <- data.table::as.data.table(cbind(none = "None", desc_mat))
  dt_desc_long <- data.table::melt(dt_desc[, .id := index_explicands],
    id.vars = ".id",
    variable.name = ".feature",
    value.name = ".description"
  )

  # Make the description into an ordered factor such that the features in the
  # bar plots follow the same order of features as in the training data.
  levels <- if (horizontal_bars) rev(unique(dt_desc_long$.description)) else unique(dt_desc_long$.description)
  dt_desc_long$.description <- factor(dt_desc_long$.description, levels = levels, ordered = TRUE)

  return(dt_desc_long)
}

#' @keywords internal
#' @author Lars Henry Berge Olsen
create_Shapley_value_figure_dt <- function(dt_Shapley_values,
                                           dt_desc_long,
                                           digits,
                                           horizontal_bars) {
  # This function takes in the wide `dt_Shapley_values` data.table, melt it and merge it with
  # dt_desc_long. Add some headers and do some manipulations based on if the plots are horizontal or not.

  # Melt the data.table from a wide to long format
  dt_Shapley_values_long <- data.table::melt(dt_Shapley_values,
    id.vars = c(".id", ".pred", ".method"),
    variable.name = ".feature",
    value.name = ".phi"
  )
  dt_Shapley_values_long$.feature <- as.ordered(dt_Shapley_values_long$.feature)


  # Data table for plotting
  dt_Shapley_values_long <- merge(dt_Shapley_values_long, dt_desc_long)

  # Make the .id column into an ordered column
  dt_Shapley_values_long$.id <- factor(dt_Shapley_values_long$.id,
    levels = unique(dt_Shapley_values_long$.id),
    ordered = TRUE
  )

  # Adding header for each individual plot
  dt_Shapley_values_long[, .header := paste0("id: ", .id, ", pred = ", format(.pred, digits = digits))]
  dt_Shapley_values_long$.header <- factor(dt_Shapley_values_long$.header,
    levels = unique(dt_Shapley_values_long$.header),
    ordered = TRUE
  )

  # If flip coordinates, then we need to change the order of the levels such that the order
  # of the bars in the figure match the order in the legend.
  if (horizontal_bars) {
    dt_Shapley_values_long$.method <- factor(dt_Shapley_values_long$.method,
      levels = rev(levels(dt_Shapley_values_long$.method)),
      ordered = TRUE
    )
    breaks <- rev(levels(dt_Shapley_values_long$.method))
  } else {
    breaks <- levels(dt_Shapley_values_long$.method)
  }

  return(dt_Shapley_values_long)
}
