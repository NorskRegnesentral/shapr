#' Plot of the Shapley value explanations
#'
#' @description Plots the individual prediction explanations.
#'
#' @param x An \code{shapr} object.
#'  The output from [explain()].
#' @param plot_type Character.
#'  Specifies the type of plot to produce.
#'  \code{"bar"} (the default) gives a regular horizontal bar plot of the Shapley value magnitudes.
#'  \code{"waterfall"} gives a waterfall plot indicating the changes in the prediction score due to each features
#'  contribution (their Shapley values).
#'  \code{"scatter"} plots the feature values on the x-axis and Shapley values on the y-axis, as well as
#'  (optionally) a background scatter_hist showing the distribution of the feature data.
#'  \code{"beeswarm"} summarises the distribution of the Shapley values along the x-axis for all the features. Each point gives
#'  the shapley value of a given instance, where the points are colored by the feature value of that instance.
#' @param digits Integer.
#' Number of significant digits to use in the feature description
#' @param plot_phi0 Logical.
#' Whether to include \code{phi0} in the plot.
#' @param index_x_explain Integer vector.
#' Which of the test observations to plot. E.g. if you have
#' explained 10 observations using \code{\link{explain}}, you can generate a plot for the first 5
#' observations by setting \code{index_x_explain = 1:5}.
#' @param top_k_features Integer.
#' How many features to include in the plot. E.g. if you have 15
#' features in your model you can plot the 5 most important features, for each explanation, by setting
#' \code{top_k_features = 1:5}.
#' @param col Character vector (length depends on plot type).
#' The color codes (hex codes or other names understood by [ggplot2::ggplot()]) for positive and negative
#' Shapley values, respectively.
#' The default is \code{col=NULL}, plotting with the default colors respective to the plot type.
#' For \code{plot_type = "bar"} and \code{plot_type = "waterfall"}, the default is \code{c("#00BA38","#F8766D")}.
#' For \code{plot_type = "beeswarm"}, the default is \code{c("#F8766D","yellow","#00BA38")}.
#' For \code{plot_type = "scatter"}, the default is \code{"#619CFF"}.
#'
#' If you want to alter the colors i the plot, the length of the \code{col} vector depends on plot type.
#' For \code{plot_type = "bar"} or \code{plot_type = "waterfall"}, two colors should be provided, first for positive and then for negative Shapley values.
#' For \code{plot_type = "beeswarm"}, either two or three colors can be given. If two colors are given, then the first color determines
#' the color that points with high feature values will have, and the second determines the color of points with low feature values.
#' If three colors are given, then the first colors high feature values, the second colors mid-range feature values, and the third colors low feature values.
#' For instance, \code{col = c("red", "yellow", "blue")} will make high values red, mid-range values yellow, and low values blue.
#' For \code{plot_type = "scatter"}, a single color is to be given, which determines the color of the points on the scatter plot.
#' @param plot_order Character.
#' Specifies what order to plot the features with respect to the magnitude of the shapley values.
#'  \code{"largest_first"} (the default) plots the features ordered from largest to smallest absolute Shapley value.
#'  \code{"smallest_first"} plots the features ordered from smallest to largest absolute Shapley value.
#'  \code{"original"} plots the features in the original order of the data table.
#' @param scatter_features Integer or character vector.
#' Only used for \code{plot_type = "scatter"}.
#' Specifies what features to include in (scatter) plot. Can be a numerical vector indicating feature index, or a
#' character vector, indicating the name(s) of the feature(s) to plot.
#' @param scatter_hist Logical.
#' Only used for \code{plot_type = "scatter"}.
#' Whether to include a scatter_hist indicating the distribution of the data when making the scatter plot. Note that the
#' bins are scaled so that when all the bins are stacked they fit the span of the y-axis of the plot.
#' @param ... Currently not used.
#'
#' @details See the examples below, or \code{vignette("understanding_shapr", package = "shapr")} for an examples of
#' how you should use the function.
#'
#' @return ggplot object with plots of the Shapley value explanations
#'
#' @export
#' @examples
#'
#' data("airquality")
#' airquality <- airquality[complete.cases(airquality), ]
#' # Split data into test- and training data
#' dat_train <- head(airquality, -50)
#' dat_explain <- tail(airquality, 50)
#'
#' # Fit a linear model
#' model <- lm(Ozone ~ Solar.R + Wind+ Temp + Month, data = dat_train)
#'
#' p <- mean(dat_train$Ozone)
#'
#' x <- explain(
#' dat_train,
#' dat_explain,
#' model = model,
#' approach = "empirical",
#' prediction_zero = p
#' )
#'
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#' # The default plotting option is a bar plot of the Shapley values
#' # We draw bar plots for the first 4 observations
#' plot(x, index_x_explain = 1:4)
#'
#' # We can also make waterfall plots
#' plot(x, plot_type = "waterfall", index_x_explain = 1:4)
#' # And only showing the 2 features with largest contribution
#' plot(x, plot_type = "waterfall", index_x_explain = 1:4, top_k_features = 2)
#'
#' # Or scatter plots showing the distribution of the shapley values and feature values
#' plot(x, plot_type = "scatter")
#' # And only for a specific feature
#' plot(x, plot_type = "scatter", scatter_features = "Temp")
#'
#' # Or a beeswarm plot summarising the Shapley values and feature values for all features
#' plot(x, plot_type = "beeswarm")
#' plot(x, plot_type = "beeswarm", col = c("red", "black")) # we can change colors
#' }
#'
#' @author Martin Jullum, Vilde Ung
plot.shapr <- function(x,
                       plot_type = "bar",
                       digits = 3,
                       plot_phi0 = TRUE,
                       index_x_explain = NULL,
                       top_k_features = NULL,
                       col = NULL, #first increasing color, then decreasing color
                       plot_order = "largest_first",
                       scatter_features = NULL,
                       scatter_hist = TRUE,
                       ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is not installed. Please run install.packages('ggplot2')")
  }
  if(!(plot_type%in%c("bar", "waterfall", "scatter", "beeswarm"))){
    stop(paste(plot_type, "is an invalid plot type. Try plot_type='bar', plot_type='waterfall', plot_type='scatter', or plot_type='beeswarm'."))
  }
  if(!(plot_order%in%c("largest_first", "smallest_first", "original"))){
    stop(paste(plot_order, "is an invalid plot order. Try plot_order='largest_first', plot_order='smallest_first' or plot_order='original'."))
  }
  rank_waterfall <- end <- start <- phi_significant <- y_text <- hjust_text <- arrow_color <- NULL # due to NSE warnings
  sign <- y_text_bar <- hjust_text_bar <- feature_value <- positive <- feature_value_scaled <- text_color_bar <- NULL
  unique_label <- pred_label <- pred_x <- element_rect <- element_line <- guide_colourbar <- NULL
  x_start <- x_end <- y_start <- y_end <- phi0_x <- phi0_label <- id <- phi <- NULL
  header <- variable <- pred <- description <- min <- max <- NULL

  if (is.null(index_x_explain)) index_x_explain <- seq(x$internal$parameters$n_explain)
  if (is.null(top_k_features)) top_k_features <- x$internal$parameters$n_features + 1

  is_groupwise <- x$internal$parameters$is_groupwise

  # melting Kshap
  shap_names <- colnames(x$shapley_values)[-1]
  KshapDT <- data.table::copy(x$shapley_values)
  KshapDT[, id := .I]
  meltKshap <- data.table::melt(KshapDT, id.vars = "id", value.name = "phi")
  meltKshap[, sign := factor(sign(phi), levels = c(1, -1), labels = c("Increases", "Decreases"))]

  # Converting and melting Xtest
  if (!is_groupwise) {
    desc_mat <- format(x$internal$data$x_explain, digits = digits)
    for (i in 1:ncol(desc_mat)) {
      desc_mat[, i] <- paste0(shap_names[i], " = ", desc_mat[, i])
    }
  } else {
    desc_mat <- format(x$shapley_values[, -1], digits = digits)
    for (i in 1:ncol(desc_mat)) {
      desc_mat[, i] <- paste0(shap_names[i])
    }
  }

  desc_dt <- data.table::as.data.table(cbind(none = "none", desc_mat))
  melt_desc_dt <- data.table::melt(desc_dt[, id := .I], id.vars = "id", value.name = "description")

  # Data table for plotting
  plotting_dt <- merge(meltKshap, melt_desc_dt)

  # Adding the predictions
  predDT <- data.table::data.table(id = KshapDT$id, pred = x$p)
  plotting_dt <- merge(plotting_dt, predDT, by = "id")

  # Adding header for each individual plot
  plotting_dt[, header := paste0("id: ", id, ", pred = ", format(pred, digits = digits+1))]

  if(plot_type == "scatter" | plot_type == "beeswarm"){
    # Add feature values to data table
    feature_vals <- data.table::copy(x$internal$data$x_explain)
    feature_vals <- as.data.table(cbind(none=NA, feature_vals))
    feature_vals[, id:=.I]
    melt_feature_vals <- suppressWarnings(data.table::melt(feature_vals, id.vars = "id",
                                                           value.name = "feature_value")) #this gives a warning because none-values are NA...
    plotting_dt <- merge(plotting_dt, melt_feature_vals, by=c("id", "variable"))
  }

  if(plot_type=="scatter"){
    # Only plot the desired observations
    plotting_dt <- plotting_dt[id %in% index_x_explain]
    gg <- make_scatter_plot(plotting_dt, scatter_features, scatter_hist, col)

  } else if (plot_type=="beeswarm"){
    gg <- make_beeswarm_plot(plotting_dt, col, index_x_explain, x)

  } else { #if bar og waterfall plot
    # Only plot the desired observations
    plotting_dt <- plotting_dt[id %in% index_x_explain]

    if(length(plotting_dt[, unique(id)]) > 10){
      stop("Too many observations to plot together! Try for instance setting index_x_explain = 1:10 so that the max. is not exceeded.")
    }

    if(plot_order == "largest_first"){
        plotting_dt[variable != "none", rank := data.table::frank(-abs(phi)), by = "id"]
      } else if (plot_order == "smallest_first"){
        plotting_dt[variable != "none", rank := data.table::frank(abs(phi)), by = "id"]
      } else if (plot_order == "original"){
        plotting_dt[variable != "none", rank := seq_along(phi), by = "id"]
      }
    plotting_dt[variable == "none", rank := 0]
    N_features <- x$internal$parameters$n_features

    # collapse phi-value for features that are not in top k features
    plotting_dt[rank > top_k_features,  phi:= sum(phi), by=id]
    plotting_dt[rank > top_k_features, variable:="rest", by=id]
    plotting_dt[variable == "rest", rank := min(rank), by=id]
    plotting_dt[variable == "rest", description := paste(N_features - top_k_features, "other features")]
    plotting_dt[variable == "rest", sign := ifelse(phi < 0, "Decreases", "Increases")]
    plotting_dt <- unique(plotting_dt)

    #unique label for correct order when plotting multiple observations
    plotting_dt[, unique_label := rev(seq_along(description))]
    plotting_dt[variable == "none", unique_label := 0] #such that none is always at top of plot
    plotting_dt[variable == "rest", unique_label := -1] #such that rest is always at bottom of plot
    if(plot_order=="largest_first"){
      unique_levels <- c(-1, plotting_dt[variable != "none" & variable != "rest", unique_label[order(abs(phi))]], 0)
    } else if(plot_order=="smallest_first"){
      unique_levels <- c(-1, plotting_dt[variable != "none" & variable != "rest", unique_label[order(-abs(phi))]], 0)
    } else if (plot_order == "original"){
      unique_levels <- c(-1, rev(plotting_dt[variable != "none" & variable != "rest", unique_label]), 0)
    }
    plotting_dt[, unique_label := factor(unique_label, levels = unique_levels)]
    if(plot_order=="largest_first"){
      plotting_dt[variable != "none", rank_waterfall := data.table::frank(abs(phi)), by = "id"]
    } else if(plot_order=="smallest_first"){
      plotting_dt[variable != "none", rank_waterfall := data.table::frank(-abs(phi)), by = "id"]
    } else if (plot_order == "original"){
      plotting_dt[variable != "none", rank_waterfall := rev(seq_along(phi)), by = "id"]
    }
    plotting_dt[variable == "none", rank_waterfall := 0]

    # compute start and end values for waterfall rectangles
    data.table::setorder(plotting_dt, rank_waterfall)
    plotting_dt[, end:= cumsum(phi), by = id]
    expected <- x$internal$parameters$prediction_zero
    plotting_dt[, start := c(expected, head(end, -1)), by = id]
    plotting_dt[, phi_significant := format(phi, digits=digits), by=id]

    # helpers for labelling y-axis correctly
    if(plot_order=="largest_first"){
      desc_labels <- plotting_dt[variable!="none" & variable != "rest", description[order(abs(phi))]]
    } else if(plot_order=="smallest_first"){
      desc_labels <- plotting_dt[variable!="none" & variable != "rest", description[order(-abs(phi))]]
    }else if (plot_order == "original"){
      desc_labels <- plotting_dt[variable!="none" & variable != "rest", description[order(unique_label)]]
    }
    if (top_k_features < x$internal$parameters$n_features){ #if there is a "rest" of collapsed lower-rank features
      desc_labels <- c(paste(x$internal$parameters$n_features - top_k_features, "other features"),
                       desc_labels)
    }
    if (!plot_phi0 | plot_type == "waterfall") { #if none is not to be included in plot
      plotting_dt <- plotting_dt[variable != "none"]
    } else {
      desc_labels <- c(desc_labels, "None")
    }
    breaks <- levels(droplevels(plotting_dt[, unique_label])) #removes -1 if no rest and 0 if no none in plot

    if (plot_type == "bar"){
      gg <- make_bar_plot(plotting_dt, plot_phi0, col, breaks, desc_labels)
    } else if (plot_type == "waterfall"){
      gg <- make_waterfall_plot(plotting_dt, expected, col, digits, plot_order, breaks, desc_labels)
    }
  }
  return(gg)
}

compute_scatter_hist_values <- function(plotting_dt, scatter_features){
  rank_waterfall <- end <- start <- phi_significant <- y_text <- hjust_text <- arrow_color <- NULL # due to NSE warnings
  sign <- y_text_bar <- hjust_text_bar <- feature_value <- positive <- feature_value_scaled <- text_color_bar <- NULL
  unique_label <- pred_label <- pred_x <- element_rect <- element_line <- guide_colourbar <- NULL
  x_start <- x_end <- y_start <- y_end <- phi0_x <- phi0_label <- id <- phi <- NULL
  header <- variable <- pred <- description <- min <- max <- NULL

  n_feat_vals <- plotting_dt[ , .N, by = variable][1,"N"] #number of points to plot
  if(n_feat_vals > 500){
    num_breaks <- 50
  } else if(n_feat_vals > 200){
    num_breaks <- 20
  } else if(n_feat_vals > 100){
    num_breaks <- 10
  } else {
    num_breaks <-5
  }

  scatter_hist_dt_list <- list()
  for(feature_name in scatter_features){
    x <- plotting_dt[variable==feature_name, feature_value]
    if(min(x)==max(x)){
      scatter_hist_object <- hist(x, breaks = 1, plot=FALSE)

    } else{
      scatter_hist_object <- hist(x, breaks = seq(min(x), max(x), length.out = num_breaks), plot=FALSE)

    }
    y_max <- max(plotting_dt[variable==feature_name, phi])
    y_min <- min(plotting_dt[variable==feature_name, phi])
    y_tot <- y_max-y_min #what if these happen to be the same...?
    count_tot <- sum(scatter_hist_object$count)
    count_scale <- y_tot/count_tot

    xvals <- scatter_hist_object$breaks
    x_start <- xvals[-length(xvals)]
    x_end <- xvals[-1]
    y_end <- count_scale*scatter_hist_object$count + y_min

    bins_dt <- data.table(x_start = x_start,
                          x_end = x_end,
                          y_end = y_end,
                          y_start = y_min,
                          variable = feature_name)

    scatter_hist_dt_list[[feature_name]] <- bins_dt
  }
  scatter_hist_dt <- data.table::rbindlist(scatter_hist_dt_list)

  return(scatter_hist_dt)
}

make_scatter_plot <- function(plotting_dt, scatter_features, scatter_hist, col){
  rank_waterfall <- end <- start <- phi_significant <- y_text <- hjust_text <- arrow_color <- NULL # due to NSE warnings
  sign <- y_text_bar <- hjust_text_bar <- feature_value <- positive <- feature_value_scaled <- text_color_bar <- NULL
  unique_label <- pred_label <- pred_x <- element_rect <- element_line <- guide_colourbar <- NULL
  x_start <- x_end <- y_start <- y_end <- phi0_x <- phi0_label <- id <- phi <- NULL
  header <- variable <- pred <- description <- min <- max <- NULL

  if(is.null(col)){
    col = "#619CFF"
  } else if (length(col) != 1){
    stop("'col' must be of length 1 when making scatter plot.")
  }

  plotting_dt <- plotting_dt[variable != "none", ]

  if(is.null(scatter_features)){
    scatter_features <- unique(plotting_dt[, variable])
  } else if(is.numeric(scatter_features)){
    scatter_features <- plotting_dt[scatter_features, unique(variable)] #i.e. plot first 4 features if scatter_features = 1:4
  } else if(is.character(scatter_features)){
    if(any(!(scatter_features %in% unique(plotting_dt[, variable])))){
      stop("Some or all of the listed feature names in 'scatter_features' do not match the names in the data.")
    }
  }

  plotting_dt <- plotting_dt[variable %in% scatter_features, ]
  gg <- ggplot2::ggplot(plotting_dt) +
    ggplot2::facet_wrap(~variable, scales = "free", labeller = "label_value")

  # compute bin values for scatter_hist
  if(scatter_hist){
    scatter_hist_dt <- compute_scatter_hist_values(plotting_dt, scatter_features)
    gg <- gg + ggplot2::geom_rect(data=scatter_hist_dt, ggplot2::aes(xmin=x_start, xmax=x_end, ymin=y_start,ymax=y_end), fill = "grey80")
  }

  gg <- gg + ggplot2::geom_point(ggplot2::aes(x=feature_value, y=phi), colour=col) +
    ggplot2::theme_classic(base_family = "sans") +
    ggplot2::theme(legend.position = "bottom",
                   plot.title = ggplot2::element_text(hjust = 0.5),
                   strip.background = ggplot2::element_rect(colour = "white", fill = "grey90"),
                   panel.grid.major.y = ggplot2::element_line(colour = "grey90")
    ) +
    ggplot2::labs(x = "Feature values",
         y = "Shapley values")
  return(gg)
}

make_beeswarm_plot <- function(plotting_dt, col, index_x_explain, x){
  rank_waterfall <- end <- start <- phi_significant <- y_text <- hjust_text <- arrow_color <- NULL # due to NSE warnings
  sign <- y_text_bar <- hjust_text_bar <- feature_value <- positive <- feature_value_scaled <- text_color_bar <- NULL
  unique_label <- pred_label <- pred_x <- element_rect <- element_line <- guide_colourbar <- NULL
  x_start <- x_end <- y_start <- y_end <- phi0_x <- phi0_label <- id <- phi <- NULL
  header <- variable <- pred <- description <- min <- max <- NULL

  if (!requireNamespace("ggbeeswarm", quietly = TRUE)) {
    stop("geom_beeswarm is not installed. Please run install.packages('ggbeeswarm')")
  }

  if(is.null(col)){
    col = c("#F8766D","yellow","#00BA38")
  }
  if (!(length(col) %in% c(2,3))){
    stop("'col' must be of length 2 or 3 when making beeswarm plot.")
  }

  plotting_dt <- plotting_dt[variable!="none",]

  train_dt <- data.table::copy(x$internal$data$x_train)
  train_dt <- data.table::melt(train_dt[,id:=.I], id.vars = "id", value.name = "feature_value")
  train_dt[, `:=`(max=max(feature_value),min=min(feature_value)), by=variable]
  train_dt <- train_dt[,.(variable,max,min)]
  train_dt <- unique(train_dt)
  plotting_dt <- merge(plotting_dt, train_dt, by="variable")

  # scale obs. features value to their distance from min. feature value relative to the distance between min. and max. feature value
  # in order to have a global color bar indicating magnitude of obs. feature value.
  # The feature values are scaled wrt the training data
  plotting_dt[feature_value <= max & feature_value >= min, feature_value_scaled := (feature_value - min) / (max - min), by = variable]
  plotting_dt[feature_value > max, feature_value_scaled := 1]
  plotting_dt[feature_value < min, feature_value_scaled := 0]

  # make sure features with only one value are also scaled
  plotting_dt[is.nan(feature_value_scaled), feature_value_scaled := 0.5, by = variable]

  # Only plot the desired observations
  plotting_dt <- plotting_dt[id %in% index_x_explain]

  gg <- ggplot2::ggplot(plotting_dt, ggplot2::aes(x = variable, y = phi, color = feature_value_scaled)) +
    ggplot2::geom_hline(yintercept = 0 , color="grey70", size = 0.5)+
    ggbeeswarm::geom_beeswarm(priority = 'random', cex = 0.4) + #the cex-parameter doesnt generalize well, should use corral but not available yet....
    ggplot2::coord_flip() +
    ggplot2::theme_classic() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_line(colour = "grey90", linetype = "dashed")) +
    ggplot2::labs(x = "", y = "Shapley value")+
    ggplot2::guides(color = ggplot2::guide_colourbar(ticks = FALSE,
                                   barwidth = 0.5, barheight = 10))

  if(length(col)==3){ #check is col-parameter is the default
    gg <- gg +
      ggplot2::scale_color_gradient2(low = col[3], mid = col[2], high = col[1],
                                     midpoint = 0.5,
                                     breaks = c(0,1),
                                     limits = c(0,1),
                                     labels = c("Low", "High"),
                                     name = "Feature \n value")
  } else if (length(col)==2){ # allow user to specify three colors
    gg <- gg +
      ggplot2::scale_color_gradient(low = col[2], high = col[1],
                                    breaks = c(0,1),
                                    limits = c(0,1),
                                    labels = c("Low", "High"),
                                    name = "Feature \n value")
  }

  return(gg)
}

make_bar_plot <- function(plotting_dt, plot_phi0, col, breaks, desc_labels){
  rank_waterfall <- end <- start <- phi_significant <- y_text <- hjust_text <- arrow_color <- NULL # due to NSE warnings
  sign <- y_text_bar <- hjust_text_bar <- feature_value <- positive <- feature_value_scaled <- text_color_bar <- NULL
  unique_label <- pred_label <- pred_x <- element_rect <- element_line <- guide_colourbar <- NULL
  x_start <- x_end <- y_start <- y_end <- phi0_x <- phi0_label <- id <- phi <- NULL
  header <- variable <- pred <- description <- min <- max <- NULL

  if(is.null(col)){
    col = c("#00BA38","#F8766D")
  }
  if (length(col)!=2){
    stop("'col' must be of length 2 when making bar plot.")
  }


  if (!(plot_phi0)) {
    plotting_dt <- plotting_dt[variable != "none", ]
  }

  # bar plotting helpers
  plotting_dt[, y_text_bar := ifelse(abs(phi) > max(abs(phi))/8, phi/2, 0), by = id]
  plotting_dt[, positive := sign[which.max(abs(phi))] == "Increases", by = id] # text placement depends on the direction of the largest bar, in order for text not to be clipped
  plotting_dt[, hjust_text_bar := ifelse(abs(phi) > max(abs(phi))/8, 0.5, 1), by = id]
  plotting_dt[positive == TRUE & y_text_bar == 0, hjust_text_bar := 0]
  plotting_dt[positive == TRUE & y_text_bar == 0, y_text_bar := ifelse(phi>0 , phi, 0)]
  plotting_dt[positive == FALSE & y_text_bar == 0, y_text_bar := ifelse(phi<0, phi, 0)]

  plotting_dt[, text_color_bar := ifelse(abs(phi) > max(abs(phi))/8, "white", ifelse(sign=="Increases", col[1], col[2])), by = id]
  if(plot_phi0){
    text_color_bar <- plotting_dt[, text_color_bar]
  } else{
    text_color_bar <- plotting_dt[variable != "none", text_color_bar]
  }

  #make plot
  gg <- ggplot2::ggplot(plotting_dt, ggplot2::aes(x=unique_label, fill=sign)) +
    ggplot2::facet_wrap(~header, scales = "free", labeller = "label_value", ncol=2) +
    ggplot2::theme_classic(base_family = "sans") +
    ggplot2::theme(legend.position = "bottom",
                   plot.title = ggplot2::element_text(hjust = 0.5),
                   strip.background = ggplot2::element_rect(colour = "white", fill = "white")) +
    ggplot2::scale_fill_manual(values = col, drop = TRUE) +
    ggplot2::scale_x_discrete(breaks = breaks, labels = desc_labels) +
    ggplot2::geom_col(ggplot2::aes(y=phi)) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      y = "Feature contribution",
      x = "Feature",
      fill = "",
      title = "Shapley value prediction explanation"
    ) +
    ggplot2::geom_text(ggplot2::aes(label = phi_significant,
                                    x = unique_label, y = y_text_bar,
                                    vjust = 0.5, hjust = hjust_text_bar
    ),
    size=2.5, family = "sans", col = text_color_bar
    )

  return(gg)
}

make_waterfall_plot <- function(plotting_dt,
                                expected,
                                col,
                                digits,
                                plot_order,
                                breaks,
                                desc_labels){
  rank_waterfall <- end <- start <- phi_significant <- y_text <- hjust_text <- arrow_color <- NULL # due to NSE warnings
  sign <- y_text_bar <- hjust_text_bar <- feature_value <- positive <- feature_value_scaled <- text_color_bar <- NULL
  unique_label <- pred_label <- pred_x <- element_rect <- element_line <- guide_colourbar <- NULL
  x_start <- x_end <- y_start <- y_end <- phi0_x <- phi0_label <- id <- phi <- NULL
  header <- variable <- pred <- description <- min <- max <- NULL

  if(is.null(col)){
    col = c("#00BA38","#F8766D")
  }
  if (length(col)!=2){
    stop("'col' must be of length 2 when making waterfall plot.")
  }

  # waterfall plotting helpers
  if (plot_order=="largest_first" | plot_order == "original"){
    plotting_dt[, y_text := ifelse(abs(phi) < abs(min(start, end)-max(start, end))/8,
                                   ifelse(expected<pred, ifelse(end>start, end, start), ifelse(end<start,end,start)),
                                   start + (end - start)/2 ), by=id]
  } else if(plot_order=="smallest_first"){
    plotting_dt[, y_text := ifelse(abs(phi) < abs(min(start, end)-max(start, end))/8,
                                   ifelse(expected>pred, ifelse(end>start, end, start), ifelse(end<start,end,start)),
                                   start + (end - start)/2 ), by=id]
  }

  plotting_dt[, text_color := ifelse(abs(phi) < abs(min(start, end)-max(start, end))/8,
                                     ifelse(sign=="Increases", col[1], col[2]),
                                     "white"), by=id]
  text_color <- plotting_dt[variable!="none", text_color]

  if(plot_order=="largest_first"| plot_order == "original"){
    plotting_dt[, hjust_text := ifelse(abs(phi) < abs(min(start, end)-max(start, end))/8, ifelse(expected>pred, 1, 0), 0.5), by=id]

  } else if(plot_order=="smallest_first"){
    plotting_dt[, hjust_text := ifelse(abs(phi) < abs(min(start, end)-max(start, end))/8, ifelse(expected>pred, 0, 1), 0.5), by=id]

  }

  plotting_dt[, arrow_color := ifelse(sign == "Increasing", col[1], col[2])]
  N_features <- max(plotting_dt[, rank_waterfall])
  n_obs <- length(plotting_dt[,unique(id)])
  plotting_dt[, pred_label := paste0("italic(f(x))==", format(pred, digits=digits+1))]
  plotting_dt[, pred_x:= N_features+0.8]
  plotting_dt[, phi0_label := paste0("~phi[0]==", format(expected, digits=digits+1))]
  plotting_dt[, phi0_x:= 0]

  gg <- ggplot2::ggplot(plotting_dt, ggplot2::aes(x=unique_label, fill=sign)) +
    ggplot2::facet_wrap(~header, scales = "free", labeller = "label_value", ncol=2) +
    ggplot2::theme_classic(base_family = "sans") +
    ggplot2::theme(legend.position = "bottom",
                   plot.title = ggplot2::element_text(hjust = 0.5),
                   strip.background = ggplot2::element_rect(colour = "white", fill = "white")) +
    ggplot2::scale_fill_manual(values = col, drop = TRUE) +
    ggplot2::scale_x_discrete(breaks = breaks, labels = desc_labels) +
    ggplot2::geom_segment(ggplot2::aes(x=-Inf, xend = max(rank_waterfall)+0.8, y=pred, yend=pred),
                                   linetype="dotted", col="grey30", size=0.25) +
    ggplot2::coord_flip(clip = 'off', xlim=c(0.5, ifelse(N_features+N_features*0.11 < N_features+0.5,
                                                         N_features+0.5,
                                                         N_features+N_features*0.11)))  +
    ggplot2::labs(y = "Prediction",
                  x = "Feature",
                  fill = "",
                  title = "Shapley value prediction explanation") +
    ggplot2::geom_rect(ggplot2::aes(xmin = rank_waterfall - 0.3, xmax = rank_waterfall + 0.3, ymin = end, ymax = start),
                       show.legend = NA) +
    ggplot2::geom_segment(x=-Inf, xend = 1.3, y=expected, yend=expected,
                          linetype="dotted", col="grey30", size=0.25) +
    ggplot2::geom_text(ggplot2::aes(label = phi_significant,
                                    x = rank_waterfall, y = y_text,
                                    vjust = 0.5, hjust = hjust_text),
                       size=2.5, family = "sans", col = text_color) +
    ggplot2::geom_segment(ggplot2::aes(x=rank_waterfall+0.45, xend = rank_waterfall+0.45, y = start, yend = end, color=sign),
                          arrow=ggplot2::arrow(length = ggplot2::unit(0.03, "npc")), show.legend = FALSE) +
    ggplot2::scale_color_manual(values=col) +
    ggplot2::geom_text(data=plotting_dt[1:n_obs,],
                       ggplot2::aes(x = pred_x, y = pred, label = pred_label,
                                    vjust = 0, hjust = ifelse(pred > expected, 1, 0)),
                       parse=TRUE, family = "sans", col="grey30", size = 2.5) +
    ggplot2::geom_text(data=plotting_dt[1:n_obs,],
                       ggplot2::aes(x = phi0_x, y = expected, label = phi0_label,
                                    vjust = 0, hjust = ifelse(pred < expected, 1, 0)),
                       parse=TRUE, family = "sans", col="grey30", size = 2.5)

  return(gg)
}
