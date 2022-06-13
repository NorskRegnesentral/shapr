#' Plot of the Shapley value explanations
#'
#' @description Plots the individual prediction explanations.
#'
#' @param x An \code{shapr} object. See \code{\link{explain}}.
#' @param digits Integer. Number of significant digits to use in the feature description
#' @param plot_phi0 Logical. Whether to include \code{phi0} in the plot
#' @param index_x_explain Integer vector. Which of the test observations to plot. E.g. if you have
#' explained 10 observations using \code{\link{explain}}, you can generate a plot for the first 5
#' observations by setting \code{index_x_explain = 1:5}.
#' @param top_k_features Integer. How many features to include in the plot. E.g. if you have 15
#' features in your model you can plot the 5 most important features, for each explanation, by setting
#' \code{top_k_features = 1:5}.
#' @param ... Currently not used.
#'
#' @details See \code{vignette("understanding_shapr", package = "shapr")} for an example of
#' how you should use the function.
#'
#' @return ggplot object with plots of the Shapley value explanations
#'
#' @export
#' @examples
#' if (requireNamespace("MASS", quietly = TRUE)) {
#'   #' # Load example data
#'   data("Boston", package = "MASS")
#'
#'   # Split data into test- and training data
#'   x_train <- head(Boston, -3)
#'   x_explain <- tail(Boston, 3)
#'
#'   # Fit a linear model
#'   model <- lm(medv ~ lstat + rm + dis + indus, data = x_train)
#'
#'   # Create an explainer object
#'   explainer <- shapr(x_train, model)
#'
#'   # Explain predictions
#'   p <- mean(x_train$medv)
#'
#'   # Empirical approach
#'   explanation <- explain(x_explain,
#'     explainer,
#'     approach = "empirical",
#'     prediction_zero = p,
#'     n_samples = 1e2
#'   )
#'
#'   if (requireNamespace("ggplot2", quietly = TRUE)) {
#'     # Plot the explantion (this function)
#'     plot(explanation)
#'   }
#' }
#' @author Martin Jullum
plot.shapr <- function(x,
                       digits = 3,
                       plot_phi0 = TRUE,
                       index_x_explain = NULL,
                       top_k_features = NULL,
                       plot_type = "bar",
                       col = c("#00BA38","#F8766D"), #first increasing color, then decreasing color
                       ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is not installed. Please run install.packages('ggplot2')")
  }
  #TODO: This functions needs to be updated to the new shapr paradigm -- and we could possibly add more functionality
  # like what shap() has in their package.

  if (is.null(index_x_explain)) index_x_explain <- seq(x$internal$parameters$n_explain)
  if (is.null(top_k_features)) top_k_features <- x$internal$parameters$n_features + 1
  id <- phi <- NULL # due to NSE notes in R CMD check

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
  header <- variable <- pred <- description <- NULL # due to NSE notes in R CMD check
  plotting_dt[, header := paste0("id: ", id, ", pred = ", format(pred, digits = digits+1))]

  plotting_dt[variable!="none", rank := data.table::frank(-abs(phi)), by = "id"]
  plotting_dt[variable=="none", rank:=0]

  N_features <- x$internal$parameters$n_features
  plotting_dt <- plotting_dt[id %in% index_x_explain]
  plotting_dt[rank > top_k_features,  phi:= sum(phi), by=id]
  plotting_dt[rank > top_k_features, variable:="rest", by=id]
  plotting_dt[variable == "rest", rank:=min(rank), by=id]
  plotting_dt[variable == "rest", description := paste(N_features - top_k_features,"other features")]
  plotting_dt[variable == "rest", sign:=ifelse(phi < 0, "Decreases", "Increases")]

  plotting_dt <- unique(plotting_dt)

  plotting_dt[variable!="none", rank_waterfall := data.table::frank(abs(phi)), by = "id"]
  plotting_dt[variable=="none", rank_waterfall:=0]

  #plotting_dt <- plotting_dt[rank <= top_k_features]
  plotting_dt[, description := factor(description, levels = unique(description[order(abs(phi))]))]

  setorder(plotting_dt, rank_waterfall)
  plotting_dt[, end:= cumsum(phi), by = id]
  expected <- plotting_dt[variable == "none", phi][[1]] #should phi0 be extracted from x in a more "general" way..?
  plotting_dt[, start := c(expected, head(end, -1)), by = id]
  plotting_dt[, phi_significant := format(phi, digits = digits), by=id]

  # waterfall plotting helper columns
  plotting_dt[, y_text := ifelse(abs(phi) < abs(min(start, end)-max(start, end))/10,
                                 ifelse(expected<pred, ifelse(end>start, end, start), ifelse(end<start,end,start)),
                                 start + (end - start)/2 ), by=id]
  plotting_dt[, text_color := ifelse(abs(phi) < abs(min(start, end)-max(start, end))/10,
                                     ifelse(sign=="Increases", col[1], col[2]),
                                     "white"), by=id]
  text_color <- plotting_dt[variable!="none", text_color]
  plotting_dt[, hjust_text := ifelse(abs(phi) < abs(min(start, end)-max(start, end))/10, ifelse(expected>pred, 1, 0), 0.5), by=id]
  plotting_dt[, arrow_color := ifelse(sign == "Increasing", col[1], col[2])]
  N_features <- max(plotting_dt[, rank_waterfall])
  n_obs <- max(plotting_dt[,id])
  plotting_dt[, pred_label := paste0("italic(f(x))==", format(pred, digits=digits+1))]
  plotting_dt[, pred_x:= N_features+0.8]

  if (plot_type == "bar"){
    if (!plot_phi0) {
      plotting_dt <- plotting_dt[variable != "none"]
    }

    gg <- ggplot2::ggplot(plotting_dt) +
      ggplot2::facet_wrap(~header, scales = "free_y", labeller = "label_value") +
      ggplot2::geom_col(ggplot2::aes(x = description, y = phi, fill = sign)) +
      ggplot2::coord_flip() +
      ggplot2::scale_fill_manual(values = c("steelblue", "lightsteelblue"), drop = TRUE) +
      ggplot2::labs(
        y = "Feature contribution",
        x = "Feature",
        fill = "",
        title = "Shapley value prediction explanation"
      ) +
      ggplot2::theme(
        legend.position = "bottom",
        plot.title = ggplot2::element_text(hjust = 0.5)
      )
  } else if (plot_type == "waterfall"){
    gg <- ggplot2::ggplot(plotting_dt[variable != "none", ], aes(x = description, fill = sign)) +
      ggplot2::facet_wrap(~header, scales = "free", labeller = "label_value") + #fix wrt ncol and layout for arbitrary num. obs.
      ggplot2::geom_segment(aes(x=-Inf, xend = max(rank_waterfall)+0.8, y=pred, yend=pred), linetype="dotted", col="dark grey", size=0.25) +
      ggplot2::geom_rect(aes(x=description, xmin = rank_waterfall - 0.3, xmax = rank_waterfall + 0.3, ymin = end, ymax = start), show.legend = FALSE) +
      ggplot2::coord_flip(clip = 'off', xlim=c(0.5, N_features+1.1)) +
      ggplot2::scale_fill_manual(values = col, drop = TRUE) + #why drop=TRUE?
      ggplot2::labs(
        y = "Prediction",
        x = "Feature",
        fill = "",
        title = "Shapley value prediction explanation") +
      ggplot2::theme_classic(base_family = "sans")+ #maybe?
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5)) +
      ggplot2::geom_segment(x=-Inf, xend = 1.3, y=expected, yend=expected, linetype="dotted", col="dark grey", size=0.25) +
      ggplot2::geom_text(size=2.5, family = "sans", col=text_color, aes(label = format(phi_significant, digits=digits),
                                                       x=rank_waterfall, y=y_text, vjust=0.5, hjust=hjust_text)) +
      ggplot2::annotate("text", parse=TRUE, x = -Inf, y = expected, label = paste0("~phi[0]==", format(expected, digits=digits+1)),
                        size=2.5, family = "sans", col="grey30",vjust=0, hjust=0 ) +
      ggplot2::geom_segment(aes(x=rank_waterfall+0.45, xend = rank_waterfall+0.45, y = start, yend = end, color=sign),
                            arrow=arrow(length = unit(0.03, "npc")), show.legend = FALSE) +
      ggplot2::geom_text(data=plotting_dt[1:n_obs,], aes(x = pred_x, y = pred, label = pred_label,
                         vjust=0, hjust=ifelse(pred > expected, 1, 0)),
                         parse=TRUE, family = "sans", col="grey30", size = 2.5) +
      ggplot2::scale_color_manual(values=col) #+
      # ggplot2::geom_text(size=2.5, family = "sans", col="grey30", parse=TRUE, aes(x = -Inf, y = expected,
      #                                              label = paste0("~phi[0]==", format(expected, digits=digits+1)),
      #                                              vjust=0, hjust=0)) +
      #annotation_custom(grid::linesGrob(y = c(0, 0.02),  gp = gpar(col = "black", lwd = 1.5)), ymin=expected, ymax=expected, xmin=-Inf, xmax=Inf)
  }
  return(gg)
}
