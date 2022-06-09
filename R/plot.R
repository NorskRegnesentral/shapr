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
  plotting_dt[, header := paste0("id: ", id, ", pred = ", format(pred, digits = digits + 1))]

  if (!plot_phi0) {
    plotting_dt <- plotting_dt[variable != "none"]
  }
  plotting_dt <- plotting_dt[id %in% index_x_explain]
  plotting_dt[, rank := data.table::frank(-abs(phi)), by = "id"]
  plotting_dt <- plotting_dt[rank <= top_k_features]
  plotting_dt[, description := factor(description, levels = unique(description[order(abs(phi))]))]
  plotting_dt[, description_rev := factor(description, levels = unique(description[order(-abs(phi))]))] # need reverse rank order of feature descriptions for waterfall plot
  setorder(plotting_dt, rank)
  plotting_dt[, end:= cumsum(phi), by = id]
  expected <- plotting_dt[variable == "none", phi][[1]] #should E(f(x)) be extracted from x in a more "general" way..?
  plotting_dt[, start := c(expected, head(end, -1)), by = id]

  # Plotting regular bar plot

  if (plot_type == "bar"){
    gg <- ggplot2::ggplot(plotting_dt) +
      ggplot2::facet_wrap(~header, scales = "free_y", labeller = "label_value", ncol = 2) +
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
    gg <- ggplot2::ggplot(plotting_dt[variable != "none", ], aes(x = description_rev, fill = sign)) +
      ggplot2::facet_wrap(~header, scales = "free", labeller = "label_value", ncol = 2) +
      ggplot2::geom_rect(aes(x=description_rev, xmin = rank - 0.45 - 1, xmax = rank + 0.45 - 1, ymin = end, ymax = start)) +
      ggplot2::coord_flip(clip = 'off', xlim=c(0, max(plotting_dt[, rank]))) +
      ggplot2::scale_fill_manual(values = c("#F8766D", "#00BA38"), drop = TRUE) +
      ggplot2::labs(
        y = "Prediction",
        x = "Feature",
        fill = "",
        title = "Shapley value prediction explanation"
      ) +
      ggplot2::theme(
        legend.position = "bottom",
        plot.title = ggplot2::element_text(hjust = 0.5)
      ) +
      geom_segment(x=-Inf, xend = 1.45, y=expected, yend=expected, linetype="dashed", col="#F8766D") +
      geom_segment(aes(x=ifelse(rank==last(rank), rank-0.45-1, rank-0.45-1), xend = ifelse(rank==last(rank), rank, rank+1.45-1),
                       y=end, yend=end), linetype="dashed", col="#F8766D") +
      geom_text(size=2.5, parse=TRUE,
                data    = plotting_dt[variable != "none", ],
                mapping = aes(x = last(rank)+0.1, y = pred, label = paste0("f(x)==", format(pred, digits=digits)))
      ) +
      geom_text(size=2.5, aes(label = format(phi, digits=digits), x=rank-1, y=start + (end-start)/2), position = position_dodge(width = 1))
  }


  return(gg)
}
