#' Plot of the Shapley value explanations
#'
#' @description Plots the individual prediction explanations.
#'
#' @param x An \code{shapr} object. See \code{\link{explain}}.
#' @param digits Integer. Number of significant digits to use in the feature description
#' @param plot_phi0 Logical. Whether to include \code{phi0} in the plot
#' @param index_x_test Integer vector. Which of the test observations to plot. E.g. if you have
#' explained 10 observations using \code{\link{explain}}, you can generate a plot for the first 5
#' observations by setting \code{index_x_test = 1:5}.
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
#'
#' @author Martin Jullum
plot.shapr <- function(x,
                       digits = 3,
                       plot_phi0 = TRUE,
                       index_x_test = NULL,
                       top_k_features = NULL,
                       ...) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is not installed. Please run install.packages('ggplot2')")
  }

  if (is.null(index_x_test)) index_x_test <- seq(nrow(x$x_test))
  if (is.null(top_k_features)) top_k_features <- ncol(x$x_test) + 1
  id <- phi <- NULL # due to NSE notes in R CMD check

  # melting Kshap
  cnms <- colnames(x$x_test)
  KshapDT <- data.table::copy(x$dt)
  KshapDT[, id := .I]
  meltKshap <- data.table::melt(KshapDT, id.vars = "id", value.name = "phi")
  meltKshap[, sign := factor(sign(phi), levels = c(1, -1), labels = c("Increases", "Decreases"))]

  # Converting and melting Xtest
  desc_mat <- format(x$x_test, digits = digits)
  for (i in 1:ncol(desc_mat)) {
    desc_mat[, i] <- paste0(cnms[i], " = ", desc_mat[, i])
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
  plotting_dt <- plotting_dt[id %in% index_x_test]
  plotting_dt[, rank := data.table::frank(-abs(phi)), by = "id"]
  plotting_dt <- plotting_dt[rank <= top_k_features]
  plotting_dt[, description := factor(description, levels = unique(description[order(abs(phi))]))]

  # Plotting
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

  return(gg)
}
