#' Plot of the Shapley value explanations
#'
#' Plots the individual prediction explanations. Uses facet_wrap of ggplot
#'
#' @param explanation The output from compute_kshap
#' @param Xtest Data.table. Contains the covariates of the test data
#' @param no_desc_digits Integer. Number of significant digits to use in the feature description
#' @param plot_phi0 Logical. Whether to include phi0 in the plot
#' @param plot_which_Xtest Integer vector. Which of the test observations to plot
#' @param top_k_features Integer. How many features to include in the plot
#'
#' @inheritParams global_arguments
#'
#' @return ggplot object with plots of the Shapley value explanations
#'
#' @export
#'
#' @author Martin Jullum
plot_kshap <- function(explanation,
                       Xtest,
                       no_desc_digits = 3,
                       plot_phi0 = T,
                       plot_which_Xtest = 1:nrow(l$Xtest),
                       top_k_features = ncol(l$Xtest) + 1) {
  is_installed <- requireNamespace("ggplot2", quietly = TRUE)
  if (!is_installed) stop("ggplot2 is not installed. Please run install.packages('ggplot2')")
  colnam <- colnames(Xtest)

  # melting Kshap
  KshapDT <- data.table::copy(explanation$Kshap)
  KshapDT[, id := .I]
  meltKshap <- data.table::melt(KshapDT, id.vars = "id", value.name = "phi")
  meltKshap[, sign := factor(sign(phi), levels = c(1, -1), labels = c("Increases", "Decreases"))]

  # Converting and melting Xtest
  desc_mat <- format(Xtest, digits = no_desc_digits)
  for (i in 1:ncol(desc_mat)) {
    desc_mat[, i] <- paste0(colnam[i], " = ", desc_mat[, i])
  }
  desc_dt <- data.table::as.data.table(cbind(none = "none", desc_mat))
  melt_desc_dt <- data.table::melt(desc_dt[, id := .I], id.vars = "id", value.name = "description")

  # Data table for plotting
  plotting_dt <- merge(meltKshap, melt_desc_dt)

  # Adding the predictions
  predDT <- data.table::data.table(id = KshapDT$id, pred = explanation$pred_vec)
  plotting_dt <- merge(plotting_dt, predDT, by = "id")

  # Adding header for each individual plot
  plotting_dt[, header := paste0("id: ", id, ", pred = ", format(pred, digits = no_desc_digits + 1))]

  if (!plot_phi0) {
    plotting_dt <- plotting_dt[variable != "none"]
  }
  plotting_dt <- plotting_dt[id %in% plot_which_Xtest]
  plotting_dt[, rank := data.table::frank(-abs(phi)), by = id]
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
