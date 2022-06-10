# The SHAP value of a feature represents the impact of the evidence provided by that feature on the model's
# output. The waterfall plot is designed to visually display how the SHAP values (evidence) of each feature
# move the model output from our prior expectation under the background data distribution, to the final model
# prediction given the evidence of all the features. Features are sorted by the magnitude of their SHAP values
# with the smallest magnitude features grouped together at the bottom of the plot when the number of features
# in the models exceeds the max_display parameter

#TODO: Script for collapsing a remaining bottom N-k features in plots
#TODO: scrips for ordering the plot by shap. size or by feature name

x <- res #for testing
top_k_features <- NULL
digits = 3
plot_phi0 = TRUE
index_x_explain = NULL

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("ggplot2 is not installed. Please run install.packages('ggplot2')")
}

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

if (!plot_phi0) { #should not be done if plotting waterfall
  plotting_dt <- plotting_dt[variable != "none"]
}

plotting_dt <- plotting_dt[id %in% index_x_explain]
plotting_dt[, rank := data.table::frank(abs(phi)), by = "id"]
#bottom_rank <- max(plotting_dt[,rank])
plotting_dt[variable=="none", rank:=0]

#plotting_dt[rank > top_k_features,  phi:= sum(phi), by=id]
#plotting_dt[rank > top_k_features, variable:="other", by=id]

plotting_dt[, description := factor(description, levels = unique(description[order(abs(phi))]))]
plotting_dt[, description_rev := factor(description, levels = unique(description[order(-abs(phi))]))]

setorder(plotting_dt, rank)
plotting_dt[, end:= cumsum(phi), by = id]
expected <- plotting_dt[variable == "none", phi][[1]] #should I find this in a more "general" way..?
plotting_dt[, start := c(expected, head(end, -1)), by = id]
plotting_dt[, phi_significant := format(phi, digits = digits, trim=TRUE), by=id]

# helpers
plotting_dt[, x_segment := ifelse(rank==last(rank), rank-0.45, rank-0.45)]
plotting_dt[, x_end_segment := ifelse(rank==last(rank), rank+1, rank+1.45)]
plotting_dt[, y_text := ifelse(abs(phi) < abs(min(start)-max(end))/20, ifelse(end>start, end+0.1, start), start + (end - start)/2 ), by=id]
plotting_dt[, hjust_text := ifelse(abs(phi) < abs(min(start)-max(end))/20, 0, 0.5), by=id]


# Plotting waterfall
gg <- ggplot2::ggplot(plotting_dt[variable != "none", ], aes(x = description, fill = sign)) +
  ggplot2::facet_wrap(~header, scales = "free", labeller = "label_value", ncol=2) + #fix wrt ncol and layout for arbitrary num. obs.
  ggplot2::geom_rect(aes(x=description, xmin = rank - 0.45, xmax = rank + 0.45, ymin = end, ymax = start)) +
  ggplot2::coord_flip(clip = 'off', xlim=c(0, max(plotting_dt[, rank])+1.1)) +
  ggplot2::scale_fill_manual(values = c("#00BA38","#F8766D"), drop = TRUE) +
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
  geom_segment(x=-Inf, xend = 1.45, y=expected, yend=expected, linetype="dashed", col="#00BA38") +
  geom_segment(aes(x=x_segment, xend = x_end_segment, y=end, yend=end), linetype="dashed", col="#00BA38") +
  geom_text(size=2.5, parse=TRUE,
    mapping = aes(x = last(rank)+1, y = pred, label = paste0("italic(f(x))==", format(pred, digits=digits)), vjust=0, hjust=1)
    ) +
  geom_text(size=2.5, aes(label = format(phi_significant, digits=digits), x=rank, y=y_text, vjust=0.5, hjust=hjust_text)) +
  geom_text(size=2.5, parse=TRUE,
            mapping = aes(x = -Inf, y = expected, label = paste0("~italic(E(f(x)))==", format(expected, digits=digits)), vjust=0, hjust=0)
  ) +
  geom_text(size=2.5, aes(label = format(phi_significant, digits=digits), x=rank, y=y_text, vjust=0.5, hjust=hjust_text))

gg



