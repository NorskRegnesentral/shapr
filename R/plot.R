
#' Plot of the Shapley value explanations
#'
#' Plots the individual prediction explanations. Uses facet_wrap of ggplot
#'
#' @param explanation The output from compute_kshap
#' @param no_desc_digits Integer. Number of significant digits to use in the feature description
#' @param plot_phi0 Logical. Whether to include phi0 in the plot
#' @param plot_which_Xtest Integer vector. Which of the test observations to plot
#' @param top_k_features Integer. How many features to include in the plot
#'
#' @inheritParams global_arguments
#'
#' @return ggplot object with plots of the Shapley value explanations
#'
#' @import ggplot2
#' @export
#'
#' @author Martin Jullum

plot_kshap <- function(explanation,
                       l,
                       no_desc_digits=3,
                       plot_phi0 = T,
                       plot_which_Xtest = 1:nrow(l$Xtest),
                       top_k_features = ncol(l$Xtest)+1){

  colnam <- colnames(l$Xtest)

  # melting Kshap
  meltKshap <- melt(copy(explanation$Kshap[,id:=.I]),id.vars="id",value.name="phi")
  meltKshap[,sign :=factor(sign(phi),levels=c(1,-1),labels=c("Increases","Decreases"))]

  # Converting and melting Xtest
  desc_mat <- format(l$Xtest,digits=no_desc_digits)
  for (i in 1:ncol(desc_mat)){
    desc_mat[,i] <- paste0(colnam[i]," = ",desc_mat[,i])
  }
  desc_dt <- as.data.table(cbind(none= "none",desc_mat))
  melt_desc_dt <- melt(desc_dt[,id:=.I],id.vars="id",value.name="description")

  # Data table for plotting
  plotting_dt <- merge(meltKshap,melt_desc_dt)

  if (!plot_phi0){
    plotting_dt <- plotting_dt[variable!="none"]
  }
  plotting_dt <- plotting_dt[id %in% plot_which_Xtest]

  plotting_dt[,rank := frank(-abs(phi)),by=id]
  plotting_dt <- plotting_dt[rank<=top_k_features]

#  plotting_dt <- plotting_dt[order(id,-abs(phi))]

  plotting_dt[,description:=factor(description, levels = unique(description[order(abs(phi))]))]

  # Plotting
  gg <- ggplot(plotting_dt) +
    facet_wrap(~id,scales="free_y",labeller="label_both") +
    geom_col(aes(x=description,y=phi,fill=sign)) +
    coord_flip() +
    scale_fill_manual(values = c("steelblue", "lightsteelblue"), drop = T) +
    labs(y = "Feature contribution", x = "Feature", fill = "",title = "Shapley value prediction explanation") +
    theme(legend.position = 'bottom',
          plot.title = element_text(hjust = 0.5))

  return(gg)
}
