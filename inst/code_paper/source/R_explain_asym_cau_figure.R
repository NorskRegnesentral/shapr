library(ggpubr)
library(ggplot2)

# Specify the causal ordering and confounding
causal_ordering <- list("trend",
                        c("cosyear", "sinyear"),
                        c("temp", "atemp", "windspeed", "hum"))

confounding <- c(FALSE, TRUE, FALSE)

# Asymmetric conditional
exp_asym_cond <- explain(
  model = model,
  x_train = x_train,
  x_explain = x_explain,
  phi0 = mean(y_train),
  approach = "gaussian",
  asymmetric = TRUE,
  causal_ordering = causal_ordering,
  confounding = NULL,
  seed = 1
)

# Symmetric causal
exp_sym_cau <- explain(
  model = model,
  x_train = x_train,
  x_explain = x_explain,
  phi0 = mean(y_train),
  approach = "gaussian",
  asymmetric = FALSE,
  causal_ordering = causal_ordering,
  confounding = confounding,
  seed = 1
)

# Symmetric marginal
exp_sym_marg_gaus <- explain(
  model = model,
  x_train = x_train,
  x_explain = x_explain,
  phi0 = mean(y_train),
  approach = "gaussian",
  asymmetric = FALSE,
  causal_ordering = NULL,
  confounding = TRUE,
  seed = 1
)


# Symmetric marginal
exp_sym_marg_ind <- explain(
  model = model,
  x_train = x_train,
  x_explain = x_explain,
  phi0 = mean(y_train),
  approach = "independence",
  asymmetric = FALSE,
  causal_ordering = NULL,
  confounding = NULL,
  seed = 1
)

# Plotting

# Combine the explanations
explanation_list = list("Asymmetric conditional" = exp_asym_cond,
                        "Symmetric causal" = exp_sym_cau,
                        "Symmetric marginal (gaussian)" = exp_sym_marg_gaus,
                        "Symmetric marginal (indep)" = exp_sym_marg_ind)

# Make the beeswarm plots
grobs <- lapply(seq(length(explanation_list)), function(explanation_idx) {
  gg <- plot(explanation_list[[explanation_idx]], plot_type = "beeswarm") +
    ggplot2::ggtitle(gsub("_", " ", names(explanation_list)[[explanation_idx]]))
  # ggplot2::ggtitle(tools::toTitleCase(gsub("_", " ", names(explanation_list)[[explanation_idx]])))

  # Flip the order such that the features comes in the right order
  gg <- gg +
    ggplot2::scale_x_discrete(limits = rev(levels(gg$data$variable)[levels(gg$data$variable) != "none"]))+
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 12),
                   axis.text.x = ggplot2::element_text(size = 12))
})


fig = ggpubr::ggarrange(grobs[[1]], grobs[[2]], grobs[[3]], grobs[[4]],
                            ncol=4, nrow=1, common.legend = TRUE, legend="right")

ggplot2::ggsave(file.path(path_output,"beeswarm_caus_asym.pdf"),
                scale = 1.1,
                width = 14,
                height = 4)



