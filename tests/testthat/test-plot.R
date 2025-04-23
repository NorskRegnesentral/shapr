skip_on_cran()
skip_if_not_installed("ggplot2")
skip_if_not_installed("vdiffr")
skip_if_not_installed("party")


set.seed(123) #

explain_mixed <- explain(
  testing = TRUE,
  model = model_lm_mixed,
  x_explain = x_explain_mixed,
  x_train = x_train_mixed,
  approach = "independence",
  phi0 = p0,
  seed = 1
)

explain_numeric_empirical <- explain(
  testing = TRUE,
  model = model_lm_numeric,
  x_explain = x_explain_numeric,
  x_train = x_train_numeric,
  approach = "empirical",
  phi0 = p0,
  seed = 1
)

explain_numeric_gaussian <- explain(
  testing = TRUE,
  model = model_lm_numeric,
  x_explain = x_explain_numeric,
  x_train = x_train_numeric,
  approach = "gaussian",
  phi0 = p0,
  seed = 1
)

explain_numeric_ctree <- explain(
  testing = TRUE,
  model = model_lm_numeric,
  x_explain = x_explain_numeric,
  x_train = x_train_numeric,
  approach = "ctree",
  phi0 = p0,
  seed = 1
)

explain_numeric_combined <- explain(
  testing = TRUE,
  model = model_lm_numeric,
  x_explain = x_explain_numeric,
  x_train = x_train_numeric,
  approach = c("empirical", "ctree", "gaussian", "ctree"),
  phi0 = p0,
  seed = 1
)

# Create a list of explanations with names
explanation_list_named <- list(
  "Emp." = explain_numeric_empirical,
  "Gaus." = explain_numeric_gaussian,
  "Ctree" = explain_numeric_ctree,
  "Comb." = explain_numeric_combined
)

test_that("checking default outputs", {
  vdiffr::expect_doppelganger(
    title = "bar_plot_default",
    fig = plot(explain_mixed)
  )

  vdiffr::expect_doppelganger(
    title = "waterfall_plot_default",
    fig = plot(explain_mixed, plot_type = "waterfall")
  )

  vdiffr::expect_doppelganger(
    title = "scatter_plot_default",
    fig = plot(explain_mixed, plot_type = "scatter")
  )

  skip_if_not_installed("ggbeeswarm")

  vdiffr::expect_doppelganger(
    title = "beeswarm_plot_default",
    fig = plot(explain_mixed, plot_type = "beeswarm")
  )
})

test_that("bar_plot_new_arguments", {
  vdiffr::expect_doppelganger(
    title = "bar_plot_digits_5",
    fig = plot(explain_mixed, digits = 5)
  )

  vdiffr::expect_doppelganger(
    title = "bar_plot_no_phi0",
    fig = plot(explain_mixed, bar_plot_phi0 = FALSE)
  )

  vdiffr::expect_doppelganger(
    title = "bar_plot_index_x_explain_1",
    fig = plot(explain_mixed, index_x_explain = 1)
  )

  vdiffr::expect_doppelganger(
    title = "bar_plot_top_3_features",
    fig = plot(explain_mixed, top_k_features = 3)
  )

  vdiffr::expect_doppelganger(
    title = "bar_plot_new_colors",
    fig = plot(explain_mixed, col = c("red", "black"))
  )

  vdiffr::expect_doppelganger(
    title = "bar_plot_order_original",
    fig = plot(explain_mixed, bar_plot_order = "original")
  )
})

test_that("waterfall_plot_new_arguments", {
  vdiffr::expect_doppelganger(
    title = "waterfall_plot_digits_5",
    fig = plot(explain_mixed, plot_type = "waterfall", digits = 5)
  )

  vdiffr::expect_doppelganger(
    title = "waterfall_plot_index_x_explain_1",
    fig = plot(explain_mixed, plot_type = "waterfall", index_x_explain = 1)
  )

  vdiffr::expect_doppelganger(
    title = "waterfall_plot_top_3_features",
    fig = plot(explain_mixed, plot_type = "waterfall", top_k_features = 3)
  )

  vdiffr::expect_doppelganger(
    title = "waterfall_plot_new_colors",
    fig = plot(explain_mixed, plot_type = "waterfall", col = c("red", "black"))
  )
})

test_that("scatter_plot_new_arguments", {
  vdiffr::expect_doppelganger(
    title = "scatter_plot_index_x_explain_1_2",
    fig = plot(explain_mixed, plot_type = "scatter", index_x_explain = c(1, 2))
  )

  vdiffr::expect_doppelganger(
    title = "scatter_plot_new_color",
    fig = plot(explain_mixed, plot_type = "scatter", col = "black")
  )

  vdiffr::expect_doppelganger(
    title = "scatter_plot_one_feature",
    fig = plot(explain_mixed, plot_type = "scatter", scatter_features = "Temp")
  )

  vdiffr::expect_doppelganger(
    title = "scatter_plot_no_hist",
    fig = plot(explain_mixed, plot_type = "scatter", scatter_hist = FALSE)
  )
})

test_that("beeswarm_plot_new_arguments", {
  skip_if_not_installed("ggbeeswarm")

  vdiffr::expect_doppelganger(
    title = "beeswarm_plot_new_colors",
    fig = plot(explain_mixed, plot_type = "beeswarm", col = c("red", "black"))
  )

  vdiffr::expect_doppelganger(
    title = "beeswarm_plot_index_x_explain_1_2",
    fig = plot(explain_mixed, plot_type = "beeswarm", index_x_explain = c(1, 2))
  )
})

test_that("MSEv evaluation criterion plots", {
  MSEv_plots <- plot_MSEv_eval_crit(
    explanation_list_named,
    plot_type = c("overall", "comb", "explicand"),
    CI_level = 0.95
  )

  MSEv_plots_specified_width <- plot_MSEv_eval_crit(
    explanation_list_named,
    plot_type = c("overall", "comb", "explicand"),
    geom_col_width = 0.5
  )

  vdiffr::expect_doppelganger(
    title = "MSEv_bar",
    fig = MSEv_plots$MSEv_bar
  )

  vdiffr::expect_doppelganger(
    title = "MSEv_bar 50% CI",
    fig = plot_MSEv_eval_crit(
      explanation_list_named,
      plot_type = "overall",
      CI_level = 0.50
    )
  )

  vdiffr::expect_doppelganger(
    title = "MSEv_bar without CI",
    fig = plot_MSEv_eval_crit(
      explanation_list_named,
      plot_type = "overall",
      CI_level = NULL
    )
  )

  vdiffr::expect_doppelganger(
    title = "MSEv_bar with CI different width",
    fig = MSEv_plots_specified_width$MSEv_bar
  )

  vdiffr::expect_doppelganger(
    title = "MSEv_explicand_bar",
    fig = MSEv_plots$MSEv_explicand_bar
  )

  vdiffr::expect_doppelganger(
    title = "MSEv_explicand_bar specified width",
    fig = MSEv_plots_specified_width$MSEv_explicand_bar
  )

  vdiffr::expect_doppelganger(
    title = "MSEv_explicand_line_point",
    fig = MSEv_plots$MSEv_explicand_line_point
  )

  vdiffr::expect_doppelganger(
    title = "MSEv_coalition_bar",
    fig = MSEv_plots$MSEv_coalition_bar
  )

  vdiffr::expect_doppelganger(
    title = "MSEv_coalition_bar specified width",
    fig = MSEv_plots_specified_width$MSEv_coalition_bar
  )

  vdiffr::expect_doppelganger(
    title = "MSEv_coalition_line_point",
    fig = MSEv_plots$MSEv_coalition_line_point
  )

  vdiffr::expect_doppelganger(
    title = "MSEv_explicand for specified observations",
    fig = plot_MSEv_eval_crit(
      explanation_list_named,
      plot_type = "explicand",
      index_x_explain = c(1, 3:4, 6)
    )$MSEv_explicand_bar
  )

  vdiffr::expect_doppelganger(
    title = "MSEv_coalitions for specified coalitions",
    fig = plot_MSEv_eval_crit(
      explanation_list_named,
      plot_type = "comb",
      id_coalition = c(3, 4, 9, 13:15),
      CI_level = 0.95
    )$MSEv_coalition_bar
  )
})

test_that("plot_SV_several_approaches_explanations", {
  vdiffr::expect_doppelganger(
    title = "plot_SV_several_approaches_default",
    fig = plot_SV_several_approaches(explanation_list_named)
  )


  vdiffr::expect_doppelganger(
    title = "plot_SV_several_div_input_1",
    fig = plot_SV_several_approaches(explanation_list_named,
      plot_phi0 = TRUE,
      add_zero_line = TRUE,
      facet_ncol = 3,
      facet_scales = "free_y",
      horizontal_bars = TRUE
    )
  )

  vdiffr::expect_doppelganger(
    title = "plot_SV_several_div_input_2",
    fig = plot_SV_several_approaches(explanation_list_named,
      axis_labels_n_dodge = 1,
      facet_ncol = 1,
      facet_scales = "free_x",
      horizontal_bars = FALSE,
      index_explicands = c(1, 3),
      add_zero_line = TRUE
    )
  )

  vdiffr::expect_doppelganger(
    title = "plot_SV_several_div_input_3",
    fig = plot_SV_several_approaches(explanation_list_named,
      facet_ncol = 1,
      facet_scales = "free_y",
      brewer_palette = "Set1",
      only_these_features = c("Month", "Day", "Solar.R"),
      plot_phi0 = TRUE
    )
  )
})
