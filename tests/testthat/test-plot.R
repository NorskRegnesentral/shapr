set.seed(123) #

explain_mixed <- explain(
  model = model_lm_mixed,
  x_explain = x_explain_mixed,
  x_train = x_train_mixed,
  approach = "independence",
  prediction_zero = p0,
  n_batches = 1,
  timing = FALSE
)

explain_numeric_empirical <- explain(
  model = model_lm_numeric,
  x_explain = x_explain_numeric,
  x_train = x_train_numeric,
  approach = "empirical",
  prediction_zero = p0,
  n_batches = 1,
  timing = FALSE
)

explain_numeric_gaussian <- explain(
  model = model_lm_numeric,
  x_explain = x_explain_numeric,
  x_train = x_train_numeric,
  approach = "gaussian",
  prediction_zero = p0,
  n_batches = 1,
  timing = FALSE
)

explain_numeric_ctree <- explain(
  model = model_lm_numeric,
  x_explain = x_explain_numeric,
  x_train = x_train_numeric,
  approach = "ctree",
  prediction_zero = p0,
  n_batches = 1,
  timing = FALSE
)

explain_numeric_combined <- explain(
  model = model_lm_numeric,
  x_explain = x_explain_numeric,
  x_train = x_train_numeric,
  approach = c("empirical", "ctree", "gaussian", "ctree"),
  prediction_zero = p0,
  n_batches = 10,
  timing = FALSE
)

test_that("checking default outputs", {
  skip_if_not_installed("vdiffr")

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

  vdiffr::expect_doppelganger(
    title = "beeswarm_plot_default",
    fig = plot(explain_mixed, plot_type = "beeswarm")
  )
})

test_that("bar_plot_new_arguments", {
  skip_if_not_installed("vdiffr")

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
  skip_if_not_installed("vdiffr")

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
  skip_if_not_installed("vdiffr")

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
  skip_if_not_installed("vdiffr")

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
  skip_if_not_installed("vdiffr")

  # Create a list of explanations with names
  explanation_list_named <- list(
    "Emp." = explain_numeric_empirical,
    "Gaus." = explain_numeric_gaussian,
    "Ctree" = explain_numeric_ctree,
    "Comb." = explain_numeric_combined
  )

  MSEv_plots <- plot_MSEv_eval_crit(explanation_list_named,
    make_MSEv_comb_and_explicand = TRUE,
    CI_level = 0.95
  )

  MSEv_plots_specified_width <- plot_MSEv_eval_crit(explanation_list_named,
    make_MSEv_comb_and_explicand = TRUE,
    CI_level = 0.95,
    geom_col_width = 0.5
  )

  vdiffr::expect_doppelganger(
    title = "MSEv_bar",
    fig = MSEv_plots$MSEv_bar
  )

  vdiffr::expect_doppelganger(
    title = "MSEv_bar 50% CI",
    fig = plot_MSEv_eval_crit(explanation_list_named,
      make_MSEv_comb_and_explicand = FALSE,
      CI_level = 0.50
    )
  )

  vdiffr::expect_doppelganger(
    title = "MSEv_bar without CI",
    fig = plot_MSEv_eval_crit(explanation_list_named,
      make_MSEv_comb_and_explicand = FALSE,
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
    title = "MSEv_combination_bar",
    fig = MSEv_plots$MSEv_combination_bar
  )

  vdiffr::expect_doppelganger(
    title = "MSEv_combination_bar specified width",
    fig = MSEv_plots_specified_width$MSEv_combination_bar
  )

  vdiffr::expect_doppelganger(
    title = "MSEv_combination_line_point",
    fig = MSEv_plots$MSEv_combination_line_point
  )

  vdiffr::expect_doppelganger(
    title = "MSEv_explicand for specified observations",
    fig = plot_MSEv_eval_crit(explanation_list_named,
      make_MSEv_comb_and_explicand = TRUE,
      index_x_explain = c(1, 3:4, 6),
      CI_level = 0.95
    )$MSEv_explicand_bar
  )

  vdiffr::expect_doppelganger(
    title = "MSEv_combinations for specified combinations",
    fig = plot_MSEv_eval_crit(explanation_list_named,
      make_MSEv_comb_and_explicand = TRUE,
      id_combination = c(3, 4, 9, 13:15),
      CI_level = 0.95
    )$MSEv_combination_bar
  )
})
