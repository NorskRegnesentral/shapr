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

test_that("MSEv evaluation criterion unnamed plots", {
  skip_if_not_installed("vdiffr")

  # Create a list of explanations without names
  explanation_list_unnamed <- list(
    explain_numeric_empirical,
    explain_numeric_gaussian,
    explain_numeric_ctree,
    explain_numeric_combined
  )

  vdiffr::expect_doppelganger(
    title = "default version",
    fig = suppressMessages(make_MSEv_evaluation_criterion_plots(explanation_list = explanation_list_unnamed,
                                               plot_overall_MSEv = FALSE,
                                               only_overall_MSEv = TRUE,
                                               return_figures = TRUE)$bar_plot_MSEv)
  )

  vdiffr::expect_doppelganger(
    title = "rotate axis labels version",
    fig = suppressMessages(make_MSEv_evaluation_criterion_plots(explanation_list = explanation_list_unnamed,
                                               plot_overall_MSEv = FALSE,
                                               only_overall_MSEv = TRUE,
                                               return_figures = TRUE,
                                               axis_labels_rotate_angl = 90)$bar_plot_MSEv)
  )

  vdiffr::expect_doppelganger(
    title = "dodge axis labels version",
    fig = suppressMessages(make_MSEv_evaluation_criterion_plots(explanation_list = explanation_list_unnamed,
                                               plot_overall_MSEv = FALSE,
                                               only_overall_MSEv = TRUE,
                                               return_figures = TRUE,
                                               axis_labels_n_dodge = 4)$bar_plot_MSEv)
  )

})

test_that("MSEv evaluation criterion named plots", {
  skip_if_not_installed("vdiffr")

  # Create a list of explanations with names
  explanation_list_named <- list(
    "Emp." = explain_numeric_empirical,
    "Gaus." = explain_numeric_gaussian,
    "Ctree" = explain_numeric_ctree,
    "Comb." = explain_numeric_combined
  )

  vdiffr::expect_doppelganger(
    title = "using the provided names",
    fig = make_MSEv_evaluation_criterion_plots(explanation_list = explanation_list_named,
                                               plot_overall_MSEv = FALSE,
                                               only_overall_MSEv = TRUE,
                                               return_figures = TRUE)$bar_plot_MSEv
  )

  vdiffr::expect_doppelganger(
    title = "flip bars, add text, change colors, legend_bottom",
    fig = make_MSEv_evaluation_criterion_plots(explanation_list = explanation_list_named,
                                               plot_overall_MSEv = FALSE,
                                               only_overall_MSEv = TRUE,
                                               return_figures = TRUE,
                                               flip_coordinates = TRUE,
                                               bar_text_color = "black",
                                               bar_text_size = 5,
                                               bar_text_n_decimals = 2,
                                               brewer_palette = "Set1",
                                               ggplot_theme = ggplot2::theme_minimal(),
                                               legend_position = "bottom",
                                               legend_ncol = 2)$bar_plot_MSEv
  )

  vdiffr::expect_doppelganger(
    title = "default colors, no legend, more decimals, white text",
    fig = make_MSEv_evaluation_criterion_plots(explanation_list = explanation_list_named,
                                               plot_overall_MSEv = FALSE,
                                               only_overall_MSEv = TRUE,
                                               return_figures = TRUE,
                                               flip_coordinates = TRUE,
                                               bar_text_color = "white",
                                               bar_text_size = 5,
                                               bar_text_n_decimals = 4,
                                               brewer_palette = NULL,
                                               ggplot_theme = ggplot2::theme_minimal(),
                                               legend_position = "none")$bar_plot_MSEv
  )


  vdiffr::expect_doppelganger(
    title = "default colors, no legend, more decimals, white text, no title",
    fig = make_MSEv_evaluation_criterion_plots(explanation_list = explanation_list_named,
                                               plot_overall_MSEv = FALSE,
                                               only_overall_MSEv = TRUE,
                                               return_figures = TRUE,
                                               flip_coordinates = TRUE,
                                               bar_text_color = "white",
                                               bar_text_size = 5,
                                               bar_text_n_decimals = 4,
                                               brewer_palette = NULL,
                                               ggplot_theme = ggplot2::theme_minimal(),
                                               legend_position = "none",
                                               title_text_size = 0)$bar_plot_MSEv
  )

  vdiffr::expect_doppelganger(
    title = "MSEv for all explicands",
    fig = make_MSEv_evaluation_criterion_plots(explanation_list = explanation_list_named,
                                               plot_overall_MSEv = FALSE,
                                               only_overall_MSEv = FALSE,
                                               return_figures = TRUE,
                                               geom_col_width = 0.75)$bar_plot_MSEv_for_each_explicand
  )

  vdiffr::expect_doppelganger(
    title = "MSEv for specified explicands",
    fig = make_MSEv_evaluation_criterion_plots(explanation_list = explanation_list_named,
                                               plot_overall_MSEv = FALSE,
                                               only_overall_MSEv = FALSE,
                                               return_figures = TRUE,
                                               index_explicands = c(1, 3),
                                               geom_col_width = 0.75)$bar_plot_MSEv_for_each_explicand
  )

  vdiffr::expect_doppelganger(
    title = "MSEv for all coalitions",
    fig = make_MSEv_evaluation_criterion_plots(explanation_list = explanation_list_named,
                                               plot_overall_MSEv = FALSE,
                                               only_overall_MSEv = FALSE,
                                               return_figures = TRUE,
                                               geom_col_width = 0.75)$bar_plot_MSEv_for_each_coalition
  )

  vdiffr::expect_doppelganger(
    title = "MSEv for specified coalitions",
    fig =  make_MSEv_evaluation_criterion_plots(explanation_list = explanation_list_named,
                                                plot_overall_MSEv = FALSE,
                                                only_overall_MSEv = FALSE,
                                                return_figures = TRUE,
                                                index_combinations = c(1:2, 10, 15),
                                                geom_col_width = 0.75)$bar_plot_MSEv_for_each_coalition
  )

  vdiffr::expect_doppelganger(
    title = "MSEv for specified coalitions, flipped bars, bar text, changed theme and colors",
    fig = make_MSEv_evaluation_criterion_plots(explanation_list = explanation_list_named,
                                               plot_overall_MSEv = FALSE,
                                               only_overall_MSEv = FALSE,
                                               return_figures = TRUE,
                                               flip_coordinates = TRUE,
                                               index_combinations = c(1:2, 10, 15),
                                               bar_text_size = 4,
                                               brewer_palette = "Blues",
                                               ggplot_theme = ggplot2::theme_minimal(),
                                               geom_col_width = 0.75)$bar_plot_MSEv_for_each_coalition
  )

  vdiffr::expect_doppelganger(
    title = "line/point plot default explicands",
    fig = make_MSEv_evaluation_criterion_plots(explanation_list = explanation_list_named,
                                               plot_overall_MSEv = FALSE,
                                               only_overall_MSEv = FALSE,
                                               return_figures = TRUE)$line_point_plot_MSEv_for_each_explicand
  )

  vdiffr::expect_doppelganger(
    title = "line/point plot default coalitions",
    fig = make_MSEv_evaluation_criterion_plots(explanation_list = explanation_list_named,
                                               plot_overall_MSEv = FALSE,
                                               only_overall_MSEv = FALSE,
                                               return_figures = TRUE)$line_point_plot_MSEv_for_each_coalition
  )

  vdiffr::expect_doppelganger(
    title = "square points, no line, no title, explicands",
    fig = make_MSEv_evaluation_criterion_plots(explanation_list = explanation_list_named,
                                               plot_overall_MSEv = FALSE,
                                               only_overall_MSEv = FALSE,
                                               return_figures = TRUE,
                                               title_text_size = 0,
                                               line_type = "blank",
                                               point_size = 4,
                                               point_shape = "square")$line_point_plot_MSEv_for_each_explicand
  )

  vdiffr::expect_doppelganger(
    title = "change lines, no points, explicands",
    fig = make_MSEv_evaluation_criterion_plots(explanation_list = explanation_list_named,
                                               plot_overall_MSEv = FALSE,
                                               only_overall_MSEv = FALSE,
                                               return_figures = TRUE,
                                               point_size = 0,
                                               line_width = 1,
                                               line_type = "solid")$line_point_plot_MSEv_for_each_explicand
  )

  vdiffr::expect_doppelganger(
    title = "change lines and colors, no points, coalitions",
    fig = make_MSEv_evaluation_criterion_plots(explanation_list = explanation_list_named,
                                               plot_overall_MSEv = FALSE,
                                               only_overall_MSEv = FALSE,
                                               return_figures = TRUE,
                                               point_size = 0,
                                               line_width = 1,
                                               legend_position = "bottom",
                                               legend_nrow = 1,
                                               brewer_palette = "Accent",
                                               brewer_direction = -1,
                                               ggplot_theme = ggplot2::theme_minimal(),
                                               line_type = "solid")$line_point_plot_MSEv_for_each_coalition
  )
})
