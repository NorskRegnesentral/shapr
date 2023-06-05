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
