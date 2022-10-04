set.seed(123)

explain_mixed <- explain(model = model_lm_mixed,
                         x_explain = x_explain_mixed,
                         x_train = x_train_mixed,
                         approach = "independence",
                         prediction_zero = p0)

test_that(
  desc = "checking default outputs",
  code = {
    skip_if_not_installed("vdiffr")

    vdiffr::expect_doppelganger(
      title = "checking default bar plot",
      fig = plot(explain_mixed)
    )

    vdiffr::expect_doppelganger(
      title = "checking default waterfall plot",
      fig = plot(explain_mixed, plot_type = "waterfall")
    )

    vdiffr::expect_doppelganger(
      title = "checking default scatter plot",
      fig = plot(explain_mixed, plot_type = "scatter")
    )

    vdiffr::expect_doppelganger(
      title = "checking default beeswarm plot",
      fig = plot(explain_mixed, plot_type = "beeswarm")
    )
  }
)

test_that(
  desc = "changing arguments",
  code = {
    skip_if_not_installed("vdiffr")

    vdiffr::expect_doppelganger(
      title = "checking default bar plot",
      fig = plot(explain_mixed, plot_phi0 = FALSE)
    )

    vdiffr::expect_doppelganger(
      title = "checking default waterfall plot",
      fig = plot(explain_mixed, plot_type = "waterfall", index_x_explain = 1)
    )

    vdiffr::expect_doppelganger(
      title = "checking default scatter plot",
    fig = plot(explain_mixed, plot_type = "scatter", scatter_features = "Temp", scatter_hist = FALSE)
    )

    vdiffr::expect_doppelganger(
      title = "checking default beeswarm plot",
      fig = plot(explain_mixed, plot_type = "beeswarm", col = c("red", "black"))
    )
  }
)

