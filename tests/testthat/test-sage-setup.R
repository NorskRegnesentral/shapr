skip_on_cran()

test_that("erroneous input: `y_explain`", {
  set.seed(123)

  # Missing
  expect_snapshot(
    {
      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_train_numeric,
        x_train = x_train_numeric,
        approach = "gaussian",
        phi0 = p0,
        seed = 1,
        scope = "global"
      )
    },
    error = TRUE
  )

  # Non numerical
  expect_snapshot(
    {
      y_non_numeric <- rep(c("A", "B", "C"), length.out = nrow(x_train_numeric))

      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_train_numeric,
        x_train = x_train_numeric,
        approach = "gaussian",
        phi0 = p0,
        seed = 1,
        scope = "global",
        y_explain = y_non_numeric
      )
    },
    error = TRUE
  )

  # NA
  expect_snapshot(
    {
      y_NA <- rep(NA_real_, nrow(x_train_numeric))

      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_train_numeric,
        x_train = x_train_numeric,
        approach = "gaussian",
        phi0 = p0,
        seed = 1,
        scope = "global",
        y_explain = y_NA
      )
    },
    error = TRUE
  )

  # Wrong length vector
  expect_snapshot(
    {
      y_short <- y_train_numeric[-1]

      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_train_numeric,
        x_train = x_train_numeric,
        approach = "gaussian",
        phi0 = p0,
        seed = 1,
        scope = "global",
        y_explain = y_short
      )
    },
    error = TRUE
  )
})

test_that("erroneous input: `scope`", {
  set.seed(123)

  # Non character
  expect_snapshot(
    {
      scope_numeric <- 3

      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_train_numeric,
        x_train = x_train_numeric,
        approach = "gaussian",
        phi0 = p0,
        seed = 1,
        scope = scope_numeric,
        y_explain = y_train_numeric
      )
    },
    error = TRUE
  )

  # Invalid string
  expect_snapshot(
    {
      scope_invalid <- "both"

      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_train_numeric,
        x_train = x_train_numeric,
        approach = "gaussian",
        phi0 = p0,
        seed = 1,
        scope = scope_invalid,
        y_explain = y_train_numeric
      )
    },
    error = TRUE
  )

  # Vector
  expect_snapshot(
    {
      scope_vec <- c("local", "global")

      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_train_numeric,
        x_train = x_train_numeric,
        approach = "gaussian",
        phi0 = p0,
        seed = 1,
        scope = scope_vec,
        y_explain = y_train_numeric
      )
    },
    error = TRUE
  )
})


test_that("erroneous input: `extra_computation_args$global_loss_func`", {
  set.seed(123)

  # Not a function
  expect_snapshot(
    {
      loss_non_func <- 3

      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_train_numeric,
        x_train = x_train_numeric,
        approach = "gaussian",
        phi0 = p0,
        seed = 1,
        scope = "global",
        y_explain = y_train_numeric,
        extra_computation_args = list(global_loss_func = loss_non_func)
      )
    },
    error = TRUE
  )

  # Wrong number of parameters
  expect_snapshot(
    {
      loss_wrong_n_param <- function(y, pred, loss) {
        expected <- (y - pred) * loss

        return(expected)
      }

      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_train_numeric,
        x_train = x_train_numeric,
        approach = "gaussian",
        phi0 = p0,
        seed = 1,
        scope = "global",
        y_explain = y_train_numeric,
        extra_computation_args = list(global_loss_func = loss_wrong_n_param)
      )
    },
    error = TRUE
  )
})
