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
        sage = TRUE
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
        sage = TRUE,
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
        sage = TRUE,
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
        sage = TRUE,
        y_explain = y_short
      )
    },
    error = TRUE
  )
})

test_that("erroneous input: `sage`", {
  set.seed(123)

  # Non logical
  expect_snapshot(
    {
      sage_numeric <- 3

      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_train_numeric,
        x_train = x_train_numeric,
        approach = "gaussian",
        phi0 = p0,
        seed = 1,
        sage = sage_numeric,
        y_explain = y_train_numeric
      )
    },
    error = TRUE
  )

  # Vector
  expect_snapshot(
    {
      sage_vec <- c(TRUE, TRUE)

      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_train_numeric,
        x_train = x_train_numeric,
        approach = "gaussian",
        phi0 = p0,
        seed = 1,
        sage = sage_vec,
        y_explain = y_train_numeric
      )
    },
    error = TRUE
  )

  # NULL
  expect_snapshot(
    {
      sage_null <- NULL

      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_train_numeric,
        x_train = x_train_numeric,
        approach = "gaussian",
        phi0 = p0,
        seed = 1,
        sage = sage_null,
        y_explain = y_train_numeric
      )
    },
    error = TRUE
  )
})


test_that("erroneous input: `sage_args$loss_func`", {
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
        sage = TRUE,
        y_explain = y_train_numeric,
        sage_args = list(loss_func = loss_non_func)
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
        sage = TRUE,
        y_explain = y_train_numeric,
        sage_args = list(loss_func = loss_wrong_n_param)
      )
    },
    error = TRUE
  )
})
