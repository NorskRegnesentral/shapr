test_that("shapr class has correct structure", {
  set.seed(123)

  obj <- explain(
    testing = TRUE,
    model = model_lm_numeric,
    x_explain = x_explain_numeric,
    x_train = x_train_numeric,
    approach = "independence",
    max_n_coalitions = 10,
    phi0 = p0,
    seed = 1
  )

  # Check class
  expect_s3_class(obj, "shapr")

  # Check that main components exist
  expect_true("internal" %in% names(obj))

  # Check that print method works
  expect_no_error(print(obj))

  # Check that summary returns summary.shapr
  s <- summary(obj)
  expect_s3_class(s, "summary.shapr")
})

test_that("summary.shapr class has correct structure", {
  set.seed(123)

  obj <- explain(
    testing = TRUE,
    model = model_lm_numeric,
    x_explain = x_explain_numeric,
    x_train = x_train_numeric,
    approach = "independence",
    max_n_coalitions = 10,
    phi0 = p0,
    seed = 1
  )

  s <- summary(obj)

  # Check class
  expect_s3_class(s, "summary.shapr")
  expect_s3_class(s, "list")

  # Check that main components are present (from get_results)
  expect_true("shapley_est" %in% names(s))
  expect_true("approach" %in% names(s))
  expect_true("calling_function" %in% names(s))

  # Check that print_data attribute exists and is an environment
  expect_true("print_data" %in% names(attributes(s)))
  expect_type(attr(s, "print_data"), "environment")

  # Check that printing works without error
  expect_no_error(print(s))

  # Check that digits parameter works in summary
  s2 <- summary(obj, digits = 5)
  expect_s3_class(s2, "summary.shapr")
})
