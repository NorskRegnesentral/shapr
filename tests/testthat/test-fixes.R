skip_on_cran()

test_that("vaeac one_hot_max_sizes is correct for all-categorical data with uniform level counts", {
  # Regression test: `vaeac_preprocess_data()` previously built `feature_list$factor_levels`
  # with `sapply(data, levels)`. When every feature is a factor with the SAME number of levels,
  # `sapply()` simplifies the result to a matrix instead of a list, so `one_hot_max_sizes` became
  # length `n_features * n_levels` (all 1s, i.e. every feature wrongly treated as continuous)
  # instead of length `n_features`. That corrupted the categorical encoding and produced an
  # out-of-bounds torch index ("index k is out of bounds for dimension 1 with size k") during
  # vaeac. Using `lapply()` keeps one entry per feature. `normalize = FALSE` avoids needing torch.
  set.seed(1)
  n <- 50
  n_levels <- 3
  n_features <- 4
  dt <- data.table::as.data.table(
    setNames(
      lapply(seq_len(n_features), function(j) {
        factor(sample(paste0("L", seq_len(n_levels)), n, replace = TRUE), levels = paste0("L", seq_len(n_levels)))
      }),
      paste0("f", seq_len(n_features))
    )
  )

  pp <- vaeac_preprocess_data(data = dt, normalize = FALSE)

  # One entry per feature (not n_features * n_levels), each equal to the number of levels.
  expect_length(pp$one_hot_max_sizes, n_features)
  expect_equal(pp$one_hot_max_sizes, rep(as.integer(n_levels), n_features))
})

test_that("check_computability warns when vaeac is combined with a serializing future plan", {
  skip_if_not_installed("future")

  make_internal <- function(approach) {
    list(parameters = list(
      is_groupwise = FALSE,
      max_n_coalitions = 32L,
      n_features = 5L,
      n_groups = 1L,
      exact = FALSE,
      approach = approach,
      causal_sampling = FALSE,
      asymmetric = FALSE,
      max_n_coalitions_causal = NULL
    ))
  }

  # Always restore the sequential plan afterwards so the plan does not leak to other tests.
  on.exit(future::plan("sequential"), add = TRUE)

  # Sequential plan: no warning even for vaeac.
  future::plan("sequential")
  expect_no_warning(check_computability(make_internal("vaeac")))

  # multisession (serializing) with >1 worker: warn for vaeac, since torch objects are external
  # pointers that cannot be exported to separate R processes.
  future::plan(future::multisession, workers = 2)
  expect_warning(check_computability(make_internal("vaeac")), regexp = "external pointer")

  # ...but not for a non-torch approach under the same plan.
  expect_no_warning(check_computability(make_internal("gaussian")))
})
