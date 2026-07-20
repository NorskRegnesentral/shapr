skip_on_cran()

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
  expect_no_warning(shapr:::check_computability(make_internal("vaeac")))

  # multisession (serializing) with >1 worker: warn for vaeac, since torch objects are external
  # pointers that cannot be exported to separate R processes.
  future::plan(future::multisession, workers = 2)
  expect_warning(shapr:::check_computability(make_internal("vaeac")), regexp = "external pointer")

  # ...but not for a non-torch approach under the same plan.
  expect_no_warning(shapr:::check_computability(make_internal("gaussian")))
})
