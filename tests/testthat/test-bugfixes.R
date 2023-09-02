test_that("bug with column name ordering in edge case is fixed", {
  # Before the bugfix, data.table throw the warning:
  # Column 2 ['Solar.R'] of item 2 appears in position 1 in item 1. Set use.names=TRUE to match by column name,
  # or use.names=FALSE to ignore column names. use.names='check' (default from v1.12.2) emits this message and
  # proceeds as if use.names=FALSE for  backwards compatibility.
  # See news item 5 in v1.12.2 for options to control this message.
  expect_silent({ # Apparently, expect_no_message() does not react to the data.table message/warning
    e.one_subset_per_batch <- explain(
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "gaussian",
      prediction_zero = p0,
      n_samples = 2,
      n_batches = 2^5 - 1, # Bug happens when n_batches = n_combinations - 1
      keep_samp_for_vS = TRUE,
      seed = 123
    )
  })

  # The bug causes id_combination to suddenly not be integer.
  expect_true(
    is.integer(
      e.one_subset_per_batch$internal$output$dt_samp_for_vS$id_combination[1]
    )
  )
})
