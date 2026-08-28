#!/usr/bin/env Rscript
# audit_findings.R — reproduce the numerical tables and integrity checks used
# by the published benchmark article from the committed curated result snapshot.

suppressMessages({
  library(data.table)
})

studies <- c(
  "gaussian", "empirical", "ctree", "arf", "timeseries", "vaeac",
  "independence", "copula", "categorical", "regression_separate",
  "regression_surrogate"
)

args <- commandArgs(trailingOnly = FALSE)
script_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(script_arg) == 1) sub("^--file=", "", script_arg) else "benchmarks/R/audit_findings.R"
benchmark_root <- dirname(dirname(normalizePath(script_path, mustWork = TRUE)))
result_root <- file.path(benchmark_root, "results")

read_study <- function(study) {
  result <- data.table::fread(file.path(result_root, study, "results.csv"))
  result[, study := study]
  return(result)
}

results <- data.table::rbindlist(lapply(studies, read_study), use.names = TRUE, fill = TRUE)
summaries <- data.table::rbindlist(lapply(studies, function(study) {
  summary <- data.table::fread(file.path(result_root, study, "summary.csv"))
  summary[, study := study]
  return(summary)
}), use.names = TRUE, fill = TRUE)

configuration_cols <- c(
  "study", "sweep", "dataset", "approach", "n_features", "max_n_coalitions",
  "n_MC_samples", "min_n_batches", "max_batch_size", "max_batch_cube_size",
  "workers", "backend", "dt_threads", "n_train", "n_explain", "iterative",
  "group", "group_size", "model_variant", "save_explanations", "approach_args",
  "pair_key", "pair_role"
)

#### Snapshot integrity -------------------------------------------------------

stopifnot(
  nrow(results) == 2278L,
  all(results$status == "ok"),
  sum(results$pair_budget_matches == FALSE, na.rm = TRUE) == 0L,
  results[pair_role == "source", .N] == 62L,
  results[pair_role == "dependent", .N] == 62L,
  !"is_warmup" %in% names(results),
  !any(grepl("accuracy", results$sweep))
)

configuration_counts <- results[, .N, by = configuration_cols]
stopifnot(
  nrow(configuration_counts) == 789L,
  configuration_counts[N == 3L, .N] == 700L,
  configuration_counts[N == 2L, .N] == 89L,
  all(configuration_counts$N %in% c(2L, 3L)),
  nrow(summaries) == 789L,
  sum(summaries$n) == 2278L,
  summaries[n == 3L, .N] == 700L,
  summaries[n == 2L, .N] == 89L
)

#### Comparable reference configuration -------------------------------------

reference <- results[
  n_features == 12 & max_n_coalitions == 128 & n_MC_samples == 250 &
    min_n_batches == 10 & workers == 1 & dt_threads == 1 & n_train == 1000 &
    n_explain == 25 & iterative == FALSE & group == FALSE &
    max_batch_cube_size == 1e6 & (is.na(approach_args) | approach_args == "") &
    ((approach == "categorical" & dataset == "categorical") |
      (approach != "categorical" & dataset == "numeric"))
]
reference_table <- reference[, .(
  explain_seconds = round(stats::median(wall_secs), 3),
  peak_ram_mb = round(stats::median(peak_ram_mb), 1)
), by = approach][order(explain_seconds)]
stopifnot(nrow(reference_table) == length(studies))

#### Representative heavy parallel workloads --------------------------------

parallel_case <- function(study_name, subset_expression, label) {
  study_results <- results[study == study_name]
  selected <- study_results[eval(parse(text = subset_expression))]
  table <- selected[, .(
    elapsed_seconds = round(stats::median(wall_secs), 1),
    peak_ram_mb = round(stats::median(peak_ram_mb), 0)
  ), by = workers][order(workers)]
  table[, case := label]
  data.table::setcolorder(table, c("case", "workers", "elapsed_seconds", "peak_ram_mb"))
  return(table)
}

parallel_table <- data.table::rbindlist(list(
  parallel_case(
    "gaussian", "grepl('realistic_heavy', sweep) & min_n_batches == 32",
    "Gaussian / 32 batches"
  ),
  parallel_case(
    "empirical", "sweep == 'realistic_heavy' & min_n_batches == 8",
    "Empirical / 8 batches"
  ),
  parallel_case(
    "ctree", "sweep == 'realistic_heavy' & min_n_batches == 32",
    "CTree / 32 batches"
  ),
  parallel_case(
    "arf", "sweep == 'realistic_heavy' & min_n_batches == 32",
    "ARF / 32 batches"
  ),
  parallel_case(
    "timeseries", "grepl('realistic_heavy', sweep) & min_n_batches == 8",
    "Timeseries / 8 batches"
  ),
  parallel_case(
    "vaeac", "grepl('realistic_explanation', sweep) & min_n_batches == 16",
    "VAEAC explanation-heavy / 16 batches"
  )
), use.names = TRUE)

#### Gaussian memory cap -----------------------------------------------------

memory_table <- results[
  study == "gaussian" & n_features == 12 & max_n_coalitions == 1024 &
    n_MC_samples == 250 & n_explain == 100 & min_n_batches == 8 &
    workers %in% c(1, 4),
  .(
    actual_batches = as.integer(stats::median(used_n_batches)),
    elapsed_seconds = round(stats::median(wall_secs), 1),
    peak_ram_mb = round(stats::median(peak_ram_mb), 0)
  ),
  by = .(max_batch_cube_size, workers)
][order(max_batch_cube_size, workers)]

#### Prediction model cost ---------------------------------------------------

prediction_table <- results[
  study == "gaussian" & sweep == "prediction_model",
  .(
    explain_seconds = round(stats::median(wall_secs), 2),
    peak_ram_mb = round(stats::median(peak_ram_mb), 0)
  ),
  by = .(model_variant, workers)
][order(model_variant, workers)]
prediction_table[, speedup := round(
  explain_seconds[workers == 1] / explain_seconds,
  2
), by = model_variant]

#### Replicate variability ---------------------------------------------------

variability <- results[, .(
  replicates = .N,
  median_seconds = stats::median(wall_secs),
  iqr_seconds = stats::IQR(wall_secs)
), by = configuration_cols]
variability[, relative_iqr := iqr_seconds / median_seconds]
variability_table <- variability[, .(
  configurations = .N,
  median_relative_iqr = round(stats::median(relative_iqr), 4),
  p90_relative_iqr = round(stats::quantile(relative_iqr, 0.9), 4)
), by = replicates][order(replicates)]

#### Published-value audit --------------------------------------------------

assert_table <- function(actual, expected, label) {
  data.table::setcolorder(expected, names(actual))
  comparison <- all.equal(
    as.data.frame(actual),
    as.data.frame(expected),
    check.attributes = FALSE,
    tolerance = 1e-8
  )
  if (!isTRUE(comparison)) {
    stop(sprintf("Published %s values differ from the curated results: %s", label, comparison))
  }
  return(invisible(TRUE))
}

expected_reference <- data.table::data.table(
  approach = c(
    "gaussian", "copula", "independence", "regression_surrogate", "categorical",
    "regression_separate", "empirical", "arf", "ctree", "timeseries", "vaeac"
  ),
  explain_seconds = c(2.302, 3.168, 3.184, 3.311, 4.038, 5.412, 5.417, 17.127, 17.337, 58.983, 1159.353),
  peak_ram_mb = c(273.1, 283.3, 258.8, 462.1, 278.2, 308.6, 246.1, 775.2, 357.3, 1301.2, 542.7)
)
assert_table(reference_table, expected_reference, "reference table")

expected_parallel <- data.table::data.table(
  case = rep(c(
    "Gaussian / 32 batches", "Empirical / 8 batches", "CTree / 32 batches",
    "ARF / 32 batches", "Timeseries / 8 batches", "VAEAC explanation-heavy / 16 batches"
  ), c(4, 4, 4, 4, 4, 3)),
  workers = c(rep(c(1L, 4L, 8L, 16L), 5), 1L, 4L, 16L),
  elapsed_seconds = c(
    34.5, 14.3, 11.3, 10.1, 77.6, 25.2, 16.9, 16.9, 252.2, 72.1, 43.6, 28.8,
    363.5, 116.4, 73.7, 60.9, 251.5, 75.3, 49.5, 49.7, 290.5, 209.1, 189.9
  ),
  peak_ram_mb = c(
    592, 2236, 4116, 7263, 1548, 5133, 8892, 9391, 747, 2860, 5241, 9989,
    3793, 12981, 24452, 44733, 11858, 47144, 71282, 71895, 2836, 9817, 20936
  )
)
assert_table(parallel_table, expected_parallel, "parallel table")

expected_memory <- data.table::data.table(
  max_batch_cube_size = rep(c(1e6, 4e6, 16e6, 64e6, Inf), each = 2),
  workers = rep(c(1L, 4L), 5),
  actual_batches = rep(c(342L, 79L, 20L, 8L, 8L), each = 2),
  elapsed_seconds = c(29.9, 13.3, 36.4, 14.6, 33.9, 15.0, 32.1, 14.8, 32.0, 14.8),
  peak_ram_mb = c(360, 1457, 413, 1887, 730, 2728, 1477, 5237, 1462, 5233)
)
assert_table(memory_table, expected_memory, "memory-cap table")

expected_prediction <- data.table::data.table(
  model_variant = rep(c("linear", "xgb", "xgb_large"), each = 3),
  workers = rep(c(1L, 4L, 16L), 3),
  explain_seconds = c(8.21, 4.51, 4.06, 10.44, 6.38, 5.44, 50.02, 17.09, 15.54),
  peak_ram_mb = c(300, 1120, 3221, 343, 1579, 4839, 350, 1616, 4900),
  speedup = c(1.00, 1.82, 2.02, 1.00, 1.64, 1.92, 1.00, 2.93, 3.22)
)
assert_table(prediction_table, expected_prediction, "prediction-model table")

expected_variability <- data.table::data.table(
  replicates = c(2L, 3L),
  configurations = c(89L, 700L),
  median_relative_iqr = c(0.0029, 0.0074),
  p90_relative_iqr = c(0.0103, 0.0195)
)
assert_table(variability_table, expected_variability, "replicate-variability values")

cat("Snapshot integrity: 2,278 successful runs; 789 configurations; all pairs valid.\n\n")
cat("Comparable reference configuration:\n")
print(reference_table)
cat("\nRepresentative parallel workloads:\n")
print(parallel_table)
cat("\nGaussian memory-cap calibration:\n")
print(memory_table)
cat("\nPrediction-model comparison:\n")
print(prediction_table)
cat("\nReplicate variability:\n")
print(variability_table)
