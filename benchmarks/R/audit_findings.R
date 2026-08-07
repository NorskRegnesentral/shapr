#!/usr/bin/env Rscript
# audit_findings.R — reproduce the numerical tables and integrity checks used
# by BENCHMARK_FINDINGS.md from the committed curated result snapshot.

suppressMessages({
  library(data.table)
})

studies <- c(
  "gaussian", "empirical", "ctree", "arf", "timeseries", "vaeac",
  "independence", "copula", "categorical", "regression_separate",
  "regression_surrogate"
)

# The accuracy study measures error rather than cost, so it is audited
# separately and excluded from the cost-snapshot integrity counts.
accuracy_study <- "accuracy"

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

#### Accuracy-study integrity ------------------------------------------------

accuracy_results <- data.table::fread(
  file.path(result_root, accuracy_study, "results.csv")
)
stopifnot(
  nrow(accuracy_results) == 75L,
  all(accuracy_results$status == "ok"),
  accuracy_results[sweep == "accuracy_cost", .N] == 72L,
  accuracy_results[sweep == "accuracy_reference", .N] == 3L
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
  whole_process_seconds = round(stats::median(bash_wall_secs), 3),
  explain_seconds = round(stats::median(wall_secs), 3),
  peak_ram_mb = round(stats::median(peak_ram_mb), 1)
), by = approach][order(whole_process_seconds)]
stopifnot(nrow(reference_table) == length(studies))

#### Representative heavy parallel workloads --------------------------------

parallel_case <- function(study_name, subset_expression, label) {
  study_results <- results[study == study_name]
  selected <- study_results[eval(parse(text = subset_expression))]
  table <- selected[, .(
    elapsed_seconds = round(stats::median(bash_wall_secs), 1),
    peak_ram_mb = round(stats::median(peak_ram_mb), 0)
  ), by = workers][order(workers)]
  table[, case := label]
  data.table::setcolorder(table, c("case", "workers", "elapsed_seconds", "peak_ram_mb"))
  return(table)
}

parallel_table <- data.table::rbindlist(list(
  parallel_case("gaussian", "grepl('realistic_heavy', sweep) & min_n_batches == 32",
    "Gaussian / 32 batches"),
  parallel_case("empirical", "sweep == 'realistic_heavy' & min_n_batches == 8",
    "Empirical / 8 batches"),
  parallel_case("ctree", "sweep == 'realistic_heavy' & min_n_batches == 32",
    "CTree / 32 batches"),
  parallel_case("arf", "sweep == 'realistic_heavy' & min_n_batches == 32",
    "ARF / 32 batches"),
  parallel_case("timeseries", "grepl('realistic_heavy', sweep) & min_n_batches == 8",
    "Timeseries / 8 batches"),
  parallel_case("vaeac", "grepl('realistic_explanation', sweep) & min_n_batches == 16",
    "VAEAC explanation-heavy / 16 batches")
), use.names = TRUE)

#### Gaussian memory cap -----------------------------------------------------

memory_table <- results[
  study == "gaussian" & n_features == 12 & max_n_coalitions == 1024 &
    n_MC_samples == 250 & n_explain == 100 & min_n_batches == 8 &
    workers %in% c(1, 4),
  .(
    actual_batches = as.integer(stats::median(used_n_batches)),
    elapsed_seconds = round(stats::median(bash_wall_secs), 1),
    peak_ram_mb = round(stats::median(peak_ram_mb), 0)
  ),
  by = .(max_batch_cube_size, workers)
][order(max_batch_cube_size, workers)]

#### Prediction model cost ---------------------------------------------------

prediction_table <- results[
  study == "gaussian" & sweep == "prediction_model",
  .(
    explain_seconds = round(stats::median(wall_secs), 2),
    whole_process_seconds = round(stats::median(bash_wall_secs), 2),
    peak_ram_mb = round(stats::median(peak_ram_mb), 0)
  ),
  by = .(model_variant, workers)
][order(model_variant, workers)]
prediction_table[, speedup := round(
  explain_seconds[workers == 1] / explain_seconds,
  2
), by = model_variant]

#### Accuracy/cost surface ---------------------------------------------------

accuracy <- data.table::fread(file.path(result_root, accuracy_study, "accuracy_summary.csv"))
accuracy_table <- accuracy[
  n_explain == 50 &
    ((max_n_coalitions == 32 & n_MC_samples %in% c(25, 100, 400)) |
      (max_n_coalitions == 64 & n_MC_samples == 100) |
      (max_n_coalitions == 128 & n_MC_samples %in% c(100, 400)) |
      (max_n_coalitions == 256 & n_MC_samples %in% c(100, 400))),
  .(
    max_n_coalitions, n_MC_samples,
    explain_seconds = round(wall_median, 2),
    peak_ram_mb = round(ram_mb_median, 0),
    shapley_rmse = round(accuracy_rmse_median, 3),
    replicate_instability_rmse = round(replicate_stability_rmse, 3)
  )
][order(max_n_coalitions, n_MC_samples)]

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
    "gaussian", "independence", "copula", "categorical", "regression_surrogate",
    "empirical", "regression_separate", "ctree", "arf", "timeseries", "vaeac"
  ),
  whole_process_seconds = c(4.123, 4.967, 4.967, 4.979, 6.084, 7.217, 8.188, 19.188, 19.232, 60.856, 1163.646),
  explain_seconds = c(2.302, 3.184, 3.168, 4.038, 3.311, 5.417, 5.412, 17.337, 17.127, 58.983, 1159.353),
  peak_ram_mb = c(273.1, 258.8, 283.3, 278.2, 462.1, 246.1, 308.6, 357.3, 775.2, 1301.2, 542.7)
)
assert_table(reference_table, expected_reference, "reference table")

expected_parallel <- data.table::data.table(
  case = rep(c(
    "Gaussian / 32 batches", "Empirical / 8 batches", "CTree / 32 batches",
    "ARF / 32 batches", "Timeseries / 8 batches", "VAEAC explanation-heavy / 16 batches"
  ), c(4, 4, 4, 4, 4, 3)),
  workers = c(rep(c(1L, 4L, 8L, 16L), 5), 1L, 4L, 16L),
  elapsed_seconds = c(
    36.5, 17.3, 14.6, 13.6, 79.6, 28.2, 20.2, 20.3, 254.3, 75.2, 46.9, 32.5,
    365.9, 120.1, 77.7, 65.7, 253.5, 83.1, 53.5, 53.7, 294.6, 212.8, 194.2
  ),
  peak_ram_mb = c(
    592, 2236, 4116, 7263, 1548, 5133, 8892, 9391, 747, 2860, 5241, 9989,
    3793, 12981, 24452, 44733, 11810, 47148, 71477, 72050, 3376, 10488, 17475
  )
)
assert_table(parallel_table, expected_parallel, "parallel table")

expected_memory <- data.table::data.table(
  max_batch_cube_size = rep(c(1e6, 4e6, 16e6, 64e6, Inf), each = 2),
  workers = rep(c(1L, 4L), 5),
  actual_batches = rep(c(342L, 79L, 20L, 8L, 8L), each = 2),
  elapsed_seconds = c(33.1, 16.5, 38.6, 18.2, 36.1, 18.5, 34.3, 18.0, 34.1, 17.8),
  peak_ram_mb = c(367, 1568, 425, 2057, 748, 2728, 1473, 5260, 1462, 5233)
)
assert_table(memory_table, expected_memory, "memory-cap table")

expected_prediction <- data.table::data.table(
  model_variant = rep(c("linear", "xgb", "xgb_large"), each = 3),
  workers = rep(c(1L, 4L, 16L), 3),
  explain_seconds = c(8.28, 4.54, 4.18, 10.59, 6.36, 5.75, 50.36, 18.03, 13.24),
  whole_process_seconds = c(9.39, 6.78, 6.75, 12.57, 9.44, 9.22, 52.43, 21.12, 16.74),
  peak_ram_mb = c(300, 1236, 3580, 369, 1756, 5269, 373, 1742, 5336),
  speedup = c(1.00, 1.82, 1.98, 1.00, 1.67, 1.84, 1.00, 2.79, 3.80)
)
assert_table(prediction_table, expected_prediction, "prediction-model table")

expected_accuracy <- data.table::data.table(
  max_n_coalitions = c(32L, 32L, 32L, 64L, 128L, 128L, 256L, 256L),
  n_MC_samples = c(25L, 100L, 400L, 100L, 100L, 400L, 100L, 400L),
  explain_seconds = c(1.46, 1.53, 1.84, 1.73, 2.25, 4.06, 1.78, 6.30),
  peak_ram_mb = c(230, 245, 250, 247, 252, 305, 261, 342),
  shapley_rmse = c(0.214, 0.081, 0.068, 0.054, 0.047, 0.023, 0.023, 0.014),
  replicate_instability_rmse = c(0.302, 0.140, 0.101, 0.084, 0.060, 0.033, 0.034, 0.017)
)
assert_table(accuracy_table, expected_accuracy, "accuracy table")
stopifnot(abs(unique(accuracy$reference_noise_rmse) - 0.006336911) < 1e-9)

expected_variability <- data.table::data.table(
  replicates = c(2L, 3L),
  configurations = c(89L, 700L),
  median_relative_iqr = c(0.0033, 0.0073),
  p90_relative_iqr = c(0.0133, 0.0195)
)
assert_table(variability_table, expected_variability, "replicate-variability values")

cat("Cost-snapshot integrity: 2,278 successful runs; 789 configurations; all pairs valid.\n")
cat("Accuracy study: 75 successful runs (72 candidates, 3 references).\n\n")
cat("Comparable reference configuration:\n")
print(reference_table)
cat("\nRepresentative parallel workloads:\n")
print(parallel_table)
cat("\nGaussian memory-cap calibration:\n")
print(memory_table)
cat("\nPrediction-model comparison:\n")
print(prediction_table)
cat("\nSelected accuracy/cost rows:\n")
print(accuracy_table)
cat("\nReplicate variability:\n")
print(variability_table)
