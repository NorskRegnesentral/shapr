#!/usr/bin/env Rscript

# Benchmark only the shapr ARF inner-parallel scenario while varying the number of v(S) batches.
# Run from the repository root:
#   Rscript inst/devel/arf_parallelization/benchmark_arf_batch_sweep.R

args <- commandArgs(trailingOnly = TRUE)
get_arg <- function(name, default) {
  prefix <- paste0("--", name, "=")
  hit <- grep(paste0("^", prefix), args, value = TRUE)
  if (length(hit) == 0) {
    return(default)
  }
  sub(prefix, "", hit[[1]])
}

parse_int_list <- function(x) {
  as.integer(strsplit(x, ",", fixed = TRUE)[[1]])
}

worker_budgets <- parse_int_list(get_arg("workers", "2,4,8,16,24,32"))
batch_counts <- parse_int_list(get_arg("batches", "1,2,4,8,16,32,64"))
n_reps <- as.integer(get_arg("reps", 2L))
n_train <- as.integer(get_arg("n-train", 600L))
n_explain <- as.integer(get_arg("n-explain", 50L))
n_features <- as.integer(get_arg("features", 6L))
n_trees <- as.integer(get_arg("trees", 500L))
max_iters <- as.integer(get_arg("max-iters", 2L))
n_mc_samples <- as.integer(get_arg("mc-samples", 500L))
max_n_coalitions <- as.integer(get_arg("coalitions", 100L))
results_dir <- get_arg("results-dir", "inst/devel/arf_parallelization/results_batch_sweep")

required_packages <- c("arf", "data.table", "doParallel", "foreach", "future")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop("Missing required package(s): ", paste(missing_packages, collapse = ", "), call. = FALSE)
}

suppressPackageStartupMessages({
  library(data.table)
  library(foreach)
})

if (requireNamespace("devtools", quietly = TRUE) && file.exists("DESCRIPTION")) {
  devtools::load_all(quiet = TRUE)
} else {
  library(shapr)
}

if (!dir.exists(results_dir)) dir.create(results_dir, recursive = TRUE)

results_path <- file.path(results_dir, "arf_batch_sweep_results.csv")
summary_csv_path <- file.path(results_dir, "arf_batch_sweep_summary.csv")
summary_md_path <- file.path(results_dir, "arf_batch_sweep_summary.md")
config_path <- file.path(results_dir, "arf_batch_sweep_config.txt")

set_ranger_threads <- function(n_threads) {
  Sys.setenv(R_RANGER_NUM_THREADS = as.character(n_threads))
  options(ranger.num.threads = n_threads, Ncpus = n_threads)
}

reset_parallel <- function() {
  foreach::registerDoSEQ()
  future::plan(future::sequential)
}

with_foreach_workers <- function(workers, expr) {
  on.exit(foreach::registerDoSEQ(), add = TRUE)
  doParallel::registerDoParallel(cores = workers)
  force(expr)
}

make_benchmark_data <- function(n_train, n_explain, n_features) {
  set.seed(20260612)
  rho <- 0.55
  cov_mat <- outer(seq_len(n_features), seq_len(n_features), function(i, j) rho^abs(i - j))
  z <- matrix(stats::rnorm((n_train + n_explain) * n_features), ncol = n_features)
  x <- z %*% chol(cov_mat)
  colnames(x) <- paste0("x", seq_len(n_features))
  x <- data.table::as.data.table(x)
  beta <- seq(1, 2, length.out = n_features)
  y <- as.matrix(x) %*% beta + 0.25 * stats::rnorm(n_train + n_explain)
  data <- data.table::copy(x)
  data[, y := as.numeric(y)]

  train_idx <- seq_len(n_train)
  explain_idx <- seq(n_train + 1L, n_train + n_explain)
  x_names <- paste0("x", seq_len(n_features))

  list(
    data = data,
    x_train = data[train_idx, ..x_names],
    x_explain = data[explain_idx, ..x_names],
    model = stats::lm(stats::as.formula(paste("y ~", paste(x_names, collapse = " + "))), data = data[train_idx]),
    phi0 = data[train_idx, mean(y)]
  )
}

summarise_results <- function(results) {
  results[, .(
    n = .N,
    n_success = sum(success),
    n_error = sum(!success),
    median_sec = if (any(success)) stats::median(elapsed_sec, na.rm = TRUE) else NA_real_,
    mean_sec = if (any(success)) mean(elapsed_sec, na.rm = TRUE) else NA_real_,
    min_sec = if (any(success)) min(elapsed_sec, na.rm = TRUE) else NA_real_,
    max_sec = if (any(success)) max(elapsed_sec, na.rm = TRUE) else NA_real_,
    actual_n_batches = paste(sort(unique(stats::na.omit(actual_n_batches))), collapse = ","),
    actual_n_coalitions = paste(sort(unique(stats::na.omit(actual_n_coalitions))), collapse = ","),
    error_message = paste(unique(stats::na.omit(error_message)), collapse = " | ")
  ), by = .(workers_foreach, requested_n_batches)][order(workers_foreach, requested_n_batches)]
}

write_markdown_summary <- function(summary_dt, path) {
  lines <- c(
    "# ARF Batch Sweep Summary",
    "",
    paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    "",
    "## Configuration",
    "",
    paste0("- Workers: ", paste(worker_budgets, collapse = ", ")),
    paste0("- Requested batches: ", paste(batch_counts, collapse = ", ")),
    paste0("- Repetitions: ", n_reps),
    paste0("- Training rows: ", n_train),
    paste0("- Explained rows: ", n_explain),
    paste0("- Features: ", n_features),
    paste0("- ARF trees: ", n_trees),
    paste0("- ARF max iterations: ", max_iters),
    paste0("- MC samples: ", n_mc_samples),
    paste0("- Max coalitions: ", max_n_coalitions),
    "",
    "## Scenario",
    "",
    "Only `inner_arf_only` is benchmarked: `arf.parallel_train = TRUE`, `arf.parallel_gen = TRUE`, `vS_batching_method = \"forloop\"`, `ranger` capped at one thread, and a foreach backend registered with the worker budget.",
    "",
    "`requested_n_batches` is passed as `extra_computation_args$min_n_batches` with `max_batch_size = Inf`.",
    "",
    "## Results",
    "",
    paste(capture.output(print(summary_dt)), collapse = "\n")
  )
  writeLines(lines, path)
}

append_results <- function(new_row) {
  if (file.exists(results_path)) {
    data.table::fwrite(new_row, results_path, append = TRUE)
  } else {
    data.table::fwrite(new_row, results_path)
  }

  results <- data.table::fread(results_path)
  summary_dt <- summarise_results(results)
  data.table::fwrite(summary_dt, summary_csv_path)
  write_markdown_summary(summary_dt, summary_md_path)
}

run_one <- function(bench_data, workers, requested_n_batches, rep) {
  reset_parallel()
  set_ranger_threads(1L)
  set.seed(300000 + workers * 1000L + requested_n_batches * 10L + rep)

  actual_n_batches <- NA_integer_
  actual_n_coalitions <- NA_integer_
  error_message <- NA_character_
  elapsed_sec <- tryCatch(
    {
      gc()
      unname(system.time({
        out <- with_foreach_workers(workers, shapr::explain(
          testing = TRUE,
          model = bench_data$model,
          x_explain = bench_data$x_explain,
          x_train = bench_data$x_train,
          approach = "arf",
          phi0 = bench_data$phi0,
          seed = 1,
          max_n_coalitions = max_n_coalitions,
          n_MC_samples = n_mc_samples,
          arf.num_trees = n_trees,
          arf.min_node_size = 5L,
          arf.max_iters = max_iters,
          arf.alpha = 0.1,
          arf.epsilon = 1e-15,
          arf.parallel_train = TRUE,
          arf.parallel_gen = TRUE,
          extra_computation_args = list(
            vS_batching_method = "forloop",
            min_n_batches = requested_n_batches,
            max_batch_size = Inf
          ),
          verbose = NULL,
          iterative = FALSE
        ))
        actual_n_batches <- out$internal$iter_list[[1]]$n_batches
        actual_n_coalitions <- out$internal$iter_list[[1]]$n_coalitions
        rm(out)
      })["elapsed"])
    },
    error = function(error) {
      error_message <<- conditionMessage(error)
      NA_real_
    }
  )

  data.table::data.table(
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    scenario = "inner_arf_only_batch_sweep",
    workers_foreach = workers,
    requested_n_batches = requested_n_batches,
    actual_n_batches = actual_n_batches,
    actual_n_coalitions = actual_n_coalitions,
    rep = rep,
    elapsed_sec = elapsed_sec,
    success = !is.na(elapsed_sec),
    error_message = error_message
  )
}

message("Workers: ", paste(worker_budgets, collapse = ", "))
message("Requested batches: ", paste(batch_counts, collapse = ", "))
message("Repetitions: ", n_reps)

writeLines(capture.output(sessionInfo()), config_path)
bench_data <- make_benchmark_data(n_train, n_explain, n_features)

for (workers in worker_budgets) {
  for (requested_n_batches in batch_counts) {
    for (rep in seq_len(n_reps)) {
      message(
        "Running workers=", workers,
        ", requested_n_batches=", requested_n_batches,
        ", rep=", rep, "/", n_reps
      )
      append_results(run_one(bench_data, workers, requested_n_batches, rep))
    }
  }
}

reset_parallel()
message("Wrote results to: ", normalizePath(results_dir))
print(data.table::fread(summary_csv_path))
