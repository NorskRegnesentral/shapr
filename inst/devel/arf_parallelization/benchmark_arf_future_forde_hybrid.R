#!/usr/bin/env Rscript

# Benchmark the hybrid ARF strategy: parallel ARF setup/forde plus shapr future batching,
# while keeping arf::forge() sequential inside each future worker.

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

worker_budgets <- parse_int_list(get_arg("workers", "8,16,24,32"))
n_reps <- as.integer(get_arg("reps", 2L))
n_train <- as.integer(get_arg("n-train", 600L))
n_explain <- as.integer(get_arg("n-explain", 50L))
n_features <- as.integer(get_arg("features", 6L))
n_trees <- as.integer(get_arg("trees", 500L))
max_iters <- as.integer(get_arg("max-iters", 2L))
n_mc_samples <- as.integer(get_arg("mc-samples", 500L))
max_n_coalitions <- as.integer(get_arg("coalitions", 100L))
results_dir <- get_arg("results-dir", "inst/devel/arf_parallelization/results_future_forde_hybrid")

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

with_future_workers <- function(workers, expr) {
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  future::plan(future::multisession, workers = workers)
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
    x_train = data[train_idx, ..x_names],
    x_explain = data[explain_idx, ..x_names],
    model = stats::lm(stats::as.formula(paste("y ~", paste(x_names, collapse = " + "))), data = data[train_idx]),
    phi0 = data[train_idx, mean(y)]
  )
}

run_one <- function(bench_data, workers, scenario, rep) {
  reset_parallel()
  set_ranger_threads(1L)
  set.seed(400000 + workers * 1000L + rep)

  common_args <- list(
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
    verbose = NULL,
    iterative = FALSE
  )

  if (scenario == "outer_future_only") {
    common_args$arf.parallel_train <- FALSE
    common_args$arf.parallel_gen <- FALSE
    common_args$extra_computation_args <- list(
      vS_batching_method = "future",
      min_n_batches = workers,
      max_batch_size = 5L
    )
  } else if (scenario == "future_plus_forde") {
    common_args$arf.parallel_train <- TRUE
    common_args$arf.parallel_gen <- FALSE
    common_args$extra_computation_args <- list(
      vS_batching_method = "future",
      min_n_batches = workers,
      max_batch_size = 5L
    )
  } else if (scenario == "inner_arf_one_batch") {
    common_args$arf.parallel_train <- TRUE
    common_args$arf.parallel_gen <- TRUE
    common_args$extra_computation_args <- list(
      vS_batching_method = "forloop",
      min_n_batches = 1L,
      max_batch_size = Inf
    )
  } else {
    stop("Unknown scenario: ", scenario, call. = FALSE)
  }

  error_message <- NA_character_
  elapsed_sec <- tryCatch(
    {
      gc()
      unname(system.time({
        if (scenario == "outer_future_only") {
          out <- with_future_workers(workers, do.call(shapr::explain, common_args))
        } else if (scenario == "future_plus_forde") {
          out <- with_foreach_workers(workers, with_future_workers(workers, do.call(shapr::explain, common_args)))
        } else if (scenario == "inner_arf_one_batch") {
          out <- with_foreach_workers(workers, do.call(shapr::explain, common_args))
        }
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
    workers = workers,
    scenario = scenario,
    rep = rep,
    elapsed_sec = elapsed_sec,
    success = !is.na(elapsed_sec),
    error_message = error_message
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
    error_message = paste(unique(stats::na.omit(error_message)), collapse = " | ")
  ), by = .(workers, scenario)][order(workers, median_sec)]
}

write_summary <- function(summary_dt) {
  lines <- c(
    "# ARF Future Plus Forde Hybrid Summary",
    "",
    paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    "",
    "## Scenario Definitions",
    "",
    "- `outer_future_only`: shapr future batching, ARF training/forde/forge sequential.",
    "- `future_plus_forde`: shapr future batching plus ARF setup/forde parallelized through foreach; forge sequential.",
    "- `inner_arf_one_batch`: no future; ARF setup/forde and forge parallelized through foreach; one v(S) batch.",
    "",
    "## Results",
    "",
    paste(capture.output(print(summary_dt)), collapse = "\n")
  )
  writeLines(lines, file.path(results_dir, "INTERPRETATION.md"))
}

message("Workers: ", paste(worker_budgets, collapse = ", "))
message("Repetitions: ", n_reps)

bench_data <- make_benchmark_data(n_train, n_explain, n_features)
rows <- list()
scenarios <- c("outer_future_only", "future_plus_forde", "inner_arf_one_batch")

for (workers in worker_budgets) {
  for (rep in seq_len(n_reps)) {
    for (scenario in scenarios) {
      message("Running workers=", workers, ", scenario=", scenario, ", rep=", rep, "/", n_reps)
      rows[[length(rows) + 1L]] <- run_one(bench_data, workers, scenario, rep)
      results <- data.table::rbindlist(rows)
      summary_dt <- summarise_results(results)
      data.table::fwrite(results, file.path(results_dir, "arf_future_forde_hybrid_results.csv"))
      data.table::fwrite(summary_dt, file.path(results_dir, "arf_future_forde_hybrid_summary.csv"))
      write_summary(summary_dt)
    }
  }
}

reset_parallel()
writeLines(capture.output(sessionInfo()), file.path(results_dir, "arf_future_forde_hybrid_config.txt"))
message("Wrote results to: ", normalizePath(results_dir))
print(summary_dt)
