#!/usr/bin/env Rscript

# Benchmark ARF parallelization options in shapr.
# Run from the repository root:
#   Rscript inst/devel/arf_parallelization/benchmark_arf_parallelization.R

args <- commandArgs(trailingOnly = TRUE)
get_arg <- function(name, default) {
  prefix <- paste0("--", name, "=")
  hit <- grep(paste0("^", prefix), args, value = TRUE)
  if (length(hit) == 0) {
    return(default)
  }
  sub(prefix, "", hit[[1]])
}

worker_budget <- as.integer(get_arg("workers", min(4L, parallel::detectCores(logical = TRUE))))
n_reps <- as.integer(get_arg("reps", 3L))
n_train <- as.integer(get_arg("n-train", 600L))
n_explain <- as.integer(get_arg("n-explain", 3L))
n_features <- as.integer(get_arg("features", 6L))
n_trees <- as.integer(get_arg("trees", 30L))
max_iters <- as.integer(get_arg("max-iters", 2L))
n_mc_samples <- as.integer(get_arg("mc-samples", 20L))
max_n_coalitions <- as.integer(get_arg("coalitions", 20L))
results_dir <- get_arg("results-dir", "inst/devel/arf_parallelization/results")

required_packages <- c("arf", "data.table", "doParallel", "foreach", "future", "future.apply")
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
  old_backend <- foreach::getDoParName()
  on.exit(
    {
      foreach::registerDoSEQ()
    },
    add = TRUE
  )
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
    data = data,
    x_train = data[train_idx, ..x_names],
    x_explain = data[explain_idx, ..x_names],
    model = stats::lm(stats::as.formula(paste("y ~", paste(x_names, collapse = " + "))), data = data[train_idx]),
    phi0 = data[train_idx, mean(y)]
  )
}

make_evidence <- function(x_explain, n_features, n_coalitions) {
  coalition_list <- unlist(
    lapply(seq_len(n_features - 1L), function(k) utils::combn(seq_len(n_features), k, simplify = FALSE)),
    recursive = FALSE
  )
  coalition_list <- coalition_list[seq_len(min(n_coalitions, length(coalition_list)))]

  evidence_list <- vector("list", length(coalition_list))
  for (i in seq_along(coalition_list)) {
    evidence_list[[i]] <- data.table::copy(x_explain)
    data.table::set(evidence_list[[i]], j = setdiff(seq_len(n_features), coalition_list[[i]]), value = NA)
  }
  data.table::rbindlist(evidence_list)
}

elapsed <- function(expr) {
  gc()
  unname(system.time(force(expr))["elapsed"])
}

elapsed_or_error <- function(expr) {
  error_message <- NULL
  elapsed_sec <- tryCatch(
    elapsed(expr),
    error = function(error) {
      error_message <<- conditionMessage(error)
      NA_real_
    }
  )
  list(elapsed_sec = elapsed_sec, error_message = error_message)
}

record <- function(section,
                   scenario,
                   rep,
                   elapsed_sec,
                   workers_outer,
                   workers_foreach,
                   ranger_threads,
                   notes = "",
                   error_message = NA_character_) {
  data.table::data.table(
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    section = section,
    scenario = scenario,
    rep = rep,
    elapsed_sec = elapsed_sec,
    workers_outer = workers_outer,
    workers_foreach = workers_foreach,
    ranger_threads = ranger_threads,
    success = !is.na(elapsed_sec),
    error_message = error_message,
    notes = notes
  )
}

record_timed <- function(section,
                         scenario,
                         rep,
                         expr,
                         workers_outer,
                         workers_foreach,
                         ranger_threads,
                         notes = "") {
  timing <- elapsed_or_error(expr)
  record(
    section = section,
    scenario = scenario,
    rep = rep,
    elapsed_sec = timing$elapsed_sec,
    workers_outer = workers_outer,
    workers_foreach = workers_foreach,
    ranger_threads = ranger_threads,
    notes = notes,
    error_message = timing$error_message
  )
}

run_direct_arf <- function(bench_data) {
  rows <- list()
  evidence <- make_evidence(bench_data$x_explain, n_features, n_coalitions = 12L)

  for (rep in seq_len(n_reps)) {
    set.seed(1000 + rep)
    reset_parallel()
    set_ranger_threads(1L)
    fit_seq <- NULL
    rows[[length(rows) + 1L]] <- record_timed(
      "direct_arf",
      "train_ranger_1_thread",
      rep,
      {
        fit_seq <- arf::adversarial_rf(
          bench_data$x_train,
          num_trees = n_trees,
          min_node_size = 5L,
          max_iters = max_iters,
          parallel = FALSE,
          verbose = FALSE
        )
      },
      workers_outer = 1L,
      workers_foreach = 1L,
      ranger_threads = 1L,
      notes = "arf parallel=FALSE forces ranger num.threads=1"
    )

    reset_parallel()
    set_ranger_threads(worker_budget)
    fit_threaded <- NULL
    rows[[length(rows) + 1L]] <- record_timed(
      "direct_arf",
      paste0("train_ranger_", worker_budget, "_threads"),
      rep,
      {
        fit_threaded <- arf::adversarial_rf(
          bench_data$x_train,
          num_trees = n_trees,
          min_node_size = 5L,
          max_iters = max_iters,
          parallel = TRUE,
          verbose = FALSE
        )
      },
      workers_outer = 1L,
      workers_foreach = 1L,
      ranger_threads = worker_budget,
      notes = "ranger thread count capped through options/env"
    )

    reset_parallel()
    set_ranger_threads(1L)
    params_seq <- NULL
    rows[[length(rows) + 1L]] <- record_timed(
      "direct_arf",
      "forde_sequential",
      rep,
      {
        params_seq <- arf::forde(
          fit_seq,
          bench_data$x_train,
          finite_bounds = "local",
          alpha = 0.1,
          epsilon = 1e-15,
          parallel = FALSE
        )
      },
      workers_outer = 1L,
      workers_foreach = 1L,
      ranger_threads = 1L
    )

    reset_parallel()
    set_ranger_threads(1L)
    params_parallel <- NULL
    rows[[length(rows) + 1L]] <- with_foreach_workers(worker_budget, record_timed(
      "direct_arf",
      paste0("forde_foreach_", worker_budget, "_workers"),
      rep,
      {
        params_parallel <- arf::forde(
          fit_seq,
          bench_data$x_train,
          finite_bounds = "local",
          alpha = 0.1,
          epsilon = 1e-15,
          parallel = TRUE
        )
      },
      workers_outer = 1L,
      workers_foreach = worker_budget,
      ranger_threads = 1L
    ))

    reset_parallel()
    set_ranger_threads(1L)
    rows[[length(rows) + 1L]] <- record_timed(
      "direct_arf",
      "forge_sequential",
      rep,
      {
        arf::forge(
          params_seq,
          n_synth = n_mc_samples,
          evidence = evidence,
          evidence_row_mode = "separate",
          parallel = FALSE,
          verbose = FALSE
        )
      },
      workers_outer = 1L,
      workers_foreach = 1L,
      ranger_threads = 1L
    )

    reset_parallel()
    set_ranger_threads(1L)
    rows[[length(rows) + 1L]] <- with_foreach_workers(worker_budget, record_timed(
      "direct_arf",
      paste0("forge_foreach_", worker_budget, "_workers"),
      rep,
      {
        arf::forge(
          params_seq,
          n_synth = n_mc_samples,
          evidence = evidence,
          evidence_row_mode = "separate",
          parallel = TRUE,
          verbose = FALSE
        )
      },
      workers_outer = 1L,
      workers_foreach = worker_budget,
      ranger_threads = 1L
    ))
  }

  data.table::rbindlist(rows)
}

run_shapr_scenario <- function(bench_data, scenario, rep) {
  reset_parallel()
  set.seed(2000 + rep)

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

  if (scenario == "all_sequential") {
    set_ranger_threads(1L)
    common_args$arf.parallel_train <- FALSE
    common_args$arf.parallel_gen <- FALSE
    common_args$extra_computation_args <- list(vS_batching_method = "forloop")
    return(record_timed(
      "shapr_end_to_end", scenario, rep,
      do.call(shapr::explain, common_args),
      workers_outer = 1L, workers_foreach = 1L, ranger_threads = 1L
    ))
  }

  if (scenario == "outer_future_only") {
    set_ranger_threads(1L)
    common_args$arf.parallel_train <- FALSE
    common_args$arf.parallel_gen <- FALSE
    common_args$extra_computation_args <- list(
      vS_batching_method = "future",
      min_n_batches = worker_budget,
      max_batch_size = 5L
    )
    return(with_future_workers(worker_budget, record_timed(
      "shapr_end_to_end", scenario, rep,
      do.call(shapr::explain, common_args),
      workers_outer = worker_budget, workers_foreach = 1L, ranger_threads = 1L,
      notes = "parallelizes v(S) batches; ARF internals sequential"
    )))
  }

  if (scenario == "inner_forge_only") {
    set_ranger_threads(1L)
    common_args$arf.parallel_train <- FALSE
    common_args$arf.parallel_gen <- TRUE
    common_args$extra_computation_args <- list(vS_batching_method = "forloop")
    return(with_foreach_workers(worker_budget, record_timed(
      "shapr_end_to_end", scenario, rep,
      do.call(shapr::explain, common_args),
      workers_outer = 1L, workers_foreach = worker_budget, ranger_threads = 1L,
      notes = "parallelizes arf::forge within each prepare_data call"
    )))
  }

  if (scenario == "setup_parallel_only") {
    set_ranger_threads(worker_budget)
    common_args$arf.parallel_train <- TRUE
    common_args$arf.parallel_gen <- FALSE
    common_args$extra_computation_args <- list(vS_batching_method = "forloop")
    return(record_timed(
      "shapr_end_to_end", scenario, rep,
      do.call(shapr::explain, common_args),
      workers_outer = 1L, workers_foreach = 1L, ranger_threads = worker_budget,
      notes = "parallelizes ranger training only; forde runs on sequential foreach backend"
    ))
  }

  if (scenario == "inner_arf_only") {
    set_ranger_threads(1L)
    common_args$arf.parallel_train <- TRUE
    common_args$arf.parallel_gen <- TRUE
    common_args$extra_computation_args <- list(vS_batching_method = "forloop")
    return(with_foreach_workers(worker_budget, record_timed(
      "shapr_end_to_end", scenario, rep,
      do.call(shapr::explain, common_args),
      workers_outer = 1L, workers_foreach = worker_budget, ranger_threads = 1L,
      notes = "parallelizes ARF foreach work in forde/forge; ranger capped at one thread"
    )))
  }

  stop("Unknown scenario: ", scenario, call. = FALSE)
}

run_shapr_end_to_end <- function(bench_data) {
  scenarios <- c(
    "all_sequential",
    "outer_future_only",
    "inner_forge_only",
    "setup_parallel_only",
    "inner_arf_only"
  )
  rows <- list()
  for (rep in seq_len(n_reps)) {
    for (scenario in scenarios) {
      message("Running ", scenario, " rep ", rep, "/", n_reps)
      rows[[length(rows) + 1L]] <- run_shapr_scenario(bench_data, scenario, rep)
    }
  }
  data.table::rbindlist(rows)
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
    workers_outer = workers_outer[1],
    workers_foreach = workers_foreach[1],
    ranger_threads = ranger_threads[1],
    error_message = paste(unique(stats::na.omit(.SD$error_message)), collapse = " | "),
    notes = notes[1]
  ), by = .(section, scenario)][order(section, median_sec)]
}

write_markdown_summary <- function(config, summary_dt, path) {
  lines <- c(
    "# ARF Parallelization Benchmark Summary",
    "",
    paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    "",
    "## Configuration",
    "",
    paste0("- Worker budget: ", config$worker_budget),
    paste0("- Repetitions: ", config$n_reps),
    paste0("- Training rows: ", config$n_train),
    paste0("- Explained rows: ", config$n_explain),
    paste0("- Features: ", config$n_features),
    paste0("- ARF trees: ", config$n_trees),
    paste0("- ARF max iterations: ", config$max_iters),
    paste0("- MC samples: ", config$n_mc_samples),
    paste0("- Max coalitions: ", config$max_n_coalitions),
    "",
    "## Interpretation Notes",
    "",
    "- `adversarial_rf(parallel = FALSE)` forces `ranger` to one thread.",
    "- `adversarial_rf(parallel = TRUE)` lets `ranger` use its default thread controls; this script caps it with `options(ranger.num.threads = worker_budget)` and `R_RANGER_NUM_THREADS`.",
    "- `forde(parallel = TRUE)` and `forge(parallel = TRUE)` use `foreach %dopar%`; this script registers exactly `worker_budget` workers for those scenarios.",
    "- `outer_future_only` uses shapr/future batching for `v(S)` while keeping ARF internals sequential to avoid nested parallelism.",
    "",
    "## Results",
    "",
    paste(capture.output(print(summary_dt)), collapse = "\n")
  )
  writeLines(lines, path)
}

message("Worker budget: ", worker_budget)
message("Repetitions: ", n_reps)
set_ranger_threads(worker_budget)
bench_data <- make_benchmark_data(n_train, n_explain, n_features)

results <- rbindlist(list(
  run_direct_arf(bench_data),
  run_shapr_end_to_end(bench_data)
), use.names = TRUE, fill = TRUE)

summary_dt <- summarise_results(results)

results_path <- file.path(results_dir, "arf_parallel_benchmark_results.csv")
summary_csv_path <- file.path(results_dir, "arf_parallel_benchmark_summary.csv")
summary_md_path <- file.path(results_dir, "arf_parallel_benchmark_summary.md")
config_path <- file.path(results_dir, "arf_parallel_benchmark_config.txt")

data.table::fwrite(results, results_path)
data.table::fwrite(summary_dt, summary_csv_path)
write_markdown_summary(
  config = list(
    worker_budget = worker_budget,
    n_reps = n_reps,
    n_train = n_train,
    n_explain = n_explain,
    n_features = n_features,
    n_trees = n_trees,
    max_iters = max_iters,
    n_mc_samples = n_mc_samples,
    max_n_coalitions = max_n_coalitions
  ),
  summary_dt = summary_dt,
  path = summary_md_path
)
writeLines(capture.output(sessionInfo()), config_path)

reset_parallel()
message("Wrote results to: ", normalizePath(results_dir))
print(summary_dt)
