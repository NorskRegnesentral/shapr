#!/usr/bin/env Rscript
# accuracy.R — compare saved benchmark Shapley values with the optional study's
# high-budget reference and aggregate the resulting accuracy/cost surface.

suppressMessages({
  library(data.table)
})

local({
  here <- dirname(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))
  if (is.na(here) || !nzchar(here)) here <- "R"
  source(file.path(here, "config.R"))
})

parse_args <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  config <- args[which(args == "--config") + 1]
  if (length(config) == 0 || is.na(config)) stop("Provide --config <path>")
  return(list(config = config))
}

read_shapley_matrix <- function(path) {
  shapley <- data.table::as.data.table(readRDS(path))
  feature_cols <- setdiff(names(shapley), c("explain_id", "none"))
  if (length(feature_cols) == 0) stop("No feature Shapley columns in ", path)
  return(as.matrix(shapley[, ..feature_cols]))
}

matrix_rmse <- function(x, y) {
  return(sqrt(mean((x - y)^2)))
}

reference_mean_and_noise <- function(paths) {
  matrices <- lapply(paths, read_shapley_matrix)
  dims <- vapply(matrices, function(x) paste(dim(x), collapse = "x"), character(1))
  if (length(unique(dims)) != 1) stop("Reference Shapley matrices have different dimensions")
  reference_mean <- Reduce(`+`, matrices) / length(matrices)
  noise <- vapply(matrices, matrix_rmse, numeric(1), y = reference_mean)
  return(list(mean = reference_mean, noise_rmse = median(noise)))
}

replicate_stability <- function(ids, result_dir) {
  if (length(ids) < 2) {
    return(NA_real_)
  }
  matrices <- lapply(ids, function(id) {
    read_shapley_matrix(file.path(result_dir, paste0(id, ".shapley.rds")))
  })
  pairs <- utils::combn(seq_along(matrices), 2)
  errors <- apply(pairs, 2, function(pair) {
    matrix_rmse(matrices[[pair[1]]], matrices[[pair[2]]])
  })
  return(mean(errors))
}

main <- function() {
  a <- parse_args()
  cfg <- load_config(a$config)
  result_dir <- cfg$dir$results
  results <- fread(file.path(result_dir, "results.csv"))
  results <- results[status == "ok" & is_warmup == FALSE]

  references <- results[sweep == "accuracy_reference"]
  candidates <- results[sweep == "accuracy_cost"]
  if (nrow(references) < 2) stop("Accuracy study needs at least two successful reference runs")
  if (nrow(candidates) == 0) stop("No successful accuracy_cost runs found")

  reference_paths <- file.path(result_dir, paste0(references$id, ".shapley.rds"))
  if (!all(file.exists(reference_paths))) stop("One or more reference Shapley files are missing")
  reference <- reference_mean_and_noise(reference_paths)

  metrics <- rbindlist(lapply(candidates$id, function(id) {
    candidate <- read_shapley_matrix(file.path(result_dir, paste0(id, ".shapley.rds")))
    if (ncol(candidate) != ncol(reference$mean) || nrow(candidate) > nrow(reference$mean)) {
      stop("Candidate/reference Shapley dimensions do not align for id ", id)
    }
    target <- reference$mean[seq_len(nrow(candidate)), , drop = FALSE]
    difference <- candidate - target
    data.table(
      id = id,
      accuracy_rmse = sqrt(mean(difference^2)),
      accuracy_mae = mean(abs(difference)),
      accuracy_max_abs = max(abs(difference)),
      reference_noise_rmse = reference$noise_rmse
    )
  }))

  detail_cols <- c(
    "id", "rep", "n_features", "max_n_coalitions", "n_MC_samples",
    "n_explain", "wall_secs", "bash_wall_secs", "peak_ram_mb"
  )
  accuracy_results <- merge(
    candidates[, ..detail_cols],
    metrics,
    by = "id",
    all.x = TRUE,
    sort = FALSE
  )

  accuracy_summary <- accuracy_results[, .(
    n = .N,
    wall_median = median(wall_secs),
    ram_mb_median = median(peak_ram_mb),
    accuracy_rmse_median = median(accuracy_rmse),
    accuracy_rmse_iqr = IQR(accuracy_rmse),
    accuracy_mae_median = median(accuracy_mae),
    accuracy_max_abs_median = median(accuracy_max_abs),
    reference_noise_rmse = first(reference_noise_rmse),
    replicate_stability_rmse = replicate_stability(id, result_dir)
  ), by = .(n_features, max_n_coalitions, n_MC_samples, n_explain)]
  setorder(accuracy_summary, n_explain, max_n_coalitions, n_MC_samples)

  fwrite(accuracy_results, file.path(result_dir, "accuracy_results.csv"))
  fwrite(accuracy_summary, file.path(result_dir, "accuracy_summary.csv"))
  cat(sprintf(
    "Accuracy analysis: %d candidate runs, %d references, reference noise RMSE %.6f\n",
    nrow(accuracy_results), nrow(references), reference$noise_rmse
  ))
  cat("Wrote:", file.path(result_dir, "accuracy_results.csv"), "\n")
  cat("Wrote:", file.path(result_dir, "accuracy_summary.csv"), "\n")
}

main()
