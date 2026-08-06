#!/usr/bin/env Rscript
# merge_retained_results.R — remap retained historical benchmark artifacts to
# the consolidated per-approach grids. The destination must already contain
# freshly generated grid.csv/run_meta.json files.

suppressMessages({
  library(data.table)
})

args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(flag) {
  pos <- which(args == flag)
  if (length(pos) != 1 || pos == length(args)) stop("Provide ", flag, " <path>")
  return(normalizePath(args[pos + 1], mustWork = TRUE))
}

source_root <- arg_value("--source-root")
destination_root <- arg_value("--destination-root")
source_results <- file.path(source_root, "results")
source_logs <- file.path(source_root, "logs")
destination_results <- file.path(destination_root, "results")
destination_logs <- file.path(destination_root, "logs")

dimension_cols <- c(
  "dataset", "approach", "n_features", "max_n_coalitions", "n_MC_samples",
  "min_n_batches", "max_batch_size", "max_batch_cube_size", "workers", "backend",
  "dt_threads", "n_train", "n_explain", "iterative", "group", "group_size",
  "model_variant", "save_explanations"
)
match_cols <- c("sweep", "rep", dimension_cols, "approach_args", "pair_key", "pair_role")

read_source_grid <- function(study) {
  path <- file.path(source_results, study, "grid.csv")
  grid <- data.table::fread(path)
  if ("is_warmup" %in% names(grid)) grid <- grid[is.na(is_warmup) | is_warmup == FALSE]
  return(grid)
}

candidate_rows <- list()
add_source <- function(source_study, source_sweeps, target_sweep = source_sweeps,
                       model_variant = "default", save_explanations = FALSE,
                       filter = NULL, clear_approach_args = FALSE) {
  source <- read_source_grid(source_study)[sweep %in% source_sweeps]
  if (!is.null(filter)) source <- filter(source)
  if (length(target_sweep) == 1) source[, sweep := target_sweep]
  source[, `:=`(
    source_study = source_study,
    source_id = id,
    model_variant = model_variant,
    save_explanations = save_explanations
  )]
  if (clear_approach_args) source[, approach_args := ""]
  candidate_rows[[length(candidate_rows) + 1]] <<- source
  return(invisible(NULL))
}

core_sweeps <- list(
  gaussian = c(
    "scale_train_mc", "features", "coalitions", "highdim_cap", "grouping",
    "group_size", "explain", "parallel_batching", "parallel_backend",
    "dt_threads", "batches", "iterative_budget"
  ),
  empirical = c(
    "scale_train_mc", "features", "coalitions", "highdim_cap", "explain",
    "dt_threads", "parallel", "batches", "iterative_budget", "empirical_type"
  ),
  ctree = c(
    "scale_train_mc", "features", "coalitions", "explain", "dt_threads",
    "parallel", "batches", "iterative_budget", "dataset"
  ),
  arf = c(
    "scale_train_mc", "features", "coalitions", "explain", "dt_threads",
    "parallel", "batches", "iterative_budget", "dataset"
  ),
  timeseries = c(
    "scale_train_mc", "features", "coalitions", "explain", "dt_threads",
    "parallel", "batches", "iterative_budget"
  ),
  vaeac = c(
    "scale_train_mc", "coalitions", "features", "explain", "dt_threads",
    "parallel", "batches", "dataset", "iterative_budget", "vaeac_depth",
    "vaeac_width", "vaeac_epochs", "vaeac_latent_dim", "vaeac_n_vaeacs_initialize"
  ),
  independence = c(
    "scale_train_mc", "features", "coalitions", "explain", "dt_threads",
    "parallel", "batches", "iterative_budget", "dataset"
  ),
  copula = c(
    "scale_train_mc", "features", "coalitions", "highdim_cap", "explain",
    "dt_threads", "parallel", "batches", "iterative_budget", "dataset"
  ),
  categorical = c(
    "scale_train_mc", "coalitions", "explain", "dt_threads", "parallel",
    "batches", "iterative_budget", "dataset"
  ),
  regression_separate = c(
    "scale_train_mc", "features", "coalitions", "explain", "dt_threads",
    "parallel", "batches", "iterative_budget", "dataset", "variants"
  ),
  regression_surrogate = c(
    "scale_train_mc", "features", "coalitions", "explain", "dt_threads",
    "parallel", "batches", "iterative_budget", "dataset", "surrogate_n_comb", "variant"
  )
)

for (study in names(core_sweeps)) {
  variant <- if (study == "gaussian") "xgb" else "default"
  add_source(study, core_sweeps[[study]], model_variant = variant)
}

parallel_maps <- list(
  gaussian = c(
    medium_sequential = "realistic_medium_sequential",
    medium_parallel = "realistic_medium_parallel",
    heavy_sequential = "realistic_heavy_sequential",
    heavy_parallel = "realistic_heavy_parallel"
  ),
  empirical = c(
    medium_sequential = "realistic_medium",
    medium_parallel = "realistic_medium",
    heavy_sequential = "realistic_heavy",
    heavy_parallel = "realistic_heavy"
  ),
  ctree = c(
    medium_sequential = "realistic_medium",
    medium_parallel = "realistic_medium",
    heavy_sequential = "realistic_heavy",
    heavy_parallel = "realistic_heavy"
  ),
  arf = c(
    medium_sequential = "realistic_medium",
    medium_parallel = "realistic_medium",
    heavy_sequential = "realistic_heavy",
    heavy_parallel = "realistic_heavy"
  )
)

for (study in names(parallel_maps)) {
  source_study <- paste0("extra_parallel_", study)
  for (source_sweep in names(parallel_maps[[study]])) {
    target_sweep <- parallel_maps[[study]][[source_sweep]]
    low_batch <- grepl("sequential$", source_sweep)
    if (low_batch) {
      low_value <- if (study == "empirical") 2 else if (study == "arf") 4 else 1
      add_source(source_study, source_sweep,
        paste0(target_sweep, "_low_batch"),
        model_variant = if (study == "gaussian") "xgb" else "default",
        filter = function(x) x[min_n_batches == low_value])
      add_source(source_study, source_sweep, target_sweep,
        model_variant = if (study == "gaussian") "xgb" else "default",
        filter = function(x) x[min_n_batches != low_value])
    } else {
      add_source(source_study, source_sweep, target_sweep,
        model_variant = if (study == "gaussian") "xgb" else "default")
    }
  }
}

# The Gaussian low-batch source rows keep their own sequential sweep names.
candidate_rows <- lapply(candidate_rows, function(x) {
  x[sweep == "realistic_medium_sequential_low_batch", sweep := "realistic_medium_sequential"]
  x[sweep == "realistic_heavy_sequential_low_batch", sweep := "realistic_heavy_sequential"]
  return(x)
})

add_source("extra_memory_budget_gaussian",
  c("memory_cap_sequential", "memory_cap_parallel", "memory_cap_highdim"),
  model_variant = "xgb")
add_source("extra_accuracy_interactions_gaussian", "accuracy_cost",
  model_variant = "xgb", save_explanations = TRUE)
add_source("extra_accuracy_interactions_gaussian", "accuracy_reference",
  model_variant = "xgb", save_explanations = TRUE)
add_source("extra_prediction_model_linear", "prediction_model", "prediction_model",
  model_variant = "linear")
add_source("extra_prediction_cost_gaussian",
  c("prediction_cost_sequential", "prediction_cost_parallel"), "prediction_model",
  model_variant = "xgb",
  filter = function(x) x[approach_args == "benchmark.prediction_repeats=1"],
  clear_approach_args = TRUE)
add_source("extra_prediction_model_xgb_large", "prediction_model", "prediction_model",
  model_variant = "xgb_large")

add_source("extra_parallel_timeseries", "medium_sequential", "realistic_medium_low_batch",
  filter = function(x) x[min_n_batches == 1])
add_source("extra_parallel_timeseries", "medium_sequential", "realistic_medium",
  filter = function(x) x[min_n_batches != 1])
add_source("extra_parallel_timeseries", "medium_parallel", "realistic_medium")
add_source("extra_parallel_timeseries", "heavy_sequential", "realistic_heavy_sequential")
add_source("extra_parallel_timeseries", "heavy_parallel", "realistic_heavy_parallel",
  filter = function(x) x[workers == 4])
add_source("extra_parallel_timeseries", "heavy_parallel", "realistic_heavy_safe_frontier",
  filter = function(x) x[workers == 16 & min_n_batches == 8])
add_source("extra_parallel_timeseries", "heavy_parallel_8workers", "realistic_heavy_safe_frontier")

vaeac_maps <- c(
  training_dominated_sequential = "realistic_training_sequential",
  training_dominated_parallel = "realistic_training_parallel",
  explanation_heavy_sequential = "realistic_explanation_sequential",
  explanation_heavy_parallel = "realistic_explanation_parallel"
)
for (source_sweep in names(vaeac_maps)) {
  add_source("extra_parallel_vaeac", source_sweep, vaeac_maps[[source_sweep]])
}

candidates <- data.table::rbindlist(candidate_rows, use.names = TRUE, fill = TRUE)
candidates[, is_warmup := NULL]
for (field in c("approach_args", "pair_key", "pair_role")) {
  candidates[, (field) := data.table::fifelse(is.na(get(field)), "", as.character(get(field)))]
}

copy_sidecar <- function(source, destination) {
  if (file.exists(source)) {
    dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
    if (!file.copy(source, destination, overwrite = TRUE, copy.mode = TRUE)) {
      stop("Failed to copy ", source)
    }
  }
}

studies <- names(core_sweeps)
all_maps <- list()
for (study in studies) {
  destination_dir <- file.path(destination_results, study)
  final_grid <- data.table::fread(file.path(destination_dir, "grid.csv"))
  for (field in c("approach_args", "pair_key", "pair_role")) {
    final_grid[, (field) := data.table::fifelse(is.na(get(field)), "", as.character(get(field)))]
  }
  study_candidates <- candidates[approach == study]

  mapping <- merge(
    final_grid,
    study_candidates[, c(match_cols, "source_study", "source_id"), with = FALSE],
    by = match_cols,
    all.x = TRUE,
    sort = FALSE
  )
  if (nrow(mapping) != nrow(final_grid) || anyDuplicated(mapping$id)) {
    duplicate_ids <- mapping[duplicated(id) | duplicated(id, fromLast = TRUE), unique(id)]
    cat("Non-unique mapping diagnostics for ", study, ":\n", sep = "")
    print(mapping[id %in% duplicate_ids, c("id", "sweep", "rep", "source_study", "source_id"), with = FALSE])
    stop("Mapping is not one-to-one for ", study, ": final=", nrow(final_grid),
      ", merged=", nrow(mapping), ", duplicate_ids=", length(duplicate_ids))
  }
  setorder(mapping, id)
  all_maps[[study]] <- mapping[, .(study, id, sweep, rep, source_study, source_id)]

  for (i in which(!is.na(mapping$source_id))) {
    target_id <- mapping$id[i]
    old_id <- mapping$source_id[i]
    old_study <- mapping$source_study[i]
    source_dir <- file.path(source_results, old_study)
    source_json <- file.path(source_dir, paste0(old_id, ".json"))
    if (!file.exists(source_json)) next

    result <- jsonlite::fromJSON(source_json, simplifyVector = FALSE)
    final_row <- as.list(final_grid[id == target_id])
    result$id <- target_id
    result$study <- study
    for (field in setdiff(names(final_row), c("id", "pair_key"))) {
      value <- final_row[[field]]
      result[[field]] <- if (length(value) == 0 || is.na(value)) NULL else value
    }
    result$is_warmup <- NULL
    result$source_study <- old_study
    result$source_id <- old_id

    target_json <- file.path(destination_dir, paste0(target_id, ".json"))
    jsonlite::write_json(result, target_json, auto_unbox = TRUE, pretty = TRUE, null = "null")
    for (suffix in c(".mem.json", ".time.json")) {
      copy_sidecar(
        file.path(source_dir, paste0(old_id, suffix)),
        file.path(destination_dir, paste0(target_id, suffix))
      )
    }
    source_shapley <- file.path(source_dir, paste0(old_id, ".shapley.rds"))
    if (file.exists(source_shapley)) {
      target_shapley <- file.path(destination_dir, paste0(target_id, ".shapley.rds"))
      copy_sidecar(source_shapley, target_shapley)
      result$shapley_file <- basename(target_shapley)
      jsonlite::write_json(result, target_json, auto_unbox = TRUE, pretty = TRUE, null = "null")
    }
    copy_sidecar(
      file.path(source_logs, old_study, paste0(old_id, ".log")),
      file.path(destination_logs, study, paste0(target_id, ".log"))
    )
  }

  copied <- length(list.files(destination_dir, pattern = "^[0-9]+[.]json$"))
  cat(sprintf("%-22s mapped=%4d copied=%4d planned=%4d missing=%4d\n",
    study, sum(!is.na(mapping$source_id)), copied, nrow(final_grid), nrow(final_grid) - copied))
}

data.table::fwrite(
  data.table::rbindlist(all_maps, use.names = TRUE, fill = TRUE),
  file.path(destination_results, "merge_map.csv")
)
