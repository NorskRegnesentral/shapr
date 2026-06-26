# config.R — load and merge benchmark YAML configuration files.
#
# A study config (e.g. config/oat_quick.yml) is deep-merged on top of
# config/common.yml, with the study file taking precedence. The result is a
# plain list used by grid.R, run_one.R and aggregate.R.

# Recursively merge `override` into `base` (override wins on conflicts).
deep_merge <- function(base, override) {
  for (key in names(override)) {
    if (is.list(base[[key]]) && is.list(override[[key]]) &&
      !is.null(names(override[[key]]))) {
      base[[key]] <- deep_merge(base[[key]], override[[key]])
    } else {
      base[[key]] <- override[[key]]
    }
  }
  return(base)
}

# Recursively convert the strings "Inf"/"-Inf" (as written in YAML) to numeric.
convert_inf <- function(x) {
  if (is.list(x)) {
    return(lapply(x, convert_inf))
  }
  if (is.character(x)) {
    x[x == "Inf"] <- Inf
    x[x == "-Inf"] <- -Inf
    if (all(x %in% c("Inf", "-Inf") | !is.na(suppressWarnings(as.numeric(x))))) {
      num <- suppressWarnings(as.numeric(x))
      if (!any(is.na(num))) {
        return(num)
      }
    }
  }
  return(x)
}

# Absolute path to the benchmarks/ root, regardless of the working directory.
benchmarks_root <- function() {
  # This file lives in benchmarks/R/, so the root is one level up.
  this_file <- tryCatch(
    normalizePath(sys.frame(1)$ofile, mustWork = FALSE),
    error = function(e) NA_character_
  )
  if (is.na(this_file) || !nzchar(this_file)) {
    # Fallback: assume the caller set BENCHMARKS_ROOT or runs from the root.
    root <- Sys.getenv("BENCHMARKS_ROOT", unset = normalizePath("."))
    return(root)
  }
  return(dirname(dirname(this_file)))
}

# Load common.yml + a study config, deep-merge, and add derived fields.
load_config <- function(config_path) {
  config_path <- normalizePath(config_path, mustWork = TRUE)
  common_path <- file.path(dirname(config_path), "common.yml")

  common <- yaml::read_yaml(common_path)
  study <- yaml::read_yaml(config_path)
  cfg <- deep_merge(common, study)
  cfg <- convert_inf(cfg)

  cfg$study <- tools::file_path_sans_ext(basename(config_path))
  cfg$config_path <- config_path
  cfg$root <- Sys.getenv("BENCHMARKS_ROOT", unset = dirname(dirname(config_path)))

  # Resolve output paths to absolute, study-scoped directories.
  cfg$dir <- list(
    data = file.path(cfg$root, cfg$paths$data_dir),
    results = file.path(cfg$root, cfg$paths$results_dir, cfg$study),
    logs = file.path(cfg$root, cfg$paths$logs_dir, cfg$study)
  )
  return(cfg)
}

# The ordered set of dimension columns that fully describe a single run.
grid_dimensions <- function() {
  c(
    "dataset", "approach", "n_features", "max_n_coalitions", "n_MC_samples",
    "min_n_batches", "max_batch_size", "workers", "backend", "dt_threads",
    "n_train", "n_explain", "iterative", "group"
  )
}

# Bookkeeping columns carried alongside the dimensions in grid.csv.
grid_bookkeeping <- function() {
  c("sweep", "approach_args", "pair_key", "pair_role", "coalitions_from")
}
