#!/usr/bin/env Rscript
# run_one.R — execute ONE benchmark configuration in a fresh R process and write
# results/<study>/<id>.json. One run = one explain() call.
#
# Timing model: wall time measured directly around `explain()` is the reporting
# metric. The orchestrator also records whole-Rscript wall time at the bash
# level, while this script records cached data/model load time and shapr's own
# internal timing breakdown. These diagnostic timings help decompose harness
# overhead. Model FITTING is done up front by prebuild.R and is therefore
# excluded from the measured process (run_one only reads the cached model).
#
# Usage: Rscript R/run_one.R --config config/oat_quick.yml --id 42
#        [--max-n-coalitions N]   (override; used for iterative `dependent` runs)

suppressMessages({
  library(data.table)
})

local({
  here <- dirname(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))
  if (is.na(here) || !nzchar(here)) here <- "R"
  source(file.path(here, "config.R"))
  source(file.path(here, "capability.R"))
  source(file.path(here, "registry.R"))
  source(file.path(here, "data.R"))
  source(file.path(here, "measure.R"))
})

parse_args <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  get <- function(flag) {
    i <- which(args == flag)
    if (length(i) == 0) NA_character_ else args[i + 1]
  }
  list(
    config = get("--config"),
    id = as.integer(get("--id")),
    max_n_coalitions = suppressWarnings(as.integer(get("--max-n-coalitions")))
  )
}

# Configure threading so the ONLY parallelism is the future workers + the swept
# data.table thread count.
setup_threads <- function(dt_threads, backend, workers) {
  data.table::setDTthreads(dt_threads)
  if (workers > 1) {
    plan <- switch(backend,
      multisession = future::multisession,
      multicore = future::multicore,
      future::multisession
    )
    future::plan(plan, workers = workers)
  } else {
    future::plan("sequential")
  }
}

# Decode the CSV-safe "key=value;key=value" approach-args string into a typed
# named list (numeric / logical / character as appropriate).
parse_approach_args <- function(s) {
  if (is.null(s) || is.na(s) || !nzchar(s)) {
    return(list())
  }
  parts <- strsplit(s, ";", fixed = TRUE)[[1]]
  out <- list()
  for (p in parts) {
    kv <- strsplit(p, "=", fixed = TRUE)[[1]]
    val <- kv[2]
    if (toupper(val) %in% c("TRUE", "FALSE")) {
      val <- as.logical(val)
    } else {
      num <- suppressWarnings(as.numeric(val))
      if (!is.na(num)) val <- num
    }
    out[[kv[1]]] <- val
  }
  return(out)
}

# Build an intentionally more expensive version of shapr's native prediction
# function. Optional prediction-cost studies use this to vary model-evaluation
# cost while returning exactly the same predictions.
make_repeated_predict_model <- function(model, repeats) {
  native_predict <- getFromNamespace("get_predict_model", "shapr")(
    predict_model = NULL,
    model = model
  )
  force(native_predict)
  force(repeats)
  return(function(model, newdata) {
    prediction <- NULL
    for (i in seq_len(repeats)) {
      prediction <- native_predict(model, newdata)
    }
    return(prediction)
  })
}

# Build the explain() argument list for a grid row. `coalitions_override` (if
# > 0) replaces max_n_coalitions (used for the iterative `dependent` run).
build_explain_args <- function(cfg, row, run_data, model, coalitions_override = NA_integer_) {
  approach_args <- parse_approach_args(row$approach_args)

  prediction_repeats <- approach_args[["benchmark.prediction_repeats"]] %||% 1
  approach_args[["benchmark.prediction_repeats"]] <- NULL
  if (!is.numeric(prediction_repeats) || length(prediction_repeats) != 1 ||
    prediction_repeats < 1 || prediction_repeats != as.integer(prediction_repeats)) {
    stop("benchmark.prediction_repeats must be a positive integer")
  }
  prediction_repeats <- as.integer(prediction_repeats)

  # Named regression variant -> merge its (complex) explain args from registry.
  variant_args <- list()
  if (!is.null(approach_args$variant)) {
    v <- get_variant(approach_args$variant)
    if (is.null(v)) stop(sprintf("Unknown variant '%s'", approach_args$variant))
    variant_args <- v$args
    approach_args$variant <- NULL
  }

  # Resolve max_n_coalitions: command-line override wins (dependent pair run),
  # otherwise use the grid value.
  max_nc <- row$max_n_coalitions
  if (!is.na(coalitions_override) && coalitions_override > 0) {
    max_nc <- coalitions_override
  }
  if (is.na(max_nc) || max_nc < 0) {
    stop("max_n_coalitions is the dependent-pair sentinel (-1) but no valid ",
      "--max-n-coalitions override was supplied")
  }

  # Batching controls. `max_batch_cube_size` (default 1e6 in shapr) caps
  # `max_batch_size` in high dimensions; set it to Inf in a config to disable
  # the cap and control the batch count precisely via min_n_batches /
  # max_batch_size. Older grids without the column fall back to the shapr
  # default by omitting it here.
  eca <- list(
    min_n_batches = row$min_n_batches,
    max_batch_size = row$max_batch_size
  )
  if (!is.null(row$max_batch_cube_size) && !is.na(row$max_batch_cube_size)) {
    eca$max_batch_cube_size <- row$max_batch_cube_size
  }

  base_args <- list(
    model = model,
    x_explain = run_data$x_explain,
    x_train = run_data$x_train,
    approach = row$approach,
    phi0 = mean(run_data$y_train),
    max_n_coalitions = max_nc,
    n_MC_samples = row$n_MC_samples,
    iterative = as.logical(row$iterative),
    extra_computation_args = eca,
    verbose = NULL,
    seed = cfg$seed + row$id
  )
  if (prediction_repeats > 1L) {
    base_args$predict_model <- make_repeated_predict_model(model, prediction_repeats)
  }

  # Feature grouping (group sweep): partition features into groups of
  # `group_size` consecutive columns. group_size is a swept grid dimension;
  # fall back to the config-level / default value for older grids.
  if (isTRUE(as.logical(row$group))) {
    gsize <- if (!is.null(row$group_size) && !is.na(row$group_size)) {
      as.integer(row$group_size)
    } else {
      cfg$group_size %||% 2L
    }
    base_args$group <- build_groups(colnames(run_data$x_train), group_size = gsize)
  }

  return(c(base_args, variant_args, approach_args))
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x

main <- function() {
  a <- parse_args()
  cfg <- load_config(a$config)
  grid <- fread(file.path(cfg$dir$results, "grid.csv"))
  row <- grid[id == a$id]
  if (nrow(row) != 1) stop("No unique grid row for id ", a$id)
  row <- as.list(row)

  out_path <- file.path(cfg$dir$results, paste0(a$id, ".json"))

  result <- c(
    list(id = a$id, study = cfg$study),
    row[c("sweep", "rep", grid_dimensions(), "approach_args",
      "pair_role", "coalitions_from")],
    list(coalitions_override = if (is.na(a$max_n_coalitions)) NA_integer_ else a$max_n_coalitions),
    run_metadata()
  )

  # Skip approaches / variants whose extra dependencies are unavailable.
  variant_name <- parse_approach_args(row$approach_args)$variant
  variant_deps <- if (!is.null(variant_name)) (get_variant(variant_name)$deps %||% character(0)) else character(0)
  miss <- missing_dependency(row$approach, variant_deps)
  if (!is.na(miss)) {
    result$status <- "skipped_missing_dep"
    result$error <- paste0("missing: ", miss)
    jsonlite::write_json(result, out_path, auto_unbox = TRUE, pretty = TRUE)
    cat(sprintf("[id %d] skipped (%s missing)\n", a$id, miss))
    return(invisible())
  }

  setup_threads(row$dt_threads, row$backend, row$workers)

  res <- tryCatch(
    {
      # Load pre-processed data + cached model (timed separately).
      load0 <- Sys.time()
      run_data <- build_run_data(cfg, row$dataset, row$n_features, row$n_train, row$n_explain)
      model <- get_model(cfg, row$dataset, run_data$x_train, run_data$y_train, row$model_variant)
      load_secs <- as.numeric(difftime(Sys.time(), load0, units = "secs"))

      explain_args <- build_explain_args(cfg, row, run_data, model, a$max_n_coalitions)

      # Reset gc peak just before the measured section.
      invisible(gc(reset = TRUE))

      wall0 <- Sys.time()
      cpu0 <- proc.time()
      expl <- do.call(shapr::explain, explain_args)
      cpu1 <- proc.time()
      wall1 <- Sys.time()

      list(expl = expl, wall0 = wall0, wall1 = wall1, cpu0 = cpu0, cpu1 = cpu1,
        load_secs = load_secs)
    },
    error = function(e) e
  )

  if (inherits(res, "error")) {
    result$status <- "error"
    result$error <- conditionMessage(res)
  } else {
    cpu <- res$cpu1 - res$cpu0
    result$status <- "ok"
    result$data_load_secs <- res$load_secs
    result$wall_secs <- as.numeric(difftime(res$wall1, res$wall0, units = "secs"))
    result$cpu_user_secs <- as.numeric(cpu[["user.self"]])
    result$cpu_sys_secs <- as.numeric(cpu[["sys.self"]])
    result$cpu_user_child_secs <- as.numeric(cpu[["user.child"]])
    result$cpu_sys_child_secs <- as.numeric(cpu[["sys.child"]])
    result$gc_peak_bytes <- gc_peak_bytes()
    result$used_n_coalitions <- used_n_coalitions(res$expl)
    result$n_iterations <- used_n_iterations(res$expl)
    nb_vec <- iter_n_batches(res$expl)
    result$used_n_batches <- if (all(is.na(nb_vec))) NA_integer_ else as.integer(nb_vec[length(nb_vec)])
    result$used_n_batches_max <- if (all(is.na(nb_vec))) NA_integer_ else as.integer(max(nb_vec, na.rm = TRUE))
    result$effective_max_batch_size <- effective_max_batch_size(res$expl)
    result$timing <- flatten_timing(res$expl)
    if (isTRUE(as.logical(row$save_explanations))) {
      shapley_path <- file.path(cfg$dir$results, paste0(a$id, ".shapley.rds"))
      saveRDS(res$expl$shapley_values_est, shapley_path, compress = FALSE)
      result$shapley_file <- basename(shapley_path)
    }
  }

  jsonlite::write_json(result, out_path, auto_unbox = TRUE, pretty = TRUE, null = "null")
  cat(sprintf("[id %d] %s  approach=%s dataset=%s  %s\n",
    a$id, result$status, row$approach, row$dataset,
    if (!is.null(result$wall_secs)) sprintf("%.2fs", result$wall_secs) else ""))
}

main()
