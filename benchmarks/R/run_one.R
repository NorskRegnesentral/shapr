#!/usr/bin/env Rscript
# run_one.R — execute ONE benchmark configuration in a fresh R process and write
# results/<study>/<id>.json. One run = one explain() call with everything else
# (data, model) prepared but excluded from the measured time.
#
# Usage: Rscript R/run_one.R --config config/oat_quick.yml --id 42

suppressMessages({
  library(data.table)
})

local({
  here <- dirname(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))
  if (is.na(here) || !nzchar(here)) here <- "R"
  source(file.path(here, "config.R"))
  source(file.path(here, "capability.R"))
  source(file.path(here, "data.R"))
  source(file.path(here, "measure.R"))
})

parse_args <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  get <- function(flag) {
    i <- which(args == flag)
    if (length(i) == 0) NA_character_ else args[i + 1]
  }
  list(config = get("--config"), id = as.integer(get("--id")))
}

# Configure threading so the ONLY parallelism is the future workers.
setup_threads <- function(cfg, backend, workers) {
  data.table::setDTthreads(cfg$controls$dt_threads)
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

# Build the explain() argument list for a grid row.
build_explain_args <- function(cfg, row, run_data, model) {
  approach_args <- parse_approach_args(row$approach_args)
  base_args <- list(
    model = model,
    x_explain = run_data$x_explain,
    x_train = run_data$x_train,
    approach = row$approach,
    phi0 = mean(run_data$y_train),
    max_n_coalitions = row$max_n_coalitions,
    n_MC_samples = row$n_MC_samples,
    iterative = as.logical(row$iterative),
    extra_computation_args = list(
      min_n_batches = row$min_n_batches,
      max_batch_size = row$max_batch_size
    ),
    verbose = NULL,
    seed = cfg$seed + row$id
  )
  return(c(base_args, approach_args))
}

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
    row[c("sweep", "rep", "is_warmup", grid_dimensions(), "approach_args")],
    run_metadata()
  )

  # Skip approaches whose extra dependencies are unavailable.
  miss <- missing_dependency(row$approach)
  if (!is.na(miss)) {
    result$status <- "skipped_missing_dep"
    result$error <- paste0("missing: ", miss)
    jsonlite::write_json(result, out_path, auto_unbox = TRUE, pretty = TRUE)
    cat(sprintf("[id %d] skipped (%s missing)\n", a$id, miss))
    return(invisible())
  }

  setup_threads(cfg, row$backend, row$workers)

  res <- tryCatch(
    {
      run_data <- build_run_data(cfg, row$dataset, row$n_features, row$n_train, row$n_explain)
      model <- get_model(cfg, row$dataset, run_data$x_train, run_data$y_train)
      explain_args <- build_explain_args(cfg, row, run_data, model)

      # Free intermediates and reset gc peak just before the measured section.
      invisible(gc(reset = TRUE))

      wall0 <- Sys.time()
      cpu0 <- proc.time()
      expl <- do.call(shapr::explain, explain_args)
      cpu1 <- proc.time()
      wall1 <- Sys.time()

      list(expl = expl, wall0 = wall0, wall1 = wall1, cpu0 = cpu0, cpu1 = cpu1)
    },
    error = function(e) e
  )

  if (inherits(res, "error")) {
    result$status <- "error"
    result$error <- conditionMessage(res)
  } else {
    cpu <- res$cpu1 - res$cpu0
    result$status <- "ok"
    result$wall_secs <- as.numeric(difftime(res$wall1, res$wall0, units = "secs"))
    result$cpu_user_secs <- as.numeric(cpu[["user.self"]])
    result$cpu_sys_secs <- as.numeric(cpu[["sys.self"]])
    result$cpu_user_child_secs <- as.numeric(cpu[["user.child"]])
    result$cpu_sys_child_secs <- as.numeric(cpu[["sys.child"]])
    result$gc_peak_bytes <- gc_peak_bytes()
    result$used_n_coalitions <- used_n_coalitions(res$expl)
    result$timing <- flatten_timing(res$expl)
  }

  jsonlite::write_json(result, out_path, auto_unbox = TRUE, pretty = TRUE, null = "null")
  cat(sprintf("[id %d] %s  approach=%s dataset=%s  %s\n",
    a$id, result$status, row$approach, row$dataset,
    if (!is.null(result$wall_secs)) sprintf("%.2fs", result$wall_secs) else ""))
}

main()
