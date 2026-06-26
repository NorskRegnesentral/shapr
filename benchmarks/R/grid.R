#!/usr/bin/env Rscript
# grid.R — expand a study config into a grid of runs (results/<study>/grid.csv)
# and a small run_meta.json that orchestrate.sh reads.
#
# Usage: Rscript R/grid.R --config config/oat_quick.yml

suppressMessages({
  library(data.table)
})

local({
  here <- dirname(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))
  if (is.na(here) || !nzchar(here)) here <- "R"
  source(file.path(here, "config.R"))
  source(file.path(here, "capability.R"))
})

parse_args <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  config <- args[which(args == "--config") + 1]
  if (length(config) == 0 || is.na(config)) stop("Provide --config <path>")
  return(list(config = config))
}

# Encode approach-specific arguments as a CSV-safe "key=value;key=value" string
# (storing JSON in a CSV column breaks on quote escaping).
encode_approach_args <- function(args) {
  if (length(args) == 0) {
    return("")
  }
  paste(names(args), vapply(args, as.character, character(1)), sep = "=", collapse = ";")
}

# A single grid row as a one-row data.table with all dimension columns plus
# bookkeeping columns. `base` is the baseline named list; `overrides` replaces
# specific dimensions.
make_row <- function(base, overrides = list(), sweep = "baseline",
                     approach_args = list()) {
  row <- modifyList(base, overrides)
  dt <- data.table::as.data.table(row[grid_dimensions()])
  dt[, sweep := sweep]
  dt[, approach_args := encode_approach_args(approach_args)]
  return(dt)
}

build_oat <- function(cfg) {
  base <- cfg$baseline
  rows <- list(make_row(base, sweep = "baseline"))

  # One-factor-at-a-time sweeps.
  for (dim in names(cfg$sweeps)) {
    values <- cfg$sweeps[[dim]]
    if (dim == "approach") {
      # Cross approaches with the chosen datasets, filter by capability.
      for (appr in values) {
        for (ds in cfg$approach_sweep_datasets) {
          if (!approach_supports(appr, ds)) next
          rows[[length(rows) + 1]] <- make_row(
            base, list(approach = appr, dataset = ds), sweep = "approach"
          )
        }
      }
    } else {
      for (v in values) {
        rows[[length(rows) + 1]] <- make_row(base, setNames(list(v), dim), sweep = dim)
      }
    }
  }

  # Targeted 2-D interaction grids.
  for (grid in cfg$grids_2d) {
    name <- grid$name
    dims <- setdiff(names(grid), "name")
    combos <- do.call(expand.grid, c(grid[dims], stringsAsFactors = FALSE))
    for (i in seq_len(nrow(combos))) {
      ov <- as.list(combos[i, dims, drop = FALSE])
      rows[[length(rows) + 1]] <- make_row(base, ov, sweep = paste0("grid:", name))
    }
  }

  # Light approach-specific parameter sweeps.
  for (ps in cfg$approach_param_sweeps) {
    for (v in ps$values) {
      rows[[length(rows) + 1]] <- make_row(
        base, list(approach = ps$approach, dataset = ps$dataset),
        sweep = paste0("param:", ps$param),
        approach_args = setNames(list(v), ps$param)
      )
    }
  }

  return(rbindlist(rows, use.names = TRUE))
}

build_factorial <- function(cfg) {
  base <- modifyList(cfg$baseline, cfg$fixed)
  combos <- do.call(expand.grid, c(cfg$factors, stringsAsFactors = FALSE))
  rows <- list()
  for (i in seq_len(nrow(combos))) {
    ov <- as.list(combos[i, names(cfg$factors), drop = FALSE])
    rows[[length(rows) + 1]] <- make_row(base, ov, sweep = "factorial")
  }
  return(rbindlist(rows, use.names = TRUE))
}

main <- function() {
  a <- parse_args()
  cfg <- load_config(a$config)

  grid <- if (identical(cfg$design, "oat")) build_oat(cfg) else build_factorial(cfg)

  # Drop approach/dataset combinations that are not supported.
  grid <- grid[mapply(approach_supports, approach, dataset)]

  # De-duplicate identical dimension rows (the baseline appears in many sweeps).
  key_cols <- c(grid_dimensions(), "approach_args")
  grid <- grid[!duplicated(grid[, ..key_cols])]

  # Expand replicates (+ optional warm-up rep flagged is_warmup = TRUE).
  reps <- seq_len(cfg$replicates)
  rep_dt <- data.table(rep = reps, is_warmup = FALSE)
  if (isTRUE(cfg$warmup)) {
    rep_dt <- rbind(data.table(rep = 0L, is_warmup = TRUE), rep_dt)
  }
  grid <- grid[, cbind(.SD, rep_dt), by = seq_len(nrow(grid))][, seq_len := NULL]

  grid[, id := .I]
  setcolorder(grid, c("id", "sweep", "rep", "is_warmup", grid_dimensions(), "approach_args"))

  dir.create(cfg$dir$results, recursive = TRUE, showWarnings = FALSE)
  grid_path <- file.path(cfg$dir$results, "grid.csv")
  fwrite(grid, grid_path)

  # Run order (seeded shuffle if requested).
  ids <- grid$id
  if (isTRUE(cfg$randomize_order)) {
    set.seed(cfg$seed)
    ids <- sample(ids)
  }
  meta <- list(
    study = cfg$study,
    design = cfg$design,
    scale = cfg$scale,
    n_runs = nrow(grid),
    ram_method = cfg$ram$method,
    poll_interval_ms = cfg$ram$poll_interval_ms,
    cooldown_sec = cfg$cooldown_sec,
    run_order = ids
  )
  jsonlite::write_json(meta, file.path(cfg$dir$results, "run_meta.json"),
    auto_unbox = TRUE, pretty = TRUE)

  cat(sprintf("Study '%s' (%s/%s): %d runs -> %s\n",
    cfg$study, cfg$design, cfg$scale, nrow(grid), grid_path))
}

main()
