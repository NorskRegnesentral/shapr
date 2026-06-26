#!/usr/bin/env Rscript
# grid.R — expand a study config into a grid of runs (results/<study>/grid.csv)
# and a small run_meta.json that orchestrate.sh reads.
#
# Usage: Rscript R/grid.R --config config/oat_quick.yml
#
# OAT design (tiered):
#   * PER-APPROACH sweeps  — repeated for EACH approach in `approaches`, on the
#     approach's primary dataset (or all supported datasets for the `dataset`
#     sweep), because the cost behaviour differs a lot per approach:
#       dataset, max_n_coalitions, n_MC_samples, n_train, n_explain, n_features,
#       iterative (TRUE/FALSE pair), group.
#   * BASELINE-ONLY (infra) sweeps — run once at the gaussian/numeric baseline,
#     because they are approach-agnostic infrastructure levers:
#       min_n_batches, dt_threads, and the workers x backend parallel grid.
#   * APPROACH-SPECIFIC sweeps — scalar approach params (empirical.type,
#     vaeac.*, regression.surrogate_n_comb) and named regression variants.
#
# The iterative sweep emits a dependent PAIR: a `source` run (iterative = TRUE)
# and a `dependent` run (iterative = FALSE) whose max_n_coalitions is resolved
# at run time from the number of coalitions the source actually used.

suppressMessages({
  library(data.table)
})

local({
  here <- dirname(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))
  if (is.na(here) || !nzchar(here)) here <- "R"
  source(file.path(here, "config.R"))
  source(file.path(here, "capability.R"))
})

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

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
# bookkeeping columns. `base` is a baseline named list; `overrides` replaces
# specific dimensions.
make_row <- function(base, overrides = list(), sweep = "baseline",
                     approach_args = list(), pair_key = NA_character_,
                     pair_role = NA_character_) {
  row <- modifyList(base, overrides)
  dt <- data.table::as.data.table(row[grid_dimensions()])
  dt[, sweep := sweep]
  dt[, approach_args := encode_approach_args(approach_args)]
  dt[, pair_key := pair_key]
  dt[, pair_role := pair_role]
  dt[, coalitions_from := NA_integer_]
  return(dt)
}

# Concrete datasets (from config$datasets) that an approach supports.
supported_datasets <- function(appr, cfg) {
  Filter(function(ds) approach_supports(appr, ds), names(cfg$datasets))
}

build_oat <- function(cfg) {
  base <- cfg$baseline
  rows <- list(make_row(base, sweep = "baseline"))
  add <- function(r) rows[[length(rows) + 1]] <<- r

  ps <- cfg$per_approach_sweeps
  mixed_default <- cfg$mixed_default %||% "mixed_fc_fl"
  high_dim <- cfg$high_dim
  iter_cap <- cfg$iterative_cap %||% base$max_n_coalitions

  # ---- Per-approach sweeps -------------------------------------------------
  for (appr in cfg$approaches) {
    pdata <- primary_dataset(appr, mixed_default = mixed_default)
    abase <- modifyList(base, list(approach = appr, dataset = pdata))

    # dataset sweep: the approach across every dataset it supports.
    if (isTRUE(ps$dataset)) {
      for (ds in supported_datasets(appr, cfg)) {
        add(make_row(base, list(approach = appr, dataset = ds), sweep = "dataset"))
      }
    }

    # scalar numeric sweeps held on the approach's primary dataset.
    for (dim in c("max_n_coalitions", "n_MC_samples", "n_train", "n_explain")) {
      for (v in (ps[[dim]] %||% list())) {
        add(make_row(abase, setNames(list(v), dim), sweep = dim))
      }
    }

    # n_features sweep is meaningful only for the numeric family (column
    # subsetting). The high-dim point pairs a large n_features with a
    # restrictive coalition cap.
    if (!is.null(ps$n_features) && dataset_family(pdata) == "numeric") {
      for (v in ps$n_features) {
        ov <- list(n_features = v)
        if (!is.null(high_dim) && v == high_dim$n_features) {
          ov$max_n_coalitions <- high_dim$max_n_coalitions
        }
        add(make_row(abase, ov, sweep = "n_features"))
      }
    }

    # group sweep: same config but with features grouped.
    if (isTRUE(ps$group)) {
      add(make_row(abase, list(group = TRUE), sweep = "group"))
    }

    # iterative PAIR: source (iterative TRUE) + dependent (iterative FALSE with
    # max_n_coalitions resolved from the source at run time, sentinel -1).
    if (isTRUE(ps$iterative)) {
      pk <- paste0("iter_", appr)
      add(make_row(abase, list(iterative = TRUE, max_n_coalitions = iter_cap),
        sweep = "iterative", pair_key = pk, pair_role = "source"))
      add(make_row(abase, list(iterative = FALSE, max_n_coalitions = -1L),
        sweep = "iterative", pair_key = pk, pair_role = "dependent"))
    }
  }

  # ---- Baseline-only (infrastructure) sweeps -------------------------------
  for (dim in names(cfg$infra_sweeps %||% list())) {
    for (v in cfg$infra_sweeps[[dim]]) {
      add(make_row(base, setNames(list(v), dim), sweep = paste0("infra:", dim)))
    }
  }

  # workers x backend parallel grid, combined with batching.
  pg <- cfg$parallel_grid
  if (!is.null(pg)) {
    combos <- expand.grid(workers = pg$workers, backend = pg$backend,
      stringsAsFactors = FALSE)
    for (i in seq_len(nrow(combos))) {
      ov <- list(workers = combos$workers[i], backend = combos$backend[i])
      if (!is.null(pg$min_n_batches)) ov$min_n_batches <- pg$min_n_batches
      add(make_row(base, ov, sweep = "grid:parallel"))
    }
  }

  # ---- Approach-specific scalar parameter sweeps ---------------------------
  for (psw in (cfg$approach_param_sweeps %||% list())) {
    for (v in psw$values) {
      args <- setNames(list(v), psw$param)
      if (!is.null(psw$variant)) args <- c(list(variant = psw$variant), args)
      add(make_row(base, list(approach = psw$approach, dataset = psw$dataset),
        sweep = paste0("param:", psw$param), approach_args = args))
    }
  }

  # ---- Named regression variant sweeps -------------------------------------
  for (rv in (cfg$regression_variant_sweeps %||% list())) {
    for (vn in rv$variants) {
      add(make_row(base, list(approach = rv$approach, dataset = rv$dataset),
        sweep = "variant", approach_args = list(variant = vn)))
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

# After id assignment, link each iterative `dependent` row to its `source` row
# (matched on pair_key + rep + is_warmup) via the coalitions_from column.
resolve_pairs <- function(grid) {
  dep_keys <- unique(grid[pair_role == "dependent", pair_key])
  for (pk in dep_keys) {
    sub <- grid[pair_key == pk]
    for (rp in unique(sub$rep)) {
      for (wu in unique(sub[rep == rp, is_warmup])) {
        sid <- grid[pair_key == pk & rep == rp & is_warmup == wu &
          pair_role == "source", id]
        if (length(sid) == 1) {
          grid[pair_key == pk & rep == rp & is_warmup == wu &
            pair_role == "dependent", coalitions_from := sid]
        }
      }
    }
  }
  return(grid)
}

# Seeded run order in which each iterative `dependent` is placed immediately
# after its `source` (so the source's used-coalition count is available).
make_run_order <- function(grid, seed, randomize) {
  ids <- grid$id
  if (isTRUE(randomize)) {
    set.seed(seed)
    ids <- sample(ids)
  }
  pos <- setNames(seq_along(ids), as.character(ids))
  key <- as.numeric(pos[as.character(grid$id)])
  dep <- which(!is.na(grid$coalitions_from))
  for (i in dep) {
    src_id <- grid$coalitions_from[i]
    key[i] <- pos[as.character(src_id)] + 0.5
  }
  return(grid$id[order(key)])
}

main <- function() {
  a <- parse_args()
  cfg <- load_config(a$config)

  grid <- if (identical(cfg$design, "oat")) build_oat(cfg) else build_factorial(cfg)

  # Drop approach/dataset combinations that are not supported.
  grid <- grid[mapply(approach_supports, approach, dataset)]

  # De-duplicate identical rows (the baseline appears in many sweeps). Pair and
  # variant rows differ on dimensions / approach_args so they are preserved.
  key_cols <- c(grid_dimensions(), "approach_args", "pair_key", "pair_role")
  grid <- grid[!duplicated(grid[, ..key_cols])]

  # Expand replicates (+ optional warm-up rep flagged is_warmup = TRUE).
  reps <- seq_len(cfg$replicates)
  rep_dt <- data.table(rep = reps, is_warmup = FALSE)
  if (isTRUE(cfg$warmup)) {
    rep_dt <- rbind(data.table(rep = 0L, is_warmup = TRUE), rep_dt)
  }
  grid <- grid[, cbind(.SD, rep_dt), by = seq_len(nrow(grid))][, seq_len := NULL]

  grid[, id := .I]
  grid <- resolve_pairs(grid)
  setcolorder(grid, c("id", "sweep", "rep", "is_warmup", grid_dimensions(),
    "approach_args", "pair_key", "pair_role", "coalitions_from"))

  dir.create(cfg$dir$results, recursive = TRUE, showWarnings = FALSE)
  grid_path <- file.path(cfg$dir$results, "grid.csv")
  fwrite(grid, grid_path)

  run_order <- make_run_order(grid, cfg$seed, cfg$randomize_order)
  meta <- list(
    study = cfg$study,
    design = cfg$design,
    scale = cfg$scale,
    n_runs = nrow(grid),
    ram_method = cfg$ram$method,
    poll_interval_ms = cfg$ram$poll_interval_ms,
    cooldown_sec = cfg$cooldown_sec,
    timeout_sec = cfg$timeout_sec %||% 600,
    aggregate_every = cfg$aggregate_every %||% 0,
    run_order = run_order
  )
  jsonlite::write_json(meta, file.path(cfg$dir$results, "run_meta.json"),
    auto_unbox = TRUE, pretty = TRUE)

  cat(sprintf("Study '%s' (%s/%s): %d runs -> %s\n",
    cfg$study, cfg$design, cfg$scale, nrow(grid), grid_path))
}

main()
