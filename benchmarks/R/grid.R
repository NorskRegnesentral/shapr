#!/usr/bin/env Rscript
# grid.R — expand a study config into a grid of runs (results/<study>/grid.csv)
# and a small run_meta.json that orchestrate.sh reads.
#
# Usage: Rscript R/grid.R --config config/gaussian.yml
#
# Block design (per approach):
#   A study is ONE approach (`approach:`) plus a list of named `blocks`. Each
#   block is a small mini-design (a 1-D sweep, a 2-D/3-D grid, an approach-arg
#   grid, or an iterative pair) that varies a few dimensions around the shared
#   baseline (see common.yml) while everything else is held fixed. Because cost
#   behaviour differs enormously per approach, each approach gets its own config
#   with grids sized to its cost (coarser for the slow approaches).
#
#   An iterative block (`pair: iterative`) emits a dependent PAIR per grid point:
#   a `source` run (iterative = TRUE) and a `dependent` run (iterative = FALSE)
#   whose max_n_coalitions is resolved at run time from the number of coalitions
#   the source actually used, so the two are compared at an equal budget.

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
                     pair_role = NA_character_, replicates = 1L) {
  row <- modifyList(base, overrides)
  dt <- data.table::as.data.table(row[grid_dimensions()])
  dt[, sweep := sweep]
  dt[, approach_args := encode_approach_args(approach_args)]
  dt[, pair_key := pair_key]
  dt[, pair_role := pair_role]
  dt[, coalitions_from := NA_integer_]
  dt[, n_replicates := as.integer(replicates)]
  return(dt)
}

# ---------------------------------------------------------------------------
# Block-based grid builder.
#
# A study is ONE approach (`approach:`) described by a list of `blocks`. Each
# block is a named mini-design that varies a handful of dimensions around the
# baseline while everything else is held at the baseline value:
#
#   blocks:
#     - name: scale_train_mc          # a 2-D grid over standard dimensions
#       grid: {n_train: [500, 5000], n_MC_samples: [50, 250]}
#     - name: empirical_type          # an approach-argument grid
#       approach_args: {empirical.type: [fixed_sigma, independence]}
#     - name: iterative_budget        # iterative-vs-fixed at equal budget
#       pair: iterative
#       grid: {max_n_coalitions: [512]}
#
# `grid:` cross-multiplies standard grid_dimensions() (a single entry = a 1-D
# sweep); `approach_args:` cross-multiplies approach-specific arguments (encoded
# into the approach_args column, incl. the named regression `variant`). If both
# are present they cross-multiply. `pair: iterative` emits, per combination, a
# source (iterative = TRUE) and a dependent (iterative = FALSE, sentinel -1)
# whose coalition budget is resolved from the source at run time.
# ---------------------------------------------------------------------------

# Cross-product of a named list of value-vectors into a list of override lists.
# Empty / NULL yields a single empty override (i.e. pure baseline).
grid_combos <- function(spec) {
  if (is.null(spec) || length(spec) == 0) {
    return(list(list()))
  }
  # unlist() each entry so mixed numeric/Inf sequences (read as a list) collapse
  # to a plain vector while preserving type (numeric / character / logical).
  spec <- lapply(spec, function(v) if (is.list(v)) unlist(v) else v)
  df <- do.call(expand.grid, c(spec, list(stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)))
  lapply(seq_len(nrow(df)), function(i) as.list(df[i, , drop = FALSE]))
}

build_blocks <- function(cfg) {
  if (is.null(cfg$approach)) stop("A block config must set a single `approach:`.")
  if (is.null(cfg$blocks) || length(cfg$blocks) == 0) stop("A block config must define `blocks:`.")

  base <- modifyList(cfg$baseline, list(approach = cfg$approach))
  if (!is.null(cfg$dataset)) base$dataset <- cfg$dataset
  if (!is.null(cfg[["model_variant"]])) base$model_variant <- cfg[["model_variant"]]

  rows <- list()
  add <- function(r) rows[[length(rows) + 1]] <<- r

  for (blk in cfg$blocks) {
    name <- blk$name %||% "block"
    dim_combos <- grid_combos(blk$grid)
    arg_combos <- grid_combos(blk$approach_args)
    is_pair <- identical(blk$pair, "iterative")
    replicates <- blk$replicates %||% cfg$replicates
    if (length(replicates) != 1 || is.na(replicates) || replicates < 1 || replicates %% 1 != 0) {
      stop("Block `", name, "` must use a positive integer `replicates` value.")
    }
    combo_idx <- 0L

    for (ov in dim_combos) {
      for (aa in arg_combos) {
        if (is_pair) {
          combo_idx <- combo_idx + 1L
          pk <- paste0("iter_", cfg$approach, "_", name, "_", combo_idx)
          src_ov <- modifyList(ov, list(iterative = TRUE))
          dep_ov <- modifyList(ov, list(iterative = FALSE, max_n_coalitions = -1L))
          add(make_row(base, src_ov, sweep = name, approach_args = aa,
            pair_key = pk, pair_role = "source", replicates = replicates))
          add(make_row(base, dep_ov, sweep = name, approach_args = aa,
            pair_key = pk, pair_role = "dependent", replicates = replicates))
        } else {
          add(make_row(base, ov, sweep = name, approach_args = aa, replicates = replicates))
        }
      }
    }
  }

  return(rbindlist(rows, use.names = TRUE))
}

# After id assignment, link each iterative `dependent` row to its `source` row
# (matched on pair_key + rep) via the coalitions_from column.
resolve_pairs <- function(grid) {
  dep_keys <- unique(grid[pair_role == "dependent", pair_key])
  for (pk in dep_keys) {
    sub <- grid[pair_key == pk]
    for (rp in unique(sub$rep)) {
      sid <- grid[pair_key == pk & rep == rp & pair_role == "source", id]
      if (length(sid) == 1) {
        grid[pair_key == pk & rep == rp &
          pair_role == "dependent", coalitions_from := sid]
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

  grid <- build_blocks(cfg)

  # Drop approach/dataset combinations that are not supported.
  grid <- grid[mapply(approach_supports, approach, dataset)]

  # De-duplicate identical rows (a baseline point may recur across blocks).
  # Blocks are ordered from the core design to optional extensions, so an
  # overlap retains the core block's replicate count and sweep label.
  key_cols <- c(grid_dimensions(), "approach_args", "pair_key", "pair_role")
  grid <- grid[!duplicated(grid[, ..key_cols])]

  # Expand measured replicates, allowing expensive blocks to override the
  # study-wide default without changing the rest of the experiment.
  grid <- grid[, cbind(.SD, data.table(rep = seq_len(n_replicates))),
    by = seq_len(nrow(grid))][, c("seq_len", "n_replicates") := NULL]

  grid[, id := .I]
  grid <- resolve_pairs(grid)
  setcolorder(grid, c("id", "sweep", "rep", grid_dimensions(),
    "approach_args", "pair_key", "pair_role", "coalitions_from"))

  dir.create(cfg$dir$results, recursive = TRUE, showWarnings = FALSE)
  grid_path <- file.path(cfg$dir$results, "grid.csv")
  fwrite(grid, grid_path)

  run_order <- make_run_order(grid, cfg$seed, cfg$randomize_order)
  meta <- list(
    study = cfg$study,
    approach = cfg$approach,
    n_runs = nrow(grid),
    ram_method = cfg$ram$method,
    poll_interval_ms = cfg$ram$poll_interval_ms,
    cooldown_sec = cfg$cooldown_sec,
    timeout_sec = cfg$timeout_sec %||% 600,
    time_budget_sec = cfg$time_budget_sec %||% 0,
    aggregate_every = cfg$aggregate_every %||% 0,
    run_order = run_order
  )
  jsonlite::write_json(meta, file.path(cfg$dir$results, "run_meta.json"),
    auto_unbox = TRUE, pretty = TRUE)

  cat(sprintf("Study '%s' (approach %s): %d runs -> %s\n",
    cfg$study, cfg$approach, nrow(grid), grid_path))
}

main()
