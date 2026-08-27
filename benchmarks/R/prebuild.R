#!/usr/bin/env Rscript
# prebuild.R — pre-generate every dataset pool and pre-fit every prediction
# model referenced by a study's grid, so that the timed runs (run_one.R) only
# ever READ cached objects. This keeps model FITTING out of the bash-level
# whole-Rscript timing.
#
# Usage: Rscript R/prebuild.R --config config/oat_quick.yml

suppressMessages({
  library(data.table)
})

local({
  here <- dirname(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))
  if (is.na(here) || !nzchar(here)) here <- "R"
  source(file.path(here, "config.R"))
  source(file.path(here, "capability.R"))
  source(file.path(here, "data.R"))
})

parse_args <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  config <- args[which(args == "--config") + 1]
  if (length(config) == 0 || is.na(config)) stop("Provide --config <path>")
  return(list(config = config))
}

main <- function() {
  a <- parse_args()
  cfg <- load_config(a$config)
  grid <- fread(file.path(cfg$dir$results, "grid.csv"))

  # Unique dataset/model combinations to build. Skip rows whose
  # approach has missing dependencies (they will be skipped at run time anyway,
  # but their data/model is cheap to build so we keep them for completeness).
  combos <- unique(grid[, .(dataset, n_features, n_train, model_variant)])
  cat(sprintf("Prebuilding %d dataset/model combinations...\n", nrow(combos)))

  for (i in seq_len(nrow(combos))) {
    ds <- combos$dataset[i]
    nf <- combos$n_features[i]
    nt <- combos$n_train[i]
    run_data <- build_run_data(cfg, ds, nf, nt, n_explain = 1)
    variant <- combos$model_variant[i]
    invisible(get_model(cfg, ds, run_data$x_train, run_data$y_train, variant))
    cat(sprintf("  [%d/%d] %s model=%s n_features=%s n_train=%s\n",
      i, nrow(combos), ds, variant, nf, nt))
  }
  cat("Prebuild complete.\n")
}

main()
