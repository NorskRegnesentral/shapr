#!/usr/bin/env Rscript
# aggregate.R — combine per-run JSON results (+ *.mem.json from the sampler)
# into results/<study>/results.csv and a small summary.csv (median/IQR per
# configuration over replicates, warm-up runs excluded).
#
# Usage: Rscript R/aggregate.R --config config/oat_quick.yml

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

# Flatten one result list into a single-row data.table (nested $timing expanded
# to phase_* columns).
flatten_result <- function(r) {
  timing <- r$timing
  r$timing <- NULL
  flat <- r[!vapply(r, is.list, logical(1))]
  dt <- as.data.table(lapply(flat, function(x) if (is.null(x)) NA else x))
  if (!is.null(timing)) {
    for (nm in names(timing)) {
      dt[[nm]] <- if (is.null(timing[[nm]])) NA_real_ else as.numeric(timing[[nm]])
    }
  }
  return(dt)
}

main <- function() {
  a <- parse_args()
  cfg <- load_config(a$config)
  rdir <- cfg$dir$results

  files <- list.files(rdir, pattern = "^[0-9]+\\.json$", full.names = TRUE)
  if (length(files) == 0) {
    cat("No result files found in", rdir, "\n")
    return(invisible())
  }

  rows <- lapply(files, function(f) {
    r <- jsonlite::fromJSON(f, simplifyVector = TRUE)
    dt <- flatten_result(r)
    # Merge sampler memory output, if present.
    mem_file <- sub("\\.json$", ".mem.json", f)
    if (file.exists(mem_file)) {
      m <- jsonlite::fromJSON(mem_file, simplifyVector = TRUE)
      set(dt, j = "peak_rss_tree_bytes", value = as.numeric(m$peak_rss_tree_bytes %||% NA))
      set(dt, j = "peak_cgroup_bytes", value = as.numeric(m$peak_cgroup_bytes %||% NA))
    }
    dt
  })
  results <- rbindlist(rows, use.names = TRUE, fill = TRUE)
  setorder(results, id)

  # Headline peak-RAM column: prefer cgroup, fall back to the poll/tree number.
  results[, peak_ram_bytes := fifelse(
    !is.na(peak_cgroup_bytes) & peak_cgroup_bytes > 0,
    peak_cgroup_bytes, peak_rss_tree_bytes
  )]
  results[, peak_ram_mb := round(peak_ram_bytes / 1024^2, 1)]

  out_csv <- file.path(rdir, "results.csv")
  fwrite(results, out_csv)

  # Summary over measured replicates only.
  ok <- results[status == "ok" & is_warmup == FALSE]
  summary <- NULL
  if (nrow(ok) > 0) {
    by_cols <- intersect(c(grid_dimensions(), "sweep"), names(ok))
    summary <- ok[, .(
      n = .N,
      wall_median = round(median(wall_secs), 3),
      wall_iqr = round(IQR(wall_secs), 3),
      cpu_user_median = round(median(cpu_user_secs), 3),
      ram_mb_median = round(median(peak_ram_mb, na.rm = TRUE), 1),
      ram_mb_max = round(max(peak_ram_mb, na.rm = TRUE), 1)
    ), by = by_cols]
    setorder(summary, -wall_median)
    fwrite(summary, file.path(rdir, "summary.csv"))
  }

  n_ok <- sum(results$status == "ok")
  n_err <- sum(results$status == "error")
  n_skip <- sum(results$status == "skipped_missing_dep")
  cat(sprintf("Aggregated %d runs (ok=%d, error=%d, skipped=%d)\n",
    nrow(results), n_ok, n_err, n_skip))
  cat("Wrote:", out_csv, "\n")
  if (!is.null(summary)) {
    cat("Wrote:", file.path(rdir, "summary.csv"), "\n\n")
    cat("Slowest configurations (median wall seconds):\n")
    print(utils::head(summary[, .(approach, dataset, sweep, n_train, n_MC_samples,
      workers, wall_median, ram_mb_median)], 15))
  }
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || is.na(x)) y else x

main()
