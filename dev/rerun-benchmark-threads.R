args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2L) stop("Usage: Rscript dev/rerun-benchmark-threads.R <state-dir> <prepare|run|verify> [study]")
state_dir <- normalizePath(args[1], mustWork = TRUE)
mode <- args[2]
manifest_path <- file.path(state_dir, "manifest.csv")
original_dir <- file.path(state_dir, "original")
suppressMessages(library(data.table))

verify_study <- function(study, ids) {
  original <- data.table::fread(file.path(original_dir, "results", study, "results.csv"))
  current <- data.table::fread(file.path("benchmarks/results", study, "results.csv"))
  original_grid <- file.path(original_dir, "results", study, "grid.csv")
  current_grid <- file.path("benchmarks/results", study, "grid.csv")
  stopifnot(unname(tools::md5sum(original_grid)) == unname(tools::md5sum(current_grid)))
  stopifnot(setequal(current$id, original$id), nrow(current) == nrow(original))
  selected <- current[id %in% ids]
  stopifnot(all(c(
    "dt_threads_effective_before", "dt_threads_effective_after",
    "peak_cgroup_bytes", "exit_code", "shapr_library_path",
    "openblas_num_threads", "mkl_num_threads"
  ) %in% names(selected)))
  stopifnot(
    nrow(selected) == length(ids), all(selected$status == "ok"),
    all(selected$shapr_library_path == normalizePath(file.path(state_dir, "library/shapr"))),
    all(selected$openblas_num_threads == 1), all(selected$mkl_num_threads == 1),
    all(selected$dt_threads_effective_before == selected$dt_threads),
    all(selected$dt_threads_effective_after == selected$dt_threads),
    all(selected$peak_cgroup_bytes > 0), all(selected$exit_code == 0)
  )
  columns <- names(original)
  stopifnot(all(columns %in% names(current)))
  unchanged <- all.equal(
    original[!id %in% ids, ..columns], current[!id %in% ids, ..columns],
    check.attributes = FALSE
  )
  if (!isTRUE(unchanged)) stop(study, ": unrelated results changed: ", paste(unchanged, collapse = "; "))
  cat("Verified", study, ":", length(ids), "corrected runs; unrelated rows unchanged.\n")
  return(invisible(TRUE))
}

if (mode == "prepare") {
  stopifnot(!file.exists(manifest_path))
  studies <- basename(list.dirs(file.path(original_dir, "results"), recursive = FALSE))
  manifest <- data.table::rbindlist(lapply(studies, function(study) {
    results <- data.table::fread(file.path(original_dir, "results", study, "results.csv"))
    source("benchmarks/R/config.R", local = TRUE)
    columns <- setdiff(c(grid_dimensions(), "approach_args", "pair_key", "pair_role"), "dt_threads")
    targets <- unique(results[dt_threads > 1, ..columns])
    references <- merge(results[dt_threads == 1], targets, by = columns)
    selected <- results[id %in% c(results[dt_threads > 1, id], references$id), .(id, dt_threads)]
    selected[, study := study]
    return(selected)
  }))
  stopifnot(
    nrow(manifest) == 124L, manifest[dt_threads > 1, .N] == 92L,
    manifest[dt_threads == 1, .N] == 32L
  )
  paths <- unlist(lapply(seq_len(nrow(manifest)), function(index) {
    row <- manifest[index]
    c(
      file.path("benchmarks/results", row$study, paste0(row$id, c(".json", ".mem.json", ".time.json"))),
      file.path("benchmarks/logs", row$study, paste0(row$id, ".log"))
    )
  }))
  originals <- file.path(original_dir, sub("^benchmarks/", "", paths))
  stopifnot(
    all(file.exists(paths)), all(file.exists(originals)),
    identical(unname(tools::md5sum(paths)), unname(tools::md5sum(originals)))
  )
  data.table::fwrite(manifest, manifest_path)
  stopifnot(all(file.remove(paths)))
  saveRDS(utils::sessionInfo(), file.path(state_dir, "session_info.rds"))
  cat("Prepared 124 archived runs; original IDs and cached inputs retained.\n")
} else if (mode %in% c("run", "verify")) {
  stopifnot(normalizePath(find.package("shapr")) == normalizePath(file.path(state_dir, "library/shapr")))
  manifest <- data.table::fread(manifest_path)
  studies <- c(
    "gaussian", "independence", "copula", "empirical", "timeseries",
    "categorical", "ctree", "arf", "regression_separate", "regression_surrogate", "vaeac"
  )
  if (length(args) >= 3L) {
    stopifnot(args[3] %in% studies)
    studies <- args[3]
  }
  for (study in studies) {
    selected_study <- study
    ids <- manifest[study == selected_study, id]
    if (mode == "run") {
      stopifnot(system2("systemd-run", c("--user", "--scope", "--quiet", "--", "true")) == 0L)
      status <- system2("bash", c(
        "benchmarks/bin/orchestrate.sh",
        paste0("config/", study, ".yml"), "--existing-grid",
        paste0("--run-ids=", paste(ids, collapse = ","))
      ))
      if (status != 0L) stop("Launcher failed for ", study)
    }
    verify_study(study, ids)
  }
  if (length(args) == 2L) {
    writeLines(format(Sys.time()), file.path(state_dir, "verified-complete.txt"))
  }
} else {
  stop("Unknown mode: ", mode)
}
