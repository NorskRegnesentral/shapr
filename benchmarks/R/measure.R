# measure.R — helpers to measure a single explain() call and assemble a result
# record. Process-tree peak RAM is captured by the external sampler (sampler.R);
# here we capture wall/CPU time, in-process gc peak, and shapr's own timing
# breakdown.

# Flatten shapr's $timing into a named list of phase -> seconds.
flatten_timing <- function(expl) {
  out <- list()
  timing <- tryCatch(expl$timing, error = function(e) NULL)
  if (is.null(timing)) {
    return(out)
  }
  out$total_time_secs <- tryCatch(timing$summary$total_time_secs, error = function(e) NA_real_)
  overall <- tryCatch(timing$overall_timing_secs, error = function(e) NULL)
  if (!is.null(overall)) {
    for (nm in names(overall)) {
      out[[paste0("phase_", nm)]] <- as.numeric(overall[[nm]])
    }
  }
  return(out)
}

# Number of coalitions actually used (best effort; iterative may differ from
# the requested max_n_coalitions).
used_n_coalitions <- function(expl) {
  tryCatch(
    {
      il <- expl$internal$iter_list
      last <- il[[length(il)]]
      as.integer(last$n_coalitions)
    },
    error = function(e) NA_integer_
  )
}

# gc() peak since the last reset, in bytes (parent process only).
gc_peak_bytes <- function() {
  g <- gc(reset = FALSE)
  # Columns "max used" are in cells; multiply by 8 (Vcells) and node size.
  ncells_max <- g["Ncells", "max used"]
  vcells_max <- g["Vcells", "max used"]
  # Ncells ~ 56 bytes, Vcells = 8 bytes on 64-bit R.
  bytes <- ncells_max * 56 + vcells_max * 8
  return(as.numeric(bytes))
}

# Static metadata recorded with every run.
run_metadata <- function() {
  git_sha <- tryCatch(
    trimws(system2("git", c("rev-parse", "--short", "HEAD"), stdout = TRUE, stderr = FALSE)),
    error = function(e) NA_character_
  )
  list(
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    hostname = Sys.info()[["nodename"]],
    r_version = as.character(getRversion()),
    shapr_version = as.character(utils::packageVersion("shapr")),
    git_sha = if (length(git_sha) == 1) git_sha else NA_character_,
    pid = Sys.getpid()
  )
}
