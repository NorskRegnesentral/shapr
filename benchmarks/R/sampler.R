#!/usr/bin/env Rscript
# sampler.R — measure peak memory of a benchmark run and write a *.mem.json.
#
# Three modes:
#   --sid <SID>     Poll the summed RSS of every process in a dedicated session.
#                   This catches workers even if they detach from the parent.
#   --pid <PID>     Poll the RSS of the process tree rooted at PID. This is a
#                   fallback for launches that cannot create a new session.
#   --unit <NAME>   The run was launched under `systemd-run --user --scope
#                   --unit=NAME`; read the scope's cgroup-v2 memory.peak (exact)
#                   AND poll the process-tree RSS of the cgroup members.
#
# Output JSON fields (bytes; NA if unavailable):
#   peak_rss_tree_bytes  -> max summed RSS of the process tree (poll method)
#   peak_cgroup_bytes    -> cgroup memory.peak / max(memory.current) (cgroup)
#   method, n_samples

suppressMessages(if (!requireNamespace("ps", quietly = TRUE)) {
  stop("The 'ps' package is required for sampler.R")
})

parse_args <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  get <- function(flag, default = NA) {
    i <- which(args == flag)
    if (length(i) == 0) default else args[i + 1]
  }
  list(
    pid = get("--pid"),
    sid = get("--sid"),
    unit = get("--unit"),
    out = get("--out"),
    interval_ms = as.numeric(get("--interval-ms", 15)),
    max_seconds = as.numeric(get("--max-seconds", 86400))
  )
}

sum_rss <- function(pids) {
  total <- 0
  for (p in pids) {
    rss <- tryCatch(ps::ps_memory_info(ps::ps_handle(as.integer(p)))[["rss"]],
      error = function(e) 0)
    if (length(rss) == 1 && is.finite(rss)) total <- total + rss
  }
  return(total)
}

tree_pids <- function(pid) {
  h <- tryCatch(ps::ps_handle(as.integer(pid)), error = function(e) NULL)
  if (is.null(h)) {
    return(integer(0))
  }
  desc <- tryCatch(ps::ps_children(h, recursive = TRUE), error = function(e) list())
  c(as.integer(pid), vapply(desc, ps::ps_pid, integer(1)))
}

# Read the session id from /proc/<pid>/stat. The command name is parenthesized
# and may itself contain spaces or closing parentheses, so split after the last
# ") " before selecting field 6 (the fourth token in the remaining suffix).
proc_session_id <- function(pid) {
  stat_path <- file.path("/proc", pid, "stat")
  stat <- tryCatch(suppressWarnings(readLines(stat_path, warn = FALSE, n = 1)),
    error = function(e) character(0)
  )
  if (length(stat) == 0) {
    return(NA_integer_)
  }
  suffix <- sub("^.*\\) ", "", stat)
  fields <- strsplit(suffix, " ", fixed = TRUE)[[1]]
  if (length(fields) < 4) {
    return(NA_integer_)
  }
  return(suppressWarnings(as.integer(fields[4])))
}

session_pids <- function(sid) {
  proc_dirs <- Sys.glob("/proc/[0-9]*")
  pids <- suppressWarnings(as.integer(basename(proc_dirs)))
  sessions <- vapply(pids, proc_session_id, integer(1))
  return(pids[!is.na(sessions) & sessions == as.integer(sid)])
}

read_int_file <- function(path) {
  if (!file.exists(path)) return(NA_real_)
  val <- tryCatch(suppressWarnings(as.numeric(readLines(path, warn = FALSE)[1])),
    error = function(e) NA_real_)
  return(val)
}

cgroup_dir_for_unit <- function(unit) {
  cg <- tryCatch(
    system2("systemctl", c("--user", "show", unit, "-p", "ControlGroup", "--value"),
      stdout = TRUE, stderr = FALSE),
    error = function(e) ""
  )
  cg <- trimws(paste(cg, collapse = ""))
  if (!nzchar(cg)) return(NA_character_)
  return(file.path("/sys/fs/cgroup", sub("^/", "", cg)))
}

write_result <- function(out, method, peak_tree, peak_cgroup, n) {
  res <- list(
    method = method,
    peak_rss_tree_bytes = if (is.finite(peak_tree)) peak_tree else NA,
    peak_cgroup_bytes = if (is.finite(peak_cgroup)) peak_cgroup else NA,
    n_samples = n
  )
  jsonlite::write_json(res, out, auto_unbox = TRUE, pretty = TRUE, null = "null")
}

sample_by_pid <- function(a) {
  interval <- a$interval_ms / 1000
  deadline <- Sys.time() + a$max_seconds
  peak <- 0
  n <- 0L
  repeat {
    pids <- tree_pids(a$pid)
    if (length(pids) == 0) break
    peak <- max(peak, sum_rss(pids))
    n <- n + 1L
    if (Sys.time() > deadline) break
    Sys.sleep(interval)
  }
  write_result(a$out, "poll", peak, NA_real_, n)
}

sample_by_sid <- function(a) {
  interval <- a$interval_ms / 1000
  deadline <- Sys.time() + a$max_seconds
  peak <- 0
  n <- 0L
  empty_samples <- 0L
  repeat {
    pids <- session_pids(a$sid)
    if (length(pids) == 0) {
      empty_samples <- empty_samples + 1L
      if (empty_samples >= 2L) break
    } else {
      empty_samples <- 0L
      peak <- max(peak, sum_rss(pids))
      n <- n + 1L
    }
    if (Sys.time() > deadline) break
    Sys.sleep(interval)
  }
  write_result(a$out, "poll_session", peak, NA_real_, n)
}

sample_by_unit <- function(a) {
  interval <- a$interval_ms / 1000
  deadline <- Sys.time() + a$max_seconds

  # Wait (briefly) for the scope cgroup to appear.
  cg <- NA_character_
  wait_until <- Sys.time() + 10
  repeat {
    cg <- cgroup_dir_for_unit(a$unit)
    if (!is.na(cg) && dir.exists(cg)) break
    if (Sys.time() > wait_until) break
    Sys.sleep(0.02)
  }

  peak_tree <- 0
  peak_cg <- 0
  n <- 0L
  if (!is.na(cg) && dir.exists(cg)) {
    repeat {
      procs_path <- file.path(cg, "cgroup.procs")
      pids <- tryCatch(suppressWarnings(readLines(procs_path, warn = FALSE)),
        error = function(e) character(0)
      )
      pids <- pids[nzchar(pids)]
      # cgroup.peak (preferred) else running memory.current.
      mp <- read_int_file(file.path(cg, "memory.peak"))
      mc <- read_int_file(file.path(cg, "memory.current"))
      if (is.finite(mp)) peak_cg <- max(peak_cg, mp)
      if (is.finite(mc)) peak_cg <- max(peak_cg, mc)
      if (length(pids) > 0) peak_tree <- max(peak_tree, sum_rss(pids))
      n <- n + 1L
      if (length(pids) == 0 && n > 1L) break
      if (!dir.exists(cg)) break
      if (Sys.time() > deadline) break
      Sys.sleep(interval)
    }
  }
  write_result(a$out, "cgroup", peak_tree, peak_cg, n)
}

main <- function() {
  a <- parse_args()
  if (is.na(a$out)) stop("Provide --out <path>")
  if (!is.na(a$unit)) {
    sample_by_unit(a)
  } else if (!is.na(a$sid)) {
    sample_by_sid(a)
  } else if (!is.na(a$pid)) {
    sample_by_pid(a)
  } else {
    stop("Provide --sid, --pid, or --unit")
  }
}

main()
