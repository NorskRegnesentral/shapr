#!/usr/bin/env bash
# ============================================================================
# orchestrate.sh — run a full benchmark study, one isolated R process per config.
#
#   benchmarks/bin/orchestrate.sh config/oat_quick.yml [--retry-timeouts]
#
# Steps:
#   1. Build the run grid (R/grid.R)   -> results/<study>/grid.csv + run_meta.json
#   2. Pre-build all data pools + models (R/prebuild.R) so model FITTING is
#      excluded from the timed runs.
#   3. For each run id (dependency-aware order, resumable): launch a FRESH
#      Rscript run_one.R under a wall-clock `timeout`, with the RAM sampler
#      attached. The WHOLE-Rscript wall time is measured at the bash level and
#      written to <id>.time.json (this is the headline timing number). Then cool
#      down.
#   4. Aggregate everything (R/aggregate.R) -> results/<study>/results.csv
#
# Each config runs in its own process so there is no cross-run caching, no warm
# heap, and no reused future workers. BLAS/OpenMP are pinned to 1 thread so the
# ONLY parallelism is the swept future worker count + the swept data.table
# thread count.
#
# Timeout handling: a run exceeding `timeout_sec` (from run_meta.json) is killed
# and gets an <id>.json marker with status="timeout" so resume skips it. Use
# `--retry-timeouts` to delete previous timeout markers (and any dependents that
# were skipped because their source timed out) and re-attempt them.
#
# Iterative pairs: a `dependent` run reuses the coalition budget the matching
# `source` run actually consumed (read from the source's <id>.json) via
# `--max-n-coalitions`. If the source is missing or never recorded a usable
# coalition budget (e.g. it timed out or errored), the dependent is marked
# status="skipped_missing_dep" instead of being run with the invalid sentinel.
# ============================================================================
set -uo pipefail

# --- Single-threaded BLAS/OpenMP/data.table (see README) --------------------
export OMP_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1
export MKL_NUM_THREADS=1
export R_DATATABLE_NUM_THREADS=1

# --- Locate paths -----------------------------------------------------------
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
export BENCHMARKS_ROOT="$ROOT"
RDIR="$ROOT/R"
RSCRIPT="${RSCRIPT:-Rscript}"

RETRY_TIMEOUTS=false
CONFIG=""
for arg in "$@"; do
  case "$arg" in
    --retry-timeouts) RETRY_TIMEOUTS=true ;;
    *) CONFIG="$arg" ;;
  esac
done

if [[ -z "$CONFIG" ]]; then
  echo "Usage: $0 <config.yml> [--retry-timeouts]" >&2
  exit 1
fi

[[ -f "$CONFIG" ]] || CONFIG="$ROOT/$CONFIG"
[[ -f "$CONFIG" ]] || { echo "Config not found: $1" >&2; exit 1; }

STUDY="$(basename "${CONFIG%.*}")"
RESULTS="$ROOT/results/$STUDY"
LOGS="$ROOT/logs/$STUDY"
mkdir -p "$RESULTS" "$LOGS"

# --- 1. Build the grid ------------------------------------------------------
"$RSCRIPT" "$RDIR/grid.R" --config "$CONFIG" || { echo "grid build failed" >&2; exit 1; }

GRID="$RESULTS/grid.csv"
META="$RESULTS/run_meta.json"
read_meta() { "$RSCRIPT" -e "cat(jsonlite::fromJSON('$META')[['$1']])"; }

RAM_METHOD="$(read_meta ram_method)"
POLL_MS="$(read_meta poll_interval_ms)"
COOLDOWN="$(read_meta cooldown_sec)"
TIMEOUT="$(read_meta timeout_sec)"
AGG_EVERY="$(read_meta aggregate_every)"
RUN_ORDER="$(read_meta run_order)"
[[ -n "$TIMEOUT" && "$TIMEOUT" != "0" ]] || TIMEOUT=600
[[ "$AGG_EVERY" =~ ^[0-9]+$ ]] || AGG_EVERY=0

# cgroup measurement needs systemd-run --user; fall back to poll if missing.
if [[ "$RAM_METHOD" != "poll" ]] && ! command -v systemd-run >/dev/null 2>&1; then
  echo "systemd-run not available; falling back to RAM method 'poll'." >&2
  RAM_METHOD="poll"
fi

echo "Study '$STUDY': RAM='$RAM_METHOD', poll=${POLL_MS}ms, cooldown=${COOLDOWN}s, timeout=${TIMEOUT}s, aggregate_every=${AGG_EVERY}"
echo "Run order has $(wc -w <<<"$RUN_ORDER") runs."

# --- Optionally clear timeout / cascade markers so they get retried ----------
# Besides genuine timeouts we also clear dependents that were skipped because
# their source timed out, and legacy sentinel `error` markers from the same
# cause, so that retrying a source lets its dependent re-run too.
if [[ "$RETRY_TIMEOUTS" == "true" ]]; then
  for j in "$RESULTS"/*.json; do
    [[ -f "$j" ]] || continue
    if grep -q '"status"[: ]*"timeout"' "$j"; then
      echo "Retrying previously timed-out run: $(basename "$j")"
      rm -f "$j"
    elif grep -q '"status"[: ]*"skipped_missing_dep"' "$j"; then
      echo "Retrying previously skipped dependent: $(basename "$j")"
      rm -f "$j"
    elif grep -q 'dependent-pair sentinel' "$j"; then
      echo "Retrying legacy sentinel-cascade error: $(basename "$j")"
      rm -f "$j"
    fi
  done
fi

# --- 2. Pre-build data pools + models (excluded from timed runs) ------------
"$RSCRIPT" "$RDIR/prebuild.R" --config "$CONFIG" || { echo "prebuild failed" >&2; exit 1; }

# --- Grid lookup helpers (awk over grid.csv) --------------------------------
# Columns: 1=id 2=sweep 3=rep 4=is_warmup 5=dataset 6=approach ...
# last=coalitions_from, last-1=pair_role.
grid_field() { # grid_field <id> <awk-field-expr>
  awk -F, -v id="$1" 'NR>1 && $1==id {print '"$2"'; exit}' "$GRID"
}
grid_pair_role()       { grid_field "$1" '$(NF-1)'; }
grid_coalitions_from() { grid_field "$1" '$NF'; }
grid_approach()        { grid_field "$1" '$6'; }
grid_dataset()         { grid_field "$1" '$5'; }

# --- Run one id -------------------------------------------------------------
run_id() {
  local id="$1"
  local out="$RESULTS/$id.json"
  local mem="$RESULTS/$id.mem.json"
  local tjson="$RESULTS/$id.time.json"
  local log="$LOGS/$id.log"

  if [[ -f "$out" ]]; then
    echo "[id $id] already done, skipping."
    return 0
  fi

  # Iterative dependent: reuse the source run's used coalition budget. If the
  # source result is missing or never recorded a usable coalition budget (it
  # timed out / errored), the dependent cannot run with a meaningful budget, so
  # mark it skipped rather than letting run_one error on the sentinel value.
  local extra_args=()
  local role cfrom
  role="$(grid_pair_role "$id")"
  cfrom="$(grid_coalitions_from "$id")"
  if [[ "$role" == "dependent" && -n "$cfrom" && "$cfrom" != "NA" ]]; then
    local src_json="$RESULTS/$cfrom.json"
    local mnc=""
    if [[ -f "$src_json" ]]; then
      mnc="$(sed -n 's/.*"used_n_coalitions"[: ]*\([0-9][0-9]*\).*/\1/p' "$src_json" | head -1)"
    fi
    if [[ -n "$mnc" ]]; then
      extra_args+=(--max-n-coalitions "$mnc")
    else
      local appr ds reason
      appr="$(grid_approach "$id")"
      ds="$(grid_dataset "$id")"
      if [[ -f "$src_json" ]]; then
        reason="source $cfrom has no used_n_coalitions (it timed out or errored)"
      else
        reason="source result $cfrom.json is missing"
      fi
      cat >"$out" <<EOF
{
  "id": $id,
  "study": "$STUDY",
  "approach": "$appr",
  "dataset": "$ds",
  "status": "skipped_missing_dep",
  "message": "$reason"
}
EOF
      echo "[id $id] SKIPPED: $reason"
      return 0
    fi
  fi

  local start end elapsed rc timed_out=false
  start="$(date +%s.%N)"

  if [[ "$RAM_METHOD" == "poll" ]]; then
    timeout --signal=TERM "$TIMEOUT" \
      "$RSCRIPT" "$RDIR/run_one.R" --config "$CONFIG" --id "$id" "${extra_args[@]}" \
      >"$log" 2>&1 &
    local rpid=$!
    "$RSCRIPT" "$RDIR/sampler.R" --pid "$rpid" --out "$mem" --interval-ms "$POLL_MS" \
      >>"$log" 2>&1 &
    local spid=$!
    wait "$rpid"; rc=$?
    wait "$spid" 2>/dev/null || true
  else
    local unit="shaprbench-${STUDY}-${id}-$$"
    systemd-run --user --scope --quiet --unit="$unit" \
      -- timeout --signal=TERM "$TIMEOUT" \
      "$RSCRIPT" "$RDIR/run_one.R" --config "$CONFIG" --id "$id" "${extra_args[@]}" \
      >"$log" 2>&1 &
    local rpid=$!
    "$RSCRIPT" "$RDIR/sampler.R" --unit "$unit.scope" --out "$mem" --interval-ms "$POLL_MS" \
      >>"$log" 2>&1 &
    local spid=$!
    wait "$rpid"; rc=$?
    wait "$spid" 2>/dev/null || true
  fi

  end="$(date +%s.%N)"
  elapsed="$(awk "BEGIN{printf \"%.3f\", $end - $start}")"
  if [[ "$rc" -eq 124 || "$rc" -eq 137 ]]; then
    timed_out=true
  fi

  # Bash-level timing sidecar (headline wall time).
  cat >"$tjson" <<EOF
{
  "id": $id,
  "bash_wall_secs": $elapsed,
  "exit_code": $rc,
  "timed_out": $timed_out
}
EOF

  # On timeout, run_one never wrote its result -> drop a marker so resume skips
  # it and aggregate has a row.
  if [[ "$timed_out" == "true" && ! -f "$out" ]]; then
    local appr ds
    appr="$(grid_approach "$id")"
    ds="$(grid_dataset "$id")"
    cat >"$out" <<EOF
{
  "id": $id,
  "study": "$STUDY",
  "approach": "$appr",
  "dataset": "$ds",
  "status": "timeout"
}
EOF
    echo "[id $id] TIMEOUT after ${elapsed}s (limit ${TIMEOUT}s)"
  else
    tail -n 1 "$log"
  fi
}

# --- 3. Execute runs --------------------------------------------------------
completed=0
for id in $RUN_ORDER; do
  run_id "$id"
  completed=$((completed + 1))

  # Periodic re-aggregation so results.csv / summary.csv stay current during a
  # long study (aggregate_every = 0 disables this).
  if [[ "$AGG_EVERY" -gt 0 && $((completed % AGG_EVERY)) -eq 0 ]]; then
    echo "[checkpoint] re-aggregating after $completed runs..."
    "$RSCRIPT" "$RDIR/aggregate.R" --config "$CONFIG" >/dev/null 2>&1 \
      || echo "[checkpoint] aggregate failed (continuing)."
  fi

  if [[ -n "$COOLDOWN" && "$COOLDOWN" != "0" ]]; then
    sleep "$COOLDOWN"
  fi
done

# --- 4. Aggregate -----------------------------------------------------------
"$RSCRIPT" "$RDIR/aggregate.R" --config "$CONFIG"
echo "Done. Results in $RESULTS/results.csv"
