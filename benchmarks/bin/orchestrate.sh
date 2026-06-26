#!/usr/bin/env bash
# ============================================================================
# orchestrate.sh — run a full benchmark study, one isolated R process per config.
#
#   benchmarks/bin/orchestrate.sh config/oat_quick.yml
#
# Steps:
#   1. Build the run grid (R/grid.R) -> results/<study>/grid.csv + run_meta.json
#   2. For each run id (in randomised order, resumable): launch a FRESH Rscript
#      run_one.R, with the RAM sampler attached, then cool down.
#   3. Aggregate everything (R/aggregate.R) -> results/<study>/results.csv
#
# Each config runs in its own process so there is no cross-run caching, no
# warm heap, and no reused future workers. BLAS/OpenMP are pinned to 1 thread
# so the ONLY parallelism is the swept future worker count.
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

if [[ $# -lt 1 ]]; then
  echo "Usage: $0 <config.yml>   (e.g. config/oat_quick.yml)" >&2
  exit 1
fi

CONFIG="$1"
[[ -f "$CONFIG" ]] || CONFIG="$ROOT/$CONFIG"
[[ -f "$CONFIG" ]] || { echo "Config not found: $1" >&2; exit 1; }

STUDY="$(basename "${CONFIG%.*}")"
RESULTS="$ROOT/results/$STUDY"
LOGS="$ROOT/logs/$STUDY"
mkdir -p "$RESULTS" "$LOGS"

# --- 1. Build the grid ------------------------------------------------------
"$RSCRIPT" "$RDIR/grid.R" --config "$CONFIG" || { echo "grid build failed" >&2; exit 1; }

META="$RESULTS/run_meta.json"
read_meta() { "$RSCRIPT" -e "cat(jsonlite::fromJSON('$META')[['$1']])"; }

RAM_METHOD="$(read_meta ram_method)"
POLL_MS="$(read_meta poll_interval_ms)"
COOLDOWN="$(read_meta cooldown_sec)"
RUN_ORDER="$(read_meta run_order)"

# cgroup measurement needs systemd-run --user; fall back to poll if missing.
if [[ "$RAM_METHOD" != "poll" ]] && ! command -v systemd-run >/dev/null 2>&1; then
  echo "systemd-run not available; falling back to RAM method 'poll'." >&2
  RAM_METHOD="poll"
fi

echo "Study '$STUDY': RAM method='$RAM_METHOD', poll=${POLL_MS}ms, cooldown=${COOLDOWN}s"
echo "Run order has $(wc -w <<<"$RUN_ORDER") runs."

run_id() {
  local id="$1"
  local out="$RESULTS/$id.json"
  local mem="$RESULTS/$id.mem.json"
  local log="$LOGS/$id.log"

  if [[ -f "$out" ]]; then
    echo "[id $id] already done, skipping."
    return 0
  fi

  if [[ "$RAM_METHOD" == "poll" ]]; then
    "$RSCRIPT" "$RDIR/run_one.R" --config "$CONFIG" --id "$id" >"$log" 2>&1 &
    local rpid=$!
    "$RSCRIPT" "$RDIR/sampler.R" --pid "$rpid" --out "$mem" --interval-ms "$POLL_MS" \
      >>"$log" 2>&1 &
    local spid=$!
    wait "$rpid"
    wait "$spid" 2>/dev/null || true
  else
    local unit="shaprbench-${STUDY}-${id}-$$"
    systemd-run --user --scope --quiet --unit="$unit" \
      -- "$RSCRIPT" "$RDIR/run_one.R" --config "$CONFIG" --id "$id" >"$log" 2>&1 &
    local rpid=$!
    "$RSCRIPT" "$RDIR/sampler.R" --unit "$unit.scope" --out "$mem" --interval-ms "$POLL_MS" \
      >>"$log" 2>&1 &
    local spid=$!
    wait "$rpid"
    wait "$spid" 2>/dev/null || true
  fi

  tail -n 1 "$log"
}

# --- 2. Execute runs --------------------------------------------------------
for id in $RUN_ORDER; do
  run_id "$id"
  # Cooldown between runs (helps avoid thermal drift on this box).
  if [[ -n "$COOLDOWN" ]] && [[ "$COOLDOWN" != "0" ]]; then
    sleep "$COOLDOWN"
  fi
done

# --- 3. Aggregate -----------------------------------------------------------
"$RSCRIPT" "$RDIR/aggregate.R" --config "$CONFIG"
echo "Done. Results in $RESULTS/results.csv"
