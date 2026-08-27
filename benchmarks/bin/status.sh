#!/usr/bin/env bash
# ============================================================================
# status.sh — one-shot progress snapshot for the per-approach benchmark suite.
#
#   benchmarks/bin/status.sh            # snapshot of every study
#   watch -n 30 benchmarks/bin/status.sh   # live-updating dashboard
#
# For each study (results/<approach>/) it reports how many runs are done out of
# the planned total (from run_meta.json), the status breakdown (ok / timeout /
# error / skipped), the median wall time and max RAM seen so far (from the
# incrementally-updated summary.csv), and when the most recent run finished.
# ============================================================================
set -uo pipefail
shopt -s nullglob

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
RESULTS="$ROOT/results"

[[ -d "$RESULTS" ]] || { echo "No results directory yet ($RESULTS)."; exit 0; }

printf "%-22s %10s %5s %5s %5s %5s %5s %8s\n" \
  APPROACH DONE/TOTAL OK TMO KILL ERR SKIP LAST
printf '%.0s-' {1..70}; echo

grand_done=0; grand_total=0
for d in "$RESULTS"/*/; do
  name="$(basename "$d")"
  meta="$d/run_meta.json"
  [[ -f "$meta" ]] || continue
  # Only show current studies (those with a matching config); this hides stale
  # output dirs from superseded studies.
  [[ -f "$ROOT/config/$name.yml" ]] || continue

  total="$(grep -oE '"n_runs"[[:space:]]*:[[:space:]]*[0-9]+' "$meta" | grep -oE '[0-9]+$' | head -1)"
  total="${total:-0}"

  # Result files for this study: <id>.json only (exclude the <id>.time.json and
  # <id>.mem.json sidecars, which also start with a digit). nullglob is set, so
  # an empty study yields an empty array.
  files=()
  for f in "$d"[0-9]*.json; do
    [[ "$f" == *.time.json || "$f" == *.mem.json ]] && continue
    files+=( "$f" )
  done
  done="${#files[@]}"
  ok=0; tmo=0; killed=0; err=0; skip=0; last=""
  if [[ "$done" -gt 0 ]]; then
    # /dev/null guards grep from reading stdin if the list is short.
    ok="$(grep -l '"status": "ok"'                   "${files[@]}" /dev/null | wc -l | tr -d ' ')"
    tmo="$(grep -l '"status": "timeout"'             "${files[@]}" /dev/null | wc -l | tr -d ' ')"
    killed="$(grep -l '"status": "killed_resource"' "${files[@]}" /dev/null | wc -l | tr -d ' ')"
    err="$(grep -l '"status": "error"'               "${files[@]}" /dev/null | wc -l | tr -d ' ')"
    skip="$(grep -l '"status": "skipped_'             "${files[@]}" /dev/null | wc -l | tr -d ' ')"
    newest="$(ls -t "${files[@]}" 2>/dev/null | head -1)"
    [[ -n "$newest" ]] && last="$(date -r "$newest" '+%m-%d %H:%M')"
  fi

  grand_done=$((grand_done + done))
  grand_total=$((grand_total + total))

  printf "%-22s %5d/%-4d %5s %5s %5s %5s %5s %8s\n" \
    "$name" "$done" "$total" "$ok" "$tmo" "$killed" "$err" "$skip" "$last"
done

printf '%.0s-' {1..70}; echo
pct=0
[[ "$grand_total" -gt 0 ]] && pct=$((100 * grand_done / grand_total))
printf "%-22s %5d/%-4d  (%d%%)\n" "TOTAL" "$grand_done" "$grand_total" "$pct"

# Show whether an orchestrate/run_week process is currently active.
if pgrep -af "run_one.R|orchestrate.sh|run_week.sh" >/dev/null 2>&1; then
  echo
  echo "Active:"
  pgrep -af "run_one.R" | sed 's/.*--config \(\S*\).*--id \([0-9]*\).*/  running \1 id \2/' | head -3
fi
