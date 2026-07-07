#!/usr/bin/env bash
# ============================================================================
# run_week.sh — run the full per-approach benchmark suite in sequence.
#
#   benchmarks/bin/run_week.sh [--retry-timeouts] [approach ...]
#
# Each approach is its own study (config/<approach>.yml) and carries its own
# per-run timeout and per-approach wall-clock budget (time_budget_sec, default
# 24 h) from common.yml, enforced by orchestrate.sh. Studies run one after
# another, cheapest approaches first so useful results arrive early and the
# expensive vaeac study runs last.
#
# Everything is RESUMABLE: re-running skips runs that already have a result
# file, and skips whole studies quickly once their budget window is used up.
# Pass --retry-timeouts to re-attempt runs previously killed by the per-run
# timeout (raise `timeout_sec` in common.yml first to give them more time).
#
# Examples:
#   bin/run_week.sh                          # all approaches, default order
#   bin/run_week.sh gaussian empirical       # just these two
#   bin/run_week.sh --retry-timeouts vaeac   # retry vaeac's timed-out runs
# ============================================================================
set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
ORCH="$SCRIPT_DIR/orchestrate.sh"

# Default order: cheapest first, vaeac (by far the most expensive) last.
DEFAULT_ORDER=(
  gaussian
  independence
  copula
  empirical
  timeseries
  categorical
  ctree
  arf
  regression_separate
  regression_surrogate
  vaeac
)

PASSTHRU=()
CONFIGS=()
for arg in "$@"; do
  case "$arg" in
    --*) PASSTHRU+=("$arg") ;;
    *)   CONFIGS+=("$arg") ;;
  esac
done
[[ ${#CONFIGS[@]} -eq 0 ]] && CONFIGS=("${DEFAULT_ORDER[@]}")

overall_start=$(date +%s)
for name in "${CONFIGS[@]}"; do
  cfg="$ROOT/config/$name.yml"
  [[ -f "$cfg" ]] || cfg="$name"    # allow passing an explicit path
  if [[ ! -f "$cfg" ]]; then
    echo "run_week: config not found for '$name' — skipping." >&2
    continue
  fi
  echo
  echo "############################################################"
  echo "# $(date '+%F %T')  starting study: $name"
  echo "############################################################"
  "$ORCH" "$cfg" ${PASSTHRU[@]+"${PASSTHRU[@]}"} \
    || echo "run_week: study '$name' exited non-zero (continuing)."
done

elapsed=$(( $(date +%s) - overall_start ))
printf 'run_week: all requested studies done in %dh%02dm.\n' \
  $((elapsed / 3600)) $(((elapsed % 3600) / 60))
