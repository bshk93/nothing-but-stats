#!/bin/bash
set -e
set -o pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SEASONS_CONF="$SCRIPT_DIR/seasons.conf"
JOB_R="$SCRIPT_DIR/job.R"
SERVICE_NAME="shiny-release.service"
LOG_FILE="/var/log/refresh.log"
NBS_PROJECT_DIR="$(dirname "$SCRIPT_DIR")"

exec > >(tee -a "$LOG_FILE") 2>&1
trap 'echo "Error occurred at $(date). Exiting!" | tee -a "$LOG_FILE"; exit 1' ERR
trap 'echo "Script exited at $(date)" | tee -a "$LOG_FILE"' EXIT

SUBCOMMAND="${1:-}"
if [[ "$#" -gt 0 ]]; then shift; fi

# Parse flags
SEASON=""
PLAYOFFS_FROM=""
THROUGH=""
PLAYOFFS_FROM_EXPLICIT=false

while [[ "$#" -gt 0 ]]; do
  case "$1" in
    --season)        SEASON="$2";        shift 2 ;;
    --playoffs-from) PLAYOFFS_FROM="$2"; PLAYOFFS_FROM_EXPLICIT=true; shift 2 ;;
    --through)       THROUGH="$2";       shift 2 ;;
    *) echo "Unknown option: $1"; exit 1 ;;
  esac
done

# Infer season from today's date if not provided (Sep 30 cutoff, matches job.R logic)
if [[ -z "$SEASON" ]]; then
  current_year=$(date +%Y)
  current_month=$(date +%-m)
  if [[ "$current_month" -le 9 ]]; then
    y1=$(( current_year - 1 ))
    y2=$current_year
  else
    y1=$current_year
    y2=$(( current_year + 1 ))
  fi
  SEASON="${y1: -2}-${y2: -2}"
fi

# Auto-lookup playoffs_from from seasons.conf when not provided explicitly
if [[ "$PLAYOFFS_FROM_EXPLICIT" == false && -f "$SEASONS_CONF" ]]; then
  PLAYOFFS_FROM=$(grep "^${SEASON}=" "$SEASONS_CONF" | cut -d= -f2 || true)
fi

export NBS_DATA_DIR="${NBS_DATA_DIR:-/var/lib/nothing-but-stats}"

do_pull() {
  echo "--- pull: downloading Sheets and validating ---"
  cd "$NBS_PROJECT_DIR"
  git pull
  Rscript "$JOB_R" pull "$SEASON" "$PLAYOFFS_FROM" "$THROUGH"
}

do_build() {
  echo "--- build: computing RDS/CSV outputs ---"
  Rscript "$JOB_R" build "$SEASON" "" ""
  find "$NBS_DATA_DIR" -maxdepth 1 -name "*.csv" -exec cp {} /var/www/stats.nbn.today/files/ \;
}

do_deploy() {
  echo "--- deploy: committing, pushing, restarting service ---"
  cd "$NBS_PROJECT_DIR"
  if [[ -n $(git status --porcelain) ]]; then
    git add .
    git commit -m "Automatic update from script"
    git push
  else
    echo "No changes to commit."
  fi
  cd /srv/shiny/nothing-but-stats
  git pull
  echo "Restarting the Shiny app service..."
  sudo systemctl restart "$SERVICE_NAME"
  systemctl is-active "$SERVICE_NAME" || { echo "Service $SERVICE_NAME failed to restart."; exit 1; }
}

echo "=== nbs $SUBCOMMAND started at $(date) ==="
echo "Season: $SEASON, Playoffs From: ${PLAYOFFS_FROM:-none}, Through: ${THROUGH:-today}"

case "$SUBCOMMAND" in
  pull)
    do_pull
    ;;
  build)
    do_build
    ;;
  deploy)
    do_deploy
    ;;
  refresh)
    cd "$NBS_PROJECT_DIR"
    git pull
    Rscript "$JOB_R" refresh "$SEASON" "$PLAYOFFS_FROM" "$THROUGH"
    find "$NBS_DATA_DIR" -maxdepth 1 -name "*.csv" -exec cp {} /var/www/stats.nbn.today/files/ \;
    do_deploy
    ;;
  *)
    echo "Usage: nbs.sh {pull|build|deploy|refresh} [--season S] [--playoffs-from DATE] [--through DATE]"
    echo ""
    echo "  pull     Download Sheets, validate, write per-season CSVs"
    echo "  build    Load CSVs from disk, compute and write all RDS/derived-CSV outputs"
    echo "  deploy   Commit/push local changes, pull to prod, restart shiny service"
    echo "  refresh  Full pipeline: pull + build + deploy (single R session)"
    echo ""
    echo "  --season         Season string, e.g. 25-26  (default: inferred from today)"
    echo "  --playoffs-from  First date of playoff games, e.g. 2026-04-15  (default: auto-looked up from seasons.conf)"
    echo "  --through        Drop game rows after this date  (default: today, pull only)"
    exit 1
    ;;
esac

echo "=== nbs $SUBCOMMAND completed at $(date) ==="
