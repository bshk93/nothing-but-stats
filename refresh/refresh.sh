#!/bin/bash
set -e
set -o pipefail

# error handling
trap 'echo "Error occurred at $(date). Exiting!" | tee -a "$LOG_FILE"; exit 1' ERR
trap 'echo "Script exited at $(date)" | tee -a "$LOG_FILE"' EXIT

REFRESH_DIR="$HOME/nothing-but-stats/refresh"
PREPROCESS_SCRIPT="$HOME/nothing-but-stats/refresh/job.R"
SERVICE_NAME="shiny-release.service" 
LOG_FILE="/var/log/refresh.log"

# redirect stdout and stderr to log file
exec > >(tee -a "$LOG_FILE") 2>&1

# flag parsing
while [[ "$#" -gt 0 ]]; do
  case "$1" in
    --season)
      SEASON="$2"
      shift 2
      ;;
    --playoff-date)
      PLAYOFF_DATE="$2"
      shift 2
      ;;
    --drop-date)
      DROP_DATE="$2"
      shift 2
      ;;
    --skip-achievements)
      SKIP_ACHIEVEMENTS="$2"
      shift 2
      ;;
    *)
      echo "Unknown option: $1"
      exit 1
      ;;
  esac
done

echo "Starting update process at $(date)"
echo "Season: $SEASON, Playoff Date: $PLAYOFF_DATE, Drop Date: $DROP_DATE"

cd "$REFRESH_DIR" || { echo "Failed to navigate to $REFRESH_DIR"; exit 1; }

echo "Pulling latest changes from remote..."
git pull

echo "Running preprocessing script..."
Rscript "$PREPROCESS_SCRIPT" "$SEASON" "$PLAYOFF_DATE" "$DROP_DATE" "$SKIP_ACHIEVEMENTS"

# copy files from files/ to /var/www/stats.nbn.today/files/
find "$HOME/nothing-but-stats/files" -type f -exec cp {} /var/www/stats.nbn.today/files/ \;

# check for git status updates
if [[ -n $(git status --porcelain) ]]; then
  git add ..
  git commit -m "Automatic update from script"
  git push
else
  echo "No changes to commit."
fi

# pull from /srv/shiny/nothing-but-stats
cd /srv/shiny/nothing-but-stats
git pull

# restart shiny app service
echo "Restarting the Shiny app service..."
sudo systemctl restart "$SERVICE_NAME"
systemctl is-active "$SERVICE_NAME" || { echo "Service $SERVICE_NAME failed to restart."; exit 1; }

echo "Update completed successfully at $(date)"