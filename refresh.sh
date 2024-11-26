#!/bin/bash
set -e
set -o pipefail

# Trap errors and exit gracefully
trap 'echo "Error occurred at $(date). Exiting!" | tee -a "$LOG_FILE"; exit 1' ERR
trap 'echo "Script exited at $(date)" | tee -a "$LOG_FILE"' EXIT

# Define paths
# APP_DIR="/srv/shiny/nothing-but-stats/app"
# PREPROCESS_SCRIPT="/srv/shiny/nothing-but-stats/refresh-job.R"
APP_DIR="$HOME/nothing-but-stats/app"
PREPROCESS_SCRIPT="$HOME/nothing-but-stats/refresh-job.R"
SERVICE_NAME="shiny-release.service" 
LOG_FILE="/var/log/refresh.log"

# Redirect all stdout and stderr to the log file
exec > >(tee -a "$LOG_FILE") 2>&1

# Parse flags
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
    *)
      echo "Unknown option: $1"
      exit 1
      ;;
  esac
done

echo "Starting update process at $(date)"
echo "Season: $SEASON, Playoff Date: $PLAYOFF_DATE, Drop Date: $DROP_DATE"

# Navigate to the app directory
cd "$APP_DIR" || { echo "Failed to navigate to $APP_DIR"; exit 1; }

# Pull the latest changes
echo "Pulling latest changes from remote..."
git pull

# Run the preprocessing script
echo "Running preprocessing script..."
Rscript "$PREPROCESS_SCRIPT" "$SEASON" "$PLAYOFF_DATE" "$DROP_DATE"

# Check for changes before committing
if [[ -n $(git status --porcelain) ]]; then
  git add ..
  git commit -m "Automatic update from script"
  git push
else
  echo "No changes to commit."
fi

# Restart the Shiny app service
echo "Restarting the Shiny app service..."
sudo systemctl restart "$SERVICE_NAME"
systemctl is-active "$SERVICE_NAME" || { echo "Service $SERVICE_NAME failed to restart."; exit 1; }

echo "Update completed successfully at $(date)"
