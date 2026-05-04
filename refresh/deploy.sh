#!/bin/bash
set -e
set -o pipefail

trap 'echo "Error occurred at $(date). Exiting!" | tee -a "$LOG_FILE"; exit 1' ERR
trap 'echo "Script exited at $(date)" | tee -a "$LOG_FILE"' EXIT

REFRESH_DIR="$HOME/nothing-but-stats/refresh"
SERVICE_NAME="shiny-release.service"
LOG_FILE="/var/log/refresh.log"

exec > >(tee -a "$LOG_FILE") 2>&1

echo "Starting deploy at $(date)"

cd "$REFRESH_DIR" || { echo "Failed to navigate to $REFRESH_DIR"; exit 1; }

echo "Checking for uncommitted changes..."
if [[ -n $(git status --porcelain) ]]; then
  git add ..
  git commit -m "Deploy code changes"
  git push
else
  echo "No changes to commit."
fi

echo "Pulling latest changes into prod..."
cd /srv/shiny/nothing-but-stats
git pull

echo "Restarting the Shiny app service..."
sudo systemctl restart "$SERVICE_NAME"
systemctl is-active "$SERVICE_NAME" || { echo "Service $SERVICE_NAME failed to restart."; exit 1; }

echo "Deploy completed successfully at $(date)"
