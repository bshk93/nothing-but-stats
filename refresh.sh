#!/bin/bash
set -e
set -o pipefail

# Trap errors and exit gracefully
trap 'echo "Error occurred at $(date). Exiting!" | tee -a "$LOG_FILE"; exit 1' ERR
trap 'echo "Script exited at $(date)" | tee -a "$LOG_FILE"' EXIT

if [ "$#" -ne 3 ]; then
  echo "Provide exactly three arguments: the season YYYY-YY, the playoff start date YYYY-MM-DD, and the drop after date YYYY-MM-DD."
  exit 1
fi

# Validate argument formats
if ! [[ "$1" =~ ^[0-9]{4}-[0-9]{2}$ && "$2" =~ ^[0-9]{4}-[0-9]{2}-[0-9]{2}$ && "$3" =~ ^[0-9]{4}-[0-9]{2}-[0-9]{2}$ ]]; then
  echo "Invalid argument format. Expected: YYYY-YY for season, YYYY-MM-DD for dates."
  exit 1
fi

# Define paths
APP_DIR="/srv/shiny/nothing-but-stats/app"
PREPROCESS_SCRIPT="/srv/shiny/nothing-but-stats/refresh-job.R"
SERVICE_NAME="shiny-release.service" 
LOG_FILE="/var/log/refresh.log"

# Redirect all stdout and stderr to the log file
exec > >(tee -a "$LOG_FILE") 2>&1

echo "Starting update process at $(date)"

# Navigate to the app directory
cd "$APP_DIR" || { echo "Failed to navigate to $APP_DIR"; exit 1; }

# Pull the latest changes
echo "Pulling latest changes from remote..."
git pull

# Run the preprocessing script
echo "Running preprocessing script..."
Rscript "$PREPROCESS_SCRIPT" "$1" "$2" "$3"

# Restart the Shiny app service
echo "Restarting the Shiny app service..."
sudo systemctl restart "$SERVICE_NAME"
systemctl is-active "$SERVICE_NAME" || { echo "Service $SERVICE_NAME failed to restart."; exit 1; }

echo "Update completed successfully at $(date)"
