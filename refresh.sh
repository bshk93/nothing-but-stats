#!/bin/bash
set -e
set -o pipefail

if [ "$#" -ne 3 ]; then
  echo "You must provide exactly three arguments: the season YYYY-YY, the playoff start date YYYY-MM-DD, and the drop after date YYYY-MM-DD."
  exit 1
fi

# Define paths
APP_DIR="/srv/shiny/nothing-but-stats/app"
PREPROCESS_SCRIPT="/srv/shiny/nothing-but-stats/refresh-job.R"
SERVICE_NAME="shiny-release.service" 

# Log file for tracking updates
LOG_FILE="/var/log/refresh.log"

echo "Starting update process at $(date)" | tee -a "$LOG_FILE"

# Step 1: Navigate to the app directory
cd "$APP_DIR" || { echo "Failed to navigate to $APP_DIR" | tee -a "$LOG_FILE"; exit 1; }

## Step 2: Check if we're on the main branch
#CURRENT_BRANCH=$(git rev-parse --abbrev-ref HEAD)
#if [ "$CURRENT_BRANCH" != "main" ]; then
#    echo "Currently on branch $CURRENT_BRANCH. Switching to 'main'..." | tee -a "$LOG_FILE"
#    # Stash changes if any
#    git stash save "Auto-stash before switching branches on $(date)" | tee -a "$LOG_FILE"
#    git checkout main | tee -a "$LOG_FILE"
#fi

## Step 3: Pull the latest changes from main
#echo "Pulling latest changes from main branch..." | tee -a "$LOG_FILE"
#git pull origin main | tee -a "$LOG_FILE"

# Step 4: Run the preprocessing script
echo "Running preprocessing script..." | tee -a "$LOG_FILE"
Rscript "$PREPROCESS_SCRIPT" "$1" "$2" "$3" | tee -a "$LOG_FILE"
if [ $? -ne 0 ]; then
    echo "Preprocessing script failed. Aborting update." | tee -a "$LOG_FILE"
    exit 1
fi

# Step 5: Restart the Shiny app service
echo "Restarting the Shiny app service..." | tee -a "$LOG_FILE"
sudo systemctl restart "$SERVICE_NAME" | tee -a "$LOG_FILE"
if [ $? -ne 0 ]; then
    echo "Failed to restart the service $SERVICE_NAME. Please check the service configuration." | tee -a "$LOG_FILE"
    exit 1
fi

echo "Update completed successfully at $(date)" | tee -a "$LOG_FILE"
