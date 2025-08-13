#!/bin/bash

# --- Configuration ---
# Set the base directory for Emacs configuration.
EMACS_DIR="$HOME/.config/emacs"
# Define the directory where logs will be stored.
LOG_DIR="$EMACS_DIR/log"

# --- Script Body ---

# Ensure the script is running from the correct base directory.
# This makes all subsequent paths simpler and more reliable.
cd "$EMACS_DIR" || {
  echo "Error: Could not change to $EMACS_DIR. Aborting."
  exit 1
}

# 1. Create the log directory if it doesn't already exist.
# The '-p' flag prevents errors if the directory is already present.
mkdir -p "$LOG_DIR"

# 2. Generate a human-readable, timestamped log file name.
# Format: YYYY-MM-DD_HH-MM(AM/PM).log (e.g., 2023-10-27_05-30PM.log)
TIMESTAMP=$(date +"%Y-%m-%d_%I-%M%p")
LOG_FILE="$LOG_DIR/emacs-run_$TIMESTAMP.log"

# --- User Information ---

echo "--- Emacs First Run & Logging Script ---"
echo ""
echo "This script will:"
echo "  1. Tangle your config.org to produce init.el."
echo "  2. Kill any existing Emacs instances."
echo "  3. Start the Emacs daemon."
echo "  4. Capture all startup and package installation output."
echo ""
echo "Logging to:"
echo "  $LOG_FILE"
echo ""
echo "You can monitor the installation progress in another terminal by running:"
echo "  tail -f \"$LOG_FILE\""
echo ""

# --- Execution ---

# Ensure any previous Emacs instances are terminated to start fresh.
echo "Killing any lingering Emacs processes..."
killall emacs >/dev/null 2>&1
sleep 1 # Give processes a moment to die gracefully.

# Tangle the Org configuration file into a loadable init.el.
# Running this with --batch prevents Emacs from opening a window.
echo "Tangling config.org into init.el..."
emacs --batch --eval "(require 'org)" --eval '(org-babel-tangle-file "Emacs.org")'

# Start the Emacs daemon and redirect all output (stdout and stderr)
# to the timestamped log file. The script will wait here until you manually
# close the Emacs application (or kill the daemon).
echo "Starting Emacs daemon. Output is being logged..."
emacs --daemon &>"$LOG_FILE"

# --- Completion ---

echo ""
echo "------------------------------------------------"
echo "Emacs daemon has been started in the background."
echo ""
echo "The full output from the Emacs session has been saved to:"
echo "  $LOG_FILE"
echo ""
echo "You can now review the captured output using commands like:"
echo "  less \"$LOG_FILE\""
echo "  cat \"$LOG_FILE\""
echo "------------------------------------------------"
