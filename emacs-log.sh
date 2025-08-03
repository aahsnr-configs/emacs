#!/bin/bash

# Define the log file name for the Emacs instance run.
LOG_FILE="emacs_first_run_output.log"

echo "--- Emacs First Run Output Recorder ---"
echo ""

# Inform the user about the process.
echo "This script will start Emacs and capture all its initial output,"
echo "which is useful for logging the first-time package installation process."
echo ""
echo "All terminal output from Emacs (stdout and stderr) will be recorded to:"
echo "  $LOG_FILE"
echo ""
echo "Emacs will start as a new process. The script will wait here until you"
echo "manually close the Emacs application."
echo ""
echo "You can monitor the installation progress in another terminal by running:"
echo "  tail -f \"$LOG_FILE\""
echo ""

# Run emacs and redirect all output (stdout and stderr) to the specified log file.
# The script will pause here and wait for the 'emacs' command to complete,
# which happens when the user quits the Emacs application.
# The '--init-directory .' flag is kept to ensure it uses the local config.
emacs --batch --eval "(require 'org)" --eval '(org-babel-tangle-file "config.org")'
killall emacs
killall emacs
emacs --daemon --init-directory . &>"$LOG_FILE"

# This part of the script will only execute after Emacs has been closed.
echo ""
echo "------------------------------------------------"
echo "Emacs has been closed."
echo ""
echo "The full output from the Emacs session has been saved to:"
echo "  $LOG_FILE"
echo ""
echo "You can now review the captured output using commands like:"
echo "  less \"$LOG_FILE\""
echo "  cat \"$LOG_FILE\""
echo "------------------------------------------------"
