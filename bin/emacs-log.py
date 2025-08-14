#!/usr/bin/env python3
"""
A robust script to initialize an Emacs configuration, handle interactive
prompts during startup, and log the entire process.

This script is designed for Unix-like systems (Linux, macOS) due to its
use of the `pty` module.
"""

import os
import pty
import select
import subprocess
import sys
import time
from datetime import datetime
from pathlib import Path
from typing import Dict, List

# --- Configuration ---
# Set the base directory for Emacs configuration.
EMACS_DIR: Path = Path.home() / ".config" / "emacs"
# Define the directory where logs will be stored.
LOG_DIR: Path = EMACS_DIR / "log"
# Name of the Org Mode configuration file to be tangled.
ORG_CONFIG_FILE: str = "Emacs.org"


def main() -> None:
    """Main function to orchestrate the Emacs setup and launch."""
    # Ensure we are running on a compatible OS.
    if sys.platform == "win32":
        print(
            "Error: This script requires a Unix-like OS (Linux, macOS) due to the 'pty' module.",
            file=sys.stderr,
        )
        sys.exit(1)

    # Ensure the script runs from the Emacs configuration directory.
    try:
        os.chdir(EMACS_DIR)
    except FileNotFoundError:
        print(
            f"Error: Emacs directory not found at '{EMACS_DIR}'. Aborting.",
            file=sys.stderr,
        )
        sys.exit(1)

    # 1. Create the log directory.
    LOG_DIR.mkdir(parents=True, exist_ok=True)

    # 2. Generate a timestamped log file name.
    timestamp = datetime.now().strftime("%Y-%m-%d_%I-%M%p")
    log_file = LOG_DIR / f"emacs-run_{timestamp}.log"

    print_intro(log_file)

    # --- Execution ---

    # 3. Kill any lingering Emacs processes for a clean start.
    print("--> Killing any lingering Emacs processes...")
    subprocess.run(
        ["killall", "emacs"], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL
    )
    time.sleep(1)  # Give processes a moment to terminate.

    # 4. Tangle the Org configuration file into init.el.
    print(f"--> Tangling '{ORG_CONFIG_FILE}' into init.el...")
    try:
        subprocess.run(
            [
                "emacs",
                "--batch",
                "--eval",
                "(require 'org)",
                "--eval",
                f'(org-babel-tangle-file "{ORG_CONFIG_FILE}")',
            ],
            check=True,  # Raise an exception if tangling fails
            capture_output=True,
            text=True,
        )
    except FileNotFoundError:
        print(
            "Error: 'emacs' command not found. Is Emacs installed and in your PATH?",
            file=sys.stderr,
        )
        sys.exit(1)
    except subprocess.CalledProcessError as e:
        print(f"Error: Failed to tangle '{ORG_CONFIG_FILE}'.", file=sys.stderr)
        print(f"--- Emacs Output ---\n{e.stderr}", file=sys.stderr)
        sys.exit(1)

    # 5. Start the Emacs daemon and handle prompts interactively.
    print("--> Starting Emacs daemon and monitoring for prompts...")

    # Define the prompts and the desired responses.
    interactions = {
        "Check for compatible module binary to download? (y or n)": "n",
        "ZMQ module not found. Build it? (y or n)": "y",
    }

    # Run the command with pty to handle interaction.
    success = run_emacs_with_pty(
        command=["emacs", "--daemon"], log_file_path=log_file, interactions=interactions
    )

    if not success:
        print(
            "\nWarning: Not all interactive prompts were handled. Check the log for details."
        )

    print_outro(log_file)


def print_intro(log_file: Path) -> None:
    """Prints the introductory information for the user."""
    print("--- Emacs First Run & Logging Script (Python Version) ---")
    print("\nThis script will:")
    print("  1. Kill any existing Emacs instances.")
    print(f"  2. Tangle your '{ORG_CONFIG_FILE}' to produce init.el.")
    print("  3. Start the Emacs daemon and answer startup prompts automatically.")
    print("  4. Capture all output to a log file.")
    print(f"\nLogging to:\n  {log_file}")
    print(f'\nMonitor progress in another terminal:\n  tail -f "{log_file}"\n')


def print_outro(log_file: Path) -> None:
    """Prints the concluding messages for the user."""
    print("\n------------------------------------------------")
    print("Emacs daemon has been started in the background.")
    print(f"\nThe full output has been saved to:\n  {log_file}")
    print("\nYou can now review the log using commands like:")
    print(f'  less "{log_file}"')
    print(f'  cat "{log_file}"')
    print("------------------------------------------------")


def run_emacs_with_pty(
    command: List[str], log_file_path: Path, interactions: Dict[str, str]
) -> bool:
    """
    Executes a command in a pseudoterminal to automate interactive prompts.

    This function exits after all prompts are handled or after a timeout,
    leaving the daemon process running in the background.

    Args:
        command: The command and its arguments to execute.
        log_file_path: The path to the log file.
        interactions: A mapping of prompt strings to response strings.

    Returns:
        True if all prompts were successfully handled, False otherwise.
    """
    pending_interactions = interactions.copy()
    master_fd, slave_fd = pty.openpty()

    try:
        with open(log_file_path, "wb") as log_file:
            process = subprocess.Popen(
                command,
                stdin=slave_fd,
                stdout=slave_fd,
                stderr=slave_fd,
            )
            os.close(slave_fd)  # Close child's end in the parent

            output_buffer = b""

            # Loop only as long as there are prompts to handle
            while process.poll() is None and pending_interactions:
                # Use select for non-blocking read with a timeout
                ready, _, _ = select.select(
                    [master_fd], [], [], 5.0
                )  # 5-second timeout

                if not ready:
                    # Timeout reached, assume no more prompts are coming
                    print(
                        "  - Timed out waiting for prompts. Assuming startup is complete."
                    )
                    break

                try:
                    output = os.read(master_fd, 1024)
                    if not output:  # EOF
                        break

                    log_file.write(output)
                    log_file.flush()
                    output_buffer += output

                    # Use a list to safely modify while iterating
                    handled_prompts = []
                    for prompt, response in pending_interactions.items():
                        if prompt.encode() in output_buffer:
                            print(f"  - Detected: '{prompt}'")
                            print(f"  - Sending:  '{response}'")
                            os.write(master_fd, (response + "\n").encode())

                            handled_prompts.append(prompt)
                            output_buffer = b""  # Clear buffer after response
                            break  # Handle one prompt per read cycle

                    for prompt in handled_prompts:
                        del pending_interactions[prompt]

                except OSError:
                    # This occurs if the child process closes its end of the pty
                    break
    finally:
        os.close(master_fd)  # Ensure the master descriptor is always closed

    return not pending_interactions


if __name__ == "__main__":
    main()
