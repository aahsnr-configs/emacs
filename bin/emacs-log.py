#!/usr/bin/env python3
"""
A robust script to initialize an Emacs configuration, handle interactive
prompts during startup, and log the entire process.

This script is designed for Unix-like systems (Linux, macOS) and is
compatible with Python 3.13.
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
# Time in seconds to wait for new output from Emacs before looping.
# This handles pauses during package installation.
IO_INACTIVITY_TIMEOUT: float = 15.0
# Maximum total time in seconds to monitor the startup process.
# This is a safety net to prevent the script from hanging indefinitely.
TOTAL_STARTUP_TIMEOUT: float = 180.0


def main() -> None:
    """Main function to orchestrate the Emacs setup and launch."""
    if sys.platform == "win32":
        print(
            "Error: This script requires a Unix-like OS (Linux, macOS) due to the 'pty' module.",
            file=sys.stderr,
        )
        sys.exit(1)

    try:
        os.chdir(EMACS_DIR)
    except FileNotFoundError:
        print(
            f"Error: Emacs directory not found at '{EMACS_DIR}'. Aborting.",
            file=sys.stderr,
        )
        sys.exit(1)

    LOG_DIR.mkdir(parents=True, exist_ok=True)
    timestamp = datetime.now().strftime("%Y-%m-%d_%I-%M%p")
    log_file = LOG_DIR / f"emacs-run_{timestamp}.log"

    print_intro(log_file)

    print("--> Killing any lingering Emacs processes...")
    subprocess.run(
        ["killall", "emacs"], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL
    )
    time.sleep(1)

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
            check=True,
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

    print("--> Starting Emacs daemon and monitoring for prompts...")
    interactions = {
        "Check for compatible module binary to download? (y or n)": "n",
        "ZMQ module not found. Build it? (y or n)": "y",
    }

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
    It uses a dual-timeout strategy to robustly handle the startup process.
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
            os.close(slave_fd)

            start_time = time.monotonic()
            output_buffer = b""

            while time.monotonic() - start_time < TOTAL_STARTUP_TIMEOUT:
                if not pending_interactions or process.poll() is not None:
                    break  # Exit if all prompts handled or process died

                # Wait for I/O with the inactivity timeout
                ready, _, _ = select.select([master_fd], [], [], IO_INACTIVITY_TIMEOUT)
                if not ready:
                    continue  # Inactivity detected, loop again to check total timeout

                try:
                    output = os.read(master_fd, 1024)
                    if not output:
                        break  # EOF

                    log_file.write(output)
                    log_file.flush()
                    output_buffer += output

                    handled_prompts = []
                    for prompt, response in pending_interactions.items():
                        if prompt.encode() in output_buffer:
                            print(f"  - Detected: '{prompt}'")
                            print(f"  - Sending:  '{response}'")
                            os.write(master_fd, (response + "\n").encode())

                            handled_prompts.append(prompt)
                            output_buffer = b""
                            break

                    for prompt in handled_prompts:
                        del pending_interactions[prompt]

                except OSError:
                    break  # pty was closed by the child process

            # Report the final status of the monitoring phase
            if not pending_interactions:
                print("  - All prompts handled successfully. Monitoring stopped.")
            elif process.poll() is not None:
                print("  - Emacs process terminated unexpectedly. Monitoring stopped.")
            else:  # Total timeout was reached
                print(
                    f"  - Reached total startup timeout of {TOTAL_STARTUP_TIMEOUT}s. Assuming startup complete."
                )

    finally:
        os.close(master_fd)

    return not pending_interactions


if __name__ == "__main__":
    main()
