#!/usr/bin/env python3
"""
A robust script to initialize an Emacs configuration, handle interactive
prompts during startup, and log the entire process.

This script includes live diagnostics to help identify the exact text of prompts.
It is compatible with Python 3.13.
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
EMACS_DIR: Path = Path.home() / ".config" / "emacs"
LOG_DIR: Path = EMACS_DIR / "log"
ORG_CONFIG_FILE: str = "Emacs.org"
IO_INACTIVITY_TIMEOUT: float = 15.0
TOTAL_STARTUP_TIMEOUT: float = 180.0


def main() -> None:
    """Main function to orchestrate the Emacs setup and launch."""
    if sys.platform == "win32":
        print("Error: This script requires a Unix-like OS.", file=sys.stderr)
        sys.exit(1)

    try:
        os.chdir(EMACS_DIR)
    except FileNotFoundError:
        print(f"Error: Emacs directory not found at '{EMACS_DIR}'.", file=sys.stderr)
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
    except (FileNotFoundError, subprocess.CalledProcessError) as e:
        handle_subprocess_error(e)
        sys.exit(1)

    print("--> Starting Emacs daemon and monitoring for prompts...")
    # IMPORTANT: Check the diagnostic output from the script and ensure these
    # strings EXACTLY match what Emacs prints.
    interactions = {
        "check for compatible module binary to download? (y or n)": "n",
        "zmq module not found. build it? (y or n)": "y",
    }

    success = run_emacs_with_pty(
        command=["emacs", "--daemon"], log_file_path=log_file, interactions=interactions
    )

    if not success:
        print(
            "\nWarning: Not all interactive prompts were handled before timeout. Please check the log."
        )
        print(
            "         Ensure the prompt text in the script's 'interactions' dictionary is correct."
        )

    print_outro(log_file)


def run_emacs_with_pty(
    command: List[str], log_file_path: Path, interactions: Dict[str, str]
) -> bool:
    """Executes a command in a pseudoterminal to automate interactive prompts."""
    pending_interactions = interactions.copy()
    master_fd, slave_fd = pty.openpty()

    try:
        with open(log_file_path, "wb") as log_file:
            process = subprocess.Popen(
                command, stdin=slave_fd, stdout=slave_fd, stderr=slave_fd
            )
            os.close(slave_fd)

            start_time = time.monotonic()
            output_buffer = b""

            while time.monotonic() - start_time < TOTAL_STARTUP_TIMEOUT:
                if not pending_interactions or process.poll() is not None:
                    break

                ready, _, _ = select.select([master_fd], [], [], IO_INACTIVITY_TIMEOUT)
                if not ready:
                    continue

                try:
                    output = os.read(master_fd, 1024)
                    if not output:
                        break

                    # --- Key Change 1: Diagnostic Printing ---
                    # Print the raw output to the user's terminal to help debug prompts.
                    print(f"[EMACS OUTPUT] {output.decode(errors='ignore')}", end="")

                    log_file.write(output)
                    log_file.flush()
                    output_buffer += output

                    handled_prompts = []
                    # --- Key Change 2: Case-Insensitive and Robust Matching ---
                    buffer_lower = output_buffer.lower()

                    for prompt, response in pending_interactions.items():
                        prompt_lower_bytes = prompt.lower().encode()

                        if prompt_lower_bytes in buffer_lower:
                            print(f"\n[SCRIPT ACTION] Detected: '{prompt}'")
                            print(f"[SCRIPT ACTION] Sending response: '{response}'")
                            os.write(master_fd, (response + "\n").encode())

                            # --- Key Change 3: Safer Buffer Handling ---
                            # Remove only the processed part of the buffer.
                            prompt_index = buffer_lower.find(prompt_lower_bytes)
                            output_buffer = output_buffer[
                                prompt_index + len(prompt_lower_bytes) :
                            ]

                            handled_prompts.append(prompt)
                            break

                    for prompt in handled_prompts:
                        del pending_interactions[prompt]

                except OSError:
                    break

            # Final status reporting
            if not pending_interactions:
                print(
                    "\n[SCRIPT INFO] All prompts handled successfully. Monitoring stopped."
                )
            elif process.poll() is not None:
                print("\n[SCRIPT INFO] Emacs process terminated unexpectedly.")
            else:
                print(
                    f"\n[SCRIPT INFO] Reached total startup timeout of {TOTAL_STARTUP_TIMEOUT}s."
                )

    finally:
        os.close(master_fd)

    return not pending_interactions


# Helper functions for clarity
def print_intro(log_file: Path) -> None:
    """Prints the introductory information for the user."""
    print("--- Emacs First Run & Logging Script (Python Version) ---")
    print("\nThis script will:")
    print("  1. Kill any existing Emacs instances.")
    print(f"  2. Tangle '{ORG_CONFIG_FILE}' to produce init.el.")
    print("  3. Start the Emacs daemon and answer startup prompts automatically.")
    print("  4. Capture all output to a log file.")
    print(f"\nLogging to: {log_file}")
    print(f'Monitor full log: tail -f "{log_file}"\n')


def print_outro(log_file: Path) -> None:
    """Prints the concluding messages for the user."""
    print("\n------------------------------------------------")
    print("Emacs daemon should now be running in the background.")
    print(f"The full output has been saved to: {log_file}")
    print("\nReview the log with commands like:")
    print(f'  less "{log_file}"')
    print("------------------------------------------------")


def handle_subprocess_error(e: Exception) -> None:
    """Handles errors from subprocess calls."""
    if isinstance(e, FileNotFoundError):
        print(
            "Error: 'emacs' command not found. Is Emacs installed and in your PATH?",
            file=sys.stderr,
        )
    elif isinstance(e, subprocess.CalledProcessError):
        print(f"Error: Failed to tangle '{ORG_CONFIG_FILE}'.", file=sys.stderr)
        print(f"--- Emacs Output ---\n{e.stderr}", file=sys.stderr)


if __name__ == "__main__":
    main()

