#!/usr/bin/env python3
"""
A script to initialize an Emacs configuration, logging the entire process.
It acts as an interactive proxy, forwarding startup prompts from the Emacs
daemon to the user's terminal for manual input.

This script is designed for Unix-like systems and is compatible with Python 3.13.
"""

import os
import pty
import select
import subprocess
import sys
import time
from datetime import datetime
from pathlib import Path
from typing import List

# --- Configuration ---
EMACS_DIR: Path = Path.home() / ".config" / "emacs"
LOG_DIR: Path = EMACS_DIR / "log"
ORG_CONFIG_FILE: str = "Emacs.org"
# Maximum total time in seconds to keep the interactive session open.
# After this, the script will detach, leaving the daemon running.
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
    except (FileNotFoundError, subprocess.CalledProcessError) as e:
        handle_subprocess_error(e)
        sys.exit(1)

    print("--> Starting Emacs daemon in interactive proxy mode...")
    print("--- You can now respond to any Emacs prompts directly below ---")

    run_emacs_with_pty(command=["emacs", "--daemon"], log_file_path=log_file)

    print_outro(log_file)


def run_emacs_with_pty(command: List[str], log_file_path: Path) -> None:
    """
    Executes a command in a pseudoterminal, acting as a proxy for I/O.
    This allows the user to interact with the daemon's startup prompts.
    """
    master_fd, slave_fd = pty.openpty()

    try:
        with open(log_file_path, "wb") as log_file:
            process = subprocess.Popen(
                command, stdin=slave_fd, stdout=slave_fd, stderr=slave_fd
            )
            os.close(slave_fd)

            start_time = time.monotonic()
            interactive_session_ended = False  # flag that is used to exit from main loop once an issue has taken place

            while (
                time.monotonic() - start_time < TOTAL_STARTUP_TIMEOUT
                and interactive_session_ended == False
            ):
                ready_to_read, _, _ = select.select(
                    [master_fd, sys.stdin.fileno()], [], [], 1.0
                )  # Use fileno here to fix some systems.

                # 1. Handle output from Emacs
                if master_fd in ready_to_read:
                    try:
                        output = os.read(master_fd, 1024)
                        if not output:
                            print(
                                "\n[SCRIPT INFO] Emacs process closed its output stream. Interactive session complete."
                            )
                            interactive_session_ended = True
                            break  # Exit loop, Emacs closed its output stream.

                        # Write to log file (bytes)
                        log_file.write(output)
                        log_file.flush()

                        # Write to user's terminal (decoded string)
                        sys.stdout.write(output.decode(errors="ignore"))
                        sys.stdout.flush()
                    except OSError:
                        print(
                            "\n[SCRIPT INFO] Emacs process closed its output stream. Interactive session complete."
                        )
                        interactive_session_ended = True
                        break  # pty was closed

                # 2. Handle input from the User
                if sys.stdin.fileno() in ready_to_read:
                    user_input = os.read(sys.stdin.fileno(), 1024)
                    os.write(master_fd, user_input)

            else:  # This 'else' belongs to the 'while' loop
                if not interactive_session_ended:
                    print(
                        f"\n[SCRIPT INFO] Interactive session timed out after {TOTAL_STARTUP_TIMEOUT}s."
                    )
                    print(
                        "              If Emacs continues to run in the background, prompts may still occur."
                    )

    finally:
        os.close(master_fd)

    # Check if the Emacs process is still running after the interactive phase
    if process.poll() is None:
        print("[SCRIPT INFO] Emacs daemon is still running in the background.")
        print(
            "              The script has completed its initial setup.  You can close this terminal."
        )
    else:
        print("[SCRIPT INFO] Emacs daemon has exited.")


# Helper functions for clarity
def print_intro(log_file: Path) -> None:
    """Prints the introductory information for the user."""
    print("--- Emacs First Run & Logging Script (Interactive Python Version) ---")
    print("\nThis script will:")
    print("  1. Kill any existing Emacs instances.")
    print(f"  2. Tangle '{ORG_CONFIG_FILE}' to produce init.el.")
    print("  3. Start the Emacs daemon and forward any startup prompts to you.")
    print("\nIMPORTANT: When prompted, type your input and press Enter.")
    print(f"The full, raw log will be saved to: {log_file}\n")


def print_outro(log_file: Path) -> None:
    """Prints the concluding messages for the user."""
    print("\n------------------------------------------------")
    print("Emacs daemon setup complete.")
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

