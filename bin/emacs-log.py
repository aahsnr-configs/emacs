#!/usr/bin/env python3
"""
Emacs Configuration Initializer with Interactive Logging

A robust script to initialize Emacs configuration with comprehensive logging,
colored output, and Unicode support. Features interactive daemon startup
monitoring with proper error handling and resource management.

Requirements: Python 3.13+, Unix-like system, Emacs
Author: Enhanced Emacs Configuration Manager
License: MIT
"""

from __future__ import annotations

import os
import pty
import select
import signal
import subprocess
import sys
import time
from contextlib import contextmanager, suppress
from dataclasses import dataclass, field
from datetime import datetime
from enum import StrEnum
from pathlib import Path
from typing import Final, NoReturn, TextIO


class LogLevel(StrEnum):
    """Log levels with semantic meaning."""

    INFO = "info"
    SUCCESS = "success"
    WARNING = "warning"
    ERROR = "error"
    DEBUG = "debug"


@dataclass(frozen=True)
class Colors:
    """ANSI color codes and Unicode symbols for enhanced terminal output."""

    # Reset and formatting
    RESET: Final[str] = "\033[0m"
    BOLD: Final[str] = "\033[1m"
    DIM: Final[str] = "\033[2m"

    # Standard colors
    RED: Final[str] = "\033[31m"
    GREEN: Final[str] = "\033[32m"
    YELLOW: Final[str] = "\033[33m"
    BLUE: Final[str] = "\033[34m"
    MAGENTA: Final[str] = "\033[35m"
    CYAN: Final[str] = "\033[36m"
    WHITE: Final[str] = "\033[37m"

    # Bright colors
    BRIGHT_RED: Final[str] = "\033[91m"
    BRIGHT_GREEN: Final[str] = "\033[92m"
    BRIGHT_YELLOW: Final[str] = "\033[93m"
    BRIGHT_BLUE: Final[str] = "\033[94m"
    BRIGHT_MAGENTA: Final[str] = "\033[95m"
    BRIGHT_CYAN: Final[str] = "\033[96m"

    # Unicode symbols
    CHECK_MARK: Final[str] = "✓"
    CROSS_MARK: Final[str] = "✗"
    WARNING_SIGN: Final[str] = "⚠"
    INFO_SIGN: Final[str] = "ℹ"
    ARROW_RIGHT: Final[str] = "→"
    BULLET: Final[str] = "•"
    GEAR: Final[str] = "⚙"
    SPARKLES: Final[str] = "✨"
    ROCKET: Final[str] = "🚀"
    CLOCK: Final[str] = "⏱"
    HOURGLASS: Final[str] = "⧗"


@dataclass(frozen=True)
class Config:
    """Immutable configuration for Emacs setup process."""

    # Paths
    EMACS_DIR: Final[Path] = field(
        default_factory=lambda: Path.home() / ".config" / "emacs"
    )
    LOG_DIR: Final[Path] = field(init=False)
    ORG_CONFIG_FILE: Final[str] = "Emacs.org"

    # Timeouts (in seconds)
    TOTAL_STARTUP_TIMEOUT: Final[float] = 300.0
    IO_TIMEOUT: Final[float] = 2.0
    DAEMON_CHECK_INTERVAL: Final[float] = 10.0
    SUBPROCESS_TIMEOUT: Final[float] = 60.0

    # Behavior settings
    MAX_NO_OUTPUT_CHECKS: Final[int] = 5
    BUFFER_SIZE: Final[int] = 4096
    MAX_OUTPUT_BUFFER: Final[int] = 1024 * 1024  # 1MB

    def __post_init__(self) -> None:
        # Use object.__setattr__ for frozen dataclass
        object.__setattr__(self, "LOG_DIR", self.EMACS_DIR / "log")


class EmacsSetupError(Exception):
    """Base exception for Emacs setup related errors."""

    pass


class DaemonTimeoutError(EmacsSetupError):
    """Raised when daemon startup exceeds timeout."""

    pass


class ConfigurationError(EmacsSetupError):
    """Raised when configuration-related operations fail."""

    pass


class SystemError(EmacsSetupError):
    """Raised when system requirements are not met."""

    pass


class TerminalLogger:
    """Advanced terminal logger with Unicode symbols and color support."""

    def __init__(
        self, enable_colors: bool | None = None, enable_debug: bool = False
    ) -> None:
        self.enable_debug = enable_debug
        self.colors_enabled = (
            enable_colors if enable_colors is not None else self._detect_color_support()
        )
        self.colors = Colors()

        # Cache formatted symbols for performance
        self._symbol_cache: dict[LogLevel, str] = {}
        self._init_symbol_cache()

    def _detect_color_support(self) -> bool:
        """Detect if terminal supports color output."""
        if not (hasattr(sys.stdout, "isatty") and sys.stdout.isatty()):
            return False

        term = os.environ.get("TERM", "").lower()
        colorterm = os.environ.get("COLORTERM", "").lower()

        # Check for known color-supporting terminals
        return (
            "color" in term
            or "ansi" in term
            or "256" in term
            or "truecolor" in colorterm
            or colorterm in ("24bit", "yes")
        )

    def _init_symbol_cache(self) -> None:
        """Initialize symbol cache for better performance."""
        symbol_map = {
            LogLevel.INFO: (self.colors.BLUE, self.colors.INFO_SIGN),
            LogLevel.SUCCESS: (self.colors.BRIGHT_GREEN, self.colors.CHECK_MARK),
            LogLevel.WARNING: (self.colors.YELLOW, self.colors.WARNING_SIGN),
            LogLevel.ERROR: (self.colors.BRIGHT_RED, self.colors.CROSS_MARK),
            LogLevel.DEBUG: (self.colors.DIM, self.colors.BULLET),
        }

        for level, (color, symbol) in symbol_map.items():
            self._symbol_cache[level] = self._colorize(symbol, color)

    def _colorize(self, text: str, color: str) -> str:
        """Apply color formatting if colors are enabled."""
        return f"{color}{text}{self.colors.RESET}" if self.colors_enabled else text

    def _format_message(self, message: str, level: LogLevel) -> str:
        """Format message with appropriate styling."""
        symbol = self._symbol_cache.get(level, self.colors.BULLET)
        color = {
            LogLevel.INFO: self.colors.BLUE,
            LogLevel.SUCCESS: self.colors.BRIGHT_GREEN,
            LogLevel.WARNING: self.colors.YELLOW,
            LogLevel.ERROR: self.colors.BRIGHT_RED,
            LogLevel.DEBUG: self.colors.DIM,
        }.get(level, self.colors.WHITE)

        styled_message = self._colorize(message, color)
        return f"{symbol} {styled_message}"

    def log(
        self, message: str, level: LogLevel = LogLevel.INFO, file: TextIO = sys.stdout
    ) -> None:
        """Log message with specified level and styling."""
        if level == LogLevel.DEBUG and not self.enable_debug:
            return

        formatted = self._format_message(message, level)
        print(formatted, file=file, flush=True)

    def info(self, message: str) -> None:
        """Log informational message."""
        self.log(message, LogLevel.INFO)

    def success(self, message: str) -> None:
        """Log success message."""
        self.log(message, LogLevel.SUCCESS)

    def warning(self, message: str) -> None:
        """Log warning message."""
        self.log(message, LogLevel.WARNING)

    def error(self, message: str) -> None:
        """Log error message."""
        self.log(message, LogLevel.ERROR, sys.stderr)

    def debug(self, message: str) -> None:
        """Log debug message if debug mode is enabled."""
        self.log(message, LogLevel.DEBUG)

    def header(self, title: str, subtitle: str = "") -> None:
        """Display styled section header."""
        title_colored = self._colorize(
            title, f"{self.colors.BOLD}{self.colors.BRIGHT_CYAN}"
        )
        separator = self._colorize("─" * min(len(title), 60), self.colors.CYAN)

        print()
        print(f"{self.colors.SPARKLES} {title_colored}")
        if subtitle:
            subtitle_colored = self._colorize(subtitle, self.colors.DIM)
            print(f"  {subtitle_colored}")
        print(separator)

    def step(self, message: str, step_num: int = 0) -> None:
        """Log process step with numbering."""
        arrow = self._colorize(self.colors.ARROW_RIGHT, self.colors.CYAN)
        if step_num > 0:
            step_text = self._colorize(f"Step {step_num}", self.colors.BOLD)
            print(f"{arrow} {step_text}: {message}")
        else:
            print(f"{arrow} {message}")

    def progress(self, message: str, current: int = 0, total: int = 0) -> None:
        """Display progress information."""
        clock = self._colorize(self.colors.HOURGLASS, self.colors.YELLOW)
        if total > 0:
            percent = (current / total) * 100
            print(f"{clock} {message} ({current}/{total} - {percent:.1f}%)")
        else:
            print(f"{clock} {message}")

    def separator(self, char: str = "─", width: int = 50) -> None:
        """Print decorative separator line."""
        line = self._colorize(char * width, self.colors.DIM)
        print(line)


class ProcessManager:
    """Manages subprocess operations with proper cleanup and error handling."""

    def __init__(self, logger: TerminalLogger, config: Config) -> None:
        self.logger = logger
        self.config = config
        self._active_processes: list[subprocess.Popen] = []

        # Setup signal handlers for cleanup
        signal.signal(signal.SIGINT, self._signal_handler)
        signal.signal(signal.SIGTERM, self._signal_handler)

    def _signal_handler(self, signum: int, frame) -> NoReturn:
        """Handle signals by cleaning up processes."""
        self.logger.warning(f"Received signal {signum}, cleaning up...")
        self.cleanup_all()
        sys.exit(128 + signum)

    def run_command(
        self,
        command: list[str],
        timeout: float | None = None,
        check: bool = True,
        capture_output: bool = True,
    ) -> subprocess.CompletedProcess:
        """Run command with proper error handling and timeout."""
        timeout = timeout or self.config.SUBPROCESS_TIMEOUT

        try:
            self.logger.debug(f"Running command: {' '.join(command)}")
            result = subprocess.run(
                command,
                timeout=timeout,
                check=check,
                capture_output=capture_output,
                text=True,
            )
            return result

        except FileNotFoundError as e:
            raise SystemError(f"Command not found: {command[0]}") from e
        except subprocess.TimeoutExpired as e:
            raise EmacsSetupError(
                f"Command timed out after {timeout}s: {' '.join(command)}"
            ) from e
        except subprocess.CalledProcessError as e:
            error_msg = (
                f"Command failed with exit code {e.returncode}: {' '.join(command)}"
            )
            if e.stderr:
                error_msg += f"\nError output: {e.stderr.strip()}"
            raise EmacsSetupError(error_msg) from e

    def start_process(self, command: list[str], **kwargs) -> subprocess.Popen:
        """Start process and track it for cleanup."""
        process = subprocess.Popen(command, **kwargs)
        self._active_processes.append(process)
        return process

    def cleanup_process(self, process: subprocess.Popen) -> None:
        """Clean up a specific process."""
        if process in self._active_processes:
            self._active_processes.remove(process)

        if process.poll() is None:
            try:
                process.terminate()
                process.wait(timeout=5)
            except subprocess.TimeoutExpired:
                process.kill()
                process.wait()

    def cleanup_all(self) -> None:
        """Clean up all tracked processes."""
        for process in self._active_processes[
            :
        ]:  # Copy list to avoid modification during iteration
            self.cleanup_process(process)


class EmacsConfigurationManager:
    """Core manager for Emacs configuration and daemon setup."""

    def __init__(self, config: Config, logger: TerminalLogger) -> None:
        self.config = config
        self.logger = logger
        self.process_manager = ProcessManager(logger, config)
        self.log_file_path: Path | None = None

    def initialize(self) -> Path:
        """Initialize the setup environment and return log file path."""
        self._validate_environment()
        self._prepare_workspace()
        return self._create_log_file()

    def _validate_environment(self) -> None:
        """Validate system environment and requirements."""
        if sys.platform == "win32":
            raise SystemError("Unix-like operating system required (Linux, macOS, BSD)")

        if not self.config.EMACS_DIR.exists():
            raise ConfigurationError(
                f"Emacs directory not found: {self.config.EMACS_DIR}"
            )

        # Check for required commands
        for cmd in ["emacs", "killall"]:
            try:
                self.process_manager.run_command(
                    [cmd, "--version"], capture_output=True, check=False
                )
            except SystemError:
                if cmd == "emacs":
                    raise SystemError(f"Required command '{cmd}' not found in PATH")
                else:
                    self.logger.warning(
                        f"Command '{cmd}' not available, some features may not work"
                    )

    def _prepare_workspace(self) -> None:
        """Prepare the working environment."""
        try:
            os.chdir(self.config.EMACS_DIR)
            self.config.LOG_DIR.mkdir(parents=True, exist_ok=True)
            self.logger.debug(f"Working directory: {self.config.EMACS_DIR}")
            self.logger.debug(f"Log directory: {self.config.LOG_DIR}")
        except OSError as e:
            raise ConfigurationError(f"Failed to prepare workspace: {e}") from e

    def _create_log_file(self) -> Path:
        """Create timestamped log file."""
        timestamp = datetime.now().strftime("%Y-%m-%d_%H-%M-%S")
        log_file = self.config.LOG_DIR / f"emacs-daemon_{timestamp}.log"
        self.log_file_path = log_file

        try:
            # Create empty log file to verify write permissions
            log_file.touch()
            self.logger.debug(f"Log file created: {log_file}")
        except OSError as e:
            raise ConfigurationError(f"Cannot create log file: {e}") from e

        return log_file

    def cleanup_existing_instances(self) -> None:
        """Terminate existing Emacs instances."""
        self.logger.step("Cleaning up existing Emacs instances", 1)

        try:
            # Try graceful shutdown first
            self.process_manager.run_command(
                ["emacsclient", "-e", "(kill-emacs)"], check=False, timeout=10
            )
            time.sleep(2)
        except EmacsSetupError:
            pass  # Client might not be running

        # Force kill if needed
        try:
            result = self.process_manager.run_command(
                ["killall", "emacs"], check=False, timeout=10
            )
            if result.returncode == 0:
                self.logger.info("Terminated existing Emacs processes")
                time.sleep(1)
            else:
                self.logger.debug("No existing Emacs processes found")
        except EmacsSetupError:
            self.logger.debug("killall command failed, continuing anyway")

        self.logger.success("Cleanup completed")

    def process_configuration(self) -> None:
        """Process Org configuration file into init.el."""
        self.logger.step(
            f"Processing configuration: {self.config.ORG_CONFIG_FILE} → init.el", 2
        )

        org_file = self.config.EMACS_DIR / self.config.ORG_CONFIG_FILE
        if not org_file.exists():
            self.logger.warning(f"Configuration file not found: {org_file}")
            self.logger.info("Skipping configuration tangling")
            return

        try:
            self.process_manager.run_command(
                [
                    "emacs",
                    "--batch",
                    "--eval",
                    "(require 'org)",
                    "--eval",
                    f'(org-babel-tangle-file "{self.config.ORG_CONFIG_FILE}")',
                ]
            )
            self.logger.success("Configuration processing completed")

        except EmacsSetupError as e:
            raise ConfigurationError(f"Failed to process configuration: {e}") from e

    def start_daemon_interactive(self) -> bool:
        """Start Emacs daemon with interactive monitoring."""
        self.logger.step("Starting Emacs daemon with interactive monitoring", 3)
        self.logger.info("Respond to any Emacs prompts below")

        if not self.log_file_path:
            raise EmacsSetupError("Log file path not initialized")

        try:
            success = self._run_daemon_with_pty(["emacs", "--daemon"])

            if success:
                self.logger.success("Emacs daemon started successfully!")
                self._verify_daemon_functionality()
            else:
                self.logger.error("Emacs daemon startup failed")

            return success

        except Exception as e:
            self.logger.error(f"Daemon startup error: {e}")
            return False

    def _run_daemon_with_pty(self, command: list[str]) -> bool:
        """Execute daemon command with PTY for interactive I/O."""
        master_fd, slave_fd = pty.openpty()

        try:
            with open(self.log_file_path, "wb") as log_file:
                process = self.process_manager.start_process(
                    command, stdin=slave_fd, stdout=slave_fd, stderr=slave_fd
                )
                os.close(slave_fd)

                return self._monitor_daemon_process(process, master_fd, log_file)

        except Exception as e:
            self.logger.error(f"PTY setup failed: {e}")
            return False
        finally:
            with suppress(OSError):
                os.close(master_fd)

    def _monitor_daemon_process(
        self, process: subprocess.Popen, master_fd: int, log_file
    ) -> bool:
        """Monitor daemon startup with comprehensive feedback."""
        start_time = time.monotonic()
        last_output_time = start_time
        no_output_count = 0
        total_output_size = 0

        self.logger.progress("Monitoring daemon startup process")

        while time.monotonic() - start_time < self.config.TOTAL_STARTUP_TIMEOUT:
            # Check process status
            poll_result = process.poll()
            if poll_result is not None:
                return self._handle_process_exit(poll_result)

            # Monitor I/O with timeout
            try:
                ready, _, _ = select.select(
                    [master_fd, sys.stdin.fileno()], [], [], self.config.IO_TIMEOUT
                )
            except (ValueError, OSError):
                # File descriptors closed or invalid
                break

            output_received = False

            # Handle Emacs output
            if master_fd in ready:
                output_data = self._read_emacs_output(master_fd, log_file)
                if output_data is None:  # Process completed
                    return True
                elif output_data:
                    output_received = True
                    total_output_size += len(output_data)
                    last_output_time = time.monotonic()
                    no_output_count = 0

                    # Prevent memory issues with excessive output
                    if total_output_size > self.config.MAX_OUTPUT_BUFFER:
                        self.logger.warning(
                            "Output buffer limit reached, continuing monitoring..."
                        )
                        total_output_size = 0

            # Handle user input
            if sys.stdin.fileno() in ready:
                self._forward_user_input(master_fd)

            # Check daemon status during quiet periods
            if not output_received:
                no_output_count += 1
                if self._should_check_daemon(last_output_time, no_output_count):
                    if self._test_daemon_connectivity():
                        self.logger.success("Daemon is responding to client requests")
                        return True
                    no_output_count = 0
                    last_output_time = time.monotonic()

        # Handle timeout
        return self._handle_daemon_timeout(process)

    def _read_emacs_output(self, master_fd: int, log_file) -> bytes | None:
        """Read and process output from Emacs. Returns None if process completed."""
        try:
            output = os.read(master_fd, self.config.BUFFER_SIZE)
            if not output:
                return None  # EOF reached

            # Write to log file
            log_file.write(output)
            log_file.flush()

            # Display to terminal with error handling
            try:
                decoded = output.decode("utf-8", errors="replace")
                sys.stdout.write(decoded)
                sys.stdout.flush()
            except UnicodeError:
                # Fallback for problematic encodings
                sys.stdout.write(output.decode("latin1", errors="replace"))
                sys.stdout.flush()

            return output

        except OSError:
            return None

    def _forward_user_input(self, master_fd: int) -> None:
        """Forward user input to Emacs process."""
        try:
            user_input = os.read(sys.stdin.fileno(), self.config.BUFFER_SIZE)
            if user_input:
                os.write(master_fd, user_input)
                self.logger.debug("User input forwarded")
        except OSError:
            self.logger.debug("Failed to forward user input")

    def _should_check_daemon(
        self, last_output_time: float, no_output_count: int
    ) -> bool:
        """Determine if daemon connectivity should be tested."""
        time_since_output = time.monotonic() - last_output_time
        return (
            time_since_output >= self.config.DAEMON_CHECK_INTERVAL
            and no_output_count >= self.config.MAX_NO_OUTPUT_CHECKS
        )

    def _test_daemon_connectivity(self) -> bool:
        """Test if Emacs daemon is responding to client requests."""
        try:
            result = self.process_manager.run_command(
                ["emacsclient", "--eval", "(+ 1 2)"], timeout=5, check=False
            )
            return result.returncode == 0 and "3" in result.stdout
        except EmacsSetupError:
            return False

    def _handle_process_exit(self, return_code: int) -> bool:
        """Handle daemon process exit."""
        if return_code == 0:
            self.logger.success("Daemon process completed successfully")
            return True
        else:
            self.logger.error(f"Daemon process failed with exit code {return_code}")
            return False

    def _handle_daemon_timeout(self, process: subprocess.Popen) -> bool:
        """Handle daemon startup timeout."""
        elapsed = self.config.TOTAL_STARTUP_TIMEOUT
        self.logger.warning(f"Daemon startup timed out after {elapsed:.1f}s")

        # Final connectivity test
        if process.poll() is None and self._test_daemon_connectivity():
            self.logger.success("Daemon is actually working despite timeout!")
            return True

        self.logger.error("Daemon is not responding properly")
        return False

    def _verify_daemon_functionality(self) -> None:
        """Verify daemon is fully functional."""
        self.logger.info("Verifying daemon functionality...")

        tests = [
            ("Basic arithmetic", ["emacsclient", "--eval", "(+ 1 1)"]),
            ("Version check", ["emacsclient", "--eval", "(emacs-version)"]),
        ]

        for test_name, command in tests:
            try:
                result = self.process_manager.run_command(command, timeout=10)
                self.logger.debug(f"{test_name}: ✓")
            except EmacsSetupError:
                self.logger.warning(f"{test_name}: Failed")

    def cleanup(self) -> None:
        """Clean up resources."""
        self.process_manager.cleanup_all()


@contextmanager
def setup_signal_handlers():
    """Context manager for proper signal handling."""
    old_sigint = signal.signal(signal.SIGINT, signal.SIG_IGN)
    old_sigterm = signal.signal(signal.SIGTERM, signal.SIG_IGN)

    try:
        yield
    finally:
        signal.signal(signal.SIGINT, old_sigint)
        signal.signal(signal.SIGTERM, old_sigterm)


def display_introduction(logger: TerminalLogger, log_file: Path) -> None:
    """Display introduction and setup instructions."""
    logger.header(
        "Emacs Configuration Initializer",
        "Interactive daemon setup with comprehensive logging",
    )

    print("This script performs the following operations:")
    print("  1. Cleanup: Terminate any existing Emacs instances")
    print("  2. Configuration: Tangle Org configuration to init.el")
    print("  3. Daemon: Start Emacs daemon with interactive monitoring")
    print()

    logger.info("Respond to any Emacs prompts that appear below")
    logger.info(f"Complete session log: {log_file}")
    print()


def display_conclusion(logger: TerminalLogger, log_file: Path, success: bool) -> None:
    """Display conclusion and usage instructions."""
    logger.separator("═", 60)

    if success:
        logger.header(f"{Colors().ROCKET} Setup Completed Successfully!")

        print("Available commands:")
        print("  • GUI frame:      emacsclient -c")
        print("  • Terminal:       emacsclient -t")
        print("  • Evaluate:       emacsclient -e '(expression)'")
        print("  • Stop daemon:    emacsclient -e '(kill-emacs)'")
        print()

        logger.success("Emacs daemon is ready for connections!")

    else:
        logger.header("Setup Incomplete", "Please review the logs for details")
        logger.error("Daemon startup failed - check configuration and try again")

    print()
    logger.info(f"Session log: {log_file}")
    logger.info(f"Review with: less '{log_file}'")
    logger.separator("═", 60)


def main() -> NoReturn:
    """Main entry point with comprehensive error handling."""
    # Parse debug flag from environment
    debug_mode = os.environ.get("EMACS_SETUP_DEBUG", "").lower() in ("1", "true", "yes")

    logger = TerminalLogger(enable_debug=debug_mode)
    config = Config()
    manager: EmacsConfigurationManager | None = None

    try:
        with setup_signal_handlers():
            # Initialize manager
            manager = EmacsConfigurationManager(config, logger)
            log_file = manager.initialize()

            # Display introduction
            display_introduction(logger, log_file)

            # Execute setup sequence
            manager.cleanup_existing_instances()
            manager.process_configuration()
            success = manager.start_daemon_interactive()

            # Display results
            display_conclusion(logger, log_file, success)

            sys.exit(0 if success else 1)

    except KeyboardInterrupt:
        logger.warning("Setup interrupted by user (Ctrl+C)")
        if manager:
            manager.cleanup()
        sys.exit(130)

    except (SystemError, ConfigurationError) as e:
        logger.error(f"Setup failed: {e}")
        sys.exit(1)

    except EmacsSetupError as e:
        logger.error(f"Emacs setup error: {e}")
        sys.exit(1)

    except Exception as e:
        logger.error(f"Unexpected error: {e}")
        if debug_mode:
            import traceback

            traceback.print_exc()
        sys.exit(1)

    finally:
        if manager:
            manager.cleanup()


if __name__ == "__main__":
    main()

