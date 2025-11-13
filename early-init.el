;;; early-init.el --- sets stuff before init.el -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Code:
;; Ensure that quitting only occurs once Emacs finishes native compiling, preventing incomplete or leftover compilation files in `/tmp`.
(setq native-comp-async-query-on-exit t)
(setq confirm-kill-processes t)

;; Defer garbage collection during startup for a massive speed boost.
(setq gc-cons-threshold 100000000)
(setq package-enable-at-startup nil)
(setq read-process-output-max (* 2 1024 1024)) ;; 2mb


;; Defer UI elements for performance.
(menu-bar-mode -1)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Inhibit the startup screen and messages.
(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
      inhibit-startup-buffer-menu t)
      ;; inhibit-splash-screen t
      ;; initial-scratch-message nil
      ;; initial-buffer-choice nil)

;; Disable byte-compilation warnings.
(setq byte-compile-warnings nil)
(setq native-comp-async-report-warnings-errors 'silent)

;; Start in fundamental-mode to avoid loading other major modes.
;(setq initial-major-mode 'fundamental-mode)
(setq warning-suppress-types '((org-element)))

;; Make startup quieter.
;;(setq ring-bell-function 'ignore
;;      visible-bell nil)

;; Temporarily disable file-name-handler-alist for a significant optimization.
(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

(setq site-run-file nil)
;; Add a hook to restore deferred settings after Emacs has fully started.
;;; early-init.el ends here
