;;; early-init.el --- Optimized Early Initialization -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;; This file runs before init.el and package initialization.
;; Focus: Maximum startup speed and minimal resource usage.

;;; Code:

;; ====================
;; CRITICAL: GC OPTIMIZATION
;; ====================
;; During startup, set GC threshold to maximum to prevent collections
;; This gives the biggest startup speed boost (30-50% faster)
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Store original file-name-handler-alist for restoration later
(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

;; ====================
;; PACKAGE SYSTEM
;; ====================
(setq package-enable-at-startup nil)

;; ====================
;; NATIVE COMPILATION
;; ====================
;; Ensure quit prompt only after native compilation finishes
(setq native-comp-async-query-on-exit t
      native-comp-async-report-warnings-errors 'silent
      native-comp-speed 2  ;; Balance compilation speed vs runtime speed
      native-comp-deferred-compilation t)

;; Suppress byte-compilation warnings
(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))

;; ====================
;; PROCESS COMMUNICATION
;; ====================
;; Critical for LSP performance - increase read buffer size
(setq read-process-output-max (* 3 1024 1024))  ;; 3MB (up from 2MB)

;; ====================
;; UI INITIALIZATION
;; ====================
;; Disable UI elements BEFORE they're rendered for faster startup
(setq frame-inhibit-implied-resize t)  ;; Prevent frame resize jank during startup

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(mouse-color . "white") default-frame-alist)

;; Apply immediately (not just to default-frame-alist)
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; ====================
;; STARTUP SCREEN
;; ====================
(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
      inhibit-startup-buffer-menu t
      inhibit-splash-screen t
      initial-scratch-message nil)

;; ====================
;; DISABLE WARNINGS
;; ====================
(setq warning-suppress-types '((org-element) (comp)))
(setq warning-minimum-level :error)

;; ====================
;; SITE FILES
;; ====================
;; Don't load default.el or site-start.el
(setq site-run-file nil)

;; ====================
;; FILE HANDLERS
;; ====================
;; We'll restore file-name-handler-alist in init.el after startup

;;; early-init.el ends here
