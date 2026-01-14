;;; early-init.el --- Optimized Early Initialization -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;; Critical early initialization for Emacs 29+
;;
;; STARTUP SEQUENCE:
;;   1. Load early-init.el (THIS FILE)
;;   2. Call package-activate-all (UNLESS package-enable-at-startup is nil)
;;   3. Initialize GUI
;;   4. Load init.el
;;   5. Run after-init-hook
;;   6. Run emacs-startup-hook
;;
;; PRINCIPLE: Only settings that MUST run before package/GUI initialization
;; belong here. Everything else should be in init.el (tangled from config.org).

;;; Code:

;; ====================
;; STORE ORIGINAL VALUES FOR RESTORATION
;; ====================
(defvar emacs--file-name-handler-alist-original file-name-handler-alist
  "Original `file-name-handler-alist' for restoration after startup.")

(defvar emacs--vc-handled-backends-original vc-handled-backends
  "Original `vc-handled-backends' for restoration after startup.")

;; ====================
;; GARBAGE COLLECTION DEFERRAL
;; ====================
;; Set GC threshold to maximum during startup to prevent GC pauses.
;; GCMH will handle GC strategy after init.el loads.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.8)

;; ====================
;; FILE NAME HANDLER OPTIMIZATION
;; ====================
;; Disable file handlers (TRAMP, compressed files, etc.) during startup.
;; Massive performance boost as regexp matching is expensive.
(setq file-name-handler-alist nil)

;; ====================
;; VERSION CONTROL OPTIMIZATION
;; ====================
;; Disable VC backend checks during startup for faster file operations.
;; Will be restored to '(Git) only in emacs-startup-hook.
(setq vc-handled-backends nil)

;; ====================
;; CRITICAL PERFORMANCE SETTINGS (Must be set before GUI init)
;; ====================
;; These MUST be in early-init for startup performance
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)

;; Don't compact font caches during GC (prevents startup stutter)
(setq inhibit-compacting-font-caches t)

;; ====================
;; UI ELEMENT REMOVAL (Prevent Visual Flashing)
;; ====================
;; Setting via default-frame-alist prevents elements from ever appearing.
;; This is faster and cleaner than mode toggling in init.el.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(mouse-color . "white") default-frame-alist)

;; Immediately disable UI elements for good measure
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; ====================
;; STARTUP SCREEN SUPPRESSION
;; ====================
(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
      inhibit-startup-buffer-menu t
      inhibit-splash-screen t
      initial-scratch-message nil)

;; ====================
;; PACKAGE SYSTEM CONTROL
;; ====================
;; CRITICAL: Disable automatic package activation so we can manually
;; load no-littering and auto-compile BEFORE packages are activated.
(setq package-enable-at-startup nil)

;; ====================
;; NATIVE COMPILATION (Emacs 29+)
;; ====================
(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  ;; Silence warnings (synced with config.org)
  (setq native-comp-async-report-warnings-errors 'silent)
  
  ;; Prevent quitting during compilation
  (setq native-comp-async-query-on-exit t)
  
  ;; Balanced compilation speed
  (setq native-comp-speed 2))

;; ====================
;; FILE LOADING PREFERENCES
;; ====================
;; Always prefer newer source files over byte-compiled versions
(setq load-prefer-newer t)

;; Reduce byte-compile noise (synced with config.org)
(setq byte-compile-warnings '(not obsolete free-vars unresolved noruntime lexical make-local))

;; ====================
;; LSP PERFORMANCE (Critical for subprocess communication)
;; ====================
;; Increase read limit from 4KB to 3MB for LSP servers
(setq read-process-output-max (* 3 1024 1024))

;; ====================
;; WARNING SUPPRESSION
;; ====================
(setq warning-suppress-types '((org-element) (comp)))
(setq warning-minimum-level :error)

;; ====================
;; MISC CRITICAL SETTINGS
;; ====================
(setq site-run-file nil)  ; Don't load site-start.el

;; ====================
;; LOAD PATH FOR GIT SUBMODULES
;; ====================
;; Add lisp/ directory BEFORE loading no-littering and auto-compile
(let ((lisp-dir (expand-file-name "lisp" user-emacs-directory)))
  (when (file-directory-p lisp-dir)
    (add-to-list 'load-path lisp-dir)
    (normal-top-level-add-subdirs-to-load-path)))

;; ====================
;; NO-LITTERING (Load BEFORE package-initialize)
;; ====================
(require 'no-littering)

;; Configure recentf exclusions
(require 'recentf)
(add-to-list 'recentf-exclude
             (recentf-expand-file-name no-littering-var-directory))
(add-to-list 'recentf-exclude
             (recentf-expand-file-name no-littering-etc-directory))

;; Set custom file location
(setq custom-file (no-littering-expand-etc-file-name "custom.el"))

;; Redirect native comp cache (Emacs 29+)
(when (and (fboundp 'startup-redirect-eln-cache)
           (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name "var/eln-cache/" user-emacs-directory))))

;; ====================
;; AUTO-COMPILE (Load BEFORE package-initialize)
;; ====================
(require 'auto-compile)
(auto-compile-on-load-mode)
(auto-compile-on-save-mode)

;; ====================
;; PACKAGE SYSTEM INITIALIZATION
;; ====================
(require 'package)

;; Configure package archives
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

;; Initialize package system
(package-initialize)

;; Refresh package list if needed
(unless package-archive-contents
  (package-refresh-contents))

;; ====================
;; USE-PACKAGE SETUP
;; ====================
(require 'use-package)
(require 'bind-key)

(setq use-package-verbose nil
      use-package-expand-minimally t
      use-package-compute-statistics nil
      use-package-always-ensure t
      use-package-enable-imenu-support t)

;; ====================
;; POST-STARTUP RESTORATION
;; ====================
(add-hook 'emacs-startup-hook
          (lambda ()
            ;; Restore GC threshold to 16MB (GCMH will manage from here)
            (setq gc-cons-threshold (* 16 1024 1024)
                  gc-cons-percentage 0.1)
            
            ;; Restore file name handlers
            (setq file-name-handler-alist
                  (delete-dups (append file-name-handler-alist
                                       emacs--file-name-handler-alist-original)))
            
            ;; Restore VC backends (Git only, as per config.org)
            (setq vc-handled-backends '(Git))
            
            ;; Clean up temporary variables
            (makunbound 'emacs--file-name-handler-alist-original)
            (makunbound 'emacs--vc-handled-backends-original))
          101) ; High depth ensures this runs late

;; ====================
;; MINIBUFFER GC OPTIMIZATION (Doom Emacs strategy)
;; ====================
;; Defer GC when minibuffer is active to prevent completion stuttering
(defun emacs--defer-garbage-collection-h ()
  "Maximize GC threshold when entering minibuffer."
  (setq gc-cons-threshold most-positive-fixnum))

(defun emacs--restore-garbage-collection-h ()
  "Restore GC threshold after brief delay when exiting minibuffer."
  (run-at-time 1 nil (lambda () (setq gc-cons-threshold (* 16 1024 1024)))))

(add-hook 'minibuffer-setup-hook #'emacs--defer-garbage-collection-h)
(add-hook 'minibuffer-exit-hook #'emacs--restore-garbage-collection-h)

;;; early-init.el ends here
