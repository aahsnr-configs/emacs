;;; early-init.el --- sets stuff before init.el -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Code:

;; Add custom lisp directories to the load-path. [1]
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/no-littering" user-emacs-directory))

;; Ensure that quitting only occurs once Emacs finishes native compiling,
;; preventing incomplete or leftover compilation files in `/tmp`.
(setq native-comp-async-query-on-exit t)
(setq confirm-kill-processes t)

;; Non-nil means to native compile packages as part of their installation.
(setq package-native-compile t)

;; Load no-littering to apply the new path conventions. [8, 10]
(require 'no-littering)

;; Define custom paths for no-littering BEFORE loading the package. [8, 10]
(setq no-littering-var-directory (expand-file-name "var" user-emacs-directory)
      no-littering-etc-directory (expand-file-name "etc" no-littering-etc-directory))

;; For Emacs 29+, redirect the native compilation cache.
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (expand-file-name "eln-cache" no-littering-var-directory)))

;; Relocate core Emacs-generated files into the `var` and `etc` directories.
(setq backup-directory-alist `(("." . ,(expand-file-name "backups/" no-littering-var-directory)))
      auto-save-list-file-prefix (expand-file-name "auto-save-list/" no-littering-var-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "auto-saves/" no-littering-var-directory) t))
      server-dir (expand-file-name "server/" no-littering-var-directory)
      custom-file (expand-file-name "custom.el" no-littering-etc-directory) ; [16]
      savehist-file (expand-file-name "savehist" no-littering-var-directory) ; [5, 28]
      recentf-save-file (expand-file-name "recentf" no-littering-var-directory) ; [3, 22]
      abbrev-file-name (expand-file-name "abbrevs.el" no-littering-etc-directory)
      bookmark-default-file (expand-file-name "bookmarks" no-littering-var-directory)
      tramp-persistency-file-name (expand-file-name "tramp" no-littering-var-directory))

;; Configure Elpaca directories. [29, 31]
(setq elpaca-directory (expand-file-name "elpaca/" user-emacs-directory)
      package-user-dir (expand-file-name "elpa" no-littering-var-directory)) ; [7, 13]

;; Persist the scratch buffer across sessions.
(setq persistent-scratch-save-file (expand-file-name "scratch" no-littering-var-directory))

;;
;; --- 2. Performance & Startup Optimizations ---
;;

;; Prevent flash of unstyled modeline at startup.
(setq-default mode-line-format nil)

;; Defer garbage collection during startup for a massive speed boost.
(setq gc-cons-threshold most-positive-fixnum)

;; Defer UI elements for performance.
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Inhibit frame resizing.
(setq frame-inhibit-implied-resize t)
(setq frame-resize-pixelwise t)

;; Inhibit the startup screen and messages.
(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
      inhibit-startup-buffer-menu t
      inhibit-splash-screen t
      initial-scratch-message nil
      initial-buffer-choice nil)

;; Disable byte-compilation warnings.
(setq byte-compile-warnings nil)
(setq native-comp-async-report-warnings-errors 'silent)

;; Start in fundamental-mode to avoid loading other major modes.
(setq initial-major-mode 'fundamental-mode)

;; Make startup quieter.
(setq ring-bell-function 'ignore
      visible-bell nil)

;; Disable package.el at startup; it will be initialized manually in init.el.
(setq package-enable-at-startup nil)

;; Temporarily disable file-name-handler-alist for a significant optimization.
(defvar my/file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Add a hook to restore deferred settings after Emacs has fully started.
(add-hook 'emacs-startup-hook
          (defun my/restore-startup-settings ()
            "Restore settings that were deferred during startup."
            (setq gc-cons-threshold (* 100 1024 1024)) ; 100MB
            (setq file-name-handler-alist my/file-name-handler-alist-original)
            (setq read-process-output-max (* 2 1024 1024)) ; 2MB
            (setq-default mode-line-format (default-value 'mode-line-format)))
          100)

;;; early-init.el ends here
