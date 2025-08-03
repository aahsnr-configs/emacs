;;; early-init.el --- sets stuff before init.el -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Code:

;; Ensure Emacs loads the most recent byte-compiled files.
(setq load-prefer-newer t)

;; Make Emacs Native-compile .elc files asynchronously by setting
;; `native-comp-jit-compilation' to t.
(setq native-comp-jit-compilation t)
(setq native-comp-deferred-compilation native-comp-jit-compilation)  ; Deprecated

;;
;; --- 1. File & Directory Configuration (no-littering & Elpaca) ---
;; This section MUST come first to ensure all subsequent operations
;; use the correct paths.
;;

;; Set the user-emacs-directory to ~/.config/emacs as the first step.
(setq user-emacs-directory (file-name-as-directory "~/.config/emacs"))

;; Add your custom lisp directories to the load-path so Emacs can find them.
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/no-littering" user-emacs-directory))

;; Define custom paths for no-littering BEFORE loading the package.
(setq no-littering-var-directory (expand-file-name "var" user-emacs-directory))
(setq no-littering-etc-directory (expand-file-name "etc" no-littering-var-directory))

;; --- Pre-configure Elpaca's location ---
;; By defining `elpaca-directory` here, we ensure that when the standard
;; Elpaca bootstrap snippet is run from `init.el`, it will use this path
;; instead of its default. This redirects the Elpaca repository itself.
(defvar elpaca-directory (expand-file-name "elpaca/" no-littering-var-directory))

;; Load no-littering to apply the new path conventions.
(require 'no-littering)

;; For Emacs 29+, redirect the native compilation cache into the var directory.
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (expand-file-name "eln-cache" no-littering-var-directory)))

;; Relocate core Emacs-generated files into the `var` directory.
(setq backup-directory-alist `(("." . ,(expand-file-name "backups" no-littering-var-directory))))
(setq auto-save-file-name-transforms `((".*" ,(expand-file-name "auto-saves" no-littering-var-directory) t)))
(setq auto-save-list-file-prefix (expand-file-name "auto-save-list/" no-littering-var-directory))
(setq server-dir (expand-file-name "server" no-littering-var-directory))


;;
;; --- 2. Performance & Startup Optimizations ---
;;

;; Prevent flash of unstyled modeline at startup. It will be restored by the startup hook.
(setq-default mode-line-format nil)

;; Defer garbage collection during startup for a massive speed boost.
(defvar my/original-gc-cons-threshold gc-cons-threshold)
(setq gc-cons-threshold most-positive-fixnum)

;; Defer UI elements for performance.
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Inhibit frame resizing, which can be slow.
(setq frame-inhibit-implied-resize t)
(setq frame-resize-pixelwise t)

;; Inhibit the startup screen and messages for a cleaner and faster start.
(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
      inhibit-startup-buffer-menu t
      inhibit-splash-screen t
      initial-scratch-message nil
      initial-buffer-choice nil)

;; Disable byte-compilation warnings.
(setq byte-compile-warnings nil)

;; Start in fundamental-mode to avoid loading other major modes.
(setq initial-major-mode 'fundamental-mode)

;; Make startup quieter.
(setq ring-bell-function 'ignore)
(setq visible-bell nil)

;; Disable package.el at startup; it will be initialized manually in init.el.
(setq package-enable-at-startup nil)

;; Temporarily disable file-name-handler-alist. This is a significant
;; optimization as it is consulted on every `load` and `require`.
(defvar my/file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Disable Warnings
(setq byte-compile-warnings nil) ;; Suppress byte-compiler warnings
(setq native-comp-async-report-warnings-errors 'silent) ;; Suppress native-comp warnings

;; Add a hook to restore deferred settings after Emacs has fully started.
(add-hook 'emacs-startup-hook
          (defun my/restore-startup-settings ()
            "Restore settings that were deferred during startup."
            ;; Restore the GC threshold to a sane value for interactive use.
            (setq gc-cons-threshold (* 100 1024 1024)) ; 100MB
            ;; Restore the file name handler alist.
            (setq file-name-handler-alist my/file-name-handler-alist-original)
            (setq read-process-output-max (* 2 1024 1024)) ;; 2MB
            ;; Restore the modeline.
            (setq-default mode-line-format (default-value 'mode-line-format)))
          100) ; Run with high priority.

;;; early-init.el ends here;;; early-init.el ends here
