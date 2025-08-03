``````el
;;; -*- lexical-binding: t; -*-
;;; -*- early-init.el: t; -*-

;;; Commentary:
;; This file is based on the minimal-emacs.d project by James Cherti.
;; URL: https://github.com/jamescherti/minimal-emacs.d
;; It incorporates extensive optimizations for a faster Emacs startup.

;;; Code:

;; Prevent flash of unstyled modeline at startup
(setq-default mode-line-format nil)

;; Keep the original GC threshold value to restore it later.
(defvar my/original-gc-cons-threshold gc-cons-threshold)
;; Defer garbage collection during startup.
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

;; Disable bytecomp warning
(setq byte-compile-warnings nil)

;; Start in fundamental-mode to avoid loading other major modes.
(setq initial-major-mode 'fundamental-mode)

;; Make startup quieter.
(setq ring-bell-function 'ignore)
(setq visible-bell nil)

;; Disable package.el at startup; we'll initialize it manually in init.el.
(setq package-enable-at-startup nil)

;; Temporarily disable file-name-handler-alist. This is a significant
;; optimization as it is consulted on every `load` and `require`.
;; A hook will restore it after startup.
(defvar my/file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
          (defun my/restore-startup-settings ()
            "Restore settings that were deferred during startup."
            ;; Restore the GC threshold to a sane value for interactive use.
            (setq gc-cons-threshold (* 100 1024 1024)) ; 100mb
            ;; Restore the file name handler alist.
            (setq file-name-handler-alist my/file-name-handler-alist-original)
            (setq read-process-output-max (* 1 1024 1024)) ;; 2mb
            ;; Restore the modeline.
            (setq-default mode-line-format (default-value 'mode-line-format)))
          100) ; Run with high priority.

;; Kill all special buffers on startup
(defun kill-special-buffers-on-startup ()
  "Kill all special buffers that are not visiting a file on startup."
  (interactive)
  (dolist (buffer (buffer-list))
    (let ((buffer-name (buffer-name buffer)))
      (when (and (buffer-live-p buffer)
                 (string-prefix-p "*" buffer-name)
                 (not (buffer-file-name buffer)))
        (with-current-buffer buffer
          (kill-buffer (current-buffer)))))))

;; (add-hook 'emacs-startup-hook #'kill-special-buffers-on-startup)

;;; early-init.el ends here
``````
