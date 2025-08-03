``````el
;;; early-init.el --- sets stuff before init.el -*- no-byte-compile: t; lexical-binding: t; -*-

;; Set the user-emacs-directory to ~/.config/emacs as the first step.
(setq user-emacs-directory (file-name-as-directory "~/.config/emacs"))

;; Add your custom lisp directories to the load-path so Emacs can find them.
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/no-littering" user-emacs-directory))

(setq no-littering-var-directory (expand-file-name "var" user-emacs-directory))
(setq no-littering-etc-directory (expand-file-name "etc" no-littering-var-directory))

(require 'no-littering)

(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (expand-file-name "eln-cache" no-littering-var-directory)))

(setq backup-directory-alist `(("." . ,(expand-file-name "backups" no-littering-var-directory))))
(setq auto-save-file-name-transforms `((".*" ,(expand-file-name "auto-saves" no-littering-var-directory) t)))
(setq auto-save-list-file-prefix (expand-file-name "auto-save-list/" no-littering-var-directory))
(setq server-dir (expand-file-name "server" no-littering-var-directory))

``````
