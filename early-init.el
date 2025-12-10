;;; early-init.el --- Early initialization. -*- lexical-binding: t -*-

;; Copyright (C) 2019-2025 Vincent Zhang

;; Author: Vincent Zhang <seagle0128@gmail.com>
;; URL: https://github.com/seagle0128/.emacs.d

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;; Emacs 27 introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.
;;

;;; Code:

(let ((file-name-handler-alist-original file-name-handler-alist))

  ;; ====================
  ;; MAXIMUM GC DEFERRAL (Doom Strategy)
  ;; ====================
  (setq gc-cons-threshold most-positive-fixnum
        gc-cons-percentage 0.6)

  ;; Disable file name handlers completely during startup
  (setq file-name-handler-alist nil)

  ;; ====================
  ;; REDISPLAY OPTIMIZATIONS (Doom's Secret Sauce)
  ;; ====================
  (setq redisplay-skip-fontification-on-input t)  ; Skip font-lock during fast input
  (setq fast-but-imprecise-scrolling t)
  (setq inhibit-compacting-font-caches t)

  ;; ====================
  ;; FRAME OPTIMIZATION
  ;; ====================
  (setq frame-inhibit-implied-resize t)
  (setq frame-resize-pixelwise t)

  ;; ====================
  ;; PACKAGE SYSTEM
  ;; ====================
  (setq package-enable-at-startup nil)

  ;; ====================
  ;; NATIVE COMPILATION
  ;; ====================
  ;; (setq native-comp-async-query-on-exit t
  ;;       native-comp-speed 2
  ;;       native-comp-deferred-compilation t)

  (setq native-comp-jit-compilation nil)

  (setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))
  (setq use-package-enable-imenu-support t)
  ;; ====================
  ;; PROCESS COMMUNICATION (Critical for LSP)
  ;; ====================
  (setq read-process-output-max (* 3 1024 1024))  ; 3MB

  ;; Prevent flash of unstyled mode line
  (setq mode-line-format nil)

  ;; In noninteractive sessions, prioritize non-byte-compiled source files to
  ;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
  ;; to skip the mtime checks on every *.elc file.
  (setq load-prefer-newer noninteractive)

  ;; Explicitly set the prefered coding systems to avoid annoying prompt
  ;; from emacs (especially on Microsoft Windows)
  (prefer-coding-system 'utf-8)

  ;; For LSP performance
  ;; @see https://emacs-lsp.github.io/lsp-mode/page/performance/
  (setenv "LSP_USE_PLISTS" "true")



  ;; ====================
  ;; UI INITIALIZATION
  ;; ====================
  (push '(menu-bar-lines . 0) default-frame-alist)
  (push '(tool-bar-lines . 0) default-frame-alist)
  (push '(vertical-scroll-bars) default-frame-alist)
  (push '(mouse-color . "white") default-frame-alist)

  (menu-bar-mode -1)
  (when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

  ;; ====================
  ;; STARTUP SCREEN
  ;; ====================
  (setq inhibit-startup-screen t
        inhibit-startup-echo-area-message user-login-name
        inhibit-startup-buffer-menu t
        inhibit-splash-screen t
        initial-scratch-message nil)

  ;; ====================
  ;; WARNINGS
  ;; ====================
  (setq warning-suppress-types '((org-element) (comp)))
  (setq warning-minimum-level :error)

  ;; ====================
  ;; SITE FILES
  ;; ====================
  (setq site-run-file nil)

  ;; ====================
  ;; RESTORE AFTER STARTUP
  ;; ====================
  (add-hook 'emacs-startup-hook
            (lambda ()
              (setq file-name-handler-alist file-name-handler-alist-original))
            100))  ; Run late
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; early-init.el ends here
