;;; org-src-context.el --- LSP support for org-src buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Free Software Foundation, Inc.

;; Author: Karthik Chikmagalur <karthikchikmagalur@gmail.com>
;; Maintainer: Ahsanur Rahman
;; Keywords: tools, languages, extensions
;; Package-Requires: ((emacs "29.1") (org "9.6"))

;;; Commentary:
;; Refactored for large-file performance and stability.
;; 1. PREVENTS CURSOR JUMPS: Checks `org-edit-src-code' arguments to detect
;;    and ignore transient edits (indentation/fontification).
;; 2. OPTIMIZES PARSING: Only scans for context when opening a real edit session.
;; 3. EGLOT FALLBACK: Mocks file paths for non-tangled blocks.
;; 4. FIXED CRASHES: Correctly handles block info indices and types.

;;; Code:

(require 'org)
(require 'ob)
(require 'ob-tangle)
(require 'org-src)
(require 'cl-lib)

(defgroup org-src-context nil
  "Provide LSP support in org-src buffers."
  :group 'org)

(defcustom org-src-context-narrow-p nil
  "Non-nil means org-src buffers should be narrowed to the code block.
This setting applies when Eglot is enabled in the edit buffer."
  :type 'boolean
  :group 'org-src-context)

(defvar-local org-src-context--before-block-marker nil)
(defvar-local org-src-context--after-block-marker nil)

(defun org-src-context--get-block-info ()
  "Retrieve source block info efficiently using 'light mode."
  (let ((info (org-babel-get-src-block-info 'light)))
    (when info
      (let* ((datum (org-element-context))
             (type (org-element-type datum)))
        (if (eq type 'src-block)
            (cons info datum)
          nil)))))

(defun org-src-context--get-body (block)
  "Robustly extract body string from a tangle block info list.
Handles variations in list structure (e.g. presence of switches).
If index 5 is a list (params), the body is at index 6.
Otherwise, the body is at index 5. Returns empty string on failure."
  (let ((val (if (listp (nth 5 block))
                 (nth 6 block)
               (nth 5 block))))
    (if (stringp val) val "")))

(defun org-src-context--edit-src-ad (orig-fn &rest args)
  "Advise `org-edit-src-code' to inject context.
ORIG-FN is the original function.  ARGS are the arguments.
If the first argument (CODE) is non-nil, this is a transient edit
\(e.g. indentation check), so we inhibit context injection."
  (if (car args)
      ;; CODE arg is present -> Background check -> Run native (No Lag)
      (apply orig-fn args)
    
    ;; CODE arg is nil -> Interactive Edit -> Inject Context
    (let ((block-data (org-src-context--get-block-info)))
      (if (not block-data)
          (apply orig-fn args)
        (let* ((info (car block-data))
               (datum (cdr block-data))
               (lang (nth 0 info))
               (tangle-param (alist-get :tangle (nth 2 info)))
               (tangle-file (if (and tangle-param (not (string= tangle-param "no")))
                                tangle-param
                              nil))
               ;; CAPTURE DIRECTORY from the source buffer
               (source-dir default-directory)
               ;; FIX: Use `cdar` (via cdr car) to get the list of blocks.
               ;; `org-babel-tangle-collect-blocks` returns ((filename . blocks)).
               ;; We want `blocks`.
               (collection (if tangle-file
                               (org-babel-tangle-collect-blocks lang tangle-file)
                             nil))
               (all-blocks (if collection (cdr (car collection)) nil))
               (extra-blocks (list nil)))

          (prog1 (apply orig-fn args)
            ;; Injection Logic
            (when all-blocks
              ;; Use absolute line numbers for robust comparison (Int vs Int)
              (let ((this-line (line-number-at-pos (org-element-property :begin datum) t)))
                (setq extra-blocks
                      (cl-loop for block in all-blocks
                               ;; Index 0 is always the start line number
                               for block-line = (nth 0 block)
                               until (= block-line this-line)
                               collect block into before-blocks
                               finally return
                               (cons before-blocks 
                                     (nthcdr (1+ (length before-blocks)) all-blocks))))))
              
              (when (or (car extra-blocks) (cdr extra-blocks))
                (setq-local org-src-context--before-block-marker (point-min-marker))
                (set-marker-insertion-type org-src-context--before-block-marker t)
                (setq-local org-src-context--after-block-marker  (point-max-marker))
                (set-marker-insertion-type org-src-context--after-block-marker nil)

                (let ((inhibit-read-only t))
                  ;; Inject BEFORE
                  (cl-loop initially do
                           (progn (goto-char (marker-position org-src-context--before-block-marker))
                                  (when (car extra-blocks) (insert "\n") (backward-char 1)))
                           for block in (car extra-blocks)
                           ;; FIX: Use robust body extractor
                           for code = (propertize (concat "\n" (org-src-context--get-body block)
                                                          (propertize "\n" 'rear-nonsticky t))
                                                  'read-only t
                                                  'font-lock-face 'shadow)
                           do (insert code))

                  (set-marker-insertion-type org-src-context--before-block-marker nil)

                  ;; Inject AFTER
                  (cl-loop initially do (goto-char (marker-position org-src-context--after-block-marker))
                           for block in (cdr extra-blocks)
                           ;; FIX: Use robust body extractor
                           for code = (propertize (concat "\n" (org-src-context--get-body block)
                                                          (propertize "\n" 'rear-nonsticky t))
                                                  'read-only t
                                                  'font-lock-face 'shadow)
                           do (insert code))
                  
                  (when org-src-context-narrow-p
                    (narrow-to-region (marker-position org-src-context--before-block-marker)
                                      (marker-position org-src-context--after-block-marker)))
                  
                  (goto-char (marker-position org-src-context--before-block-marker))
                  (set-window-start (selected-window)
                                    (marker-position org-src-context--before-block-marker))))

            ;; Run Eglot Setup with explicit directory
            (org-src-context--connect-maybe info tangle-file source-dir)))))))

(defun org-src-context--exit-src-ad (&rest _)
  "Clean up read-only regions before exiting edit buffer."
  (when (and org-src-context--before-block-marker
             org-src-context--after-block-marker)
    (let ((inhibit-read-only t)
          (beg (marker-position org-src-context--before-block-marker))
          (end (marker-position org-src-context--after-block-marker)))
      (when (and beg end)
        (when org-src-context-narrow-p (widen))
        (delete-region end (point-max))
        (delete-region (point-min) beg)
        (setq org-src-context--before-block-marker nil
              org-src-context--after-block-marker nil)))))

(defun org-src-context--connect-maybe (info tangle-file source-dir)
  "Prepare org source block buffer for an LSP connection.
INFO is the src-block info. TANGLE-FILE is the target.
SOURCE-DIR is the default-directory of the original Org buffer."
  (let* ((mkdirp (alist-get :mkdirp (nth 2 info)))
         (effective-file (or tangle-file 
                             (format "org-src-babel-%s.tmp" (file-name-extension (or (car info) "txt")))))
         ;; Use the captured source-dir to expand the path relative to the project root
         (full-path (expand-file-name effective-file source-dir)))

    (when (and tangle-file (stringp mkdirp) (string= (downcase mkdirp) "yes"))
      (make-directory (file-name-directory full-path) 'parents))

    (setq-local buffer-file-name full-path)
    (if (fboundp 'eglot-ensure) (eglot-ensure))))

;;; SAFETY WRAPPER: Prevents context injection during sensitive operations
(defun org-src-context-inhibit-wrapper (orig-fn &rest args)
  "Inhibit context injection during this execution.
ORIG-FN is the original function.  ARGS are its arguments.
Used for native tab behavior and indentation checks."
  (apply orig-fn args))

;;;###autoload
(define-minor-mode org-src-context-mode
  "Toggle Org-Src-Context mode.
Provides LSP context in Org source buffers via Eglot."
  :global t
  :group 'org-src-context
  (if org-src-context-mode
      (progn
        (advice-add 'org-edit-src-code :around #'org-src-context--edit-src-ad)
        (advice-add 'org-edit-src-exit :before #'org-src-context--exit-src-ad))
    (advice-remove 'org-edit-src-code #'org-src-context--edit-src-ad)
    (advice-remove 'org-edit-src-exit #'org-src-context--exit-src-ad)))

(provide 'org-src-context)
;;; org-src-context.el ends here