;;; org-src-context.el --- LSP and Context support for org-src buffers -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Ahsanur Rahman
;; Author: Ahsanur Rahman
;; Keywords: tools, languages, extensions, lsp
;; Package-Requires: ((emacs "30.1") (org "9.6"))
;; Version: 0.9

;;; Commentary:
;; This package injects surrounding source blocks into the `org-edit-special'
;; buffer to provide context for LSP servers (Eglot).
;;
;; IT IS OPTIMIZED FOR PERFORMANCE AND STABILITY:
;; 1. Unified Collector: Robustly handles Property inheritance and Tangle targets.
;; 2. Lazy Execution: Strictly ignores transient calls (indentation/export).
;; 3. Marker-Based Tracking: Uses markers to track context boundaries safely.
;; 4. Safe Exit: Deletes injected context *before* Org writes back to file.
;; 5. Extension Mapping: Handles ":tangle yes" and custom filenames correctly.
;; 6. Deduplication: Strictly excludes the current block from context.

;;; Code:

(require 'org)
(require 'ob)
(require 'ob-tangle)
(require 'org-src)
(require 'cl-lib)

(defgroup org-src-context nil
  "Provide LSP support in org-src buffers."
  :group 'org)

(defcustom org-src-context-narrow-p t
  "Non-nil means org-src buffers should be narrowed to the editable block.
We recommend T to keep the context visual noise low, while still allowing
LSP to 'see' the invisible text."
  :type 'boolean
  :group 'org-src-context)

(defcustom org-src-context-max-filesize 500000
  "Max size (in bytes) of an Org file to attempt context collection.
If the buffer is larger than this, context injection is skipped to prevent lag."
  :type 'integer
  :group 'org-src-context)

;; Internal markers to track the injected regions
(defvar-local org-src-context--head-marker nil)
(defvar-local org-src-context--tail-marker nil)

(defun org-src-context--get-context-blocks (src-info)
  "Collect relevant context blocks (PREV . NEXT) for SRC-INFO.
Uses a unified strategy: scan buffer and match Language + Tangle target."
  (let* ((target-lang (nth 0 src-info))
         (target-params (nth 2 src-info))
         (target-tangle (or (alist-get :tangle target-params) "no"))
         ;; Use Buffer Position, not Line Number, for accurate comparison
         (current-blk-start (nth 5 src-info)))

    ;; Optimization: Safety check for massive files
    (if (> (buffer-size) org-src-context-max-filesize)
        (progn
          (message "Org-Src-Context: File too large (%d bytes), skipping context." (buffer-size))
          nil)

      (let ((prev nil)
            (next nil))
        ;; Scan entire buffer for matching blocks
        (org-babel-map-src-blocks (buffer-file-name)
          (let* ((info (org-babel-get-src-block-info 'light))
                 (lang (nth 0 info))
                 (params (nth 2 info))
                 (tangle (or (alist-get :tangle params) "no"))
                 (blk-start (nth 5 info)))

            ;; MATCHING LOGIC:
            ;; 1. Language must match
            ;; 2. Tangle target must match (handles "no" vs "no", or "A.py" vs "A.py")
            (when (and (string= lang target-lang)
                       (string= tangle target-tangle))

              ;; DEDUPLICATION LOGIC:
              ;; Compare start positions to determine Prev/Current/Next
              (when (and (integerp blk-start) (integerp current-blk-start))
                (cond
                 ;; Previous Block
                 ((< blk-start current-blk-start)
                  (push info prev))
                 ;; Next Block
                 ((> blk-start current-blk-start)
                  (push info next))
                 ;; Current Block (Equal) -> Ignore/Exclude
                 (t nil))))))

        ;; org-babel-map-src-blocks traverses in order, so 'prev' is reversed by push
        ;; 'next' is also reversed, but we pushed them in order.
        ;; Actually, map traverses top-down.
        ;; 1. Block A (pushed to prev) -> prev: (A)
        ;; 2. Block B (pushed to prev) -> prev: (B A)
        ;; So we need to reverse PREV.
        ;; Next blocks are pushed: C, D. -> next: (D C). Wait.
        ;; If we traverse C, we push C. next: (C).
        ;; If we traverse D, we push D. next: (D C).
        ;; So both need reversing to match file order.
        (cons (nreverse prev) (nreverse next))))))

(defun org-src-context--format-block (block)
  "Format a BLOCK list into a string for injection."
  (let ((body (nth 1 block)))
    (if (stringp body)
        (concat body "\n")
      "")))

(defun org-src-context--inject (prev-blocks next-blocks)
  "Inject PREV-BLOCKS and NEXT-BLOCKS into the current (edit) buffer."
  (let ((inhibit-read-only t)
        (inhibit-modification-hooks t))

    (save-excursion
      ;; 1. INJECT HEADER (Previous blocks)
      (goto-char (point-min))
      (let ((start (point)))
        (dolist (b prev-blocks)
          (insert (org-src-context--format-block b)))

        (unless (= start (point))
          (insert "\n") ;; Separator
          ;; CRITICAL: rear-nonsticky '(read-only) ensures that if user is at
          ;; the start of THEIR code and backspaces, they don't get read-only error.
          (add-text-properties start (point)
                               '(read-only t
                                           font-lock-face shadow
                                           front-sticky t
                                           rear-nonsticky (read-only)
                                           org-src-context-block t))))

      ;; Set marker at the end of header
      (setq org-src-context--head-marker (point-marker))
      (set-marker-insertion-type org-src-context--head-marker nil) ;; Stay before inserted text

      ;; 2. INJECT FOOTER (Next blocks)
      (goto-char (point-max))
      (let ((start (point)))
        (unless (bolp) (insert "\n")) ;; Ensure newline before footer
        (dolist (b next-blocks)
          (insert (org-src-context--format-block b)))

        (when (> (point) start)
          ;; CRITICAL: front-sticky nil ensures that if user is at the end
          ;; of THEIR code and types (or RET), the new text is NOT read-only.
          (add-text-properties start (point)
                               '(read-only t
                                           font-lock-face shadow
                                           front-sticky nil
                                           rear-nonsticky t
                                           org-src-context-block t))))

      ;; Set marker at the start of footer (End of editable area)
      (let ((p (point-max)))
        ;; Scan backwards to find where the footer starts
        (while (and (> p (point-min))
                    (get-text-property (1- p) 'org-src-context-block))
          (setq p (1- p)))
        (setq org-src-context--tail-marker (copy-marker p)))

      (set-marker-insertion-type org-src-context--tail-marker t))) ; Move with text insertion

  ;; NARROW
  (when org-src-context-narrow-p
    (when (and org-src-context--head-marker org-src-context--tail-marker)
      (narrow-to-region org-src-context--head-marker org-src-context--tail-marker))))

(defun org-src-context--setup-lsp (info original-dir original-file)
  "Configure buffer-local variables so Eglot can find the root."
  (let* ((lang (nth 0 info))
         (params (nth 2 info))
         (tangle-file (alist-get :tangle params))
         ;; Determine correct extension for the language (e.g. python -> py)
         (lang-ext (or (cdr (assoc lang org-babel-tangle-lang-exts)) "txt"))
         (file-ext (if (and tangle-file
                            (not (member tangle-file '("yes" "no")))
                            (file-name-extension tangle-file))
                       (file-name-extension tangle-file)
                     lang-ext))
         ;; If no tangle file, create a dummy based on the org file name + correct ext
         (mock-name (if (and tangle-file
                           (not (member tangle-file '("yes" "no"))))
                        tangle-file
                      (concat (file-name-base original-file) "_src." file-ext))))

    ;; Set the Default Directory to the project root (or org file dir)
    (setq-local default-directory original-dir)

    ;; Set a fake file name. This is CRITICAL for Eglot to find .git/
    (setq-local buffer-file-name (expand-file-name mock-name original-dir))

    ;; Force Eglot to recognize the mode if needed
    (when (fboundp 'eglot-ensure)
      (eglot-ensure))))

(defun org-src-context--advice (orig-fn &rest args)
  "Advice for `org-edit-src-code'.
1. Checks if this is a real edit (not indentation/export).
2. Collects context from the Org buffer.
3. Runs the edit buffer creation.
4. Injects context and sets up LSP."

  ;; ROBUSTNESS CHECK: Ignore transient calls (indentation/export)
  (if (car args)
      (apply orig-fn args)

    ;; REAL EDIT SESSION
    (let* ((datum (org-element-context))
           (type (org-element-type datum)))
      (if (not (eq type 'src-block))
          (apply orig-fn args)

        (let* ((info (org-babel-get-src-block-info 'light))
               ;; 1. Collect Context (While still in Org Buffer)
               ;; This will respect inherited PROPERTIES automatically.
               (context-blocks (org-src-context--get-context-blocks info))
               (orig-dir default-directory)
               (orig-file (buffer-file-name)))

          ;; 2. Create Edit Buffer
          (apply orig-fn args)

          ;; 3. We are now in the Edit Buffer. Inject!
          (when context-blocks
            (org-src-context--inject (car context-blocks) (cdr context-blocks)))

          ;; 4. Setup LSP Mocking
          (org-src-context--setup-lsp info orig-dir orig-file))))))

(defun org-src-context--cleanup (&rest _)
  "Clean up narrowing and markers before exiting or aborting."
  (let ((inhibit-read-only t)
        (inhibit-modification-hooks t))
    (ignore-errors
      (widen)
      (when (and org-src-context--head-marker (marker-buffer org-src-context--head-marker))
        (delete-region (point-min) org-src-context--head-marker)
        (set-marker org-src-context--head-marker nil))

      (when (and org-src-context--tail-marker (marker-buffer org-src-context--tail-marker))
        (delete-region org-src-context--tail-marker (point-max))
        (set-marker org-src-context--tail-marker nil)))))

;;;###autoload
(define-minor-mode org-src-context-mode
  "Global mode to inject context into Org Src buffers for LSP.

**Performance Optimized** version by Ahsanur Rahman."
  :global t
  :group 'org-src-context
  (if org-src-context-mode
      (progn
        (advice-add 'org-edit-src-code :around #'org-src-context--advice)
        (advice-add 'org-edit-src-exit :before #'org-src-context--cleanup)
        (advice-add 'org-edit-src-abort :before #'org-src-context--cleanup))
    (advice-remove 'org-edit-src-code #'org-src-context--advice)
    (advice-remove 'org-edit-src-exit #'org-src-context--cleanup)
    (advice-remove 'org-edit-src-abort #'org-src-context--cleanup)))

(provide 'org-src-context)
;;; org-src-context.el ends here
