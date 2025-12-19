;;; org-src-context.el --- LSP and Context support for org-src buffers -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Ahsanur Rahman
;; Author: Ahsanur Rahman
;; Keywords: tools, languages, extensions, lsp
;; Package-Requires: ((emacs "30.1") (org "9.6"))
;; Version: 2.2.0

;;; Commentary:
;; This package injects surrounding source blocks into the `org-edit-special'
;; buffer to provide context for LSP servers (Eglot).
;;
;; IT IS OPTIMIZED FOR PERFORMANCE AND STABILITY:
;; 1. Lazy Execution: Strictly ignores transient calls (indentation/export).
;; 2. Marker-Based Tracking: Uses markers to track context boundaries safely.
;; 3. Safe Exit: Deletes injected context *before* Org writes back to file.
;; 4. Text Properties: Manages stickiness to prevent "ghost newlines".

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

(defun org-src-context--get-tangled-blocks (src-info)
  "Collect all blocks that belong to the same tangle target as SRC-INFO.
Returns a cons cell (PREVIOUS-BLOCKS . NEXT-BLOCKS).
This runs in the ORIGINAL Org buffer."
  (let* ((lang (nth 0 src-info))
         (params (nth 2 src-info))
         (tangle-file (alist-get :tangle params))
         (current-line (line-number-at-pos (point))))

    ;; Optimization 1: Safety check for massive files
    (if (> (buffer-size) org-src-context-max-filesize)
        (progn
          (message "Org-Src-Context: File too large (%d bytes), skipping context." (buffer-size))
          nil)

      ;; Optimization 2: Only proceed if tangling is explicitly enabled
      (if (or (null tangle-file) (string= tangle-file "no"))
          nil
        ;; Safe Collection: Catch errors during tangle collection
        (condition-case err
            (let* ((collector (org-babel-tangle-collect-blocks lang tangle-file))
                   ;; collector returns ((filename . ((head...)(head...))))
                   (blocks (if collector (cdr (car collector)) nil)))

              (if (not blocks)
                  nil
                ;; Split into before/after based on line number
                (cl-loop for block in blocks
                         ;; block structure: (start-line file lang body params ...)
                         for blk-line = (car block)
                         ;; We identify "current" block by line number proximity
                         if (< blk-line current-line)
                         collect block into prev
                         else if (> blk-line current-line)
                         collect block into next
                         finally return (cons prev next))))
          (error
           (message "Org-Src-Context: Error collecting blocks: %s" (error-message-string err))
           nil))))))

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
  (let* ((params (nth 2 info))
         (tangle-file (alist-get :tangle params))
         (lang-ext (file-name-extension (or tangle-file "code.txt")))
         ;; If no tangle file, create a dummy based on the org file name
         (mock-name (if (and tangle-file (not (string= tangle-file "no")))
                        tangle-file
                      (concat (file-name-base original-file) "_src." (or lang-ext "txt")))))

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

  ;; ROBUSTNESS CHECK: `org-edit-src-code' signature is (&optional context code edit-buffer-name)
  ;; We check the second argument `code`. If present, it's a transient operation.
  (if (nth 1 args)
      (apply orig-fn args)

    ;; REAL EDIT SESSION
    (let* ((datum (org-element-context))
           (type (org-element-type datum)))
      (if (not (eq type 'src-block))
          (apply orig-fn args)

        (let* ((info (org-babel-get-src-block-info 'light))
               ;; 1. Collect Context (While still in Org Buffer)
               (context-blocks (org-src-context--get-tangled-blocks info))
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
  "Clean up narrowing and markers before exiting or aborting.
This is CRITICAL: We must remove injected text before Org saves the buffer."
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
  "Global mode to inject context into Org Src buffers for LSP."
  :global t
  :group 'org-src-context
  (if org-src-context-mode
      (progn
        (advice-add 'org-edit-src-code :around #'org-src-context--advice)
        ;; Hook on BOTH exit and abort to ensure no garbage is left behind
        (advice-add 'org-edit-src-exit :before #'org-src-context--cleanup)
        (advice-add 'org-edit-src-abort :before #'org-src-context--cleanup))
    (advice-remove 'org-edit-src-code #'org-src-context--advice)
    (advice-remove 'org-edit-src-exit #'org-src-context--cleanup)
    (advice-remove 'org-edit-src-abort #'org-src-context--cleanup)))

(provide 'org-src-context)
;;; org-src-context.el ends here
