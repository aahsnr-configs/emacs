;;;Word Wrap Implementation
;; Custom variables
(defcustom ar/word-wrap-extra-indent 'double
  "Amount of extra indentation for wrapped code lines.
Values: 'double (2x indent), 'single (1x indent), or nil (no extra indent)."
  :type '(choice (const :tag "Double" double)
                 (const :tag "Single" single)
                 (const :tag "None" nil))
  :group 'ar-word-wrap)

(defcustom ar/word-wrap-fill-style nil
  "How to wrap long lines.
- nil: wrap at window edge
- 'soft: wrap at fill-column using visual-fill-column
- 'auto: use auto-fill-mode for new lines, soft-wrap existing"
  :type '(choice (const :tag "Window edge" nil)
                 (const :tag "Fill column (soft)" soft)
                 (const :tag "Auto (mixed)" auto))
  :group 'ar-word-wrap)

(defcustom ar/word-wrap-disabled-modes
  '(fundamental-mode so-long-mode special-mode compilation-mode)
  "Major modes where word-wrap should not be enabled."
  :type '(repeat symbol)
  :group 'ar-word-wrap)

(defcustom ar/word-wrap-text-modes
  '(text-mode markdown-mode gfm-mode org-mode rst-mode)
  "Major modes that are primarily text (no extra indent)."
  :type '(repeat symbol)
  :group 'ar-word-wrap)

(defcustom ar/word-wrap-visual-modes
  '(prog-mode conf-mode)
  "Modes where visual-line-mode should be used."
  :type '(repeat symbol)
  :group 'ar-word-wrap)

;; Internal variables
(defvar-local ar/word-wrap--prev-visual-line-mode nil)
(defvar-local ar/word-wrap--prev-adaptive-wrap-mode nil)
(defvar-local ar/word-wrap--major-mode-indent-var nil)

;; Core functions
(defun ar/word-wrap--in-comment-p ()
  "Return non-nil if point is in a comment."
  (or (nth 4 (syntax-ppss))
      (and (fboundp 'sp-point-in-comment)
           (sp-point-in-comment))))

(defun ar/word-wrap--in-string-p ()
  "Return non-nil if point is in a string."
  (nth 3 (syntax-ppss)))

(defun ar/word-wrap--major-mode-is-text-p ()
  "Return non-nil if current major mode is text-focused."
  (apply #'derived-mode-p ar/word-wrap-text-modes))

;; CRITICAL FIX: Check if we're in an org source block
(defun ar/word-wrap--in-org-src-block-p ()
  "Return non-nil if point is inside an org source block."
  (and (derived-mode-p 'org-mode)
       (org-in-src-block-p)))

(defun ar/word-wrap--calc-extra-indent ()
  "Calculate extra indentation for wrapped lines."
  ;; CRITICAL FIX: No extra indent in org source blocks or text modes
  (if (or (ar/word-wrap--major-mode-is-text-p)
          (ar/word-wrap--in-org-src-block-p))
      0
    (let ((base-indent (or (and (boundp 'evil-shift-width) evil-shift-width)
                          (bound-and-true-p tab-width)
                          2)))
      (pcase ar/word-wrap-extra-indent
        ('double (* 2 base-indent))
        ('single base-indent)
        (_ 0)))))

;; Adaptive wrap advice for context-aware indentation
(defun ar/word-wrap--adjust-extra-indent-a (orig-fun &rest args)
  "Add extra indent to wrapped lines in code, but not in comments/strings/org-src-blocks."
  (let ((extra-indent (if (or (ar/word-wrap--in-comment-p)
                              (ar/word-wrap--in-string-p)
                              (ar/word-wrap--major-mode-is-text-p)
                              (ar/word-wrap--in-org-src-block-p))
                          0
                        (ar/word-wrap--calc-extra-indent))))
    (let ((adaptive-wrap-extra-indent extra-indent))
      (apply orig-fun args))))

;; Main minor mode
(define-minor-mode ar/word-wrap-mode
  "Smart word wrapping with language-aware indentation.

Wrapped lines will be indented to match the preceding line.
In code buffers, lines not in strings/comments get additional
indentation per `ar/word-wrap-extra-indent'.

Long lines wrap at window margin by default, or can optionally
wrap at `fill-column' via `ar/word-wrap-fill-style'."
  :init-value nil
  :lighter " ↩"
  :group 'ar-word-wrap
  (if ar/word-wrap-mode
      ;; Enable
      (progn
        ;; Store previous states
        (setq ar/word-wrap--prev-visual-line-mode visual-line-mode
              ar/word-wrap--prev-adaptive-wrap-mode
              (bound-and-true-p adaptive-wrap-prefix-mode))
        
        ;; Get major mode indent variable
        (when (fboundp 'dtrt-indent--search-hook-mapping)
          (setq ar/word-wrap--major-mode-indent-var
                (caddr (dtrt-indent--search-hook-mapping major-mode))))
        
        ;; Apply adaptive wrap advice
        (advice-add #'adaptive-wrap-fill-context-prefix
                    :around #'ar/word-wrap--adjust-extra-indent-a)
        
        ;; Enable visual-line-mode
        (unless ar/word-wrap--prev-visual-line-mode
          (visual-line-mode +1))
        
        ;; Enable adaptive-wrap for smart indentation
        (unless ar/word-wrap--prev-adaptive-wrap-mode
          (require 'adaptive-wrap)
          ;; CRITICAL FIX: Set extra indent to 0 by default, let advice handle it
          (setq-local adaptive-wrap-extra-indent 0)
          (adaptive-wrap-prefix-mode +1))
        
        ;; Handle fill-column wrapping
        (pcase ar/word-wrap-fill-style
          ('soft
           (require 'visual-fill-column)
           (setq-local visual-fill-column-width fill-column
                       visual-fill-column-center-text nil
                       visual-fill-column-fringes-outside-margins nil)
           (visual-fill-column-mode +1))
          ('auto
           (require 'visual-fill-column)
           (setq-local visual-fill-column-width fill-column
                       visual-fill-column-center-text nil
                       visual-fill-column-fringes-outside-margins nil)
           (visual-fill-column-mode +1)
           (unless (bound-and-true-p auto-fill-function)
             (auto-fill-mode +1)))))
    
    ;; Disable
    (advice-remove #'adaptive-wrap-fill-context-prefix
                   #'ar/word-wrap--adjust-extra-indent-a)
    (unless ar/word-wrap--prev-adaptive-wrap-mode
      (adaptive-wrap-prefix-mode -1))
    (unless ar/word-wrap--prev-visual-line-mode
      (visual-line-mode -1))
    (when (bound-and-true-p visual-fill-column-mode)
      (visual-fill-column-mode -1))))

;; Global mode
(defun ar/word-wrap--enable-for-buffer ()
  "Enable word-wrap if appropriate for current buffer."
  (unless (or (minibufferp)
              (apply #'derived-mode-p ar/word-wrap-disabled-modes)
              (and (boundp 'special-mode)
                   (derived-mode-p 'special-mode)))
    (ar/word-wrap-mode +1)))

(define-globalized-minor-mode ar/global-word-wrap-mode
  ar/word-wrap-mode ar/word-wrap--enable-for-buffer
  :group 'ar-word-wrap)

;; Zen mode integration (optional)
(defun ar/word-wrap--setup-zen-mode ()
  "Configure word wrap for zen/writeroom mode."
  (when (bound-and-true-p writeroom-mode)
    (setq-local ar/word-wrap-fill-style 'soft)
    (ar/word-wrap-mode +1)))

(with-eval-after-load 'writeroom-mode
  (add-hook 'writeroom-mode-hook #'ar/word-wrap--setup-zen-mode))

;; Keybindings
(ar/global-leader
  "t w" '(ar/word-wrap-mode :wk "Toggle Word Wrap")
  "t W" '(ar/global-word-wrap-mode :wk "Toggle Global Word Wrap"))

;;; Solaire Mode
(use-package solaire-mode
  :hook (after-init . solaire-global-mode))
  :config
  (add-hook 'completion-list-mode-hook #'solaire-mode)
  (add-hook 'which-key-mode-hook #'solaire-mode)
  (add-hook 'help-mode-hook #'solaire-mode)
  (add-hook 'info-mode-hook #'solaire-mode)
  (add-hook 'org-src-mode-hook #'solaire-mode)
  (advice-add 'vertico--display-candidates :after
              (lambda (&rest _)
                (when (minibufferp)
                  (with-selected-window (minibuffer-window) (solaire-mode +1)))))


;;;General Behaviour
(defalias 'yes-or-no-p 'y-or-n-p)
(setq enable-recursive-minibuffers t)
(setq minibuffer-prompt-properties
      '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
(setq ring-bell-function 'ignore)
(setq visible-bell nil)
(setq sentence-end-double-space nil)
(setq require-final-newline t)
(setq scroll-error-top-bottom t)
(setq scroll-conservatively 101)
(setq scroll-margin 0)
(setq split-width-threshold 170
      split-height-threshold nil)
(setq uniquify-buffer-name-style 'forward)
(setq delete-by-moving-to-trash t)
(setq make-backup-files nil
      create-lockfiles nil)
(setq confirm-nonexistent-file-or-buffer nil)
(blink-cursor-mode -1)
(line-number-mode 1)
(setq column-number-indicator-zero-based nil)
(setq find-file-visit-truename t)
;; So Long mitigates slowness due to extremely long lines.
;; Currently available in Emacs master branch *only*!
(when (fboundp 'global-so-long-mode)
  (global-so-long-mode))

;; Add a newline automatically at the end of the file upon save.
(setq require-final-newline t)


;;; Editing and Indentation
;; Set default indentation.
(setq-default indent-tabs-mode nil
              tab-width 2
              fill-column 80)

;; Enable multi-line commenting.
(setq comment-multi-line t)
(setq comment-empty-lines t)

;; Continue wrapped lines at whitespace.
(setq-default word-wrap t)
;; But truncate lines by default for performance. (visual-line-mode will override this)
(setq-default truncate-lines t)

;; Improve `show-paren-mode` behavior.
(setq show-paren-delay 0.1
      show-paren-when-point-inside-paren t)
(setq blink-matching-paren nil) ; Don't blink, it's distracting.


;;; Frame title Formatting
(setq-default frame-title-format
              '(""
                "emacs"))

(when (fboundp 'pgtk-use-im-context)
  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (with-selected-frame frame
                (pgtk-use-im-context nil)))))

