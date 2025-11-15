;;; python-dev-config.el --- Complete Python Development Setup for Emacs
;;; Commentary:
;; A comprehensive Python development configuration using:
;; - Eglot with basedpyright for LSP
;; - Flymake with Ruff for linting
;; - Apheleia with Ruff for formatting
;; - Full org-mode integration with session-based development
;; - Optimized for large codebases

;;; Code:

;;; ============================================================
;;; PACKAGE MANAGEMENT
;;; ============================================================

(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;;; ============================================================
;;; PERFORMANCE OPTIMIZATIONS (CRITICAL FOR LARGE CODEBASES)
;;; ============================================================

;; Increase garbage collection threshold for better performance
;; with LSP servers (default is ~800KB, we set to 100MB)
(setq gc-cons-threshold 100000000)

;; Increase the amount of data Emacs reads from processes
;; Critical for LSP performance (default is 4KB, we set to 1MB)
(setq read-process-output-max (* 1024 1024))

;; Reduce keystroke echo delay for more responsive typing
(setq echo-keystrokes 0.1)

;; Increase warning thresholds to avoid excessive GC warnings
(setq warning-minimum-level :error)

;;; ============================================================
;;; PYTHON MODE CONFIGURATION
;;; ============================================================

(use-package python
  :ensure nil  ; Built-in package
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :config
  ;; Set Python interpreter (adjust path as needed)
  (setq python-shell-interpreter "python3")
  
  ;; Indentation settings
  (setq python-indent-offset 4)
  (setq python-indent-guess-indent-offset nil)
  
  ;; Highlight indentation errors
  (setq python-indent-def-block-scale 1)
  
  ;; Enable automatic indentation
  (add-hook 'python-mode-hook #'electric-indent-local-mode))

;;; ============================================================
;;; EGLOT CONFIGURATION WITH BASEDPYRIGHT
;;; ============================================================

(use-package eglot
  :ensure t
  :hook ((python-mode . eglot-ensure)
         (python-ts-mode . eglot-ensure))
  :config
  ;; Performance settings
  (setq eglot-events-buffer-config '(:size 0))  ; Disable events logging
  (setq eglot-report-progress nil)              ; Disable progress reports
  (setq eglot-send-changes-idle-time 0.5)       ; Delay before sending changes
  (setq eglot-advertise-cancellation t)         ; Enable request cancellation
  (setq eglot-autoshutdown t)                   ; Shutdown server when buffer closed
  (setq eglot-sync-connect 1)                   ; Timeout for synchronous connection
  (setq eglot-connect-timeout 10)               ; Connection timeout
  
  ;; Disable unnecessary capabilities for better performance
  (setq eglot-ignored-server-capabilities 
        '(:documentHighlightProvider
          :inlayHintProvider
          :documentOnTypeFormattingProvider))
  
  ;; Configure basedpyright as the LSP server for Python
  (add-to-list 'eglot-server-programs
               '(python-mode . ("basedpyright-langserver" "--stdio")))
  (add-to-list 'eglot-server-programs
               '(python-ts-mode . ("basedpyright-langserver" "--stdio")))
  
  ;; Basedpyright workspace configuration
  ;; Adjust these settings based on your project needs
  (setq-default eglot-workspace-configuration
                '(:basedpyright
                  (:typeCheckingMode "basic"  ; Options: "off", "basic", "standard", "strict"
                   :useLibraryCodeForTypes t
                   :autoImportCompletions t
                   :diagnosticSeverityOverrides
                   (:reportUnusedVariable "warning"
                    :reportUnusedImport "warning")
                   :analysis
                   (:autoSearchPaths t
                    :diagnosticMode "openFilesOnly"  ; Only check open files for performance
                    :useLibraryCodeForTypes t
                    :typeCheckingMode "basic"))))
  
  ;; Disable inlay hints (they can clutter the UI and impact performance)
  (add-hook 'eglot-managed-mode-hook 
            (lambda () 
              (when (fboundp 'eglot-inlay-hints-mode)
                (eglot-inlay-hints-mode -1))))
  
  ;; Eldoc configuration for smoother documentation display
  (setq eldoc-echo-area-use-multiline-p nil)
  (setq eldoc-idle-delay 0.5)
  
  :bind (:map eglot-mode-map
              ("C-c l r" . eglot-rename)
              ("C-c l o" . eglot-code-action-organize-imports)
              ("C-c l a" . eglot-code-actions)
              ("C-c l f" . eglot-format-buffer)
              ("C-c l d" . eldoc-doc-buffer)
              ("C-c l h" . eldoc-print-current-symbol-info)))

;;; ============================================================
;;; EGLOT-BOOSTER FOR IMPROVED PERFORMANCE
;;; ============================================================

;; emacs-lsp-booster significantly improves LSP performance
;; Install it first: cargo install emacs-lsp-booster
;; Then install eglot-booster package

(use-package eglot-booster
  :ensure t
  :after eglot
  :config
  (eglot-booster-mode))

;;; ============================================================
;;; FLYMAKE WITH RUFF BACKEND
;;; ============================================================

(use-package flymake
  :ensure nil  ; Built-in
  :hook (python-mode . flymake-mode)
  :config
  ;; Display diagnostics faster
  (setq flymake-no-changes-timeout 0.5)
  (setq flymake-start-on-flymake-mode t)
  (setq flymake-show-diagnostics-at-end-of-line t)
  
  :bind (:map flymake-mode-map
              ("C-c ! n" . flymake-goto-next-error)
              ("C-c ! p" . flymake-goto-prev-error)
              ("C-c ! l" . flymake-show-buffer-diagnostics)
              ("C-c ! L" . flymake-show-project-diagnostics)))

(use-package flymake-ruff
  :ensure t
  :after (flymake eglot)
  :config
  ;; CRITICAL: Prevent Eglot from disabling Flymake backends
  ;; This allows both Eglot diagnostics and Ruff linting to coexist
  (add-to-list 'eglot-stay-out-of 'flymake)
  
  ;; Function to set up both Eglot and Ruff for Flymake
  (defun my/setup-flymake-backends ()
    "Configure Flymake to use both Eglot and Ruff backends."
    (when (derived-mode-p 'python-mode 'python-ts-mode)
      (flymake-mode 1)
      (flymake-ruff-load)))
  
  ;; Add to eglot-managed-mode-hook (not python-mode-hook)
  (add-hook 'eglot-managed-mode-hook #'my/setup-flymake-backends))

;;; ============================================================
;;; APHELEIA FOR AUTOMATIC FORMATTING WITH RUFF
;;; ============================================================

(use-package apheleia
  :ensure t
  :diminish apheleia-mode
  :config
  ;; Configure Ruff for Python formatting
  ;; Ruff handles both formatting and import sorting
  (setf (alist-get 'python-mode apheleia-mode-alist) 
        '(ruff-isort ruff))
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) 
        '(ruff-isort ruff))
  
  ;; Format on save settings
  (setq apheleia-remote-algorithm 'local)
  
  ;; Enable Apheleia globally or per-mode
  :hook ((python-mode . apheleia-mode)
         (python-ts-mode . apheleia-mode))
  
  :bind (("C-c f" . apheleia-format-buffer)))

;;; ============================================================
;;; COMPANY MODE FOR COMPLETION (Alternative to corfu)
;;; ============================================================

(use-package company
  :ensure t
  :diminish company-mode
  :hook (after-init . global-company-mode)
  :config
  ;; Completion settings optimized for LSP
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.2)  ; Delay before popup appears
  (setq company-show-quick-access t)
  (setq company-tooltip-align-annotations t)
  (setq company-tooltip-limit 10)
  (setq company-backends '((company-capf :with company-yasnippet)))
  
  ;; Better navigation
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "TAB") 'company-complete-selection)
  
  :bind (:map company-mode-map
              ("<tab>" . company-indent-or-complete-common)
              ("C-c y" . company-yasnippet)))

;;; ============================================================
;;; WHICH-KEY FOR KEYBINDING DISCOVERY
;;; ============================================================

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.5))

;;; ============================================================
;;; PROJECTILE FOR PROJECT MANAGEMENT
;;; ============================================================

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config
  (projectile-mode +1)
  (setq projectile-completion-system 'default)
  (setq projectile-enable-caching t)
  (setq projectile-indexing-method 'alien)
  
  :bind-keymap
  ("C-c p" . projectile-command-map)
  
  :bind (:map projectile-mode-map
              ("C-c p f" . projectile-find-file)
              ("C-c p s" . projectile-switch-project)))

;;; ============================================================
;;; ORG-MODE CONFIGURATION FOR LITERATE PYTHON DEVELOPMENT
;;; ============================================================

(use-package org
  :ensure nil  ; Built-in
  :mode ("\\.org\\'" . org-mode)
  :config
  ;; Enable Python and other languages in Babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (emacs-lisp . t)
     (shell . t)))
  
  ;; Don't ask for confirmation before evaluating code blocks
  (setq org-confirm-babel-evaluate nil)
  
  ;; Org-src settings for better code editing experience
  (setq org-src-window-setup 'current-window)
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  (setq org-edit-src-content-indentation 0)
  (setq org-src-preserve-indentation t)
  
  ;; Better code block templates
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  
  ;; Syntax highlighting in exported HTML/PDF
  (setq org-src-fontify-natively t)
  (setq org-highlight-latex-and-related '(native))
  
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link)
         :map org-mode-map
         ("C-c '" . org-edit-special)
         ("C-c C-c" . org-ctrl-c-ctrl-c)))

;;; ============================================================
;;; EGLOT SUPPORT IN ORG-SRC BUFFERS (SESSION-BASED APPROACH)
;;; ============================================================

(defvar my/org-src-session-files (make-hash-table :test 'equal)
  "Hash table mapping session names to temporary file paths.")

(defun my/org-babel-session-temp-file (session-name)
  "Get or create a temporary file for SESSION-NAME."
  (or (gethash session-name my/org-src-session-files)
      (let* ((temp-dir (file-name-as-directory
                        (expand-file-name "org-babel-sessions" temporary-file-directory)))
             (temp-file (expand-file-name
                        (format "%s.py" (md5 session-name))
                        temp-dir)))
        ;; Ensure directory exists
        (unless (file-exists-p temp-dir)
          (make-directory temp-dir t))
        ;; Store in hash table
        (puthash session-name temp-file my/org-src-session-files)
        temp-file)))

(defun my/org-babel-edit-prep:python (info)
  "Prepare Python source block for Eglot support.
This function is called automatically when editing Python source blocks.
It sets up buffer-file-name so Eglot can work properly."
  (let* ((params (nth 2 info))
         (session (alist-get :session params))
         (tangle-file (alist-get :tangle params)))
    (cond
     ;; If tangle is specified, use that file
     ((and tangle-file (not (string= tangle-file "no")))
      (setq-local buffer-file-name
                  (expand-file-name
                   (if (string= tangle-file "yes")
                       (concat (file-name-sans-extension 
                               (buffer-file-name (buffer-base-buffer)))
                              ".py")
                     tangle-file)))
      ;; Enable Eglot
      (eglot-ensure))
     
     ;; If session is specified, create a temp file for that session
     ((and session (not (string= session "none")))
      (let ((temp-file (my/org-babel-session-temp-file session)))
        (setq-local buffer-file-name temp-file)
        ;; Write current buffer content to temp file for context
        (write-region (point-min) (point-max) temp-file nil 'silent)
        ;; Enable Eglot
        (eglot-ensure)))
     
     ;; Default: create a unique temp file
     (t
      (let ((temp-file (make-temp-file "org-babel-python-" nil ".py")))
        (setq-local buffer-file-name temp-file)
        (eglot-ensure))))))

;; Hook to automatically prepare Python source blocks
(add-hook 'org-src-mode-hook
          (lambda ()
            (when (eq major-mode 'python-mode)
              (let ((info (org-babel-get-src-block-info)))
                (when info
                  (my/org-babel-edit-prep:python info))))))

;; Alternative: Manual command to enable Eglot in org-src buffers
(defun my/org-src-enable-eglot ()
  "Manually enable Eglot in current org-src buffer."
  (interactive)
  (when (and (bound-and-p 'org-src-mode) org-src-mode)
    (let ((info (with-current-buffer (org-src-source-buffer)
                  (org-babel-get-src-block-info))))
      (when info
        (my/org-babel-edit-prep:python info)))))

;; Keybinding for manual Eglot activation in org-src buffers
(define-key org-src-mode-map (kbd "C-c C-l") #'my/org-src-enable-eglot)

;;; ============================================================
;;; PYTHON VIRTUAL ENVIRONMENT SUPPORT
;;; ============================================================

(use-package pyvenv
  :ensure t
  :config
  ;; Automatically activate venv when entering Python projects
  (defun my/auto-activate-venv ()
    "Automatically activate Python virtual environment."
    (let ((venv-dir (or (locate-dominating-file default-directory ".venv")
                        (locate-dominating-file default-directory "venv"))))
      (when venv-dir
        (pyvenv-activate (expand-file-name 
                         (if (file-exists-p (expand-file-name ".venv" venv-dir))
                             ".venv"
                           "venv")
                         venv-dir)))))
  
  (add-hook 'python-mode-hook #'my/auto-activate-venv)
  
  :bind (("C-c v a" . pyvenv-activate)
         ("C-c v d" . pyvenv-deactivate)
         ("C-c v w" . pyvenv-workon)))

;;; ============================================================
;;; MAGIT FOR GIT INTEGRATION
;;; ============================================================

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-c g" . magit-file-dispatch)))

;;; ============================================================
;;; HELPFUL UTILITIES
;;; ============================================================

;; Better help buffers
(use-package helpful
  :ensure t
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h x" . helpful-command)))

;; Show trailing whitespace
(add-hook 'python-mode-hook
          (lambda ()
            (setq show-trailing-whitespace t)))

;; Delete trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; ============================================================
;;; PROJECT-SPECIFIC CONFIGURATION HELPER
;;; ============================================================

(defun my/create-python-project-config ()
  "Create a .dir-locals.el and pyproject.toml for Python project."
  (interactive)
  (let ((project-root (read-directory-name "Project root: " default-directory)))
    
    ;; Create .dir-locals.el
    (with-temp-file (expand-file-name ".dir-locals.el" project-root)
      (insert ";;; Directory Local Variables
;;; For more information see (info \"(emacs) Directory Variables\")

((python-mode . ((eglot-workspace-configuration
                  . (:basedpyright
                     (:typeCheckingMode \"basic\"
                      :useLibraryCodeForTypes t
                      :analysis
                      (:exclude [\"build\" \"dist\" \".venv\" \"venv\" \"__pycache__\"]
                       :autoSearchPaths t
                       :diagnosticMode \"openFilesOnly\")))))))
"))
    
    ;; Create pyproject.toml
    (unless (file-exists-p (expand-file-name "pyproject.toml" project-root))
      (with-temp-file (expand-file-name "pyproject.toml" project-root)
        (insert "[tool.ruff]
line-length = 88
target-version = \"py311\"

[tool.ruff.lint]
select = [\"E\", \"F\", \"I\", \"UP\", \"B\", \"SIM\"]
ignore = []

[tool.ruff.format]
quote-style = \"double\"
indent-style = \"space\"

[tool.basedpyright]
typeCheckingMode = \"basic\"
reportMissingTypeStubs = false
")))
    
    (message "Created Python project configuration in %s" project-root)))
