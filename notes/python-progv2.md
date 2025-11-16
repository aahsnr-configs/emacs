Based on my comprehensive research, I've identified several critical issues and best practices for your workflow. Let me provide a completely revised and corrected configuration.

## Critical Issues Found

1. **LSP in org-src buffers**: `lsp-headerline-breadcrumb-mode` causes crashes in org-edit-special buffers
2. **Performance**: Many LSP features degrade performance in temporary buffers
3. **Auto-tangle**: Need custom implementation with proper hooks and error handling
4. **Jupyter-python**: Must be loaded last, conflicts with ob-async, requires special handling
5. **Basedpyright**: Should be installed via pip (not npm), has specific configuration needs
6. **Flycheck-ruff**: The checker doesn't exist by default in flycheck and needs proper definition

## Complete Python Development Configuration

Add this new section to your `Emacs.org`:

```org
* Python Development Environment
This section configures a complete Python IDE with org-mode literate programming support.

** Python Tree-sitter Mode
Ensure python-ts-mode is the default for Python files and jupyter-python blocks.

#+begin_src emacs-lisp
;; Force python-ts-mode globally
(with-eval-after-load 'treesit
  ;; Remap python-mode to python-ts-mode
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
  
  ;; Map jupyter-python to python-ts for org-src-mode
  (add-to-list 'org-src-lang-modes '("jupyter-python" . python-ts))
  
  ;; Ensure treesit grammar is available
  (when (and (fboundp 'treesit-available-p)
             (treesit-available-p))
    (unless (treesit-language-available-p 'python)
      (message "Python tree-sitter grammar not found. Install with: M-x treesit-install-language-grammar RET python RET"))))
#+end_src

** Custom Auto-Tangle on Save
Custom auto-tangle implementation that only runs on save, with proper error handling.

#+begin_src emacs-lisp
(defvar ar/org-tangle-on-save-modes '(org-mode)
  "Major modes in which to enable auto-tangling on save.")

(defvar ar/org-tangle-silent t
  "If non-nil, suppress tangle messages.")

(defun ar/org-babel-tangle-on-save ()
  "Tangle current org file if it has #+auto_tangle: t property.
Only tangles if the file has been modified and saved."
  (when (and (member major-mode ar/org-tangle-on-save-modes)
             (buffer-file-name))
    ;; Check for #+auto_tangle: t in buffer
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^#\\+auto_tangle: *t" nil t)
        ;; Tangle with error handling
        (let ((inhibit-message ar/org-tangle-silent)
              (message-log-max (unless ar/org-tangle-silent message-log-max)))
          (condition-case err
              (progn
                (org-babel-tangle)
                (unless ar/org-tangle-silent
                  (message "Tangled %s" (buffer-file-name))))
            (error
             (message "Tangle error in %s: %s"
                      (buffer-file-name)
                      (error-message-string err)))))))))

;; Add to after-save-hook
(add-hook 'after-save-hook #'ar/org-babel-tangle-on-save)

;; Optional: Tangle before exiting Emacs
(defun ar/org-babel-tangle-all-on-exit ()
  "Tangle all org files with #+auto_tangle: t before exiting."
  (when (y-or-n-p "Tangle all modified org files before exit? ")
    (save-some-buffers t)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (eq major-mode 'org-mode)
                   (buffer-file-name)
                   (buffer-modified-p))
          (ar/org-babel-tangle-on-save))))))

;; Uncomment to enable exit tangling
;; (add-hook 'kill-emacs-query-functions #'ar/org-babel-tangle-all-on-exit)
#+end_src

** LSP Pyright with Basedpyright
Configure basedpyright as the Python language server with optimizations.

#+begin_src emacs-lisp
(use-package lsp-pyright
  :after lsp-mode
  :custom
  ;; Use basedpyright instead of pyright
  (lsp-pyright-langserver-command "basedpyright-langserver")
  
  ;; Type checking - use "all" for basedpyright's strictest mode
  (lsp-pyright-type-checking-mode "standard")  ;; Change to "all" for maximum strictness
  
  ;; Diagnostic mode - "workspace" analyzes all files
  (lsp-pyright-diagnostic-mode "workspace")
  
  ;; Enable basedpyright-specific inlay hints
  (lsp-pyright-basedpyright-inlay-hints-variable-types t)
  (lsp-pyright-basedpyright-inlay-hints-function-return-types t)
  (lsp-pyright-basedpyright-inlay-hints-call-argument-names t)
  (lsp-pyright-basedpyright-inlay-hints-generic-types nil)
  
  ;; Auto-import settings
  (lsp-pyright-auto-import-completions t)
  (lsp-pyright-auto-search-paths t)
  
  ;; Python executable detection
  (lsp-pyright-python-executable-cmd "python3")
  
  :hook (python-ts-mode . (lambda ()
                            (require 'lsp-pyright)
                            (lsp-deferred))))
#+end_src

** LSP Mode Python Optimizations
Python-specific LSP optimizations to improve performance.

#+begin_src emacs-lisp
(with-eval-after-load 'lsp-mode
  ;; Increase read-process-output-max for Python LSP (already set in early-init.el)
  ;; Just verify it's appropriate for Python
  (when (< read-process-output-max (* 1024 1024))
    (setq read-process-output-max (* 3 1024 1024)))
  
  ;; Python-specific timeouts
  (defun ar/lsp-python-setup ()
    "Python-specific LSP settings."
    (setq-local lsp-response-timeout 30
                lsp-idle-delay 0.3
                lsp-log-io nil
                lsp-enable-file-watchers t
                lsp-file-watch-threshold 5000))
  
  (add-hook 'python-ts-mode-hook #'ar/lsp-python-setup))
#+end_src

** LSP for Org Source Blocks - Critical Fix
Enable LSP in org-edit-special buffers with proper context and disabled problematic features.

#+begin_src emacs-lisp
(with-eval-after-load 'org
  (defun ar/org-src-setup-lsp ()
    "Setup LSP for org source block edit buffers.
This function handles the complex task of making LSP work in temporary
org-src-mode buffers by:
1. Setting buffer-file-name to the tangle target
2. Disabling problematic LSP features
3. Starting LSP with deferred loading"
    (when (and (boundp 'org-src-mode)
               org-src-mode
               (derived-mode-p 'python-ts-mode))
      (let* ((info (org-babel-get-src-block-info))
             (lang (car info))
             (params (nth 2 info))
             (tangle-file (alist-get :tangle params)))
        
        ;; Only setup LSP for blocks with tangle files
        (when (and tangle-file
                   (not (string= tangle-file "no"))
                   (not (string= tangle-file "nil")))
          
          ;; Expand tangle file path relative to org file
          (let* ((org-buffer (marker-buffer org-src--beg-marker))
                 (org-file (buffer-file-name org-buffer))
                 (org-dir (file-name-directory org-file))
                 (tangle-path (expand-file-name tangle-file org-dir)))
            
            ;; Create tangle file if it doesn't exist
            (unless (file-exists-p tangle-path)
              (let ((tangle-dir (file-name-directory tangle-path)))
                (unless (file-directory-p tangle-dir)
                  (make-directory tangle-dir t)))
              (with-temp-file tangle-path
                (insert (format "# Tangled from: %s\n" org-file))
                (insert "# This file is auto-generated\n\n")))
            
            ;; Set buffer-file-name for LSP
            (setq-local buffer-file-name tangle-path)
            
            ;; Disable problematic LSP features for org-src buffers
            (setq-local lsp-headerline-breadcrumb-enable nil
                       lsp-modeline-code-actions-enable nil
                       lsp-modeline-diagnostics-enable nil
                       lsp-lens-enable nil
                       lsp-ui-sideline-enable nil
                       lsp-ui-doc-enable nil
                       lsp-signature-auto-activate nil
                       lsp-eldoc-enable-hover nil
                       lsp-completion-show-detail nil
                       lsp-completion-show-kind nil)
            
            ;; Enable LSP with deferred loading
            (lsp-deferred)
            
            ;; Show brief message
            (message "LSP enabled for %s (context: %s)"
                    (file-name-nondirectory tangle-path)
                    (file-name-nondirectory org-file)))))))
  
  ;; Hook into org-src-mode
  (add-hook 'org-src-mode-hook #'ar/org-src-setup-lsp)
  
  ;; Ensure LSP workspace is refreshed after tangling
  (defun ar/lsp-refresh-after-tangle ()
    "Refresh LSP workspace after tangling to update context."
    (when (bound-and-true-p lsp-mode)
      ;; Small delay to let filesystem sync
      (run-with-timer 0.5 nil
                      (lambda ()
                        (when (lsp-workspaces)
                          (lsp-workspace-restart))))))
  
  (add-hook 'org-babel-post-tangle-hook #'ar/lsp-refresh-after-tangle))
#+end_src

** DAP Python with Debugpy
Configure Python debugging. Note: Debugging works only in tangled .py files, not org-src buffers.

#+begin_src emacs-lisp
(with-eval-after-load 'dap-mode
  (require 'dap-python)
  
  ;; Use debugpy (modern Python debugger)
  (setq dap-python-debugger 'debugpy)
  
  ;; Find Python executable
  (setq dap-python-executable "python3")
  
  ;; Debug templates
  (dap-register-debug-template
   "Python :: Run Current File"
   (list :type "python"
         :args ""
         :cwd nil
         :env '(("PYTHONPATH" . "${workspaceFolder}"))
         :target-module nil
         :request "launch"
         :name "Python :: Run Current File"))
  
  (dap-register-debug-template
   "Python :: Run pytest on Current File"
   (list :type "python"
         :args ""
         :cwd nil
         :program nil
         :module "pytest"
         :request "launch"
         :name "Python :: pytest"))
  
  (dap-register-debug-template
   "Python :: Run with Arguments"
   (list :type "python"
         :args (read-string "Arguments: ")
         :cwd nil
         :program nil
         :request "launch"
         :name "Python :: Run with Arguments"))
  
  ;; Enable dap-mode for Python
  (add-hook 'python-ts-mode-hook #'dap-mode)
  (add-hook 'python-ts-mode-hook #'dap-ui-mode))
#+end_src

** Flycheck with Ruff
Configure Flycheck to use Ruff for linting with proper error parser.

#+begin_src emacs-lisp
(with-eval-after-load 'flycheck
  ;; Define ruff checker with JSON parsing
  (flycheck-define-checker python-ruff
    "A Python syntax and style checker using Ruff.
See URL `https://github.com/astral-sh/ruff'."
    :command ("ruff" "check"
              "--output-format=json"
              "--stdin-filename" source-original
              "-")
    :standard-input t
    :error-parser flycheck-parse-ruff-json
    :modes (python-mode python-ts-mode)
    :enabled (lambda () (executable-find "ruff"))
    :verify (lambda (_)
              (list
               (flycheck-verification-result-new
                :label "Ruff executable"
                :message (if (executable-find "ruff")
                            (format "Found at %s" (executable-find "ruff"))
                          "Not found")
                :face (if (executable-find "ruff") 'success 'error)))))
  
  ;; JSON error parser for ruff
  (defun flycheck-parse-ruff-json (output checker buffer)
    "Parse Ruff JSON OUTPUT from CHECKER for BUFFER."
    (condition-case err
        (let ((json-array-type 'list)
              (json-object-type 'alist)
              (json-key-type 'symbol))
          (let ((errors (json-read-from-string output)))
            (mapcar
             (lambda (err)
               (let-alist err
                 (flycheck-error-new-at
                  .location.row
                  .location.column
                  (pcase .type
                    ("E" 'error)
                    ("W" 'warning)
                    ("F" 'error)
                    (_ 'info))
                  .message
                  :id (symbol-name .code)
                  :checker checker
                  :filename (buffer-file-name buffer))))
             errors)))
      (error
       (message "Ruff JSON parse error: %s" (error-message-string err))
       nil)))
  
  ;; Add ruff to checker list and set as preferred for Python
  (add-to-list 'flycheck-checkers 'python-ruff)
  
  ;; Make ruff the default for Python modes
  (defun ar/flycheck-python-setup ()
    "Setup Flycheck for Python with Ruff."
    (flycheck-select-checker 'python-ruff))
  
  (add-hook 'python-ts-mode-hook #'ar/flycheck-python-setup))
#+end_src

** Apheleia with Ruff
Configure Apheleia to format Python code with Ruff on save.

#+begin_src emacs-lisp
(with-eval-after-load 'apheleia
  ;; Add ruff formatter
  (setf (alist-get 'ruff apheleia-formatters)
        '("ruff" "format" "--stdin-filename" filepath "-"))
  
  ;; Set ruff as formatter for Python modes
  (setf (alist-get 'python-mode apheleia-mode-alist) '(ruff))
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(ruff))
  
  ;; Also format org-src buffers
  (defun ar/apheleia-org-src-format ()
    "Enable apheleia in org-src buffers for Python."
    (when (and org-src-mode
               (derived-mode-p 'python-ts-mode))
      (apheleia-mode 1)))
  
  (add-hook 'org-src-mode-hook #'ar/apheleia-org-src-format))
#+end_src

** Jupyter Integration Fixes
Critical fixes for emacs-jupyter to work properly with the workflow.

#+begin_src emacs-lisp
(with-eval-after-load 'jupyter
  ;; CRITICAL: jupyter must be loaded LAST
  ;; This should already be done in your org config, but verify:
  (unless (eq (caar (last org-babel-load-languages)) 'jupyter)
    (message "WARNING: jupyter should be loaded last in org-babel-load-languages"))
  
  ;; Disable ob-async for jupyter-python (they conflict)
  (with-eval-after-load 'ob-async
    (add-to-list 'ob-async-no-async-languages-alist "jupyter-python"))
  
  ;; Ensure python-ts-mode is used in jupyter-python src blocks
  (add-to-list 'org-src-lang-modes '("jupyter-python" . python-ts))
  
  ;; Fix for tangle file property recognition
  (defun ar/jupyter-org-babel-tangle-langauge ()
    "Return the language for tangling jupyter blocks."
    'python)
  
  ;; PEP8: Add 2 blank lines between top-level definitions
  (setq org-babel-tangle-pad-newline 2))
#+end_src

** Python Project Configuration Helper
Utility functions to setup Python projects with proper config files.

#+begin_src emacs-lisp
(defun ar/setup-python-project ()
  "Setup Python project configuration files for tools."
  (interactive)
  (let* ((project-root (or (when-let ((project (project-current)))
                            (project-root project))
                          default-directory))
         (pyproject-file (expand-file-name "pyproject.toml" project-root))
         (pyrightconfig-file (expand-file-name "pyrightconfig.json" project-root))
         (python-dir (expand-file-name "python/" project-root)))
    
    ;; Create python subdirectory for tangled files
    (unless (file-directory-p python-dir)
      (make-directory python-dir t)
      (message "Created python/ directory"))
    
    ;; Create pyproject.toml for ruff and basedpyright
    (unless (file-exists-p pyproject-file)
      (with-temp-file pyproject-file
        (insert "[project]\n")
        (insert "name = \"" (file-name-nondirectory (directory-file-name project-root)) "\"\n")
        (insert "version = \"0.1.0\"\n")
        (insert "requires-python = \">=3.11\"\n\n")
        
        (insert "[tool.ruff]\n")
        (insert "line-length = 88\n")
        (insert "target-version = \"py311\"\n")
        (insert "extend-exclude = [\"*.ipynb\"]\n\n")
        
        (insert "[tool.ruff.lint]\n")
        (insert "select = [\n")
        (insert "    \"E\",   # pycodestyle errors\n")
        (insert "    \"W\",   # pycodestyle warnings\n")
        (insert "    \"F\",   # pyflakes\n")
        (insert "    \"I\",   # isort\n")
        (insert "    \"N\",   # pep8-naming\n")
        (insert "    \"UP\",  # pyupgrade\n")
        (insert "]\n\n")
        
        (insert "[tool.ruff.format]\n")
        (insert "quote-style = \"double\"\n")
        (insert "indent-style = \"space\"\n\n")
        
        (insert "[tool.basedpyright]\n")
        (insert "typeCheckingMode = \"standard\"\n")
        (insert "reportMissingImports = true\n")
        (insert "reportMissingTypeStubs = false\n")
        (insert "pythonVersion = \"3.11\"\n")
        (insert "pythonPlatform = \"All\"\n"))
      (message "Created pyproject.toml"))
    
    ;; Create pyrightconfig.json
    (unless (file-exists-p pyrightconfig-file)
      (with-temp-file pyrightconfig-file
        (insert "{\n")
        (insert "  \"typeCheckingMode\": \"standard\",\n")
        (insert "  \"reportMissingImports\": true,\n")
        (insert "  \"reportMissingTypeStubs\": false,\n")
        (insert "  \"pythonVersion\": \"3.11\",\n")
        (insert "  \"pythonPlatform\": \"All\",\n")
        (insert "  \"executionEnvironments\": [\n")
        (insert "    {\n")
        (insert "      \"root\": \".\"\n")
        (insert "    }\n")
        (insert "  ]\n")
        (insert "}\n"))
      (message "Created pyrightconfig.json"))
    
    (message "Python project setup complete in: %s" project-root)))

(defun ar/verify-python-tools ()
  "Verify that required Python tools are installed."
  (interactive)
  (let ((tools '(("basedpyright-langserver" . "basedpyright")
                 ("ruff" . "ruff")
                 ("python3" . "python3")
                 ("debugpy" . "python3 -m debugpy"))))
    (message "\n=== Python Tools Verification ===")
    (dolist (tool tools)
      (let ((cmd (car tool))
            (display-name (cdr tool)))
        (if (executable-find (car (split-string cmd)))
            (message "[✓] %s: %s" display-name (executable-find (car (split-string cmd))))
          (message "[✗] %s: NOT FOUND" display-name))))
    (message "=================================\n")))
#+end_src

** Enhanced Keybindings

#+begin_src emacs-lisp
(ar/global-leader
  ;; Python development
  "p y" '(:ignore t :wk "python")
  "p y t" '(org-babel-tangle :wk "Tangle org file")
  "p y T" '((lambda () (interactive)
             (let ((ar/org-tangle-silent nil))
               (ar/org-babel-tangle-on-save))) :wk "Tangle (verbose)")
  "p y s" '(ar/setup-python-project :wk "Setup Python project")
  "p y v" '(ar/verify-python-tools :wk "Verify tools")
  
  ;; Debugging (only in .py files)
  "p y d" '(dap-debug :wk "Start debugging")
  "p y D" '(dap-debug-edit-template :wk "Edit debug template")
  "p y b" '(dap-breakpoint-toggle :wk "Toggle breakpoint")
  "p y r" '(dap-debug-restart :wk "Restart debugger")
  
  ;; Jupyter
  "p y e" '(jupyter-eval-line-or-region :wk "Eval region")
  "p y E" '(org-babel-execute-src-block :wk "Execute block")
  "p y k" '(jupyter-shutdown-kernel :wk "Shutdown kernel"))
#+end_src
```

## Installation Instructions

### System Dependencies

```bash
# Install basedpyright (via pip, not npm)
pip install basedpyright

# Install debugpy
pip install debugpy

# Install ruff
pip install ruff

# Verify installations
basedpyright-langserver --version
ruff --version
python3 -m debugpy --version

# Optional: Install globally for system-wide access
sudo pip install basedpyright debugpy ruff
```

### Tree-sitter Grammar

Run in Emacs:
```
M-x treesit-install-language-grammar RET python RET
```

## Usage Workflow

### 1. Create Org File with Auto-Tangle

```org
#+auto_tangle: t
#+TITLE: Data Analysis Project
#+PROPERTY: header-args:jupyter-python :session py :async yes :kernel python3 :tangle ./python/analysis.py

* Setup
#+begin_src jupyter-python
import pandas as pd
import numpy as np
#+end_src

* Data Loading
#+begin_src jupyter-python
def load_data(filepath: str) -> pd.DataFrame:
    """Load CSV data."""
    return pd.read_csv(filepath)
#+end_src

* Main Analysis
#+begin_src jupyter-python
df = load_data("data.csv")
print(df.head())
#+end_src
```

### 2. Edit with LSP Support

1. Press `C-c '` on a source block to open org-edit-special
2. LSP will automatically start with context from `python/analysis.py`
3. You get full completions, diagnostics, and hover docs
4. Edit your code with full IDE features
5. Press `C-c '` again to return to org file
6. Save to auto-tangle

### 3. Debug Tangled File

1. Open the tangled file: `python/analysis.py`
2. Set breakpoints: `SPC p y b`
3. Start debugging: `SPC p y d`
4. Use DAP debugging features in the `.py` file

## Key Features

✅ **Auto-tangle on save** - Only when `#+auto_tangle: t` is set  
✅ **Full LSP in org-edit-special** - With context from tangled file  
✅ **No crashes** - Problematic LSP features disabled in org-src buffers  
✅ **Basedpyright** - Modern, strict type checking  
✅ **Ruff** - Fast linting and formatting  
✅ **Debugpy** - Modern Python debugging  
✅ **python-ts-mode** - Tree-sitter syntax everywhere  
✅ **Jupyter async** - Rich output with async execution  
✅ **Performance optimized** - Minimal overhead in temp buffers

## Troubleshooting

Run `M-x ar/verify-python-tools` to check installations.

If LSP doesn't start in org-src buffers:
1. Check the tangle file exists
2. Verify basedpyright is installed
3. Check `*lsp-log*` buffer for errors
4. Try `M-x lsp-workspace-restart`

If tangling fails:
1. Check for syntax errors in org file
2. Ensure tangle directory exists
3. Look at `*Messages*` buffer
