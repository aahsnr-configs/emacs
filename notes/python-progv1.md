I'll search for information about integrating LSP with org-mode source blocks and the specific tools you mentioned.Based on my research, I'll provide you with a comprehensive Python development configuration that implements your org-mode literate programming workflow with LSP support. Here's what you need:

## New Section: Python Development Environment

Add this as a new section in your `Emacs.org` file:

```org
* Python Development Environment
** Python Tree-sitter Mode
Ensure python-ts-mode is used for Python buffers, including jupyter-python blocks.

#+begin_src emacs-lisp
;; Remap python-mode to python-ts-mode globally
(with-eval-after-load 'treesit
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
  
  ;; Ensure jupyter-python uses python-ts-mode for edit-special buffers
  (add-to-list 'org-src-lang-modes '("jupyter-python" . python-ts)))
#+end_src

** Org Auto-Tangle
Automatically tangle org files on save to keep Python files synchronized.

#+begin_src emacs-lisp
(use-package org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode)
  :custom
  (org-auto-tangle-default nil)  ;; Require explicit #+auto_tangle: t
  :config
  ;; Add to specific org files with: #+auto_tangle: t at the top
  (setq org-auto-tangle-babel-safelist
        '("~/path/to/your/python/projects/")))
#+end_src

To enable auto-tangling in your org files, add this at the top:
#+begin_example
#+auto_tangle: t
#+PROPERTY: header-args:jupyter-python :session py :async yes :kernel python3 :tangle ./project.py
#+end_example

** LSP Pyright with Basedpyright
Configure basedpyright as the Python language server.

#+begin_src emacs-lisp
(use-package lsp-pyright
  :defer t
  :after lsp-mode
  :custom
  ;; Use basedpyright instead of pyright
  (lsp-pyright-langserver-command "basedpyright")
  
  ;; Type checking configuration
  (lsp-pyright-type-checking-mode "standard")  ;; or "strict" for more rigorous checking
  
  ;; Enable inlay hints (basedpyright feature)
  (lsp-pyright-basedpyright-inlay-hints-variable-types t)
  (lsp-pyright-basedpyright-inlay-hints-function-return-types t)
  (lsp-pyright-basedpyright-inlay-hints-call-argument-names t)
  
  ;; Diagnostic mode - use "workspace" for full project analysis
  (lsp-pyright-diagnostic-mode "workspace")
  
  ;; Auto-import completions
  (lsp-pyright-auto-import-completions t)
  (lsp-pyright-auto-search-paths t)
  
  :hook (python-ts-mode . (lambda ()
                            (require 'lsp-pyright)
                            (lsp-deferred))))
#+end_src

** LSP for Org Source Blocks
Enable LSP in org-edit-special buffers with context from tangled files.

#+begin_src emacs-lisp
(with-eval-after-load 'org
  (defun ar/org-src-setup-lsp ()
    "Setup LSP for org source block edit buffers."
    (when (and (boundp 'org-src-mode) org-src-mode)
      (let* ((lang (car (org-babel-get-src-block-info)))
             (tangle-file (cdr (assq :tangle (nth 2 (org-babel-get-src-block-info))))))
        ;; Only setup LSP for Python blocks with tangle files
        (when (and (member lang '("jupyter-python" "python"))
                   tangle-file
                   (not (string= tangle-file "no")))
          ;; Ensure the tangled file exists
          (unless (file-exists-p tangle-file)
            (with-temp-file tangle-file
              (insert "# Auto-generated from " (buffer-file-name))))
          
          ;; Set buffer-file-name to tangled file for LSP context
          (setq-local buffer-file-name 
                     (expand-file-name tangle-file
                                      (file-name-directory 
                                       (buffer-file-name (buffer-base-buffer)))))
          
          ;; Enable LSP
          (when (and (fboundp 'lsp-deferred)
                     (derived-mode-p 'python-ts-mode))
            (lsp-deferred))))))
  
  ;; Hook into org-src-mode
  (add-hook 'org-src-mode-hook #'ar/org-src-setup-lsp))
#+end_src

** Enhanced Auto-Tangle with LSP Refresh
Refresh LSP workspace after tangling to update context.

#+begin_src emacs-lisp
(with-eval-after-load 'org-auto-tangle
  (defun ar/refresh-lsp-after-tangle ()
    "Refresh LSP workspace after tangling."
    (when (and (bound-and-true-p lsp-mode)
               (lsp-workspaces))
      ;; Give the file system a moment to sync
      (run-with-timer 0.5 nil
                      (lambda ()
                        (lsp-workspace-restart)))))
  
  (add-hook 'org-auto-tangle-after-tangle-hook #'ar/refresh-lsp-after-tangle))
#+end_src

** DAP Python with Debugpy
Configure Python debugging with debugpy.

#+begin_src emacs-lisp
(with-eval-after-load 'dap-mode
  (require 'dap-python)
  
  ;; Use debugpy instead of ptvsd
  (setq dap-python-debugger 'debugpy)
  
  ;; Auto-start dap-ui when debugging
  (add-hook 'dap-stopped-hook
            (lambda (arg) (call-interactively #'dap-hydra)))
  
  ;; Python debug templates
  (dap-register-debug-template
   "Python :: Run Current File"
   (list :type "python"
         :args ""
         :cwd nil
         :program nil
         :request "launch"
         :name "Python :: Run Current File"))
  
  (dap-register-debug-template
   "Python :: Run pytest"
   (list :type "python"
         :args ""
         :cwd nil
         :program nil
         :module "pytest"
         :request "launch"
         :name "Python :: Run pytest"))
  
  ;; Hook for python-ts-mode
  (add-hook 'python-ts-mode-hook #'dap-mode)
  (add-hook 'python-ts-mode-hook #'dap-ui-mode))
#+end_src

** Flycheck with Ruff
Configure Flycheck to use Ruff for Python linting.

#+begin_src emacs-lisp
(with-eval-after-load 'flycheck
  ;; Define ruff checker if not already defined
  (unless (flycheck-checker-get 'python-ruff 'command)
    (flycheck-define-checker python-ruff
      "A Python syntax and style checker using Ruff.
See URL `https://github.com/astral-sh/ruff'."
      :command ("ruff" "check"
                "--output-format=json"
                "--stdin-filename" source-original
                "-")
      :standard-input t
      :error-parser flycheck-parse-ruff
      :modes (python-mode python-ts-mode)
      :enabled (lambda () (executable-find "ruff"))))
  
  ;; Ruff JSON parser
  (defun flycheck-parse-ruff (output checker buffer)
    "Parse Ruff JSON output."
    (let ((errors (json-read-from-string output)))
      (mapcar (lambda (err)
                (let-alist err
                  (flycheck-error-new-at
                   .location.row
                   .location.column
                   (pcase .type
                     ("E" 'error)
                     ("W" 'warning)
                     (_ 'info))
                   .message
                   :id .code
                   :checker checker
                   :filename (buffer-file-name buffer))))
              errors)))
  
  ;; Set ruff as the primary Python checker
  (add-to-list 'flycheck-checkers 'python-ruff)
  
  ;; Configure python-ts-mode to use ruff
  (add-hook 'python-ts-mode-hook
            (lambda ()
              (setq-local flycheck-checker 'python-ruff))))
#+end_src

** Apheleia with Ruff
Configure Apheleia to format Python code with Ruff.

#+begin_src emacs-lisp
(with-eval-after-load 'apheleia
  ;; Add ruff formatter if not present
  (unless (alist-get 'ruff apheleia-formatters)
    (setf (alist-get 'ruff apheleia-formatters)
          '("ruff" "format" "--stdin-filename" filepath "-")))
  
  ;; Set ruff as the formatter for Python modes
  (setf (alist-get 'python-mode apheleia-mode-alist) '(ruff))
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(ruff)))
#+end_src

** Python Project Configuration Helper
Helper function to setup Python projects with proper configuration files.

#+begin_src emacs-lisp
(defun ar/setup-python-project ()
  "Setup Python project configuration files for LSP and tools."
  (interactive)
  (let* ((project-root (or (project-root (project-current))
                          default-directory))
         (pyproject-file (expand-file-name "pyproject.toml" project-root))
         (pyrightconfig-file (expand-file-name "pyrightconfig.json" project-root)))
    
    ;; Create pyproject.toml for ruff configuration
    (unless (file-exists-p pyproject-file)
      (with-temp-file pyproject-file
        (insert "[tool.ruff]\n")
        (insert "line-length = 88\n")
        (insert "target-version = \"py311\"\n\n")
        (insert "[tool.ruff.lint]\n")
        (insert "select = [\"E\", \"F\", \"W\", \"I\", \"N\"]\n\n")
        (insert "[tool.basedpyright]\n")
        (insert "typeCheckingMode = \"standard\"\n")
        (insert "reportMissingImports = true\n"))
      (message "Created pyproject.toml"))
    
    ;; Create pyrightconfig.json for basedpyright
    (unless (file-exists-p pyrightconfig-file)
      (with-temp-file pyrightconfig-file
        (insert "{\n")
        (insert "  \"typeCheckingMode\": \"standard\",\n")
        (insert "  \"reportMissingImports\": true,\n")
        (insert "  \"reportMissingTypeStubs\": false,\n")
        (insert "  \"pythonVersion\": \"3.11\"\n")
        (insert "}\n"))
      (message "Created pyrightconfig.json"))))

(ar/global-leader
  "p P" '(ar/setup-python-project :wk "Setup Python project"))
#+end_src

** Python-Specific LSP Optimizations

#+begin_src emacs-lisp
(with-eval-after-load 'lsp-mode
  ;; Python-specific LSP settings
  (setq lsp-pyright-venv-path "~/.pyenv/versions"  ;; Adjust for your setup
        lsp-pyright-python-executable-cmd "python3")
  
  ;; Increase timeouts for Python (helpful for large projects)
  (setq lsp-response-timeout 30)
  
  ;; Disable some features for better performance in org-edit-special
  (defun ar/lsp-org-src-optimizations ()
    "Optimize LSP for org source blocks."
    (when (and org-src-mode (derived-mode-p 'python-ts-mode))
      (setq-local lsp-enable-symbol-highlighting nil
                  lsp-lens-enable nil
                  lsp-headerline-breadcrumb-enable nil)))
  
  (add-hook 'org-src-mode-hook #'ar/lsp-org-src-optimizations))
#+end_src

** Keybindings for Python Development

#+begin_src emacs-lisp
(ar/global-leader
  "p" '(:ignore t :wk "python")
  "p t" '(org-babel-tangle :wk "Tangle org file")
  "p d" '(dap-debug :wk "Start debugging")
  "p D" '(dap-debug-edit-template :wk "Edit debug template")
  "p b" '(dap-breakpoint-toggle :wk "Toggle breakpoint")
  "p r" '(dap-debug-restart :wk "Restart debugger")
  "p e" '(jupyter-eval-line-or-region :wk "Eval with Jupyter")
  "p E" '(jupyter-eval-buffer :wk "Eval buffer with Jupyter"))
#+end_src
```

## System Dependencies

You'll need to install these tools on your system (not just in Emacs):

```bash
# Install basedpyright
pip install basedpyright

# Install debugpy
pip install debugpy

# Install ruff
pip install ruff

# Verify installations
which basedpyright
which ruff
python -c "import debugpy; print(debugpy.__version__)"
```

## Usage Workflow

1. **Create an Org file** with your Python code:

```org
#+auto_tangle: t
#+PROPERTY: header-args:jupyter-python :session py :async yes :kernel python3 :tangle ./analysis.py

* Data Analysis

#+begin_src jupyter-python
import pandas as pd
import numpy as np

def load_data(filepath):
    return pd.read_csv(filepath)
#+end_src

#+begin_src jupyter-python
df = load_data("data.csv")
print(df.head())
#+end_src
```

2. **Edit blocks**: Press `C-c '` (org-edit-special) to edit in a dedicated Python buffer with full LSP support

3. **Save to auto-tangle**: Saving the org file will automatically tangle to `analysis.py`

4. **Debug**: Open the tangled `analysis.py` file and use `SPC p d` to start debugging

## Key Features Enabled

- ✅ **Full LSP in org-edit-special buffers** with context from tangled file
- ✅ **Auto-tangling on save**
- ✅ **Basedpyright** for advanced type checking
- ✅ **Ruff** for fast linting and formatting
- ✅ **Debugpy** for debugging tangled files
- ✅ **python-ts-mode** for tree-sitter syntax
- ✅ **Jupyter-python blocks** with async execution

This configuration creates a seamless IDE experience where you write in org-mode blocks, get full LSP features while editing, and can debug the tangled Python files with the full context of your codebase.
