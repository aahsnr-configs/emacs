## Cleaned Python Development Configuration

```org
* Python Development Environment
** Python Tree-sitter Setup
#+begin_src emacs-lisp
;; python-ts-mode is already remapped in your treesit configuration
;; Just ensure jupyter-python uses python-ts for org-src-mode
(with-eval-after-load 'org-src
  (add-to-list 'org-src-lang-modes '("jupyter-python" . python-ts)))
#+end_src

** Custom Auto-Tangle on Save
#+begin_src emacs-lisp
(defcustom ar/org-auto-tangle-default nil
  "Enable auto-tangle by default for org files."
  :type 'boolean
  :group 'org-babel)

(defun ar/org-babel-tangle-on-save ()
  "Auto-tangle org file if #+auto_tangle: t is set."
  (when (and (eq major-mode 'org-mode)
             (buffer-file-name))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^[ \t]*#\\+auto_tangle:[ \t]+t" nil t)
        (let ((start-time (current-time)))
          (message "Tangling %s..." (buffer-name))
          (org-babel-tangle)
          (message "Tangled %s in %.2fs"
                   (buffer-name)
                   (float-time (time-since start-time))))))))

(add-hook 'after-save-hook #'ar/org-babel-tangle-on-save)
#+end_src

** LSP Pyright with Basedpyright
#+begin_src emacs-lisp
(use-package lsp-pyright
  :after lsp-mode
  :custom
  ;; CRITICAL: Correct command for basedpyright
  (lsp-pyright-langserver-command "basedpyright-langserver")
  
  ;; Use basedpyright's strict type checking
  (lsp-pyright-type-checking-mode "standard")
  (lsp-pyright-diagnostic-mode "workspace")
  
  ;; Basedpyright-specific features
  (lsp-pyright-basedpyright-inlay-hints-variable-types t)
  (lsp-pyright-basedpyright-inlay-hints-function-return-types t)
  (lsp-pyright-basedpyright-inlay-hints-call-argument-names t)
  
 
  :config
  (add-to-list 'lsp-pyright-multi-root nil)
  
  :hook (python-ts-mode . (lambda ()
                            (require 'lsp-pyright)
                            (lsp-deferred))))
#+end_src

** LSP for Org Source Blocks
#+begin_src emacs-lisp
(with-eval-after-load 'org-src
  (defun ar/org-src-setup-lsp ()
    "Enable LSP in org-edit-special buffers with tangle file context."
    (when (and org-src-mode
               (derived-mode-p 'python-ts-mode))
      (when-let* ((info (ignore-errors (org-babel-get-src-block-info)))
                  (params (nth 2 info))
                  (tangle-file (alist-get :tangle params)))
        
        ;; Only proceed if there's a valid tangle target
        (when (and tangle-file
                   (not (member tangle-file '("no" "nil" nil))))
          
          (let* ((org-buffer (marker-buffer org-src--beg-marker))
                 (org-file (buffer-file-name org-buffer))
                 (org-dir (file-name-directory org-file))
                 (tangle-path (expand-file-name tangle-file org-dir)))
            
            ;; Ensure tangle file and directory exist
            (let ((tangle-dir (file-name-directory tangle-path)))
              (unless (file-directory-p tangle-dir)
                (make-directory tangle-dir t)))
            
            (unless (file-exists-p tangle-path)
              (with-temp-file tangle-path
                (insert (format "# -*- coding: utf-8 -*-\n"))
                (insert (format "# Tangled from: %s\n\n" 
                               (file-name-nondirectory org-file)))))
            
            ;; Set buffer-file-name for LSP context
            (setq-local buffer-file-name tangle-path)
            
            ;; Disable problematic LSP features for temporary buffers
            (setq-local lsp-headerline-breadcrumb-enable nil
                       lsp-modeline-code-actions-enable nil
                       lsp-modeline-diagnostics-enable nil
                       lsp-lens-enable nil
                       lsp-signature-auto-activate nil)
            
            ;; Start LSP
            (lsp-deferred))))))
  
  (add-hook 'org-src-mode-hook #'ar/org-src-setup-lsp))
#+end_src

** DAP Python Configuration
#+begin_src emacs-lisp
(with-eval-after-load 'dap-mode
  (require 'dap-python)
  
  ;; Use debugpy (modern debugger)
  (setq dap-python-debugger 'debugpy)
  
  ;; Python executable
  (setq dap-python-executable "python3")
  
  ;; Debug templates
  (dap-register-debug-template
   "Python :: Run File"
   (list :type "python"
         :args ""
         :cwd nil
         :program nil
         :request "launch"
         :name "Python :: Run File"))
  
  (dap-register-debug-template
   "Python :: Run pytest"
   (list :type "python"
         :args ""
         :cwd nil
         :module "pytest"
         :request "launch"
         :name "Python :: pytest"))
  
  ;; Enable for python-ts-mode
  (add-hook 'python-ts-mode-hook #'dap-mode)
  (add-hook 'python-ts-mode-hook #'dap-ui-mode))
#+end_src

** Flycheck with Ruff
#+begin_src emacs-lisp
(with-eval-after-load 'flycheck
  (flycheck-define-checker python-ruff
    "Python checker using Ruff."
    :command ("ruff" "check"
              "--output-format" "json"
              "--stdin-filename" source-original
              "-")
    :standard-input t
    :error-parser (lambda (output checker buffer)
                    (condition-case nil
                        (let* ((json-array-type 'list)
                               (json-object-type 'alist)
                               (errors (json-read-from-string output)))
                          (mapcar
                           (lambda (err)
                             (let-alist err
                               (flycheck-error-new-at
                                .location.row
                                .location.column
                                (pcase .type
                                  ("E" 'error)
                                  ("W" 'warning)
                                  (_ 'info))
                                .message
                                :id (symbol-name .code)
                                :checker checker
                                :filename (buffer-file-name buffer))))
                           errors))
                      (error nil)))
    :modes (python-mode python-ts-mode))
  
  (add-to-list 'flycheck-checkers 'python-ruff)
  
  ;; Use ruff for python-ts-mode
  (add-hook 'python-ts-mode-hook
            (lambda ()
              (flycheck-select-checker 'python-ruff))))
#+end_src

** Apheleia with Ruff
#+begin_src emacs-lisp
(with-eval-after-load 'apheleia
  ;; Define ruff formatter
  (setf (alist-get 'ruff apheleia-formatters)
        '("ruff" "format" "--stdin-filename" filepath "-"))
  
  ;; Use for python modes
  (setf (alist-get 'python-mode apheleia-mode-alist) '(ruff))
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(ruff)))
#+end_src

** Jupyter Integration
#+begin_src emacs-lisp
;; CRITICAL: Ensure jupyter is loaded LAST
(with-eval-after-load 'jupyter
  ;; Verify jupyter is last in org-babel-load-languages
  (unless (eq (caar (last org-babel-load-languages)) 'jupyter)
    (warn "jupyter should be loaded last in org-babel-load-languages"))
  
  ;; CRITICAL: Disable ob-async for jupyter blocks
  (with-eval-after-load 'ob-async
    (setq ob-async-no-async-languages-alist 
          '("jupyter-python")))
  
  ;; Set proper python-ts-mode
  (add-to-list 'org-src-lang-modes '("jupyter-python" . python-ts)))
#+end_src

** Project Configuration Helper
#+begin_src emacs-lisp
(defun ar/setup-python-project ()
  "Setup Python project configuration files."
  (interactive)
  (let* ((project-root (or (when-let ((proj (project-current)))
                            (project-root proj))
                          default-directory))
         (pyproject-file (expand-file-name "pyproject.toml" project-root)))
    
    (unless (file-exists-p pyproject-file)
      (with-temp-file pyproject-file
        (insert "[tool.ruff]\n")
        (insert "line-length = 88\n")
        (insert "target-version = \"py311\"\n\n")
        
        (insert "[tool.ruff.lint]\n")
        (insert "select = [\"E\", \"F\", \"W\", \"I\", \"N\", \"UP\"]\n\n")
        
        (insert "[tool.ruff.format]\n")
        (insert "quote-style = \"double\"\n")
        (insert "indent-style = \"space\"\n\n")
        
        (insert "[tool.basedpyright]\n")
        (insert "typeCheckingMode = \"standard\"\n")
        (insert "reportMissingImports = true\n")
        (insert "reportMissingTypeStubs = false\n")
        (insert "pythonVersion = \"3.11\"\n"))
      (message "Created pyproject.toml"))))

(defun ar/verify-python-tools ()
  "Verify Python tools are installed."
  (interactive)
  (let ((tools '(("basedpyright-langserver" . "Basedpyright")
                 ("ruff" . "Ruff")
                 ("python3" . "Python"))))
    (message "\n=== Python Tools ===")
    (dolist (tool tools)
      (if (executable-find (car tool))
          (message "[✓] %s" (cdr tool))
        (message "[✗] %s NOT FOUND" (cdr tool))))
    (message "===================\n")))
#+end_src

** Keybindings
#+begin_src emacs-lisp
(ar/global-leader
  "p y" '(:ignore t :wk "python")
  "p y t" '(org-babel-tangle :wk "Tangle")
  "p y s" '(ar/setup-python-project :wk "Setup project")
  "p y v" '(ar/verify-python-tools :wk "Verify tools")
  "p y d" '(dap-debug :wk "Debug")
  "p y b" '(dap-breakpoint-toggle :wk "Breakpoint")
  "p y e" '(org-babel-execute-src-block :wk "Execute block"))
#+end_src
```

## Key Changes & Fixes

### Removed Redundancies:
1. **Removed duplicate `major-mode-remap-alist`** - Already in your treesit config
2. **Removed `read-process-output-max` setting** - Already in early-init.el
3. **Removed `lsp-keymap-prefix`** - Already configured in your lsp-mode setup
4. **Simplified tree-sitter setup** - Just add jupyter-python mapping

### Fixed Errors:
1. **Corrected basedpyright command** - `basedpyright-langserver` not `basedpyright`
2. **Fixed Flycheck ruff parser** - Proper error handling and JSON parsing
3. **Simplified auto-tangle** - Cleaner implementation without unused variables
4. **Fixed ob-async conflict** - Properly exclude jupyter-python
5. **Removed aggressive LSP refresh** - Not needed, causes more issues

### Critical Additions:
1. **Proper python-ts-mode support** in lsp-pyright
2. **Error handling** in all parsers and functions
3. **Directory creation** before tangling
4. **Verification function** to check tool installation

## Installation

```bash
# Install tools (in your Python environment)
pip install basedpyright debugpy ruff

# Verify
basedpyright-langserver --version
ruff --version
python3 -m debugpy --version
```

## Usage

Create org file with:
```org
#+auto_tangle: t
#+PROPERTY: header-args:jupyter-python :session py :async yes :kernel python3 :tangle ./python/analysis.py

* Analysis
#+begin_src jupyter-python
import pandas as pd
#+end_src
```

Then:
1. Save to auto-tangle
2. Press `C-c '` in block to edit with full LSP
3. Debug tangled `.py` file with `SPC p y d`
