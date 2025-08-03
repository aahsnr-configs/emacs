Of course. I have enhanced the `shackle` configuration to manage more window types for a more consistent UI and moved it to its logical home in the "Editor Behaviour" section. This makes `shackle` a core part of your general Emacs experience rather than just a tool for the debugger.

Below are the detailed instructions for updating your `config.txt` and the final, rewritten development environment configuration.

***

### Required Changes to `config.txt`

You will need to make edits in **two separate places** within your `config.txt` file.

#### 1. Update the `* Editor Behaviour` Section

In your `config.txt` file, find the `* Editor Behaviour` section. Inside this section, add the following enhanced configuration for `shackle`. It is designed to manage help, compilation, and other popups in addition to the debugger UI.

```el
** Shackle for Popup Window Management
This provides a declarative way to control where and how special-purpose buffers
appear, ensuring a consistent and predictable windowing layout.

#+begin_src emacs-lisp
(use-package shackle
  :init (shackle-mode)
  :custom
  (shackle-rules
   '(;; Rule for Help buffers
     ("\\`\\*Help" :align bottom :size 0.3)
     ;; Rule for compilation/grep/etc.
     ("^\\*.*compilation.*\\*$" :align bottom :size 0.3)
     ("^\\*grep.*\\*$" :align bottom :size 0.3)
     ;; Rule for Embark
     ("\\`\\*Embark Collect" :align bottom :size 0.25)
     ;; Rules for the debugger (dape)
     ("\\`\\*dap-repl" :align right :size 0.4)
     ("\\`\\*dap-locals" :align right :size 0.4)
     ("\\`\\*dap-breakpoints" :align right :size 0.4)
     ("\\`\\*dap-sessions" :align right :size 0.4))
   shackle-inhibit-window-quit-on-same-buffer t))
#+end_src
```

#### 2. Replace the Development Environment

You still need to perform the original replacement for the development environment itself:

*   **Remove the entire `* Development Environment` section** from `config.txt`.
*   **Remove the entire `* Envrc` section**.
*   **Remove the `** Popup Management: Taming Special Buffers` section** (as `shackle` now handles this).
*   **Add the rewritten configuration below** to the end of your `config.txt` file.

***

## The Definitive Emacs Development Environment (Jupyter-Enhanced)

This configuration provides a highly refined, project-aware development environment. It is built on Emacs's native tooling and is designed to create a first-class, Jupyter-like experience for Python development directly within Org mode, featuring automatic rich output for both tables and plots.

### 1. Core Environment: `envrc` and Language Server Protocol

The foundation is the `purcell/envrc` package, which provides robust, language-agnostic environment management. `Eglot` is the built-in LSP client that provides features like code intelligence, diagnostics, and navigation.

```el
;; Use the purcell/envrc package for project-specific environments.
;; This provides visual feedback in the modeline so you always know when it's active.
(use-package envrc
  :config
  (envrc-global-mode +1))

;; Eglot is the built-in LSP client. It will automatically use the correct
;; language server from the PATH set by your direnv-managed environment.
(use-package eglot
  :hook ((prog-mode . eglot-ensure)
         ;; Explicitly hook into org-src-mode for LSP in code blocks.
         (org-src-mode . eglot-ensure))
  :custom
  (eglot-autoshutdown t)
  :config
  ;; Associate python-ts-mode with pyright for best-in-class Python support.
  (add-to-list 'eglot-server-programs '(python-ts-mode . ("pyright-langserver" "--stdio"))))

;; Optional but highly recommended: Boost Eglot performance.
;; Requires `cargo install emacs-lsp-booster`
(use-package eglot-booster
  :after eglot
  :config
  (eglot-booster-mode))
```

### 2. Enhanced Documentation (Eldoc)

This section transforms the built-in `eldoc` into a modern, at-point documentation system that is both powerful and unobtrusive, displaying documentation in a clean popup.

```el
;; Display eldoc documentation in a popup frame at point.
(use-package eldoc-box
  :hook (eldoc-mode . eldoc-box-hover-mode)
  :custom
  ;; Only show the popup if there is actual documentation, not just a function name.
  (eldoc-box-show-if-no-doc nil)
  ;; Disable the default minibuffer display since we have a popup.
  (eldoc-echo-area-display-truncation-p nil)
  :custom-face
  ;; Style the popup to match the doom-tokyo-night theme.
  (eldoc-box-border ((t (:foreground "#3b4261"))))
  (eldoc-highlight-symbol-face ((t (:foreground "#7aa2f7" :weight bold)))))
```

### 3. Deep UI & Workflow Integration

This section seamlessly connects the development tools with `consult`, `embark`, and `xref`, making them feel like a single, unified system for code navigation and refactoring.

```el
;; Provides a `consult` interface for Flymake diagnostics.
(use-package consult-flymake
  :after (consult flymake))

;; Provides a `consult` interface for `xref` and configures xref to use it.
(use-package consult-xref
  :after (consult xref)
  :config
  (setq xref-show-definitions-function #'consult-xref-show-definitions)
  (setq xref-show-references-function #'consult-xref-show-references))

;; Integrates Eglot's code actions directly into your Embark workflow (`C-.`).
(use-package embark-eglot
  :after (embark eglot)
  :config (add-to-list 'embark-major-mode-map '(eglot-mode . eglot)))

;; Adds dispatcher for more precise filtering (e.g., =literal, %regexp).
(use-package orderless
  :custom
  (orderless-dispatchers
   '(orderless-consult-dispatch orderless-affix-dispatch))
  (orderless-component-separator #'orderless-escapable-split-on-space))
```

### 4. Robust Debugger UI (DApE)

We use `dape` for debugging. The user interface is cleanly managed by the enhanced `shackle` configuration you added to the `* Editor Behaviour` section.

```el
(use-package dape
  :commands (dape dape-debug-recent)
  :hook
  ;; Use GUD's tooltip mode for mouse-hover variable inspection.
  (dape-session-mode-hook . gud-tooltip-mode)
  :config
  ;; Persist breakpoints across Emacs sessions.
  (add-hook 'kill-emacs-hook #'dape-breakpoint-save)
  (add-hook 'after-init-hook #'dape-breakpoint-load))
```

### 5. Syntax Checking and Formatting

This setup uses the built-in `flymake` for live diagnostics and `apheleia` for automatic, on-save formatting, ensuring consistent code style.

```el
(use-package flymake
  :ensure nil
  :hook (prog-mode . flymake-mode)
  :custom
  (flymake-check-syntax-automatically '(save mode-enabled idle-change))
  (flymake-idle-change-delay 0.4))

(use-package flymake-posframe
  :after flymake
  :hook (flymake-mode . flymake-posframe-mode))

(use-package apheleia 
  :config
  (apheleia-global-mode +1)
  (add-hook 'before-save-hook #'apheleia-format-buffer))
```

### 6. Tree-sitter for Superior Syntax Analysis

This configuration prefers Tree-sitter-based major modes for modern syntax highlighting and structural editing. `python-ts-mode` is now the default for Python.

```el
(use-package treesit-auto
  :when (treesit-available-p)
  :config
  (setq treesit-auto-langs '(bash c cpp css python toml yaml rust go))
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package treesit-fold
  :hook (treesit-auto-mode-hook . treesit-fold-mode))

(use-package evil-textobj-tree-sitter
  :after evil
  :config
  (evil-textobj-tree-sitter-setup)
  (define-key evil-inner-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.inner"))
  (define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
  (define-key evil-inner-text-objects-map "c" (evil-textobj-tree-sitter-get-textobj "class.inner"))
  (define-key evil-outer-text-objects-map "c" (evil-textobj-tree-sitter-get-textobj "class.outer"))
  (define-key evil-inner-text-objects-map "C" (evil-textobj-tree-sitter-get-textobj "comment.inner"))
  (define-key evil-outer-text-objects-map "C" (evil-textobj-tree-sitter-get-textobj "comment.outer"))
  (define-key evil-inner-text-objects-map "p" (evil-textobj-tree-sitter-get-textobj "parameter.inner"))
  (define-key evil-outer-text-objects-map "p" (evil-textobj-tree-sitter-get-textobj "parameter.outer")))
```

### 7. Python & Jupyter for Org Mode

This section enables a first-class, stateful Python experience directly within Org mode, complete with rich, inline display for both tables (e.g., pandas DataFrames) and graphical plots.

```el
;; Provides Jupyter integration for Emacs.
(use-package jupyter
  :custom
  ;; Ensure Jupyter uses the Python from the project's virtual environment.
  (jupyter-python-executable-command (executable-find "python")))

;; Integrates Jupyter kernels with Org Babel for a rich, notebook-like experience.
(use-package ob-jupyter
  :after org
  :config
  ;; Ensure that inline images are displayed whenever an Org buffer is opened.
  (add-hook 'org-mode-hook 'org-display-inline-images)
  
  ;; Prioritize rich output formats to mimic JupyterLab.
  ;; This tells ob-jupyter to prefer HTML for tables (like pandas DataFrames) and
  ;; PNG for plots over plain text, automatically rendering them inline.
  (setq ob-jupyter-response-mime-type-priorities
        '("text/html"
          "image/png"
          "image/jpeg"
          "text/plain"))
  
  ;; Load the jupyter language into babel.
  (org-babel-do-load-languages 'org-babel-load-languages '((jupyter . t)))
  
  ;; Make python blocks use jupyter by default for a stateful, interactive experience.
  ;; Results are output as files (essential for plots) and stored in a dedicated directory.
  (add-to-list 'org-babel-default-header-args:python
               '((:session . "jupyter-python")
                 (:kernel . "python3")
                 (:results . "output file") ; Use "output file" for plots, ob-jupyter handles HTML tables automatically.
                 (:exports . "results")
                 (:dir . "./.jupyter-exports/")))
  (setq ob-jupyter-startup-timeout 30))

;; --- Define available Python Linting Backends ---
(with-eval-after-load 'flymake
  (flymake-define-backend 'ar/flymake-pylint
    '(:executable "pylint" :args (list (flymake-path-relativize (current-buffer)))
      :error-parser flymake-proc-legacy-flycheck-parse))
  (flymake-define-backend 'ar/flymake-flake8
    '(:executable "flake8" :args (list "--format=default" (flymake-path-relativize (current-buffer)))
      :error-parser flymake-proc-legacy-flycheck-parse)))

;; --- Debugging Functionality for both .py files and Org blocks ---
(defun ar/dape-debug-org-src-block ()
  "Tangle the current Org src block to a temp file and debug it."
  (interactive)
  (when (derived-mode-p 'org-mode)
    (let* ((tmp-file (make-temp-file "emacs-babel-" nil ".py"))
           (info (org-babel-get-src-block-info 'light)))
      (with-temp-file tmp-file (insert (nth 2 info)))
      (message "Debugging tangled block in %s" tmp-file)
      (dape--debug
       (lambda (template)
         (let ((new-template (copy-tree template)))
           (setf (plist-get new-template :program) tmp-file)
           new-template))
       "Python :: Run Current File (debugpy)"))))

(with-eval-after-load 'dape
  (dape-register-debug-template
   "Python :: Run Current File (debugpy)"
   '(:type "python" :request "launch" :justMyCode t
     :program #'buffer-file-name
     :cwd #'projectile-project-root)))
```

### Project Setup Summary

To set up a new Python project, follow these two steps in the project's root directory:

1.  **Create `.envrc`** and run `direnv allow` in your terminal.
    ```sh
    # .envrc
    layout python3
    source .direnv/bin/activate
    # Install all necessary tools inside the virtual environment
    # pip install jupyter_client ipykernel debugpy pyright pylint flake8 black pandas matplotlib
    ```
2.  **Create `.dir-locals.el`** to activate your chosen linters for the project. Note that `eglot` diagnostics are enabled by default.
    ```el
    ;; .dir-locals.el
    ((python-ts-mode . ((eval . (setq-local flymake-diagnostic-functions
                                        (append eglot-flymake-backend '(ar/flymake-pylint ar/flymake-flake8)))))))
    ```

### 8. Final Streamlined Keybindings

These keybindings provide ergonomic access to the entire toolchain, including new shortcuts for managing the Jupyter kernel, a core part of a notebook-style workflow.

```el
;; Org Jupyter Keybindings
(ar/global-leader
  "o" '(:ignore t :which-key "org")
  "o j" '(:ignore t :which-key "jupyter")
  "o j r" '(jupyter-restart-kernel :wk "restart kernel")
  "o j i" '(jupyter-interrupt-kernel :wk "interrupt kernel")
  "o j c" '(jupyter-connect-to-kernel :wk "connect to kernel")
  "o j l" '(jupyter-list-kernels :wk "list kernels"))

;; LSP/XRef Keybindings (Eglot, enhanced by Consult/Embark)
(ar/global-leader
  "l" '(:ignore t :which-key "lsp (xref)")
  "l d" '(xref-find-definitions :wk "go to definition")
  "l D" '(xref-find-declarations :wk "go to declaration")
  "l i" '(xref-find-implementations :wk "go to implementation")
  "l r" '(xref-find-references :wk "find references")
  "l s" '(consult-imenu :wk "symbol outline")
  "l R" '(eglot-rename :wk "rename")
  "l f" '(apheleia-format-buffer :wk "format buffer")
  "l e" '(consult-flymake :wk "workspace errors")
  "l h" '(:ignore t :which-key "help")
  "l h h" '(eldoc-doc-buffer :wk "show full documentation"))

;; Debugging Keybindings (DApE)
(ar/global-leader
  "d" '(:ignore t :which-key "debug (dape)")
  "d b" '(dape-toggle-breakpoint-at-point :wk "breakpoint")
  "d c" '(dape-continue :wk "continue")
  "d n" '(dape-next :wk "next")
  "d i" '(dape-step-in :wk "step in")
  "d o" '(dape-step-out :wk "step out")
  "d q" '(dape-disconnect :wk "quit")
  "d r" '(dape-debug-recent :wk "debug recent")
  "d e" '(dape :wk "debug new")
  "d B" '(ar/dape-debug-org-src-block :wk "debug org block"))
```
