Of course. Here is the complete, refined configuration presented in a structured and readable markdown format.

# Emacs Configuration Update: A Unified Guide 🚀

This guide contains all the necessary changes to upgrade your Emacs setup. It is divided into two parts:

  * **Prerequisite Changes:** Two small but crucial updates to your existing configuration.
  * **The Development Environment:** A single, comprehensive block to replace your old setup entirely.

-----

## Part 1: Prerequisite Changes to `config.txt`

Before adding the new development environment, you must apply the following two modifications to your `config.txt` file.

### 1. Update the `orderless` Configuration

In your `config.txt`, find the section for **`**Orderless for Advanced Filtering**`** and replace the entire `(use-package orderless ...)` block with this one. This adds enhanced filtering capabilities.

```el
(use-package orderless
  :custom
  ;; Use orderless as the primary completion style.
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  ;; Use standard completion for file paths for a more predictable experience.
  (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-category-overrides '((file (styles basic partial-completion))))
  ;; Add dispatchers for more precise filtering (e.g., =literal, %regexp)
  (orderless-dispatchers
   '(orderless-consult-dispatch orderless-affix-dispatch)))
```

### 2. Update the `Embark` Configuration

Next, find the **`**Embark**`** section. Modify its `:config` block to integrate Eglot's code actions, making them available via your `C-.` keybinding.

```el
(use-package embark
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Integrate Eglot code actions directly into Embark
  (add-to-list 'embark-major-mode-map '(eglot-mode . eglot))
  (define-key embark-collect-mode-map (kbd "e") #'embark-export)
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))
```

-----

## Part 2: The Definitive Development Environment

Now, remove your entire old `* Development Environment` section from `config.txt`. Replace it with this single, comprehensive configuration block.

This configuration provides a highly refined, project-aware development environment. It is built on Emacs's native tooling and is designed to create a first-class, Jupyter-like experience for Python development directly within Org mode, featuring automatic rich output for both tables and plots.

### Core Environment: `envrc` and Language Server Protocol

The foundation is the `purcell/envrc` package for robust environment management and `eglot` for the Language Server Protocol, providing code intelligence, diagnostics, and navigation.

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

### UI and Workflow Integration

This section seamlessly connects development tools with your existing `consult`, `embark`, and `xref` setup for a unified code navigation and refactoring experience.

```el
;; Configure xref to use the powerful UI provided by the main consult package.
;; This is the most robust way to ensure consult is loaded first.
(with-eval-after-load 'consult
  (setq xref-show-definitions-function #'consult-xref-show-definitions)
  (setq xref-show-references-function #'consult-xref-show-references))

;; Provides a powerful consult interface for Eglot's LSP features.
(use-package consult-eglot
  :after (consult eglot))

;; Integrates consult-eglot sources with Embark actions.
(use-package consult-eglot-embark
  :after (consult-eglot embark))

;; Display eldoc documentation in a popup frame at point.
(use-package eldoc-box
  :hook (eldoc-mode . eldoc-box-hover-mode)
  :custom
  ;; Only show the popup if there is actual documentation.
  (eldoc-box-show-if-no-doc nil)
  ;; Disable the default minibuffer display since we have a popup.
  (eldoc-echo-area-display-truncation-p nil)
  :custom-face
  ;; Style the popup to match the doom-tokyo-night theme.
  (eldoc-box-border ((t (:foreground "#3b4261"))))
  (eldoc-highlight-symbol-face ((t (:foreground "#7aa2f7" :weight bold)))))
```

### Robust Debugger UI (`dape`)

We use `dape` for debugging. The UI for debugger windows is cleanly managed by the enhanced `shackle` configuration in your `* Editor Behaviour` section.

```emacs-lisp
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

### Syntax Checking and Formatting

This setup uses the built-in `flymake` for live diagnostics, enhanced by `flymake-collection` for easy linter integration, and `apheleia` for automatic, on-save formatting.

```el
;; Use the built-in flymake for syntax checking.
(use-package flymake
  :ensure nil
  :hook (prog-mode . flymake-mode)
  :custom
  (flymake-check-syntax-automatically '(save mode-enabled idle-change))
  (flymake-idle-change-delay 0.4))

;; A much cleaner way to add support for checkers like pylint and flake8.
(use-package flymake-collection
  :after flymake
  :config
  (flymake-collection-hook-setup))

;; Display flymake errors in a popup frame instead of the echo area.
(use-package flymake-posframe
  :after flymake
  :hook (flymake-mode . flymake-posframe-mode))

;; Handle auto-formatting on save.
(use-package apheleia
  :init (apheleia-global-mode +1)
  :config
  (add-hook 'before-save-hook #'apheleia-format-buffer))
```

### Tree-sitter for Superior Syntax Analysis

This configuration prefers Tree-sitter-based major modes for modern syntax highlighting and structural editing.

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

### Python & Jupyter for Org Mode 🐍

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
  (add-hook 'org-mode-hook #'org-display-inline-images)
  ;; Prioritize rich output formats to mimic JupyterLab.
  (setq ob-jupyter-response-mime-type-priorities
        '("text/html" "image/png" "image/jpeg" "text/plain"))
  ;; Load the jupyter language into babel.
  (org-babel-do-load-languages 'org-babel-load-languages '((jupyter . t)))
  ;; Make python blocks use jupyter by default for a stateful, interactive experience.
  (add-to-list 'org-babel-default-header-args:python
               '((:session . "jupyter-python")
                 (:kernel . "python3")
                 (:results . "output file")
                 (:exports . "results")
                 (:dir . "./.jupyter-exports/")))
  (setq ob-jupyter-startup-timeout 30))

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

-----

## Project Setup Summary 📝

To set up a new Python project, follow these two steps in the project's root directory:

1.  Create `.envrc` and run `direnv allow` in your terminal.

    ```sh
    # .envrc
    layout python3
    source .direnv/bin/activate
    # Install all necessary tools inside the virtual environment
    # pip install jupyter_client ipykernel debugpy pyright-cli pylint flake8 black pandas matplotlib
    ```

2.  Create `.dir-locals.el` to activate your chosen linters. Eglot (`pyright`) diagnostics are enabled by default and will run alongside these.

    ```el
    ;; .dir-locals.el
    ((python-ts-mode . ((eval . (flymake-collection-add-checker 'python-pylint 'python-flake8)))))
    ```

-----

## Final Streamlined Keybindings ⌨️

These keybindings provide ergonomic access to the entire toolchain, with distinct keys for buffer-local vs. project-wide actions and a new dedicated key for code actions.

```el
;; Org Jupyter Keybindings
(ar/global-leader
 "o" '(:ignore t :which-key "org")
 "o j" '(:ignore t :which-key "jupyter")
 "o j r" '(jupyter-restart-kernel :wk "restart kernel")
 "o j i" '(jupyter-interrupt-kernel :wk "interrupt kernel")
 "o j c" '(jupyter-connect-to-kernel :wk "connect to kernel")
 "o j l" '(jupyter-list-kernels :wk "list kernels"))

;; LSP/XRef Keybindings (Eglot, enhanced by Consult)
(ar/global-leader
 "l" '(:ignore t :which-key "lsp (eglot)")
 "l a" '(eglot-code-actions :wk "code actions")
 "l d" '(xref-find-definitions :wk "go to definition")
 "l D" '(xref-find-declarations :wk "go to declaration")
 "l i" '(xref-find-implementations :wk "go to implementation")
 "l r" '(xref-find-references :wk "find references")
 "l s" '(consult-imenu :wk "buffer symbols")
 "l S" '(consult-eglot-symbols :wk "project symbols")
 "l R" '(eglot-rename :wk "rename")
 "l f" '(apheleia-format-buffer :wk "format buffer")
 "l e" '(consult-flymake :wk "buffer errors")
 "l E" '(consult-eglot-diagnostics :wk "project errors")
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
