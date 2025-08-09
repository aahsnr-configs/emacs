Of course. Here is a comprehensive overhaul of your Emacs configuration, replacing the specified packages and enhancing them to create a modern, powerful, and integrated IDE-like experience, optimized for Emacs 30 and inspired by Doom Emacs principles.

Each section below details the packages being removed and the new configurations being introduced. You should replace the corresponding sections in your `init.org` file with this new content.

***

## 1. Completion Framework Overhaul: Ivy, Counsel & Company

This is the most significant change, replacing the entire Vertico/Corfu ecosystem with the classic and powerful Ivy/Counsel and Company combination.

### Packages to Remove:
- `orderless`
- `vertico`
- `marginalia`
- `nerd-icons-completion`
- `consult`
- `embark`
- `embark-consult`
- `corfu`
- `nerd-icons-corfu`
- `cape`

### New Configuration:
Replace the entire `*Completion Framework*` and `*Corfu*` sections with the following:

```org
* Completion Framework: Ivy, Counsel & Company
This configuration replaces the default completion system with the powerful Ivy/Counsel/Swiper stack for minibuffer completion and Company for in-buffer completion. This combination is highly customizable and provides a fluid, interactive experience.

** Ivy: The Core Completion UI
Ivy provides the flexible, vertical completion UI that powers all other components.

#+begin_src emacs-lisp
(use-package ivy
  :diminish
  :init (ivy-mode 1)
  :custom
  (ivy-height 15)
  (ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d) ")
  (enable-recursive-minibuffers t)
  (ivy-re-builders-alist
   '((swiper . ivy--regex-plus)
     (t . ivy--regex-fuzzy)))
  :config
  (set-face-attribute 'ivy-current-match :background "#3b4261" :weight 'bold))
#+end_src

** Counsel: A Collection of Enhanced Commands
Counsel provides a suite of Ivy-powered commands that enhance and replace built-in Emacs functions.

#+begin_src emacs-lisp
(use-package counsel
  :after ivy
  :demand t
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-x b" . counsel-switch-buffer)
         ("<f1> f" . counsel-describe-function)
         ("<f1> v" . counsel-describe-variable)
         ("<f1> b" . counsel-descbinds)
         ("C-c g" . counsel-rg) ; Ripgrep integration
         :map ivy-minibuffer-map
         ("C-r" . counsel-minibuffer-history))
  :custom
  (counsel-find-file-ignore-regexp "\\`\\.\\'"))
#+end_src

** Swiper: Enhanced Buffer Search
#+begin_src emacs-lisp
(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))
#+end_src

** Ivy Enhancements: Rich Annotations and Smart Sorting
These packages add rich annotations to the Ivy UI and improve sorting to be more intuitive, similar to `marginalia` and `orderless`.

#+begin_src emacs-lisp
(use-package ivy-rich
  :after ivy
  :init (ivy-rich-mode 1)
  :config
  (ivy-rich-project-root-cache-mode 1))

(use-package ivy-prescient
  :after ivy
  :init (ivy-prescient-mode 1)
  :custom
  (ivy-prescient-retain-classic-highlighting t))
#+end_src

** Company: In-Buffer Completion
Company replaces Corfu for providing intelligent, in-buffer completions for code and text.

#+begin_src emacs-lisp
(use-package company
  :hook (after-init . global-company-mode)
  :diminish
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.1)
  (company-tooltip-align-annotations t)
  (company-selection-wrap-around t)
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              ("M-." . company-show-doc-buffer)
              ("<tab>" . company-complete-selection)
              ([tab] . company-complete-selection)))

;; Enhances the Company UI with icons and better documentation popups.
(use-package company-quickhelp
  :after company
  :hook (global-company-mode . company-quickhelp-mode))

(use-package nerd-icons-company
  :after (company nerd-icons)
  :config
  (add-to-list 'company-transformers #'nerd-icons-company-transformer))
#+end_src

```

## 2. Project & Workspace Management Upgrade: Projectile & Persp-mode

Here, we replace the built-in `project.el` and `perspective.el` with the more powerful and feature-rich `projectile` and `persp-mode.el`.

### Packages to Remove:
- `perspective`
- `project`

### New Configuration:
Replace the `*Workspaces*` and `*Project Management*` sections under `*Workflow Management*` with this:

```org
* Workflow Management
** Workspaces with Persp-Mode
`persp-mode` provides powerful, persistent workspaces that can be tied to projects, allowing for seamless context switching. It replaces the simpler `perspective.el`.

#+begin_src emacs-lisp
(use-package persp-mode
  :vc (:url "https://github.com/Bad-ptr/persp-mode.el.git" :branch "master")
  :init
  (setq persp-state-default-file (expand-file-name "persp-mode-state" no-littering-var-directory))
  (persp-mode)
  :custom
  (persp-autokill-buffer-on-remove 'kill-if-not-modified)
  (persp-auto-resume-time -1) ; Don't auto-resume, we do it via hook
  :config
  ;; Load saved perspectives when Emacs starts.
  (add-hook 'emacs-startup-hook
            (lambda ()
              (when (file-exists-p persp-state-default-file)
                (persp-load-state-from-file persp-state-default-file t)))))

;; Keybindings for workspaces
(ar/global-leader
 "w" '(:ignore t :wk "workspaces")
 "w n" '(persp-next :wk "next workspace")
 "w p" '(persp-prev :wk "previous workspace")
 "w s" '(persp-switch :wk "switch workspace")
 "w b" '(persp-consult-buffer :wk "switch buffer in workspace")
 "w a" '(persp-add-new :wk "add workspace")
 "w r" '(persp-rename :wk "rename workspace")
 "w k" '(persp-kill :wk "kill workspace"))
#+end_src

** Project Management with Projectile
`projectile` is a feature-rich and ubiquitous project management library for Emacs. It integrates perfectly with `counsel` and `persp-mode`.

#+begin_src emacs-lisp
(use-package projectile
  :diminish
  :init
  (projectile-mode +1)
  :config
  (setq projectile-project-search-path '("~/Projects/" "~/src/"))
  (setq projectile-enable-caching t)
  (setq projectile-completion-system 'ivy))

;; Integrates Projectile with Counsel for a superior UI.
(use-package counsel-projectile
  :after (projectile counsel)
  :init (counsel-projectile-mode)
  :config
  ;; Enable async actions for better performance
  (setq counsel-projectile-key-bindings
        (let ((map (make-sparse-keymap)))
          (define-key map "f" #'counsel-projectile-find-file)
          (define-key map "d" #'counsel-projectile-find-dir)
          (define-key map "s" '(:ignore t :wk "search")
          (define-key map "ss" #'counsel-projectile-ag) ; The Silver Searcher
          (define-key map "b" #'counsel-projectile-switch-to-buffer)
          (define-key map "p" #'counsel-projectile-switch-project)
          (define-key map "c" #'projectile-compile-project)
          (define-key map "r" #'projectile-replace)
          (define-key map "k" #'projectile-kill-buffers)
          map)))

;; Automatically create and manage perspectives for each project.
(use-package persp-projectile
  :vc (:url "https://github.com/bbatsov/persp-projectile.git" :branch "master")
  :after (persp-mode projectile)
  :config
  (setq persp-projectile-switch-when-finding-file 'always)
  (add-hook 'projectile-after-switch-project-hook #'persp-projectile-find-file-hook))

;; Global Project Keybindings
(ar/global-leader
 "p" '(counsel-projectile-switch-project :wk "switch project")
 "p f" '(counsel-projectile-find-file :wk "find file in project")
 "p g" '(counsel-projectile-ag :wk "grep in project")
 "p b" '(counsel-projectile-switch-to-buffer :wk "find buffer in project"))
#+end_src
```

## 3. Development Environment Upgrade: LSP-Mode, DAP-Mode & Flycheck

This section replaces `eglot`, `dape`, and `flymake` with their more feature-rich counterparts, providing a true IDE experience with enhanced UI components.

### Packages to Remove:
- `eglot`
- `eglot-booster`
- `consult-eglot`
- `consult-eglot-embark`
- `dape`
- `flymake`
- `flymake-collection`
- `flymake-posframe`

### New Configuration:
Replace the entire `*Development Environment*` section with the following:

```org
* Development Environment
This section configures a full-featured Integrated Development Environment (IDE) using `lsp-mode` for language intelligence, `dap-mode` for debugging, and `flycheck` for on-the-fly syntax checking.

** Language Server Protocol: lsp-mode
`lsp-mode` is a powerful LSP client that provides features like code completion, navigation, and diagnostics. It is enhanced by `lsp-ui` for a better user interface.

#+begin_src emacs-lisp
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (prog-mode . lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :custom
  (lsp-eldoc-render-all nil)
  (lsp-idle-delay 0.5)
  (lsp-enable-file-watchers nil) ; Can be demanding, enable if needed
  :config
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map))

(use-package lsp-ui
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-sideline-show-code-actions t))
#+end_src

** Debugger: dap-mode
`dap-mode` provides a standard, feature-rich debugging interface that integrates with `lsp-mode`.

#+begin_src emacs-lisp
(use-package dap-mode
  :after lsp-mode
  :commands (dap-debug dap-debug-edit-template dap-install)
  :config
  ;; Configure UI to use side windows for a classic IDE layout.
  (setq dap-ui-controls-background-color nil)
  (setq dap-ui-sessions-style 'tree)
  (dap-ui-controls-mode 1)

  (with-eval-after-load 'evil
    (evil-define-key 'motion dap-mode-map (kbd "k") 'dap-up-frame)
    (evil-define-key 'motion dap-mode-map (kbd "j") 'dap-down-frame)))
#+end_src

** Syntax Checking: Flycheck
`flycheck` is a modern, extensible syntax-checking framework. It integrates seamlessly with `lsp-mode`.

#+begin_src emacs-lisp
(use-package flycheck
  :hook (after-init . global-flycheck-mode)
  :diminish
  :config
  ;; Integrate with the lsp-mode diagnostics backend.
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (flycheck-add-next-checker 'lsp 'pylint 'append))

(use-package flycheck-posframe
  :after flycheck
  :hook (flycheck-mode . flycheck-posframe-mode))
#+end_src

** Formatting: Apheleia (Unchanged)
Your `apheleia` configuration is excellent and does not need to be replaced. It works perfectly with this new stack.
#+begin_src emacs-lisp
(use-package apheleia
  :defer t
  :config
  (apheleia-global-mode +1))
#+end_src

** Keybindings
These keybindings are updated for `lsp-mode` and `dap-mode`.

#+begin_src emacs-lisp
(ar/global-leader
 "l" '(:ignore t :which-key "lsp")
 "l a" '(lsp-execute-code-action :wk "code actions")
 "l d" '(lsp-find-definition :wk "go to definition")
 "l i" '(lsp-find-implementation :wk "go to implementation")
 "l r" '(lsp-find-references :wk "find references")
 "l s" '(counsel-imenu :wk "buffer symbols")
 "l S" '(lsp-ivy-workspace-symbol :wk "project symbols")
 "l R" '(lsp-rename :wk "rename")
 "l f" '(apheleia-format-buffer :wk "format buffer")
 "l e" '(flycheck-list-errors :wk "buffer errors")
 "l h" '(lsp-ui-doc-show :wk "show documentation"))

(ar/global-leader
 "d" '(:ignore t :wk "debug (dap)")
 "d b" '(dap-toggle-breakpoint :wk "breakpoint")
 "d c" '(dap-continue :wk "continue")
 "d n" '(dap-next :wk "next")
 "d i" '(dap-step-in :wk "step in")
 "d o" '(dap-step-out :wk "step out")
 "d q" '(dap-disconnect :wk "quit")
 "d r" '(dap-debug-recent :wk "debug recent")
 "d e" '(dap-debug :wk "debug new")
 "d B" '(ar/dap-debug-org-src-block :wk "debug org block"))
#+end_src
```

## 4. Language-Specific Configurations

We now update the Python, Markdown, and LaTeX configurations to use the new IDE stack.

### Python & Jupyter
Replace the entire `*Python & Jupyter for Org Mode*` section.

```org
* Python & Jupyter IDE
This configures a complete Python development environment.

** LSP and Completion
#+begin_src emacs-lisp
(use-package lsp-pyright
  :ensure t
  :after lsp-mode)

(use-package company-jedi
  :after company)
#+end_src

** Debugging with dap-python
#+begin_src emacs-lisp
(use-package dap-python
  :after dap-mode
  :config
  (require 'dap-python)
  ;; This ensures debugpy is installed in a shared location.
  (setq dap-python-debugger 'debugpy))

;; --- Updated Debugging Function for Org Blocks ---
(defun ar/dap-debug-org-src-block ()
  "Tangle the current Org src block to a temp file and debug it with dap-mode."
  (interactive)
  (when (derived-mode-p 'org-mode)
    (let* ((tmp-file (make-temp-file "emacs-babel-" nil ".py"))
           (info (org-babel-get-src-block-info 'light)))
      (with-temp-file tmp-file (insert (nth 2 info)))
      (message "Debugging tangled block in %s" tmp-file)
      (dap-debug
       `(:name "Python :: Debug Org Block"
         :type "python"
         :request "launch"
         :program ,tmp-file
         :justMyCode t)))))
#+end_src

** Jupyter (Mostly Unchanged)
Your Jupyter configuration is well-structured. We just need to ensure it uses the new `ar/global-leader` keys.
#+begin_src emacs-lisp
(use-package jupyter
  :defer t
  :after org
  :hook (org-mode . org-display-inline-images)
  :custom
  (jupyter-channel-build-if-needed t)
  (jupyter-python-executable-command (executable-find "python3"))
  (ob-jupyter-response-mime-type-priorities
   '("text/html" "image/png" "image/jpeg" "text/plain"))
  (ob-jupyter-startup-timeout 30)
  :config
  (org-babel-do-load-languages 'org-babel-load-languages '((jupyter . t)))
  (add-to-list 'org-babel-default-header-args:python
               '((:session . "jupyter-python")
                 (:kernel . "python3")
                 (:results . "output file")
                 (:exports . "results")
                 (:dir . "./.jupyter-exports/"))))
#+end_src
```

### Markdown Environment
Replace the `flymake-markdownlint` block with a `flycheck` equivalent.

```org
** Live Linting with *flycheck* and *markdownlint*
#+begin_src emacs-lisp
(use-package flycheck-markdown
  :after flycheck
  :config
  (flycheck-markdown-setup))
#+end_src

** LSP for Markdown (Optional, but recommended)
#+begin_src emacs-lisp
(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration '(markdown-mode . "markdown"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("marksman" "server"))
                    :major-modes '(markdown-mode gfm-mode markdown-ts-mode)
                    :server-id 'marksman)))
#+end_src
```

### LaTeX Writing Environment
Replace the `consult-bibtex` and related packages with `ivy-bibtex`, and update the LSP integration.

**Packages to Remove/Replace in LaTeX section:**
- `consult-bibtex` -> `ivy-bibtex`
- `biblio` -> No longer strictly necessary as `ivy-bibtex` handles this.
- `citar-embark` -> Actions are handled differently in Ivy, often with `ivy-hydra` or custom commands.

**New Citation Configuration:**
Replace the `*Citation Ecosystem*` subsection with this:

```org
** Citation Ecosystem
This section configures a powerful citation workflow centered around `ivy-bibtex`.

#+begin_src emacs-lisp
(use-package reftex
  :ensure nil
  :after tex
  :config
  (add-hook 'LaTeX-mode-hook #'reftex-mode)
  (setq reftex-plug-into-AUCTeX t)
  (setq reftex-default-bibliography '())
  (setq reftex-bibliography-commands '("bibliography" "nobibliography" "addbibresource")))

;; ivy-bibtex provides a powerful Ivy UI for finding and inserting citations.
(use-package ivy-bibtex
  :after (ivy)
  :init
  (setq bibtex-completion-bibliography '()) ; Let dir-locals handle this
  (setq bibtex-completion-library-path "~/Zotero/storage")
  (setq bibtex-completion-notes-path my/org-roam-directory)
  (setq ivy-bibtex-default-action 'ivy-bibtex-insert-citation)
  (setq ivy-bibtex-display-formats
        '((author . 30)
          (title . 80)
          (year . 4)
          (journal . 20)
          (doi . 10)))
  :config
  ;; Link with org-roam notes
  (setq bibtex-completion-notes-template-one-file
        (concat
         "#+title: ${title}\n"
         "#+author: ${author-or-editor}\n"
         "#+roam_key: ${=key=}\n\n"
         "* Notes\n\n")))

;; Connects Org Roam to your bibliography for knowledge management.
(use-package org-roam-bibtex
  :after (org-roam)
  :hook (org-roam-db-autosync-mode . org-roam-bibtex-mode))
#+end_src
```

**Updated Keybindings:**
Replace the keybindings for `citar` and `consult-bibtex` with the new ones for `ivy-bibtex`.

```org
(ar/global-leader
  "c" '(:ignore t :wk "compile/cite")
  "c c" '(TeX-command-master :wk "Compile Document")
  "c v" '(TeX-view :wk "View Output")
  "c e" '(TeX-clean :wk "Clean Aux Files")
  "c r" '(reftex-reference :wk "Insert Reference (RefTeX)")
  "c b" '(:ignore t :wk "bibliography")
  "c b b" '(ivy-bibtex :wk "Insert Citation (ivy-bibtex)"))
#+end_src
```

These changes will transform your Emacs configuration into a more powerful and cohesive environment, aligning with your request for an IDE-like setup based on the specified packages. Remember to run `M-x package-install` for all the new packages mentioned.
