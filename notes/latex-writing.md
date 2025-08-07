Of course. Here is a comprehensively revised LaTeX writing environment that has been enhanced to support project-specific bibliographies and deep integration with Zotero.

This setup provides a flexible, powerful, and modern workflow. It moves away from a single global bibliography to a project-based system and allows you to interface directly with your Zotero library from within Emacs.

### **Prerequisites: External Tools**

Before adding the configuration, ensure the following are installed:

1.  **Zotero:** The reference manager itself. Download from `https://www.zotero.org/`.
2.  **Better BibTeX (BBT) for Zotero:** This is a crucial Zotero addon.
    *   Install it from `https://retorque.re/zotero-better-bibtex/`.
    *   **Configuration:** In Zotero's preferences, go to the "Better BibTeX" tab. Set "Citation key format" to your preference (e.g., `[auth:lower][year]`).
    *   **Auto-Export:** For seamless integration, you can have BBT automatically export your entire library or specific collections to a `.bib` file whenever they change. To do this, select a collection in Zotero, choose "Export Collection," select "Better BibLaTeX" or "Better CSL JSON" as the format, and check **"Keep updated."**
3.  **Tectonic:** The modern TeX engine. `https://tectonic-typesetting.org/docs/install.html`.
4.  **Texlab:** The language server. `cargo install texlab`.

***

### 1. Core Backend: AUCTeX, Tectonic, and Texlab

This configures the foundational packages. `AUCTeX` is the primary editing environment, enhanced with `Tectonic` as the default compiler and `texlab` for LSP features via Eglot.

```el
;;;* 1. Core LaTeX Backend
(use-package tex
  :ensure auctex
  :defer t
  :config
  ;; Set the default TeX engine to Tectonic for its modern, all-in-one approach.
  (setq TeX-engine 'tectonic)
  (add-to-list 'TeX-engine-alist
               '(tectonic "Tectonic" "tectonic -X compile %s -o %o" "tectonic -X compile %s -o %o" "tectonic -X compile %s -o %o"))
  (add-to-list 'TeX-command-list
               '("Tectonic" "tectonic -X compile %s" TeX-run-command nil (latex-mode) :help "Compile with Tectonic"))
  (add-to-list 'TeX-command-list
               '("Latexmk" "latexmk -pdf %s" TeX-run-command nil (latex-mode) :help "Compile with Latexmk for continuous compilation"))

  ;; Use PDF-Tools as the default viewer and enable source correlation (SyncTeX).
  (setq TeX-view-program-selection '((output-pdf "PDF Tools")))
  (setq TeX-source-correlate-mode t)
  (setq TeX-PDF-mode t)

  ;; Enable folding of macros and environments, which is built into AUCTeX.
  (add-hook 'LaTeX-mode-hook #'TeX-fold-mode))

;; Integrate texlab with Eglot for LSP support (diagnostics, completion, etc.).
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((latex-mode tex-mode plain-tex-mode) . ("texlab"))))

;; Provides evil-mode integration for AUCTeX environments.
(use-package evil-tex
  :after (tex evil)
  :defer t)
```

### 2. Project-Based Bibliography Management

Instead of a single global bibliography, we use Emacs's built-in `.dir-locals.el` feature. This allows you to specify different `.bib` files for each writing project, making your setup modular and portable.

**How to Use:**

In the root directory of your LaTeX project, create a file named `.dir-locals.el` with the following content. Emacs will automatically apply these settings to all files within that directory.

**Example `~/Projects/MyPaper/.dir-locals.el`:**
```emacs-lisp
;; This is a sample .dir-locals.el file.

((latex-mode . ((citar-bibliography . ("references.bib" "extra-notes.bib"))
                 (reftex-default-bibliography . ("references.bib" "extra-notes.bib"))))

 (org-mode . ((citar-bibliography . ("references.bib" "extra-notes.bib")))))
```
*   This example tells `citar` and `reftex` to use `references.bib` and `extra-notes.bib` for this specific project.
*   You can list as many `.bib` files as you need.

### 3. Zotero and Citation Ecosystem

This section configures a powerful citation workflow centered around Zotero. `citar` is the main interface, `citar-zotero` allows direct communication with Zotero, and `reftex` handles in-document cross-referencing.

```emacs-lisp
;;;* 2. Zotero, Citation, and Referencing
(use-package reftex
  :ensure nil
  :after tex
  :config
  ;; Enable RefTeX in LaTeX mode for labels, references, and citations.
  (add-hook 'LaTeX-mode-hook #'reftex-mode)
  ;; Allow RefTeX to scan the whole multifile document.
  (setq reftex-plug-into-AUCTeX t)
  ;; This will now be set by .dir-locals.el, but we provide a fallback.
  (setq reftex-default-bibliography '())
  (setq reftex-bibliography-commands '("bibliography" "nobibliography" "addbibresource")))

;; Citar provides a powerful UI for finding and inserting citations.
(use-package citar
  :hook ((latex-mode . citar-capf-setup)
         (org-mode . citar-capf-setup))
  :custom
  ;; No global bibliography; this is handled by .dir-locals.el
  (citar-bibliography '())
  (citar-notes-paths (list my/org-roam-directory))
  (citar-symbols
   `((file ,(nerd-icons-mdicon "nf-md-file_document") . " ")
     (note ,(nerd-icons-mdicon "nf-md-note_text") . " ")
     (link ,(nerd-icons-mdicon "nf-md-link") . " "))))

;; Integrates Citar with Org Roam to link literature notes to citations.
(use-package citar-org-roam
  :after (citar org-roam)
  :config (citar-org-roam-mode 1))

;; Provides Embark actions for Citar candidates (e.g., open PDF, open notes).
(use-package citar-embark
  :after (citar embark)
  :config (citar-embark-mode))

;; Adds Zotero as a direct source for Citar.
(use-package citar-zotero
  :after citar
  :config
  ;; Choose your preferred way of interacting with Zotero.
  ;; 'gourami' is generally faster and more robust.
  (setq citar-zotero-client 'gourami)
  ;; Add Zotero to the list of citation sources.
  (setq citar-sources '(zotero bibtex)))

;; Although Citar has its own parser, bibtex-completion is a robust backend
;; for managing associated files (like PDFs) and notes.
(use-package bibtex-completion
  :defer t
  :custom
  ;; Point this to your Zotero data directory to find attached PDFs.
  (bibtex-completion-library-path "~/Zotero/storage")
  (bibtex-completion-notes-path my/org-roam-directory))

;; Connects Org Roam to your bibliography for knowledge management.
(use-package org-roam-bibtex
  :after (org-roam)
  :hook (org-roam-mode . org-roam-bibtex-mode))
```

### 4. Writing UI and Editing Enhancements

This section improves the interactive writing experience with faster math input, aesthetic ligatures, and intelligent snippet activation.

```emacs-lisp
;;;* 3. Writing and Editing UI
(use-package cdlatex
  :hook (LaTeX-mode . cdlatex-mode))

(use-package laas
  :hook (LaTeX-mode . laas-mode))

;; Use `prettify-symbols-mode` to render LaTeX macros as unicode characters.
(defun ar/latex-prettify-symbols-setup ()
  "Enable prettify-symbols-mode and add custom LaTeX ligatures."
  (prettify-symbols-mode 1)
  ;; Add rules without overwriting the mode's defaults.
  (mapc (lambda (rule) (push rule prettify-symbols-compose-rules))
        '(("\\sum" . ?∑) ("\\int" . ?∫) ("\\in" . ?∈) ("\\forall" . ?∀)
          ("\\exists" . ?∃) ("\\lambda" . ?λ) ("\\alpha" . ?α) ("\\beta" . ?β)
          ("\\gamma" . ?γ) ("\\delta" . ?δ) ("\\epsilon" . ?ε) ("\\pi" . ?π)
          ("\\rightarrow" . ?→) ("\\leftarrow" . ?←) ("\\Rightarrow" . ?⇒)
          ("\\leq" . ?≤) ("\\geq" . ?≥))))
(add-hook 'LaTeX-mode-hook #'ar/latex-prettify-symbols-setup)
```

### 5. Org Mode Integration

This ensures Org mode is a first-class citizen, with export configured for Tectonic and full integration with our Citar setup.

```emacs-lisp
;;;* 4. Org Mode Integration
(use-package org
  :ensure nil
  :config
  ;; Configure Org's citation engine to use our Citar setup.
  (setq org-cite-global-bibliography '()) ;; Handled by .dir-locals.el
  (setq org-cite-insert-processor 'citar)
  (setq org-cite-follow-processor 'citar)
  (setq org-cite-activate-processor 'citar)

  ;; Set Tectonic as the default compiler for Org LaTeX exports.
  (setq org-latex-compiler "tectonic")
  (setq org-latex-pdf-process
        '("tectonic -X compile %f -o %o"))

  ;; Define custom LaTeX classes for flexible document creation.
  (add-to-list 'org-latex-classes
        '("article"
           "\\documentclass{article}"
           ("\\section{%s}" . "\\section*{%s}")
           ("\\subsection{%s}" . "\\subsection*{%s}")
           ("\\subsubsection{%s}" . "\\subsubsection*{%s}")) t)
  (add-to-list 'org-latex-classes
        '("beamer"
           "\\documentclass{beamer}"
           ("\\section{%s}" . "\\section*{%s}")
           ("\\subsection{%s}" . "\\subsection*{%s}")) t))
```

### 6. Snippets and Keybindings

For a clean and maintainable setup, define snippets in dedicated files within `~/.config/emacs/snippets/latex-mode/`. The `yasnippet` configuration from your base setup will automatically load them.

Finally, add these keybindings to your `ar/global-leader` definition in `config.el`.

```emacs-lisp
(ar/global-leader
  ;; ... existing keybindings ...

  "c" '(:ignore t :wk "compile/cite")
  "c c" '(TeX-command-master :wk "Compile Document")
  "c v" '(TeX-view :wk "View Output")
  "c e" '(TeX-clean :wk "Clean Aux Files")
  "c b" '(citar-insert-citation :wk "Insert Citation (Citar)")
  "c o" '(citar-open :wk "Open Reference (Citar)")
  "c r" '(reftex-reference :wk "Insert Reference (RefTeX)"))
```
