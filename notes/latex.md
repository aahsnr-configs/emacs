Of course. Here is the complete LaTeX writing environment configuration, broken down into individual, logical markdown blocks for enhanced readability.

---

### Updated LaTeX Writing Environment

This setup provides a comprehensive and modern LaTeX and Org mode writing environment. It combines AUCTeX's powerful editing features with advanced LSP-based diagnostics from LTeX, seamless live previews via org-fragtog, robust completion, and deep Zotero integration.

### Core Backend: AUCTeX, Eglot (LTeX), and Tectonic

This configures the foundational packages. _AUCTeX_ is the primary editing environment, with _Tectonic_ as the compiler. _Eglot_ is configured to use the powerful _LTeX_ language server for advanced grammar, style, and spelling checks.

```el
(use-package tex
  :ensure auctex
  :defer t
  :hook (LaTeX-mode . (lambda ()
                        ;; Enable AUCTeX's Flymake backend for compiler errors.
                        (TeX-flymake-mode 1)
                        ;; Add AUCTeX's completion function to the list of CAPFs.
                        ;; This works alongside eglot's completion.
                        (add-hook 'completion-at-point-functions #'TeX-complete-symbol nil t)
                        ;; Enable folding of macros and environments.
                        (TeX-fold-mode 1)))
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
  (setq TeX-PDF-mode t))

;; Integrate LTeX language server via Eglot for advanced diagnostics.
;; This provides grammar, style, and spell checking for LaTeX, Org, and Markdown.
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((latex-mode tex-mode plain-tex-mode org-mode) . ("ltex-ls"))))

;; Provides evil-mode integration for AUCTeX environments.
(use-package evil-tex
  :after (tex evil)
  :defer t)

;; Configure Apheleia to use latexindent for formatting LaTeX files.
;; AUCTeX provides excellent semantic indentation as you type; this is for
;; whole-buffer formatting on demand.
(with-eval-after-load 'apheleia
  (setf (alist-get 'LaTeX-mode apheleia-formatters)
        '("latexindent")))
```

### Citation Ecosystem

This section configures a powerful citation workflow centered around Zotero. Citar is the main interface, and it is configured to use the `.bib` files provided by the `.dir-locals.el` setup, which are kept in sync by Zotero.

```el
(use-package reftex
  :ensure nil
  :after tex
  :config
  ;; Enable RefTeX in LaTeX mode for labels, references, and citations.
  (add-hook 'LaTeX-mode-hook #'reftex-mode)
  ;; Allow RefTeX to scan the whole multifile document.
  (setq reftex-plug-into-AUCTeX t)
  ;; The bibliography files will be set by .dir-locals.el
  (setq reftex-default-bibliography '())
  (setq reftex-bibliography-commands '("bibliography" "nobibliography" "addbibresource")))

;; Citar provides a powerful UI for finding and inserting citations.
(use-package citar
  ;; Integrate citar-capf for completion-at-point functionality.
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

;; Provides a unified backend for managing bibliography files, which will
;; be used by consult-bibtex.
(use-package biblio
  :defer t
  :config
  ;; Set the default backend to citar to better integrate with your
  ;; existing setup. Biblio will find and manage the bib files, and
  ;; citar will handle the note-taking and file associations.
  (setq biblio-bibtex-backend 'citar))

;; Provides a powerful 'consult' interface for BibTeX files.
(use-package consult-bibtex
  :vc (:url "https://github.com/mohkale/consult-bibtex"
       :branch "master")
  :after (consult biblio)
  :custom
  ;; We leave this empty, as biblio will automatically find the bib files
  ;; specified in your .dir-locals.el, following your existing pattern.
  (consult-bibtex-files '())
  (consult-bibtex-format-entry-function #'consult-bibtex-format-entry-default)
  :config
  (setq consult-bibtex-default-action 'consult-bibtex-insert-citation))
```

### Writing UI and Editing Enhancements

This section improves the interactive writing experience with faster math input, aesthetic ligatures, and intelligent snippet activation.

```emacs-lisp
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

### Org Mode Integration

This ensures Org mode is a first-class citizen, with export configured for Tectonic, full integration with our Citar setup, and seamless, live previews for LaTeX fragments.

```emacs-lisp
;; Provides seamless, on-demand previews of LaTeX fragments in Org mode.
(use-package org-fragtog
  :hook (org-mode . org-fragtog-mode))

(with-eval-after-load 'ox-latex
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

### Keybindings

These keybindings provide quick access to compilation, citation, and preview commands.

```el
(ar/global-leader
  "c" '(:ignore t :wk "compile/cite")
  "c c" '(TeX-command-master :wk "Compile Document")
  "c v" '(TeX-view :wk "View Output")
  "c e" '(TeX-clean :wk "Clean Aux Files")
  "c p" '(org-fragtog-toggle :wk "Toggle LaTeX Previews")
  "c o" '(citar-open :wk "Open Reference (Citar)")
  "c r" '(reftex-reference :wk "Insert Reference (RefTeX)")
  "c b" '(:ignore t :wk "bibliography")
  "c b c" '(citar-insert-citation :wk "Insert Citation (Citar)")
  "c b b" '(consult-bibtex :wk "Insert Citation (BibTeX)"))
```
