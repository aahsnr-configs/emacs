```elisp
* Markdown
#+begin_src emacs-lisp
(use-package markdown-mode
  :defer t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  ;; 1. VISUAL SETTINGS (The "View" aspect)
  (setq markdown-enable-wiki-links t
        markdown-italic-underscore t
        markdown-asymmetric-header t
        markdown-fontify-code-blocks-natively t
        markdown-gfm-uppercase-checkbox t
        markdown-enable-math t  ; Highlight LaTeX math inside markdown

        ;; Header Scaling (Like Org Mode)
        markdown-header-scaling t
        markdown-header-scaling-values '(1.2 1.15 1.1 1.05 1.0 1.0)

        ;; IN-BUFFER MEDIA
        ;; Display images directly in the buffer (toggle with C-c C-i i)
        markdown-display-message nil

        ;; CRITICAL: Hide Markup (The "WYSIWYG" feel)
        ;; Hides *, _, `, [], () characters for cleaner reading.
        markdown-hide-urls t
        markdown-hide-markup t)

  ;; 2. EDITING SETTINGS
  (setq markdown-list-indent-width 2)

  :hook ((markdown-mode . visual-line-mode)        ; Soft wrapping
         (markdown-mode . flyspell-mode)           ; Spell check
         (markdown-mode . markdown-toc-mode)       ; Auto-update TOC
         ;; Auto-display inline images when opening a file
         (markdown-mode . markdown-toggle-inline-images))

  :config
  ;; 3. NAVIGATION & UX
  ;; Allow easily toggling the "Hidden" view when you need to edit the raw syntax
  ;; Mnemonic: "Toggle Markup"
  (define-key markdown-mode-map (kbd "C-c C-x C-m") #'markdown-toggle-markup-hiding)

  ;; Make sure jumping to headers works smoothly
  (setq markdown-imenu-generic-expression markdown-imenu-generic-expression-simple)

  (with-no-warnings
    (advice-add #'markdown--command-map-prompt :override #'ignore)
    (advice-add #'markdown--style-map-prompt   :override #'ignore)))

;; Automatic Table of Contents
(use-package markdown-toc
  :defer t
  :after markdown-mode
  :hook (markdown-mode . markdown-toc-mode)
  :config
  ;; Insert TOC at current position (usually top), remove noise
  (setq markdown-toc-header-toc-title "\n## Table of Contents"
        markdown-toc-user-toc-structure-manipulation-fn 'cdr))
#+end_src
```
