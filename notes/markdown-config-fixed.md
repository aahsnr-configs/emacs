```emacs-lisp
* Markdown
#+begin_src emacs-lisp
(use-package markdown-mode
  :defer t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  ;; Basic settings
  (setq markdown-italic-underscore t
        markdown-asymmetric-header t
        markdown-fontify-code-blocks-natively t
        markdown-gfm-uppercase-checkbox t
        markdown-enable-math nil
        markdown-header-scaling t
        markdown-header-scaling-values '(1.2 1.15 1.1 1.05 1.0 1.0)
        markdown-display-message nil
        markdown-hide-urls t
        markdown-hide-markup t)

  ;; List indentation
  (setq markdown-list-indent-width 2)
  
  ;; Performance optimizations
  (setq markdown-fontify-whole-heading-line nil
        markdown-fontify-quote-and-verse-blocks nil)
  
  ;; FIX: Set imenu expression to the simple version to avoid void variable error
  (setq markdown-imenu-generic-expression 
        '(("^\\(.*\\)[ \t]*\n=+$" 1)
          ("^\\(.*\\)[ \t]*\n-+$" 1)
          ("^\\(#+ .*\\)" 1)))

  :hook ((markdown-mode . visual-line-mode)
         (markdown-mode . ar/markdown-setup-rendering))

  :config
  ;; REMOVED: All gfm-mode specific settings as they are not needed
  
  ;; FIX: Dynamic markup toggle using text properties (proper method)
  (defun ar/markdown-in-element-p ()
    "Check if point is inside a markdown element that should show raw markup.
Uses text properties to detect code, emphasis, and links."
    (let ((face (get-text-property (point) 'face)))
      (or
       ;; Check for code blocks and inline code
       (and face 
            (or (eq face 'markdown-code-face)
                (eq face 'markdown-inline-code-face)
                (eq face 'markdown-pre-face)
                (eq face 'markdown-gfm-code-block-face)
                (and (listp face)
                     (or (memq 'markdown-code-face face)
                         (memq 'markdown-inline-code-face face)
                         (memq 'markdown-pre-face face)
                         (memq 'markdown-gfm-code-block-face face)))))
       ;; Check for emphasis (bold, italic)
       (and face
            (or (eq face 'markdown-bold-face)
                (eq face 'markdown-italic-face)
                (and (listp face)
                     (or (memq 'markdown-bold-face face)
                         (memq 'markdown-italic-face face)))))
       ;; Check for links
       (and face
            (or (eq face 'markdown-link-face)
                (eq face 'markdown-url-face)
                (and (listp face)
                     (or (memq 'markdown-link-face face)
                         (memq 'markdown-url-face face)))))
       ;; Check if we're on markup characters themselves
       (save-excursion
         (and (looking-at-p "[*_`\\[\\]()]")
              (or (looking-back "[*_`]" 1)
                  (looking-at "[*_`]")))))))
  
  (defun ar/markdown-toggle-markup-at-point ()
    "Toggle markup hiding based on cursor position.
Shows markup when cursor is inside markdown elements."
    (when (derived-mode-p 'markdown-mode)
      (if (ar/markdown-in-element-p)
          ;; Inside element: show markup locally
          (remove-text-properties (line-beginning-position) (line-end-position)
                                '(display nil composition nil))
        ;; Outside element: restore hiding if enabled globally
        (when markdown-hide-markup
          (font-lock-flush (line-beginning-position) (line-end-position))))))
  
  (defun ar/markdown-setup-rendering ()
    "Setup cursor-based markup visibility toggling."
    ;; Use post-command-hook for immediate response
    (add-hook 'post-command-hook #'ar/markdown-toggle-markup-at-point nil t))
  
  ;; FIX: Make checkboxes appear filled
  (defface ar/markdown-checkbox-face
    '((t (:inherit markdown-list-face :box (:line-width 1 :style released-button))))
    "Face for markdown checkboxes."
    :group 'markdown-faces)
  
  (defun ar/markdown-fontify-checkboxes (last)
    "Fontify checkboxes with custom appearance up to LAST."
    (when (markdown-match-generic-metadata "^[ \t]*- \\[[ X]\\]" last)
      (let ((checkbox-begin (match-beginning 0))
            (checkbox-end (match-end 0))
            (checkbox-state (match-string 0)))
        (if (string-match-p "\\[X\\]" checkbox-state)
            ;; Checked checkbox - use filled square
            (compose-region (+ checkbox-begin 2) (+ checkbox-begin 5) "☑")
          ;; Unchecked checkbox - use empty square  
          (compose-region (+ checkbox-begin 2) (+ checkbox-begin 5) "☐"))
        (add-text-properties checkbox-begin checkbox-end
                           '(face ar/markdown-checkbox-face)))
      t))
  
  ;; Add checkbox fontification to markdown
  (font-lock-add-keywords 'markdown-mode
                         '((ar/markdown-fontify-checkboxes)) 'append)
  
  ;; Additional keybindings
  (define-key markdown-mode-map (kbd "C-c C-x C-m") #'markdown-toggle-markup-hiding))
#+end_src

;; Automatic Table of Contents
#+begin_src emacs-lisp
(use-package markdown-toc
  :defer t
  ;; NOTE: :after is NOT needed because :hook already ensures markdown-mode is loaded
  :hook (markdown-mode . markdown-toc-mode)
  :config
  (setq markdown-toc-header-toc-title "\n## Table of Contents"
        markdown-toc-user-toc-structure-manipulation-fn 'cdr))
#+end_src
```
