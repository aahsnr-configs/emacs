```emacs-lisp
** LSP Mode
*** Combined LSP Hydra
#+begin_src emacs-lisp
(with-eval-after-load 'lsp-mode
  (pretty-hydra-define hydra-lsp
    (:title (pretty-hydra-title "LSP" 'faicon "nf-fa-rocket" :face 'nerd-icons-blue)
     :color blue
     :quit-key ("q" "C-g"))
    
    ("Navigation"
     (("d" lsp-find-definition "definition")
      ("D" lsp-find-declaration "declaration")
      ("r" lsp-find-references "references")
      ("i" lsp-find-implementation "implementation")
      ("t" lsp-find-type-definition "type definition")
      ("p" lsp-ui-peek-find-definitions "peek definition")
      ("R" lsp-ui-peek-find-references "peek references")
      ("I" lsp-ui-peek-find-implementation "peek implementation"))
     
     "Actions"
     (("a" lsp-execute-code-action "code action")
      ("A" lsp-ui-sideline-apply-code-actions "apply code actions")
      ("n" lsp-rename "rename")
      ("f" lsp-format-buffer "format buffer")
      ("F" lsp-format-region "format region")
      ("o" lsp-organize-imports "organize imports")
      ("h" lsp-describe-thing-at-point "describe at point")
      ("s" lsp-signature-help "signature"))
     
     "UI"
     (("u" lsp-ui-imenu "imenu")
      ("m" lsp-ui-doc-show "show doc")
      ("M" lsp-ui-doc-hide "hide doc")
      ("e" lsp-treemacs-errors-list "errors list")
      ("S" lsp-treemacs-symbols "symbols")
      ("." lsp-ui-peek-jump-backward "peek jump back")
      ("," lsp-ui-peek-jump-forward "peek jump forward"))
     
     "Workspace"
     (("w r" lsp-workspace-restart "restart")
      ("w s" lsp-workspace-shutdown "shutdown")
      ("w d" lsp-describe-session "describe")
      ("w D" lsp-disconnect "disconnect")
      ("w q" lsp-workspace-shutdown "quit"))
     
     "Toggle"
     (("t d" (progn
               (lsp-ui-doc-enable (not lsp-ui-doc-mode))
               (setq lsp-ui-doc-enable (not lsp-ui-doc-enable)))
       "doc" :toggle lsp-ui-doc-mode)
      ("t s" (progn
               (lsp-ui-sideline-enable (not lsp-ui-sideline-mode))
               (setq lsp-ui-sideline-enable (not lsp-ui-sideline-enable)))
       "sideline" :toggle lsp-ui-sideline-mode)
      ("t h" (setq lsp-ui-doc-show-with-cursor (not lsp-ui-doc-show-with-cursor))
       "doc on hover" :toggle lsp-ui-doc-show-with-cursor)
      ("t l" lsp-lens-mode "lens" :toggle lsp-lens-mode)
      ("t H" lsp-headerline-breadcrumb-mode "headerline" :toggle lsp-headerline-breadcrumb-mode))))
  
  ;; Bind the hydra
  (define-key lsp-mode-map (kbd "C-c l") #'hydra-lsp/body))
#+end_src

*** Updated LSP Mode Config
#+begin_src emacs-lisp
(use-package lsp-mode
  :defer t
  :defines (lsp-diagnostics-disabled-modes lsp-clients-python-library-directories)
  :autoload lsp-enable-which-key-integration
  :hook ((prog-mode . (lambda ()
                       (unless (derived-mode-p
                                'emacs-lisp-mode 'lisp-mode
                                'makefile-mode 'snippet-mode
                                'ron-mode)
                         (lsp-deferred))))
         ((markdown-mode yaml-mode yaml-ts-mode) . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :bind (:map lsp-mode-map
         ("C-c C-d" . lsp-describe-thing-at-point)
         ("C-c l" . hydra-lsp/body)
         ([remap xref-find-definitions] . lsp-find-definition)
         ([remap xref-find-references] . lsp-find-references))
  :init 
  (setq lsp-use-plists t
        lsp-completion-provider :none
        lsp-log-io nil
        lsp-keymap-prefix "C-c l"
        lsp-keep-workspace-alive nil
        lsp-signature-auto-activate nil
        
        lsp-modeline-code-actions-enable nil
        lsp-modeline-diagnostics-enable nil
        lsp-modeline-workspace-status-enable nil 
        lsp-progress-spinner-type nil

        lsp-semantic-tokens-enable t
        lsp-enable-file-watchers nil
        lsp-enable-folding nil
        lsp-enable-symbol-highlighting nil
        lsp-enable-text-document-color nil
        
        lsp-enable-indentation nil
        lsp-enable-on-type-formatting nil

        lsp-diagnostics-disabled-modes '(markdown-mode gfm-mode))
  :config
  (with-no-warnings
    (defun my-lsp--init-if-visible (fn &rest args)
      (unless (bound-and-true-p git-timemachine-mode)
        (apply fn args)))
    (advice-add #'lsp--init-if-visible :around #'my-lsp--init-if-visible)

    (defun my-lsp-bash-check-sh-shell (&rest _)
      (and (memq major-mode '(sh-mode bash-ts-mode))
           (memq sh-shell '(sh bash zsh))))
    (advice-add #'lsp-bash-check-sh-shell :override #'my-lsp-bash-check-sh-shell)
    (add-to-list 'lsp-language-id-configuration '(bash-ts-mode . "shellscript"))

    (when (icons-displayable-p)
      (defun my-lsp-icons-get-by-file-ext (file-ext &optional feature)
        (when (and file-ext
                   (lsp-icons--enabled-for-feature feature))
          (nerd-icons-icon-for-extension file-ext)))
      (advice-add #'lsp-icons-get-by-file-ext :override #'my-lsp-icons-get-by-file-ext)

      (defvar lsp-symbol-alist
        '((misc          nerd-icons-codicon "nf-cod-symbol_namespace" :face font-lock-warning-face)
          (document      nerd-icons-codicon "nf-cod-symbol_file" :face font-lock-string-face)
          (namespace     nerd-icons-codicon "nf-cod-symbol_namespace" :face font-lock-type-face)
          (string        nerd-icons-codicon "nf-cod-symbol_string" :face font-lock-doc-face)
          (boolean-data  nerd-icons-codicon "nf-cod-symbol_boolean" :face font-lock-builtin-face)
          (numeric       nerd-icons-codicon "nf-cod-symbol_numeric" :face font-lock-builtin-face)
          (method        nerd-icons-codicon "nf-cod-symbol_method" :face font-lock-function-name-face)
          (field         nerd-icons-codicon "nf-cod-symbol_field" :face font-lock-variable-name-face)
          (localvariable nerd-icons-codicon "nf-cod-symbol_variable" :face font-lock-variable-name-face)
          (class         nerd-icons-codicon "nf-cod-symbol_class" :face font-lock-type-face)
          (interface     nerd-icons-codicon "nf-cod-symbol_interface" :face font-lock-type-face)
          (property      nerd-icons-codicon "nf-cod-symbol_property" :face font-lock-variable-name-face)
          (indexer       nerd-icons-codicon "nf-cod-symbol_enum" :face font-lock-builtin-face)
          (enumerator    nerd-icons-codicon "nf-cod-symbol_enum" :face font-lock-builtin-face)
          (treemacs-icon-enumitem      nerd-icons-codicon "nf-cod-symbol_enum_member" :face font-lock-builtin-face)
          (constant      nerd-icons-codicon "nf-cod-symbol_constant" :face font-lock-constant-face)
          (structure     nerd-icons-codicon "nf-cod-symbol_structure" :face font-lock-variable-name-face)
          (event         nerd-icons-codicon "nf-cod-symbol_event" :face font-lock-warning-face)
          (operator      nerd-icons-codicon "nf-cod-symbol_operator" :face font-lock-comment-delimiter-face)
          (template      nerd-icons-codicon "nf-cod-symbol_snippet" :face font-lock-type-face)))

      (defun my-lsp-icons-get-by-symbol-kind (kind &optional feature)
        (when (and kind
                   (lsp-icons--enabled-for-feature feature))
          (let* ((icon (cdr (assoc (lsp-treemacs-symbol-kind->icon kind) lsp-symbol-alist)))
                 (args (cdr icon)))
            (apply (car icon) args))))
      (advice-add #'lsp-icons-get-by-symbol-kind :override #'my-lsp-icons-get-by-symbol-kind)

      (setq lsp-headerline-arrow (nerd-icons-octicon "nf-oct-chevron_right"
                                                     :face 'lsp-headerline-breadcrumb-separator-face)))))
#+end_src

*** Updated LSP UI Config
#+begin_src emacs-lisp
(use-package lsp-ui
  :defer t
  :custom-face
  (lsp-ui-sideline-code-action ((t (:inherit warning))))
  :bind (("C-c u" . lsp-ui-imenu)
         :map lsp-ui-mode-map
         ("M-<f6>" . hydra-lsp/body)
         ("s-<return>" . lsp-ui-sideline-apply-code-actions)
         ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
         ([remap xref-find-references] . lsp-ui-peek-find-references))
  :hook (lsp-mode . lsp-ui-mode)
  :init
  (setq lsp-ui-sideline-show-diagnostics nil
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-doc-delay 0.1
        lsp-ui-doc-show-with-cursor (not (display-graphic-p))
        lsp-ui-imenu-auto-refresh 'after-save
        lsp-ui-imenu-colors `(,(face-foreground 'font-lock-keyword-face)
                              ,(face-foreground 'font-lock-string-face)
                              ,(face-foreground 'font-lock-constant-face)
                              ,(face-foreground 'font-lock-variable-name-face)))
  (defun my-lsp-ui-doc-set-border ()
    (setq lsp-ui-doc-border
          (face-background 'posframe-border nil t)))
  (my-lsp-ui-doc-set-border)
  :config
  (with-no-warnings
    (defvar-local lsp-ui-peek--buffer nil)
    (defun lsp-ui-peek--peek-display (fn src1 src2)
      (if (childframe-workable-p)
          (-let* ((win-width (frame-width))
                  (lsp-ui-peek-list-width (/ (frame-width) 2))
                  (string (-some--> (-zip-fill "" src1 src2)
                            (--map (lsp-ui-peek--adjust win-width it) it)
                            (-map-indexed 'lsp-ui-peek--make-line it)
                            (-concat it (lsp-ui-peek--make-footer)))))
            (setq lsp-ui-peek--buffer (get-buffer-create " *lsp-peek--buffer*"))
           (posframe-show lsp-ui-peek--buffer
                          :string (mapconcat 'identity string "")
                          :min-width (frame-width)
                          :internal-border-color (face-background 'posframe-border nil t)
                          :internal-border-width 1
                          :poshandler #'posframe-poshandler-frame-center))
        (funcall fn src1 src2)))
    (defun lsp-ui-peek--peek-destroy (fn)
      (if (childframe-workable-p)
          (progn
            (when (bufferp lsp-ui-peek--buffer)
              (posframe-hide lsp-ui-peek--buffer))
            (setq lsp-ui-peek--last-xref nil))
        (funcall fn)))
    (advice-add #'lsp-ui-peek--peek-new :around #'lsp-ui-peek--peek-display)
    (advice-add #'lsp-ui-peek--peek-hide :around #'lsp-ui-peek--peek-destroy)

    (defun my-lsp-ui-doc--handle-hr-lines nil
      (let (bolp next before after)
        (goto-char 1)
        (while (setq next (next-single-property-change (or next 1) 'markdown-hr))
         (when (get-text-property next 'markdown-hr)
           (goto-char next)
           (setq bolp (bolp)
                 before (char-before))
           (delete-region (point) (save-excursion (forward-visible-line 1) (point)))
           (setq after (char-after (1+ (point))))
           (insert
            (concat
             (and bolp (not (equal before ?\n)) (propertize "\n" 'face '(:height 0.5)))
             (propertize "\n" 'face '(:height 0.5))
             (propertize " "
                         'display '(space :height (1))
                         'lsp-ui-doc--replace-hr t
                         'face `(:background ,(face-foreground 'font-lock-comment-face nil t)))
             (propertize " " 'display '(space :height (1)))
             (and (not (equal after ?\n)) (propertize " \n" 'face '(:height 0.5)))))))))
    (advice-add #'lsp-ui-doc--handle-hr-lines :override #'my-lsp-ui-doc--handle-hr-lines)))
#+end_src
```
