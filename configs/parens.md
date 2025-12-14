** Parenthesis Matching & Context
#+begin_src emacs-lisp
(use-package paren
  :ensure nil
  :hook (elpaca-after-init . show-paren-mode)
  :custom
  (show-paren-delay 0.1)
  (show-paren-highlight-openparen t)
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t)
  ;; 1. Clean Visuals: Only highlight the bracket character itself
  (show-paren-style 'parenthesis)
  ;; 2. Disable default messages: We use the custom ElDoc logic below
  (show-paren-context-when-offscreen nil)
  
  :config
  ;; === CUSTOM ELDOC BACKEND FOR PARENTHESIS ===
  (defun ar/paren-eldoc-offscreen-context (callback &rest _)
    "Show off-screen parenthesis context in Echo Area via ElDoc."
    (let* ((pos (point))
           (match-pos nil)
           (label nil)
           (line-content nil)
           (syntax-at (when (char-after pos) (char-syntax (char-after pos))))
           (syntax-before (when (char-before pos) (char-syntax (char-before pos))))
           ;; Default bounds are the whole buffer
           (scan-beg (point-min))
           (scan-end (point-max)))

      ;; 0. ORG MODE FIX: Constrain search to current source block
      (when (and (derived-mode-p 'org-mode)
                 (org-in-src-block-p))
        (let ((elem (org-element-at-point)))
          (when elem
            (setq scan-beg (org-element-property :begin elem))
            (setq scan-end (org-element-property :end elem)))))

      (save-restriction
        ;; Safety check: Ensure bounds are valid numbers before narrowing
        (when (and (numberp scan-beg) (numberp scan-end) (< scan-beg scan-end))
          (narrow-to-region scan-beg scan-end))

        ;; 1. FIND MATCHING POSITION
        ;; Note: Comments below must not contain raw parens to avoid Org syntax issues
        (cond
         ;; Case A: At Opening Bracket
         ((eq syntax-at ?\()
          (setq match-pos (ignore-errors (scan-sexps pos 1)))
          (when match-pos
            (setq match-pos (1- match-pos))
            (setq label "Closing: ")))
         
         ;; Case B: At Closing Bracket (Evil/Block cursor)
         ((eq syntax-at ?\))
          (setq match-pos (ignore-errors (scan-sexps (1+ pos) -1)))
          (setq label "Opening: "))
         
         ;; Case C: After Closing Bracket (Standard Emacs)
         ((eq syntax-before ?\))
          (setq match-pos (ignore-errors (scan-sexps pos -1)))
          (setq label "Opening: ")))

        ;; 2. RETRIEVE CONTENT IF OFF-SCREEN
        (when (and match-pos 
                   label
                   (not (pos-visible-in-window-p match-pos)))
          (with-current-buffer (current-buffer)
            (save-excursion
              (goto-char match-pos)
              (setq line-content (string-trim (substring-no-properties (thing-at-point 'line t)))))))

        ;; 3. DISPLAY
        (when line-content
          (funcall callback 
                   (concat
                    (propertize label 'face '(:inherit shadow :weight bold))
                    (propertize line-content 'face '(:inherit font-lock-keyword-face :weight bold))))))))

  ;; Register with ElDoc (High Priority -50) to appear before general docs
  (add-hook 'eldoc-mode-hook
            (lambda ()
              (add-hook 'eldoc-documentation-functions #'ar/paren-eldoc-offscreen-context -50 t))))
#+end_src