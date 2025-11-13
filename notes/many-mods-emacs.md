I'll help you with these tasks. Let me search for the best practices and current information.Based on my research, here are the changes needed for your Emacs configuration:

## 1. Save All Buffers with Spacemacs-Style Keybinding

**Replace the existing code** with this leader key binding:

```elisp
;; In the "Enhanced Window, Buffer & General Keybindings" section
;; Add this to the buffer management keybindings:

(defun ar/save-all-buffers ()
  "Save all file-visiting buffers without prompting."
  (interactive)
  (save-some-buffers t))

(ar/global-leader
  ;; ... existing buffer keybindings ...
  "b s" '(ar/save-all-buffers :wk "Save All Buffers")  ; Changed from save-buffer
  "b S" '(save-buffer :wk "Save Current Buffer"))      ; Add this for single buffer save
```

**Remove these lines** from your configuration:
```elisp
;; DELETE THESE:
(defun save-all-buffers ()
  "Instead of `save-buffer', save all opened buffers by calling `save-some-buffers' with ARG t."
  (interactive)
  (save-some-buffers t))
(global-set-key (kbd "C-x C-s") nil)
(global-set-key (kbd "C-x C-s") #'save-all-buffers)
```

## 2. Fix LSP-Bridge Search Words in Org-Mode

The issue is that adaptive-wrap functionality needs to be initialized after ELPA packages. Your hook function needs to explicitly check the buffer's major mode. **Replace the existing LSP-Bridge configuration** in the "LSP Bridge" section:

```elisp
;; In the "LSP Bridge" section, replace the search-words configuration:

(use-package lsp-bridge
  ;; ... existing config ...
  :config
  (global-lsp-bridge-mode)
  (setq lsp-bridge-python-multi-lsp-server "basedpyright_ruff"
        lsp-bridge-tex-lsp-server "texlab"
        lsp-bridge-nix-lsp-server "nil")
  
  ;; Disable search-words globally by default
  (setq lsp-bridge-enable-search-words nil)
  
  (setq acm-enable-doc nil
        acm-enable-jupyter t
        acm-enable-capf nil
        acm-enable-doc-markdown-render 'async
        acm-enable-icon t
        acm-candidate-match-function 'orderless-literal
        acm-backend-search-file-words-enable-fuzzy-match t)

  (setq lsp-bridge-enable-hover-diagnostic t
        lsp-bridge-enable-auto-format-code t
        lsp-bridge-enable-with-tramp t
        lsp-bridge-enable-inlay-hint t
        lsp-bridge-signature-show-function 'lsp-bridge-signature-show-with-frame
        lsp-bridge-enable-org-babel t
        lsp-bridge-org-babel-lang-list nil)

  (setq lsp-bridge-semantic-tokens t)
  (setq-default lsp-bridge-semantic-tokens-ignore-modifier-limit-types ["variable"]))

;; Enable search words only in specific modes - FIXED VERSION
(defun my/lsp-bridge-enable-search-words-for-mode ()
  "Enable search words for current buffer based on major mode.
Only enables for text-mode and markdown-mode, explicitly excluding org-mode."
  (setq-local lsp-bridge-enable-search-words
              (and (memq major-mode '(text-mode markdown-mode))
                   (not (derived-mode-p 'org-mode)))))

(add-hook 'lsp-bridge-mode-hook #'my/lsp-bridge-enable-search-words-for-mode)

;; Ensure org-mode specifically disables search-words
(add-hook 'org-mode-hook 
          (lambda () 
            (setq-local lsp-bridge-enable-search-words nil)))
```

## 3. Adaptive-Wrap Configuration

The adaptive-wrap package provides the `adaptive-wrap-prefix-mode' minor mode which sets the wrap-prefix property for better visual line wrapping. **Add this configuration** in the "Editor Behaviour" section, after the "Lines Behaviour" subsection:

```elisp
** Adaptive Wrap
#+begin_src emacs-lisp
(use-package adaptive-wrap
  :hook ((text-mode . adaptive-wrap-prefix-mode)
         (prog-mode . adaptive-wrap-prefix-mode)
         (org-mode . adaptive-wrap-prefix-mode))
  :custom
  ;; Amount of extra indent added to continuation lines
  (adaptive-wrap-extra-indent 0)
  :config
  ;; Ensure it works well with visual-line-mode
  (when (fboundp 'visual-line-mode)
    (add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode)))
#+end_src
```

## 4. Replace Doom Tokyo Night with Catppuccin Mocha

The default flavor is Mocha, and you can set the flavor with `setq catppuccin-flavor` before loading the theme. **Replace the entire "Theming" subsection** in the "UI & Theming" section:

```elisp
** Theming
#+begin_src emacs-lisp
(use-package catppuccin-theme
  :custom
  (catppuccin-enlarge-headings t)
  (catppuccin-height-title1 1.3)
  (catppuccin-height-title2 1.2)
  (catppuccin-height-title3 1.1)
  :config
  ;; Set Mocha flavor (default, but explicit is better)
  (setq catppuccin-flavor 'mocha)
  (load-theme 'catppuccin t)

  ;; Set distinct colors for bold and italic using Catppuccin Mocha colors
  (custom-set-faces
   '(bold ((t (:foreground "#89b4fa" :weight bold))))      ; Blue
   '(italic ((t (:foreground "#cba6f7" :slant italic)))))) ; Mauve
#+end_src
```

**Update all Tokyo Night color references** throughout your configuration with these Catppuccin Mocha colors:

```elisp
;; Catppuccin Mocha Color Palette
;; Rosewater: #f5e0dc
;; Flamingo:  #f2cdcd
;; Pink:      #f5c2e7
;; Mauve:     #cba6f7
;; Red:       #f38ba8
;; Maroon:    #eba0ac
;; Peach:     #fab387
;; Yellow:    #f9e2af
;; Green:     #a6e3a1
;; Teal:      #94e2d5
;; Sky:       #89dceb
;; Sapphire:  #74c7ec
;; Blue:      #89b4fa
;; Lavender:  #b4befe
;; Text:      #cdd6f4
;; Subtext1:  #bac2de
;; Subtext0:  #a6adc8
;; Overlay2:  #9399b2
;; Overlay1:  #7f849c
;; Overlay0:  #6c7086
;; Surface2:  #585b70
;; Surface1:  #45475a
;; Surface0:  #313244
;; Base:      #1e1e2e
;; Mantle:    #181825
;; Crust:     #11111b
```

**Specific sections to update:**

### Rainbow Delimiters (in "Editor Behaviour" section):
```elisp
** Rainbow Delimiters
#+begin_src emacs-lisp
(use-package rainbow-delimiters
  :hook ((text-mode . rainbow-delimiters-mode)
         (LaTeX-mode . rainbow-delimiters-mode)
         (prog-mode . rainbow-delimiters-mode))
  :custom-face
  (rainbow-delimiters-depth-1-face ((t (:foreground "#89b4fa"))))  ; Blue
  (rainbow-delimiters-depth-2-face ((t (:foreground "#cba6f7"))))  ; Mauve
  (rainbow-delimiters-depth-3-face ((t (:foreground "#f9e2af"))))  ; Yellow
  (rainbow-delimiters-depth-4-face ((t (:foreground "#89dceb"))))  ; Sky
  (rainbow-delimiters-depth-5-face ((t (:foreground "#f38ba8"))))  ; Red
  (rainbow-delimiters-depth-6-face ((t (:foreground "#a6e3a1"))))  ; Green
  (rainbow-delimiters-depth-7-face ((t (:foreground "#fab387"))))  ; Peach
  (rainbow-delimiters-depth-8-face ((t (:foreground "#cdd6f4"))))  ; Text
  (rainbow-delimiters-depth-9-face ((t (:foreground "#bac2de"))))) ; Subtext1
#+end_src
```

### Org TODO Keywords (in "Org Mode" section):
```elisp
(org-todo-keyword-faces
 '(("TODO"      . (:foreground "#f38ba8" :weight bold))   ; Red
   ("NEXT"      . (:foreground "#fab387" :weight bold))   ; Peach
   ("PROG"      . (:foreground "#89b4fa" :weight bold))   ; Blue
   ("WAIT"      . (:foreground "#f9e2af" :weight bold))   ; Yellow
   ("DONE"      . (:foreground "#a6e3a1" :weight bold))   ; Green
   ("CANCEL"    . (:foreground "#6c7086" :weight bold))   ; Overlay0
   ("PLAN"      . (:foreground "#94e2d5" :weight bold))   ; Teal
   ("ACTIVE"    . (:foreground "#cba6f7" :weight bold))   ; Mauve
   ("PAUSED"    . (:foreground "#bac2de" :weight bold))   ; Subtext1
   ("ACHIEVED"  . (:foreground "#a6e3a1" :weight bold))   ; Green
   ("DROPPED"   . (:foreground "#6c7086" :weight bold)))) ; Overlay0
```

### Org Modern (in "Org Mode" section):
```elisp
(setq org-modern-todo-faces
      '(("TODO"      . (:foreground "#f38ba8" :weight bold))
        ("NEXT"      . (:foreground "#fab387" :weight bold))
        ("PROG"      . (:foreground "#89b4fa" :weight bold))
        ("WAIT"      . (:foreground "#f9e2af" :weight bold))
        ("DONE"      . (:background "#313244" :foreground "#a6e3a1" :weight bold))
        ("CANCEL"    . (:strike-through t :foreground "#6c7086"))
        ("PLAN"      . (:foreground "#94e2d5" :weight bold))
        ("ACTIVE"    . (:foreground "#cba6f7" :weight bold))
        ("PAUSED"    . (:foreground "#bac2de" :weight bold))
        ("ACHIEVED"  . (:background "#313244" :foreground "#a6e3a1" :weight bold :box t))
        ("DROPPED"   . (:strike-through t :foreground "#6c7086"))))
```

### PDF-Tools midnight colors (in "PDF-Tools" section):
```elisp
:custom
(pdf-view-midnight-colors '("#1e1e2e" . "#cdd6f4"))  ; Base . Text

:custom-face
(pdf-view-highlight-face ((t (:background "#f9e2af" :foreground "#1e1e2e")))) ; Yellow . Base
(pdf-view-link-face ((t (:foreground "#89b4fa"))))         ; Blue
(pdf-view-active-link-face ((t (:foreground "#cba6f7"))))  ; Mauve
```

These changes will give you a complete Catppuccin Mocha theme throughout your Emacs configuration while maintaining all your existing functionality!
