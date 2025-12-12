# PLAN for custom centuar emacs fork
**Important** When writing all the contents of the org file and lisp folder files, make sure to compare it without the original centaur emacs config

- [x] place the following files in lisp subfolder:
   - [x] init-const
   - [x] init-custom
   - [x] init-func

- [ ] Place all the code of `init.el` inside an `config.org` file and make sure the order of configs is same as `init.el`

- [ ] The following files are being completely placed in `config.org`
  - [x] init-package
  - [x] init-base.el
  - [x] init-hydra.el

  - [x] init-ui.el
  - [x] init-edit.el
  - [x] init-completion.el
  - [x] init-snippet.el

  - [x] init-bookmark.el
  - [x] init-dashboard.el
  - [x] init-dired.el
  - [x] init-highlight.el
  - [x] init-ibuffer.el
  - [x] init-kill-ring.el
  - [x] init-workspace.el
  - [x] init-window.el
  - [x] init-treemacs.el

  - [ ] init-eshell.el
  - [ ] init-shell.el

  - [x] init-markdown.el
  - [x] init-org.el
  - [x] init-reader.el

  - [ ] init-dict.el
  - [ ] init-docker.el
  - [ ] init-player.el
  - [x] init-utils.el

  - [ ] init-vcs.el
  - [x] init-check.el
  - [x] init-lsp.el
  - [x] init-dap.el
  - [ ] init-ai.el

  - [x] init-prog.el
  - [x] init-elisp.el
  - [ ] init-c.el
  - [ ] init-go.el
  - [ ] init-rust.el
  - [ ] init-python.el
  - [ ] init-ruby.el
  - [ ] init-elixir.el
  - [ ] init-web.el

- [ ] leaving out `(eval-when-compile)` from the following files.
  - [x] init-package.el
  - [x] init-base.el
  - [x] init-ui.el
  - [x] init-edit.el
  - [x] init-completion.el
- [ ] Compare and contrast all the sections and subsections in `config.org` for the following commented out code:
```elisp
(eval-when-compile
  (require 'init-const)
  (require 'init-custom))
```

- [ ] Set general and evil keybindings for centaur keybindings
- [ ] Remove diminish from everywhere
- [ ] Keep all the lisp files from original centaur emacs config to keep track of
- [ ] Learn how to fork a project in github within a specific branch my repo
- [ ] Repeat the init-lsp.el integration with init-lsp-modified.el

***
# Issue
**Some of the integrations from centaur emacs do not work well for evil integrations**

# New Plan - Integrate between emacs-configv1.org and emacs-configv2.org  in emacs-mod.org

- [x] setup elpaca, evil, general and org mode at the very beginning, then use this config as the current config for further editing
- [x] don't use evil-org
- [ ] add some subsections of the highlight subsection at the end
- [x] add easysession from minimal-emacs.d to session management section
- [x] add the following code to org-mode
```elisp
  ;; (org-fontify-done-headline t)
  ;; (org-fontify-todo-headline t)
  ;; (org-fontify-whole-heading-line t)
  ;; (org-fontify-quote-and-verse-blocks t)
```
- [x] add treesit-auto from minimal-emacs.d
- [ ] add evil alternative drag-stuff
- [ ] determine if Optimization, Frame, Suppress GUI features, and Transient Posframe  subsections in UI and Theming is needed.
- [ ] don't add any frame-padding until the end
- [x] combine treesit configs from both files
- [x] for the folding behaviour use https://github.com/doomemacs/doomemacs/blob/master/modules/editor/fold/config.el as a guide and use hide-show, evil-vimish-fold and treesit fold
- [ ] list-environment is not necessary
- [ ] Assuming that I have made the changes you suggested, how do I make the area below the modeline as highlighted in the screenshot more useful? I want it to show matching parenthesis that are not visible in the buffer. I want it show org headlines and subheadlines info. I want this area to show other useful stuff as well that you think might be useful. Search the web to determine this. And only write out the changes needed. Think for a while for this task as well.

Based on my research, here are the changes to make the echo area/minibuffer more useful:

## Changes to Add

### 1. Show Matching Parenthesis Context (Emacs 29+)

Add to your **Editor Behaviour** section or create a new **Show Paren Enhancement** section:

```elisp
** Enhanced Parenthesis Matching
#+begin_src emacs-lisp
;; Show context in echo area when matching paren is offscreen
(use-package paren
  :ensure nil
  :hook (elpaca-after-init . show-paren-mode)
  :custom
  ;; CRITICAL: Show offscreen paren context in echo area (Emacs 29+)
  (show-paren-context-when-offscreen t)
  ;; Alternative: Show in overlay/child frame
  ;; (show-paren-context-when-offscreen 'overlay) ; or 'child-frame
  (show-paren-delay 0.1)
  (show-paren-style 'mixed)  ; Highlight expr if paren offscreen
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t))
#+end_src
```

### 2. Breadcrumbs for Org Headings & Project Context

Add a new section before or after your Org Mode configuration:

```elisp
* Breadcrumb Navigation
#+begin_src emacs-lisp
;; Show context breadcrumbs in header-line
(use-package breadcrumb
  :defer t
  :commands (breadcrumb-mode breadcrumb-local-mode)
  :hook ((prog-mode org-mode text-mode) . breadcrumb-local-mode)
  :custom
  ;; Customize breadcrumb appearance
  (breadcrumb-project-max-length 0.3)  ; 30% of frame width for project
  (breadcrumb-imenu-max-length 0.6)    ; 60% for imenu/org headings
  (breadcrumb-imenu-crumb-separator " › ")
  (breadcrumb-project-crumb-separator " / ")
  :config
  ;; For org-mode, breadcrumb uses imenu which shows heading hierarchy
  ;; Example: "* Main Heading › ** Sub Heading › *** Current Position"
  (setq breadcrumb-idle-delay 0.5))
#+end_src
```

### 3. Enhanced ElDoc for Contextual Documentation

Add after your **Elisp** section or in **Editor Behaviour**:

```elisp
** ElDoc - Contextual Documentation
#+begin_src emacs-lisp
;; Enhanced echo area documentation
(use-package eldoc
  :ensure nil
  :commands (eldoc-mode global-eldoc-mode)
  :hook ((elpaca-after-init . global-eldoc-mode)
         (emacs-lisp-mode . eldoc-mode)
         (lisp-interaction-mode . eldoc-mode)
         (ielm-mode . eldoc-mode))
  :custom
  ;; Show documentation after brief idle time
  (eldoc-idle-delay 0.3)
  ;; Don't truncate documentation eagerly
  (eldoc-echo-area-use-multiline-p t)
  ;; Prefer echo area over buffer
  (eldoc-echo-area-prefer-doc-buffer nil)
  ;; Display strategy for multiple sources (Emacs 28+)
  (eldoc-documentation-strategy 'eldoc-documentation-compose)
  :config
  ;; Dedicated buffer for long documentation (C-h .)
  (global-set-key (kbd "C-h .") 'eldoc-doc-buffer))
#+end_src
```

### 4. Which-Key for Keybinding Discovery

Add a new section after your **General Keybindings**:

```elisp
* Which-Key - Keybinding Hints
#+begin_src emacs-lisp
(use-package which-key
  :commands which-key-mode
  :hook (elpaca-after-init . which-key-mode)
  :custom
  ;; Show hints after brief delay
  (which-key-idle-delay 0.5)
  (which-key-idle-secondary-delay 0.05)
  
  ;; Use bottom side window (not minibuffer to avoid conflicts)
  (which-key-popup-type 'side-window)
  (which-key-side-window-location 'bottom)
  (which-key-side-window-max-height 0.25)
  
  ;; Sorting and display
  (which-key-sort-order 'which-key-key-order-alpha)
  (which-key-min-display-lines 5)
  (which-key-max-display-columns nil)
  
  ;; Add separators for readability
  (which-key-separator " → ")
  (which-key-prefix-prefix "+" )
  
  ;; Special key display
  (which-key-special-keys '("SPC" "TAB" "RET" "ESC" "DEL"))
  
  :config
  ;; Show prefix help immediately with C-h
  (setq which-key-show-early-on-C-h t)
  (setq which-key-show-prefix 'echo)
  
  ;; Hide some mode-line lighters
  (diminish 'which-key-mode))
#+end_src
```

### 5. Org-Specific: Display Outline Path

Add to your **Org Mode** configuration:

```elisp
;; Show org heading context on demand
(with-eval-after-load 'org
  (setq org-display-remote-inline-images 'cache)
  
  ;; Auto-show outline path in echo area (optional)
  ;; Uncomment if you want automatic display
  ;; (add-hook 'post-command-hook
  ;;           (lambda ()
  ;;             (when (and (derived-mode-p 'org-mode)
  ;;                        (not (active-minibuffer-window)))
  ;;               (org-display-outline-path t))))
  
  ;; Manual command to show outline path: C-c C-w
  (define-key org-mode-map (kbd "C-c C-/") 'org-display-outline-path))
```

### 6. Optional: Custom Echo Area Status

Add a new section for additional context:

```elisp
** Custom Context Display
#+begin_src emacs-lisp
;; Show additional context in mode-line (less intrusive than echo area)
(defun ar/show-file-position ()
  "Show file position info in mode-line."
  (interactive)
  (message "%s | Line %d/%d (%d%%) | Col %d | %s"
           (if buffer-file-name
               (abbreviate-file-name buffer-file-name)
             (buffer-name))
           (line-number-at-pos)
           (count-lines (point-min) (point-max))
           (/ (* 100 (- (point) (point-min)))
              (max 1 (- (point-max) (point-min))))
           (current-column)
           (format-mode-line mode-name)))

;; Bind to a key
(global-set-key (kbd "C-c i") 'ar/show-file-position)
#+end_src
```

## Summary of What Each Does

**show-paren-context-when-offscreen**: Shows context in the echo area when point is in a closing delimiter and the opening delimiter is offscreen.

**breadcrumb-mode**: Shows project path and imenu breadcrumbs (including org headings) in the header line. For org-mode, displays the full heading hierarchy.

**eldoc-mode**: Displays documentation for symbols at point in the echo area, including function arguments and variable documentation.

**which-key**: Displays available keybindings in a popup after you type a prefix key like SPC.

## Expected Results

- **Parenthesis context**: When you close a `)` and its opening `(` is offscreen, you'll see the line with the opening paren in the echo area
- **Org breadcrumbs**: Header line shows "Project › File › * Main › ** Sub › *** Current"
- **Function help**: In elisp/prog modes, hover over a function to see its signature
- **Keybinding hints**: Press `SPC` and wait 0.5s to see all available keybindings
- **Better discoverability**: No more forgetting what keys are available under a prefix

These changes make the echo area and surrounding UI much more informative without being intrusive!


***
## Integrating scimax modules
- [ ] scimax-autoformat-abbrev.el [Provides some flyspell stuff] - Also determine is flyspell is needed if jinx from below is used.
- [ ] scimax-ob.el
- [ ] scimax-jinx.el [Remove ivy and avy integrations]
- [ ] scimax-jupyter.el [Remove hydra stuff for now]
- [ ] scimax-latex.el


