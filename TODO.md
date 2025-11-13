- [ ] Use https://github.com/MatthewZMD/.emacs.d to optimize my emacs configuration


- [ ] **Duplicate/Overlapping Configurations**

Here are the changes needed to remove duplicates and conflicts:

```emacs-lisp
;; In ** Org Mode Integration (Line ~2770)
;; REMOVE duplicate org-cite configuration that's already in Citar section
;; Delete these lines as they're redundant:
(with-eval-after-load 'org
  (setq org-cite-global-bibliography (my/citar-bibliography)
        org-cite-follow-processor 'citar
        org-cite-activate-processor 'citar
        org-cite-insert-processor 'citar))

;; In ** Smartparens (around line 1790)
;; REMOVE duplicate evil replace mode hooks - they appear twice
;; Keep only the version in :config section, delete from with-eval-after-load

;; In ** Core Configuration (around line 420)
;; REMOVE duplicate visual-line-mode setup
;; The Lines Behaviour section already handles this
;; Delete this line from add-hook at line ~520:
(add-hook 'after-init-hook #'global-visual-line-mode)
```

- [ ] **Debug Green Color Issue**

The green color in the screenshot appears to be from `git-gutter` or `diff-hl`.
Add this debugging helper:

```emacs-lisp
;; Add this to ** Miscellaneous section

;; Debug face at point
(defun my/what-face ()
  "Show face information at point for debugging."
  (interactive)
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face
        (message "Face: %s" face)
      (message "No face at point"))))

;; Bind to a key
(global-set-key (kbd "C-c f") #'my/what-face)

;; If the green is from git-gutter, you can disable it:
(with-eval-after-load 'git-gutter
  ;; Adjust git-gutter colors to match theme
  (set-face-background 'git-gutter:modified "#e0af68") ; yellow
  (set-face-background 'git-gutter:added "#9ece6a")    ; green
  (set-face-background 'git-gutter:deleted "#f7768e")  ; red
  
  ;; Or disable git-gutter in specific modes if it's interfering
  (add-hook 'org-mode-hook (lambda () (git-gutter-mode -1))))
```

---

---

- [ ] **Lexical Binding Header (Line 11)**

**Problem**: The lexical binding comment is tangled to `config.el` instead of
being at the absolute top.

**Fix**: Add this as the very first line in your first code block:

```elisp
;;; config.el --- Main configuration file -*- lexical-binding: t; -*-
```

**Current code (line 11)**:

```elisp
;;; init.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-
```

**Should be**:

```elisp
;;; config.el --- Main configuration file -*- lexical-binding: t; -*-
```

---

---

- [ ] **LSP-Bridge CAPF Setting**

**Problem**: You've disabled `acm-enable-capf nil` which removes non-LSP
completion support.

**Current (line 1421-1428)**:

```elisp
(setq acm-enable-doc nil
        acm-enable-jupyter t
        acm-enable-capf nil  ;; <-- This disables useful completions
        ...
```

**Recommended fix**:

```elisp
(setq acm-enable-doc nil
        acm-enable-jupyter t
        acm-enable-capf t  ;; Enable capf for modes without LSP
        ...
```

**Rationale**: Based on
[lazycat's own config](https://github.com/manateelazycat/lazycat-emacs/blob/master/site-lisp/config/init-lsp-bridge.el#L33),
he enables this feature. It provides completion in modes without LSP support.

### 3. **Dashboard `initial-buffer-choice` Conflicts with Daemon Mode**

**Problem**: Setting `initial-buffer-choice` to dashboard can cause issues with
`emacsclient`.

**Current (line 611)**:

```elisp
(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))))
```

**Fix**:

```elisp
:config
  (dashboard-setup-startup-hook)
  ;; Better daemon-mode support
  (setq initial-buffer-choice 
        (lambda ()
          (if (daemonp)
              (get-buffer-create "*scratch*")
            (get-buffer "*dashboard*")))))
```

### 6. **Org Fragtog Hook Issue**

**Problem**: The hook sets a global variable inside a lambda, which doesn't work
as intended.

**Current (line 3159)**:

```elisp
:hook (org-mode . (lambda ()
                    (setq org-startup-with-latex-preview t)  ;; This sets it globally!
                    (org-fragtog-mode)))
```

**Fix**:

```elisp
:hook (org-mode . org-fragtog-mode)
:config
  (setq org-startup-with-latex-preview t)  ;; Set globally once
```

### 8. **Orderless Dispatcher - Missing Function**

**Current (line 2611)**:

```elisp
orderless-style-dispatchers (list #'+orderless-consult-dispatch
                                  #'orderless-kwd-dispatch  ;; This doesn't exist!
                                  #'orderless-affix-dispatch)))
```

**Fix**: Remove the nonexistent `orderless-kwd-dispatch`:

```elisp
orderless-style-dispatchers (list #'+orderless-consult-dispatch
                                  #'orderless-affix-dispatch)))
```

### 9. **Missing `require` for ox-latex**

Your org-latex configuration references `ox-latex` features but doesn't ensure
it's loaded in the right order.

**Current (line 3186)** has a comment but the fix could be better:

```elisp
;; Fixed Org Mode Integration for Tectonic
(with-eval-after-load 'org
  ;; CRITICAL: Require ox-latex BEFORE configuring latex settings
  (require 'ox-latex)
```

# Stuff to add in Emacs.org in the following order
- [x] Exec From Path Shell 
- [x] Sudo Edit
- [x] Optimize Editing Experience
- [x] UTF-8 Coding System
- [x] add recentf-exclude and some other stuff to my own recentf setup
- [x] Small Configs
- [x] Smooth Scrolling 
- [ ] Functions - *Top Level Header* & *General keybindings*
- [ ] Maybe the comprehensive escape key behaviour is not necessary if I have replaced the `C-g` keybinding with `[escape]`?
- [x] Replace the whole Session Management content with that from minimal-emacs.d
- [ ] Add outline indent from minimal-emacs.d. Make sure it works with lsp-bridge
- [x] Replace the custom remove strip whitespace function with with stripspace from minimal-emacs.d. Make sure this works in lsp-bridge as well
- [ ] Search the web and determine how to prevent evil mode from copying the same section that is pasted upon.
- [x] Add easysession from minimal-emacs.d
- [ ] Test treemacs configuration from minimal-emacs.d
- [ ] Partially replace snippets configuration with that from minimal-emacs.d
- [ ] **Important** Re-add my snippets config
- [ ] Switch to Catppuccin Mocha Theme
- [ ] **Issue** The extensive escape setup for minibuffer is not working as expected. I need to be able to hide minibuffers that appear at the bottom with escape key like doom emacs 
- [ ] **Add stuff to early-init.el file**
- [ ] **Are there alternative tools to the complex functions I wrote?**
- [ ] **Later** Add some emacs packages listed in awesome emacs
- [ ] **Important** Integrate back to minimal-emacs.d and add many of their recommended packages


