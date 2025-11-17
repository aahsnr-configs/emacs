;;; post-init.el --- Main Configuration File -*- no-byte-compile: t; lexical-binding: t; -*-
;;(declare (special minimal-emacs-use-userr-directory))

(defconst *sys/win32*
  (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")

(defconst *sys/linux*
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst *sys/mac*
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defconst python-p
  (or (executable-find "python3")
      (and (executable-find "python")
	   ;;<
           (> (length (shell-command-to-string "python --version | grep 'Python 3'")) 0)))
  "Do we have python3?")

(defconst pip-p
  (or (executable-find "pip3")
      (and (executable-find "pip")
	   ;;<
           (> (length (shell-command-to-string "pip --version | grep 'python 3'")) 0)))
  "Do we have pip3?")

(defconst clangd-p
  (or (executable-find "clangd")  ;; usually
      (executable-find "/usr/local/opt/llvm/bin/clangd"))  ;; macOS
  "Do we have clangd?")

(defconst eaf-env-p
  (and (display-graphic-p) python-p pip-p)
  "Do we have EAF environment setup?")

;; ====================
;; PGTK/Wayland Mitigations
;; ====================
;; Mitigate Input Lag by disabling GTK input methods.
;; The function `pgtk-use-im-context` MUST be called after a frame is
;; created, otherwise it will error. We hook it into `after-make-frame-functions`
;; to ensure it runs at the correct time, both on startup and for new frames
;; created by emacsclient in daemon mode.
(when (fboundp 'pgtk-use-im-context)
  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (with-selected-frame frame
                (pgtk-use-im-context nil)))))


;; ====================
;; BIDI OPTIMIZATION
;; ====================
;; Disable bidirectional text for massive performance boost in long lines
(setq-default bidi-display-reordering nil
              bidi-paragraph-direction 'left-to-right
              bidi-inhibit-bpa t)

;; Apply to common modes
(defun my/bidi-optimizations ()
  "Apply bidi optimizations for current buffer."
  (setq-local bidi-paragraph-direction 'left-to-right
              bidi-inhibit-bpa t))

(add-hook 'org-mode-hook #'my/bidi-optimizations)
(add-hook 'text-mode-hook #'my/bidi-optimizations)
(add-hook 'prog-mode-hook #'my/bidi-optimizations)

;; ====================
;; LONG LINE OPTIMIZATION
;; ====================
;; Critical for files with extremely long lines (JSON, minified files)
(setq-default long-line-threshold 1000
              large-hscroll-threshold 1000
              syntax-wholeline-max 1000)

;; Enable so-long mode for automatic handling
(when (fboundp 'global-so-long-mode)
  (global-so-long-mode))

;; ====================
;; REDISPLAY OPTIMIZATIONS
;; ====================
(setq redisplay-skip-fontification-on-input t)  ; Doom's setting
(setq jit-lock-defer-time 0)
(setq jit-lock-stealth-time 0.5)

;; ====================
;; CURSOR OPTIMIZATION
;; ====================
(setq-default cursor-in-non-selected-windows nil)  ;; Don't show cursor in other windows
(setq highlight-nonselected-windows nil)

;; ====================
;; FRAME RESIZE
;; ====================
(setq frame-resize-pixelwise t)
(setq window-resize-pixelwise nil)

;; ====================
;; COMPILATION OPTIMIZATIONS
;; ====================
(setq byte-compile-warnings '(not obsolete))
(setq native-comp-async-report-warnings-errors 'silent)

;; ====================
;; IDLE OPTIMIZATIONS
;; ====================
;; Don't ping things that look like domain names during idle
(setq ffap-machine-p-known 'reject)

;; ====================
;; FONT RENDERING
;; ====================
(setq inhibit-compacting-font-caches t)  ;; Don't compact font cache

;; Move backup files to dedicated directory
(setq backup-directory-alist `(("." . ,(expand-file-name ".backup" user-emacs-directory))))

;; Confirmation settings
(setq confirm-kill-processes nil)  ;; Auto-kill processes on exit

;; Faster echo of keystrokes
(setq echo-keystrokes 0.1)

;; Don't create lockfiles
(setq-default create-lockfiles nil)

;; Compilation settings
(setq-default compilation-always-kill t
              compilation-ask-about-save nil
              compilation-scroll-output t)

;; Suppress redefinition warnings
(setq ad-redefinition-action 'accept)

;; Custom file location
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

;; Require final newline
(setq require-final-newline t)

;; Enable commands
(put 'erase-buffer 'disabled nil)

;; Global modes
(global-hl-line-mode 1)
(delete-selection-mode 1)

;; File associations
(add-to-list 'auto-mode-alist '("\\.in\\'" . text-mode))
(add-to-list 'auto-mode-alist '("\\.out\\'" . text-mode))
(add-to-list 'auto-mode-alist '("\\.args\\'" . text-mode))
(add-to-list 'auto-mode-alist '("\\.bb\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.bbclass\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.Rmd\\'" . markdown-mode))

;; Alt key mapping
(setq x-alt-keysym 'meta)

(setq user-full-name "Ahsanur Rahman"
      user-mail-address "ahsanur041@proton.me")

;; Unbind unneeded keys
(global-set-key (kbd "C-z") nil)
(global-set-key (kbd "M-z") nil)
(global-set-key (kbd "M-m") nil)
(global-set-key (kbd "C-x C-z") nil)
(global-set-key (kbd "M-/") nil)
;; Truncate lines
(global-set-key (kbd "C-x C-l") #'toggle-truncate-lines)
;; Adjust font size like web browsers
(global-set-key (kbd "C-=") #'text-scale-increase)
(global-set-key (kbd "C-+") #'text-scale-increase)
(global-set-key (kbd "C--") #'text-scale-decrease)
;; Move up/down paragraph
(global-set-key (kbd "M-n") #'forward-paragraph)
(global-set-key (kbd "M-p") #'backward-paragraph)
;; Revert buffer
(global-set-key (kbd "<f5>") #'revert-buffer-quick)

(defun ar/reload-config ()
  "Reload the Emacs configuration."
  (interactive)
  ;; Assuming config.org is the main configuration file and this config.el is tangled from it.
  ;; If config.el is the primary config, change to: (load-file (expand-file-name "config.el" user-emacs-directory))
  (let ((config-file "~/.config/emacs/post-init.el"))
    (if (file-exists-p config-file)
        (progn
          (message "Reloading Emacs configuration from config.org...")
          (org-babel-load-file config-file)
          (message "Configuration reloaded successfully!"))
      (error "Configuration file %s not found" config-file))))

;; NEW: Proper daemon shutdown function
(defun ar/daemon-shutdown ()
  "Properly shutdown the Emacs daemon without hanging."
  (interactive)
  (if (daemonp)
      (progn
        ;; Save important state quietly - with proper checks
        (when (and (fboundp 'persp-save-state-to-file)
                   (bound-and-true-p persp-mode)
                   (boundp 'persp-hash)
                   persp-hash
                   (hash-table-p persp-hash))
          (condition-case nil
              (persp-save-state-to-file)
            (error nil)))

        ;; Save recentf quietly
        (when (bound-and-true-p recentf-mode)
          (condition-case nil
              (recentf-save-list)
            (error nil)))

        ;; Kill without interactive prompts
        (let ((kill-emacs-query-functions nil)
              (confirm-kill-processes nil))
          (save-some-buffers t)  ; Auto-save all buffers
          (kill-emacs)))
    (save-buffers-kill-terminal)))

;; Add temporarily to find the problematic functions
(defun ar/find-broken-interactive-forms ()
  "Find functions with broken interactive forms."
  (interactive)
  (occur "(interactive \"<"))

(defun my/org-babel-tangle-on-save ()
  "Tangle the current org file if #+auto_tangle: t is set."
  (when (and (eq major-mode 'org-mode)
             (buffer-file-name))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^#\\+auto_tangle: *t" nil t)
        (let ((inhibit-message t))
          (condition-case err
              (org-babel-tangle)
            (error
             (message "Tangle error: %s" (error-message-string err)))))))))

(add-hook 'after-save-hook #'my/org-babel-tangle-on-save)

(use-package autorevert
  :straight (:type built-in)
  :commands (auto-revert-mode global-auto-revert-mode)
  :hook
  (after-init . global-auto-revert-mode)
  :custom
  (auto-revert-interval 3)
  (auto-revert-remote-files nil)
  (auto-revert-use-notify t)
  (auto-revert-avoid-polling nil)
  (auto-revert-verbose t))

(use-package recentf
  :straight (:type built-in)
  :commands (recentf-mode recentf-cleanup)
  :hook
  (after-init . recentf-mode)

  :custom
  (recentf-auto-cleanup (if (daemonp) 300 'never))
  (recentf-exclude
   (list "\\.tar$" "\\.tbz2$" "\\.tbz$" "\\.tgz$" "\\.bz2$"
         "\\.bz$" "\\.gz$" "\\.gzip$" "\\.xz$" "\\.zip$"
         "\\.7z$" "\\.rar$"
         "COMMIT_EDITMSG\\'"
         "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
         "-autoloads\\.el$" "autoload\\.el$"))

  :config
  ;; A cleanup depth of -90 ensures that `recentf-cleanup' runs before
  ;; `recentf-save-list', allowing stale entries to be removed before the list
  ;; is saved by `recentf-save-list', which is automatically added to
  ;; `kill-emacs-hook' by `recentf-mode'.
  (add-hook 'kill-emacs-hook #'recentf-cleanup -90))

(use-package savehist
  :straight (:type built-in)
  :commands (savehist-mode savehist-save)
  :hook
  (after-init . savehist-mode)
  :custom
  (savehist-autosave-interval 600)
  (savehist-additional-variables
   '(kill-ring                        ; clipboard
     register-alist                   ; macros
     mark-ring global-mark-ring       ; marks
     search-ring regexp-search-ring)))

(use-package saveplace
  :straight (:type built-in)
  :commands (save-place-mode save-place-local-mode)
  :hook
  (after-init . save-place-mode)
  :custom
  (save-place-limit 400))

(use-package general
  :after evil
  :config
  (general-create-definer ar/global-leader
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (ar/global-leader
    ;; Core
    "SPC" '(execute-extended-command :wk "M-x")
    "q q" '(save-buffers-kill-terminal :wk "Quit Emacs")
    "q f" '(delete-frame :wk "Delete Emacs Client Frame")
    "q r" '(ar/reload-config :wk "Reload Config")))

(defun ar/set-fonts ()
  "Set the default, fixed-pitch, and variable-pitch fonts for the current frame."
  (set-face-attribute 'default nil :font "JetBrainsMono Nerd Font" :height 130 :weight 'medium)
  (set-face-attribute 'fixed-pitch nil :font "JetBrainsMono Nerd Font" :height 130 :weight 'medium)
  (set-face-attribute 'variable-pitch nil :font "JetBrainsMono Nerd Font" :height 130 :weight 'medium)
  ;; Apply italic slant to comments and keywords
  (set-face-attribute 'font-lock-comment-face nil :slant 'italic)
  (set-face-attribute 'font-lock-keyword-face nil :slant 'italic))

;; Set fonts on startup and for new frames in daemon mode
(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame (ar/set-fonts))))
  (ar/set-fonts))

;; Line spacing
(setq-default line-spacing 0.02)

;; Font settings
(setq font-lock-maximum-decoration t
      inhibit-compacting-font-caches t)

;; Only enable line numbers in prog-mode and conf-mode
;; CRITICAL: Never enable in org-mode or text-mode (huge performance hit)
(dolist (mode '(prog-mode-hook conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode t))))

;; Explicitly disable in text modes
(dolist (mode '(org-mode-hook text-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode -1))))

(use-package catppuccin-theme
  :demand t
  :custom
  (catppuccin-highlight-matches t)
  (catppuccin-italic-comments t)
  (catppuccin-dark-line-numbers-background nil)  ;; CHANGED: Set to nil to prevent nil backgrounds
  :config
  (setq catppuccin-flavor 'mocha)
  (load-theme 'catppuccin t)
  (custom-set-faces
   '(bold ((t (:foreground "#f5e0dc" :weight bold))))
   '(italic ((t (:foreground "#f38ba8" :slant italic))))
   '(sp-show-pair-match-face ((t (:background "#b4befe" :foreground "black" :weight medium))))
   ;; ADD: Explicit line-number faces to prevent nil values
   '(line-number ((t (:inherit default :foreground "#6c7086"))))
   '(line-number-current-line ((t (:inherit default :foreground "#cdd6f4" :weight bold))))))

(use-package solaire-mode
  :demand t
  :config
  (solaire-global-mode))

;; (use-package nerd-icons
;;   :defer 0.5
;;   :custom
;;   (nerd-icons-font-family "Symbols Nerd Font Mono")
;;   (nerd-icons-color-icons t))

(use-package nerd-icons
  :demand t  ; Force immediate load
  :custom
  (nerd-icons-font-family "Symbols Nerd Font Mono")
  (nerd-icons-color-icons t)
  :config
  (setq inhibit-compacting-font-caches t)
  ;; Pre-cache all icon families at startup
  (dolist (family '(nerd-icons-codicon
                    nerd-icons-faicon
                    nerd-icons-flicon
                    nerd-icons-mdicon
                    nerd-icons-octicon
                    nerd-icons-pomicon
                    nerd-icons-powerline
                    nerd-icons-sucicon
                    nerd-icons-wicon
                    nerd-icons-devicon))
    ;; Call each function once to cache glyphs
    (ignore-errors (funcall family "nf-md-home"))))

(use-package doom-modeline
  :defer 0.1
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-height 28 ;; changing from 28 to 1
        doom-modeline-bar-width 3
        doom-modeline-icon t
        doom-modeline-buffer-file-name-style 'auto
        doom-modeline-minor-modes nil
        doom-modeline-lsp-icon t
        doom-modeline-indent-info t
        doom-modeline-percent-position t
        doom-modeline-github-timer nil
        doom-modeline-gnus-timer nil))

(use-package dashboard
  :init
  (setq dashboard-banner-logo-title "Welcome to Emacs!")
  (setq dashboard-startup-banner 'logo)
  ;; For example: (setq dashboard-startup-banner "~/.emacs.d/emacs_logo.png")

  ;; Set the content of the dashboard
  (setq dashboard-items '((recents   . 5)
                         (bookmarks . 5)
                         (projects  . 5)
                         (agenda    . 5)))

  ;; Center the dashboard content
  (setq dashboard-center-content t)

  ;; Enable icons
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-icon-type 'nerd-icons)

  :config
  ;; Enable the dashboard on startup
  (dashboard-setup-startup-hook)
  ;; If you are using emacsclient, you'll want to see the dashboard when you create a new frame.
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))))

(use-package which-key
  :straight (:type built-in)
  :defer 0.5
  :hook (after-init . which-key-mode)
  :custom
  (which-key-idle-delay 0.1)
  (which-key-separator " → ")
  (which-key-popup-type 'minibuffer))

(setq-default internal-border-width 5)
(add-to-list 'default-frame-alist '(internal-border-width . 5))

(use-package undo-fu
  :commands (undo-fu-only-undo
             undo-fu-only-redo
             undo-fu-only-redo-all
             undo-fu-disable-checkpoint)
  :config
  (global-unset-key (kbd "C-z"))
  (global-set-key (kbd "C-z") 'undo-fu-only-undo)
  (global-set-key (kbd "C-S-z") 'undo-fu-only-redo))

(use-package undo-fu-session :commands undo-fu-session-global-mode)

(setq evil-undo-system 'undo-fu)

(use-package evil
  :demand t
  :init
  ;; Core behavior from Doom
  (setq evil-want-C-g-bindings t
        evil-want-C-i-jump nil
        evil-want-C-u-scroll t
        evil-want-C-u-delete t
        evil-want-C-w-delete t
        evil-want-Y-yank-to-eol t
        evil-want-abbrev-expand-on-insert-exit nil
        evil-want-integration t
        evil-want-keybinding nil
        evil-symbol-word-search t

        ;; Cursor appearance
        evil-default-cursor t
        evil-normal-state-cursor 'box
        evil-emacs-state-cursor 'box
        evil-insert-state-cursor 'bar
        evil-visual-state-cursor 'hollow
        evil-replace-state-cursor 'hbar
        evil-operator-state-cursor 'hollow

        ;; Search and highlighting
        evil-ex-search-vim-style-regexp t
        evil-ex-visual-char-range t
        evil-ex-interactive-search-highlight 'selected-window

        ;; Mode line
        evil-mode-line-format nil

        ;; Suppress "beginning/end of line" errors in macros
        evil-kbd-macro-suppress-motion-error t

        ;; Don't move cursor back when exiting insert mode
        evil-move-cursor-back nil

        ;; Fine undo - separate each action
        evil-want-fine-undo t

        ;; Don't move cursor beyond EOL
        evil-move-beyond-eol nil

        ;; Better search wrapping
        evil-search-wrap t
        evil-magic t
        evil-echo-state t
        evil-indent-convert-tabs t
        evil-ex-substitute-global t
        evil-insert-skip-empty-lines t
        evil-v$-excludes-newline t
        evil-respect-visual-line-mode t

        ;; Split window behavior
        evil-split-window-below t
        evil-vsplit-window-right t

        ;; Doom-specific: don't clobber the region on C-u
        evil-want-C-u-delete t

        ;; More vim-like behavior
        evil-want-Y-yank-to-eol t
        evil-want-fine-undo t)

  :config
  (evil-mode 1)
  (evil-select-search-module 'evil-search-module 'evil-search)

  ;; Set shift width
  (setq-default evil-shift-width tab-width)

  ;; Make evil-mode play nice with custom modes
  (evil-set-initial-state 'special-mode 'motion)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)

  ;; Don't interfere with localleader key
  (define-key evil-motion-state-map "\\" nil)

  ;; Make movement keys work on visual lines
  (define-key evil-motion-state-map "j" 'evil-next-visual-line)
  (define-key evil-motion-state-map "k" 'evil-previous-visual-line)
  (define-key evil-motion-state-map "gj" 'evil-next-line)
  (define-key evil-motion-state-map "gk" 'evil-previous-line))

(use-package evil-collection
  :after evil
  :init
  (setq evil-collection-setup-minibuffer t
        evil-collection-want-unimpaired-p nil)
  :config
  (evil-collection-init))

(use-package evil-nerd-commenter :after evil)
(use-package evil-numbers :after evil)
(use-package evil-args :after evil)
(use-package evil-anzu  :after evil)
(use-package evil-exchange :after evil :config (evil-exchange-install))
(use-package evil-indent-plus :after evil :config (evil-indent-plus-default-bindings))
(use-package evil-visualstar :hook (evil-mode . global-evil-visualstar-mode))
(use-package evil-snipe :after evil :config (evil-snipe-mode 1) (evil-snipe-override-mode 1))

(use-package evil-lion
  :after evil
  :hook (prog-mode . evil-lion-mode))

(use-package evil-multiedit :after evil :config (evil-multiedit-default-keybinds))
(use-package evil-goggles :hook (evil-mode . evil-goggles-mode) :custom (evil-goggles-duration 0.1))

(use-package goto-chg
  :after evil
  :commands (goto-last-change goto-last-change-reverse)
  :config
  (evil-define-key 'normal 'global (kbd "g;") 'goto-last-change)
  (evil-define-key 'normal 'global (kbd "g,") 'goto-last-change-reverse))

;; evil-quickscope: Visual aid for f/F/t/T motions
(use-package evil-quickscope
  :after evil
  :hook (prog-mode . turn-on-evil-quickscope-mode)
  :custom
  (evil-quickscope-cross-lines t)
  (evil-quickscope-accepted-chars "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))

(use-package evil-surround
  :after evil
  :commands global-evil-surround-mode
  :custom
  (evil-surround-pairs-alist
   '((?\( . ("(" . ")"))
     (?\[ . ("[" . "]"))
     (?\{ . ("{" . "}"))

     (?\) . ("(" . ")"))
     (?\] . ("[" . "]"))
     (?\} . ("{" . "}"))

     (?< . ("<" . ">"))
     (?> . ("<" . ">"))))
  :hook (after-init . global-evil-surround-mode))

(use-package evil-matchit
  :after evil
  :config
  (global-evil-matchit-mode 1))

(use-package evil-textobj-anyblock
  :after evil
  :config
  (define-key evil-inner-text-objects-map "b" 'evil-textobj-anyblock-inner-block)
  (define-key evil-outer-text-objects-map "b" 'evil-textobj-anyblock-a-block))

(use-package evil-embrace
  :after evil-surround
  :config
  (evil-embrace-enable-evil-surround-integration))

;; Doom-style number increment/decrement
(general-define-key
 :states '(normal visual)
 "C-a" 'evil-numbers/inc-at-pt
 "C-x" 'evil-numbers/dec-at-pt
 "g C-a" 'evil-numbers/inc-at-pt-incremental
 "g C-x" 'evil-numbers/dec-at-pt-incremental)

;; Doom-style text objects
(general-define-key
 :states 'motion
 "gx" 'evil-exchange
 "gX" 'evil-exchange-cancel)

(defcustom ar/evil-want-o/O-to-continue-comments t
  "If non-nil, o/O keys will continue comment lines."
  :type 'boolean
  :group 'evil)

(defun ar/evil--insert-newline-below-and-respect-comments-p (context)
  "Return non-nil if newline should respect comments in CONTEXT."
  (and ar/evil-want-o/O-to-continue-comments
       (eq (plist-get context :type) 'line-comment)))

(defun ar/evil--insert-newline-above-and-respect-comments-p (context)
  "Return non-nil if newline should respect comments in CONTEXT."
  (and ar/evil-want-o/O-to-continue-comments
       (eq (plist-get context :type) 'line-comment)))

(defun ar/evil-insert-newline-below (count)
  "Insert COUNT newlines below, respecting comments."
  (interactive "p")
  (save-excursion
    (if (and (fboundp 'sp-point-in-comment)
             (sp-point-in-comment)
             ar/evil-want-o/O-to-continue-comments)
        (let ((comment-start (or comment-start ""))
              (comment-padding (or comment-padding " ")))
          (end-of-line)
          (newline-and-indent)
          (when (and comment-start (not (string-empty-p comment-start)))
            (insert comment-start comment-padding)))
      (evil-open-below count)))
  (evil-insert-state))

(defun ar/evil-insert-newline-above (count)
  "Insert COUNT newlines above, respecting comments."
  (interactive "p")
  (save-excursion
    (if (and (fboundp 'sp-point-in-comment)
             (sp-point-in-comment)
             ar/evil-want-o/O-to-continue-comments)
        (let ((comment-start (or comment-start ""))
              (comment-padding (or comment-padding " ")))
          (beginning-of-line)
          (newline)
          (forward-line -1)
          (indent-according-to-mode)
          (when (and comment-start (not (string-empty-p comment-start)))
            (insert comment-start comment-padding)))
      (evil-open-above count)))
  (evil-insert-state))

(define-key evil-normal-state-map "o" #'ar/evil-insert-newline-below)
(define-key evil-normal-state-map "O" #'ar/evil-insert-newline-above)

(defun ar/evil--visual-search (direction)
  "Search for the visual selection in DIRECTION."
  (let* ((beg (region-beginning))
         (end (region-end))
         (selection (buffer-substring-no-properties beg end)))
    (deactivate-mark)
    (evil-ex-search-activate-highlight
     (list :pattern selection
           :forward (eq direction 'forward)))
    (if (eq direction 'forward)
        (evil-search-forward nil nil nil selection)
      (evil-search-backward nil nil nil selection))))

(defun ar/evil-visual-search-forward ()
  "Search forward for the visual selection."
  (interactive)
  (ar/evil--visual-search 'forward))

(defun ar/evil-visual-search-backward ()
  "Search backward for the visual selection."
  (interactive)
  (ar/evil--visual-search 'backward))

(define-key evil-visual-state-map "*" #'ar/evil-visual-search-forward)
(define-key evil-visual-state-map "#" #'ar/evil-visual-search-backward)

(with-eval-after-load 'evil-ex
  (evil-ex-define-cmd "g[lobal]" #'evil-ex-global)

  (defun ar/evil--highlight-global-matches ()
    "Highlight matches for :global command."
    (when-let ((pattern (car evil-ex-global-match)))
      (hi-lock-face-buffer pattern 'hi-yellow)))

  (add-hook 'evil-ex-global-hook #'ar/evil--highlight-global-matches)

  (evil-ex-define-cmd "al[ign]" #'align-regexp)
  (evil-ex-define-cmd "retab" #'ar/evil-retab))

(defun ar/evil-retab (&optional beg end)
  "Convert tabs to spaces or vice versa in region or buffer."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (unless (and beg end)
    (setq beg (point-min)
          end (point-max)))
  (if indent-tabs-mode
      (tabify beg end)
    (untabify beg end)))

(defcustom ar/evil-want-move-window-to-wrap-around nil
  "If non-nil, window movement commands wrap around."
  :type 'boolean
  :group 'evil)

(defun ar/evil-window-move-left ()
  "Move to window on the left, wrapping if needed."
  (interactive)
  (if (and ar/evil-want-move-window-to-wrap-around
           (not (ignore-errors (windmove-left))))
      (evil-window-bottom-right)
    (windmove-left)))

(defun ar/evil-window-move-right ()
  "Move to window on the right, wrapping if needed."
  (interactive)
  (if (and ar/evil-want-move-window-to-wrap-around
           (not (ignore-errors (windmove-right))))
      (evil-window-top-left)
    (windmove-right)))

(defun ar/evil-window-move-up ()
  "Move to window above, wrapping if needed."
  (interactive)
  (if (and ar/evil-want-move-window-to-wrap-around
           (not (ignore-errors (windmove-up))))
      (evil-window-bottom-right)
    (windmove-up)))

(defun ar/evil-window-move-down ()
  "Move to window below, wrapping if needed."
  (interactive)
  (if (and ar/evil-want-move-window-to-wrap-around
           (not (ignore-errors (windmove-down))))
      (evil-window-top-left)
    (windmove-down)))

(defcustom ar/evil-preprocessor-regexp "^\\s-*#[a-zA-Z0-9_]"
  "Regexp for preprocessor directives (C/C++)."
  :type 'regexp
  :group 'evil)

(defun ar/evil-next-preproc-directive ()
  "Jump to next preprocessor directive."
  (interactive)
  (re-search-forward ar/evil-preprocessor-regexp nil t))

(defun ar/evil-previous-preproc-directive ()
  "Jump to previous preprocessor directive."
  (interactive)
  (re-search-backward ar/evil-preprocessor-regexp nil t))

(define-key evil-normal-state-map "]#" #'ar/evil-next-preproc-directive)
(define-key evil-normal-state-map "[#" #'ar/evil-previous-preproc-directive)

(with-eval-after-load 'evil
  (define-key evil-ex-completion-map (kbd "C-a") #'evil-beginning-of-line)
  (define-key evil-ex-completion-map (kbd "C-b") #'evil-backward-char)
  (define-key evil-ex-completion-map (kbd "C-f") #'evil-forward-char)

  (define-key evil-ex-search-keymap (kbd "C-a") #'evil-beginning-of-line)
  (define-key evil-ex-search-keymap (kbd "C-b") #'evil-backward-char)
  (define-key evil-ex-search-keymap (kbd "C-f") #'evil-forward-char))

(with-eval-after-load 'smartparens
  (add-hook 'evil-replace-state-entry-hook #'turn-off-smartparens-mode)
  (add-hook 'evil-replace-state-exit-hook #'turn-on-smartparens-mode))

(with-eval-after-load 'evil-maps
  (evil-define-key '(normal visual) 'global "gc" 'evilnc-comment-or-uncomment-lines))

;; ==========================
;; UNIFIED ESCAPE KEY SYSTEM
;; ==========================
;; DO NOT DUPLICATE ESCAPE BINDINGS ELSEWHERE

;; === CORE CONFIGURATION ===
(defvar doom-escape-hook nil
  "Hook run when `doom/escape' is called.
Each function should return non-nil to prevent further processing.")

;; === HELPER: Minibuffer Quit ===
(defun ar/minibuffer-keyboard-quit ()
  "Abort recursive edit properly in minibuffer."
  (interactive)
  (cond
   ((and delete-selection-mode transient-mark-mode mark-active)
    (setq deactivate-mark t))
   (t
    (when (get-buffer "*Completions*")
      (delete-windows-on "*Completions*"))
    (abort-recursive-edit))))

;; === MAIN ESCAPE FUNCTION ===
(defun doom/escape (&optional interactive)
  "Unified escape sequence with proper priority handling."
  (interactive "p")
  (cond
   ;; Don't interfere with keyboard macros
   ((or defining-kbd-macro executing-kbd-macro) nil)

   ;; Run custom hooks
   ((run-hook-with-args-until-success 'doom-escape-hook))

   ;; Active minibuffer
   ((minibuffer-window-active-p (minibuffer-window))
    (if (minibufferp)
        (ar/minibuffer-keyboard-quit)
      (abort-recursive-edit)))

   ;; Evil visual state
   ((and (bound-and-true-p evil-mode)
         (fboundp 'evil-visual-state-p)
         (evil-visual-state-p))
    (evil-exit-visual-state))

   ;; Evil operator state
   ((and (bound-and-true-p evil-mode)
         (fboundp 'evil-operator-state-p)
         (evil-operator-state-p))
    (evil-force-normal-state))

   ;; Other non-normal Evil states
   ((and (bound-and-true-p evil-mode)
         (fboundp 'evil-normal-state-p)
         (not (evil-normal-state-p))
         (not (evil-emacs-state-p)))
    (evil-force-normal-state))

   ;; Fallback
   (t (keyboard-quit))))

;; === EVIL INTEGRATION ===
(with-eval-after-load 'evil
  ;; CRITICAL: Enable for terminal support
  (when (fboundp 'evil-esc-mode)
    (evil-esc-mode 1))

  (setq evil-esc-delay 0.01)

  ;; Global remap
  (global-set-key [remap keyboard-quit] #'doom/escape)

  ;; Evil state bindings
  (define-key evil-insert-state-map [escape] #'evil-normal-state)
  (define-key evil-visual-state-map [escape] #'evil-exit-visual-state)
  (define-key evil-replace-state-map [escape] #'evil-normal-state)
  (define-key evil-operator-state-map [escape] #'keyboard-quit)
  (define-key evil-motion-state-map [escape] #'doom/escape)
  (define-key evil-normal-state-map [escape] #'doom/escape)
  (define-key evil-emacs-state-map [escape] #'doom/escape)

  ;; Ex maps
  (define-key evil-ex-completion-map [escape] #'abort-recursive-edit)
  (define-key evil-ex-search-keymap [escape] #'abort-recursive-edit))

;; === MINIBUFFER ===
(define-key minibuffer-local-map [escape] #'ar/minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] #'ar/minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] #'ar/minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] #'ar/minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] #'ar/minibuffer-keyboard-quit)

(with-eval-after-load 'replace
  (define-key query-replace-map [escape] #'exit))

(with-eval-after-load 'vertico
  (define-key vertico-map [escape] #'ar/minibuffer-keyboard-quit))

;; ==============================
;; HOOK FUNCTIONS
;; ==============================

;; === COMPLETION ===
(with-eval-after-load 'corfu
  (defun ar/corfu-quit-on-escape ()
    (when (and (bound-and-true-p corfu-mode)
               (frame-live-p corfu--frame))
      (corfu-quit)
      t))
  (add-hook 'doom-escape-hook #'ar/corfu-quit-on-escape)
  (define-key corfu-map [escape] #'corfu-quit))

(with-eval-after-load 'company
  (defun ar/company-abort-on-escape ()
    (when company-candidates
      (company-abort)
      t))
  (add-hook 'doom-escape-hook #'ar/company-abort-on-escape))

;; === WHICH-KEY ===
(with-eval-after-load 'which-key
  (defun ar/which-key-abort-on-escape ()
    (when (bound-and-true-p which-key--popup-showing-p)
      (which-key--hide-popup)
      t))
  (add-hook 'doom-escape-hook #'ar/which-key-abort-on-escape))

;; === POPUPS ===
(defun ar/popup-close-on-escape ()
  (cond
   ((and (bound-and-true-p popper-mode)
         (popper-popup-p (current-buffer)))
    (popper-close-latest)
    t)
   ((and (bound-and-true-p popper-mode)
         popper-open-popup-alist)
    (popper-close-latest)
    t)
   ((seq-find (lambda (win) (window-parameter win 'popup))
              (window-list))
    (delete-window (seq-find (lambda (win) (window-parameter win 'popup))
                             (window-list)))
    t)
   (t nil)))

(add-hook 'doom-escape-hook #'ar/popup-close-on-escape)

;; === SPECIAL MODES ===
(defun ar/quit-special-buffer-on-escape ()
  (when (or (derived-mode-p 'special-mode)
            (derived-mode-p 'messages-buffer-mode)
            (derived-mode-p 'compilation-mode)
            (derived-mode-p 'help-mode)
            (derived-mode-p 'helpful-mode))
    (quit-window)
    t))

(add-hook 'doom-escape-hook #'ar/quit-special-buffer-on-escape)

(with-eval-after-load 'simple
  (define-key special-mode-map [escape] #'quit-window)
  (when (boundp 'messages-buffer-mode-map)
    (define-key messages-buffer-mode-map [escape] #'quit-window)
    (define-key messages-buffer-mode-map (kbd "q") #'quit-window)))

(with-eval-after-load 'help-mode
  (define-key help-mode-map [escape] #'quit-window)
  (define-key help-mode-map (kbd "q") #'quit-window))

(with-eval-after-load 'helpful
  (define-key helpful-mode-map [escape] #'quit-window)
  (define-key helpful-mode-map (kbd "q") #'quit-window))

(with-eval-after-load 'compile
  (define-key compilation-mode-map [escape] #'quit-window))

;; === ORG MODE ===
(with-eval-after-load 'org-agenda
  (defun ar/org-agenda-quit-on-escape ()
    (when (derived-mode-p 'org-agenda-mode)
      (quit-window)
      t))
  (add-hook 'doom-escape-hook #'ar/org-agenda-quit-on-escape)
  (define-key org-agenda-mode-map [escape] #'quit-window))

;; === MAGIT ===
(with-eval-after-load 'magit
  (defun ar/magit-quit-on-escape ()
    (when (derived-mode-p 'magit-mode)
      (cond
       ((and (boundp 'magit-popup-mode) magit-popup-mode)
        (magit-popup-quit)
        t)
       (t
        (magit-mode-bury-buffer)
        t))))
  (add-hook 'doom-escape-hook #'ar/magit-quit-on-escape))

;; === OTHER MODES ===
(with-eval-after-load 'dired
  (define-key dired-mode-map [escape] #'quit-window))

(with-eval-after-load 'deft
  (define-key deft-mode-map [escape] #'quit-window)
  (define-key deft-mode-map (kbd "q") #'quit-window))

(with-eval-after-load 'proced
  (define-key proced-mode-map [escape] #'quit-window))

(with-eval-after-load 'deadgrep
  (define-key deadgrep-mode-map [escape] #'quit-window))

(with-eval-after-load 'ibuffer
  (define-key ibuffer-mode-map [escape] #'quit-window))

(with-eval-after-load 'pdf-tools
  (define-key pdf-view-mode-map [escape] #'quit-window))

(with-eval-after-load 'info
  (define-key Info-mode-map [escape] #'quit-window))

(with-eval-after-load 'man
  (define-key Man-mode-map [escape] #'quit-window))

(with-eval-after-load 'woman
  (define-key woman-mode-map [escape] #'quit-window))

(with-eval-after-load 'package
  (define-key package-menu-mode-map [escape] #'quit-window))

(with-eval-after-load 'embark
  (defun ar/embark-quit-on-escape ()
    (when (and (boundp 'embark--target)
               embark--target)
      (keyboard-quit)
      t))
  (add-hook 'doom-escape-hook #'ar/embark-quit-on-escape))

(with-eval-after-load 'treemacs
  (define-key treemacs-mode-map [escape]
    (lambda ()
      (interactive)
      (if (one-window-p)
          (doom/escape)
        (delete-window)))))

;; === EIN & ESHELL (don't quit buffer) ===
(with-eval-after-load 'ein
  (defun ar/ein-quit-on-escape ()
    (when (derived-mode-p 'ein:notebook-mode)
      (when (and (bound-and-true-p evil-mode)
                 (not (evil-normal-state-p)))
        (evil-normal-state)
        t)))
  (add-hook 'doom-escape-hook #'ar/ein-quit-on-escape))

(with-eval-after-load 'eshell
  (defun ar/eshell-quit-on-escape ()
    (when (derived-mode-p 'eshell-mode)
      (when (and (bound-and-true-p evil-mode)
                 (not (evil-normal-state-p)))
        (evil-normal-state)
        t)))
  (add-hook 'doom-escape-hook #'ar/eshell-quit-on-escape))

;; ===============================
;; TESTING
;; ===============================

(defun ar/test-escape-key ()
  "Test escape key behavior in current context."
  (interactive)
  (let ((mode-info (format "Major: %s, Evil: %s"
                          major-mode
                          (if (bound-and-true-p evil-state)
                              evil-state
                            "N/A")))
        (hooks-info (format "Hooks: %d" (length doom-escape-hook)))
        (popup-info (cond
                     ((and (bound-and-true-p popper-mode)
                           (popper-popup-p (current-buffer)))
                      "Popper popup")
                     ((seq-find (lambda (w) (window-parameter w 'popup))
                               (window-list))
                      "Shackle popup")
                     (t "No popup"))))
    (message "Escape Test: %s | %s | %s" mode-info hooks-info popup-info)))

(global-set-key (kbd "C-c t e") #'ar/test-escape-key)

(defun ar/list-escape-hooks ()
  "List all functions in doom-escape-hook."
  (interactive)
  (if doom-escape-hook
      (message "Escape hooks (%d): %s"
               (length doom-escape-hook)
               (mapconcat #'symbol-name doom-escape-hook ", "))
    (message "No escape hooks registered")))

;; =================================
;; END OF UNIFIED ESCAPE KEY SYSTEM
;; =================================

(use-package avy
  :custom
  (avy-case-fold-search nil)       ; Case-sensitive for precision
  (avy-style 'pre)                 ; A more visible overlay style
  (avy-menu t)                     ; Transient menu for ambiguous jumps
  (avy-timeout-seconds 0.5)        ; Timeout for avy-goto-char-timer

  ;; --- Quality of Life ---
  ;; Prevent Avy from activating in modes where it's not useful.
  (avy-ignored-modes
   '(image-mode doc-view-mode pdf-view-mode exwm-mode))

  :config
  ;; --- Embark Integration ---
  (defun avy-action-embark (pt)
    "Go to point PT and trigger Embark."
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)

  ;; Add the Embark action to Avy's dispatch list. Now, when Avy is
  ;; active, pressing '.' will call our function.
  (setf (alist-get ?. avy-dispatch-alist) 'avy-action-embark)

  ;; --- Keybindings ---
  ;; A direct, non-leader keybinding for quick access, common in many configs.
  (general-define-key
   :states '(normal motion)
   "-" #'avy-goto-char-timer)

  ;; Your custom function from the previous configuration.
  (defun ar/avy-copy-region (beg end)
    "Copy region from BEG to END to an Avy point."
    (interactive "r")
    (avy-copy (buffer-substring-no-properties beg end)))

  (ar/global-leader
    ;; Avy jump commands
    "j" '(:ignore t :wk "jump (avy)")
    "j j" '(avy-goto-char-timer :wk "Jump to Char")
    "j l" '(avy-goto-line :wk "Jump to Line")
    "j w" '(avy-goto-word-1 :wk "Jump to Word")
    "j a" '(consult-avy-line :wk "Jump to Line (Consult)")

    ;; Avy actions
    "j c" '(avy-copy-line :wk "Copy Line")
    "j m" '(avy-move-line :wk "Move Line")
    "j C" '(ar/avy-copy-region :wk "Copy Region to Point")

    ;; Ace-Link (hyperlink jumping)
    "j h" '(ace-link-help :wk "Jump to Link (Help)")
    "j i" '(ace-link-info :wk "Jump to Link (Info)")
    "j e" '(ace-link-eww :wk "Jump to Link (EWW)")))

(use-package ace-link :defer t)

(use-package rainbow-delimiters
  :defer t
  :hook ((text-mode . rainbow-delimiters-mode)
         (LaTeX-mode . rainbow-delimiters-mode)
         (org-src-mode . rainbow-delimiters-mode)
         (prog-mode . rainbow-delimiters-mode))
  :config
  (custom-set-faces
   '(rainbow-delimiters-depth-1-face ((t (:foreground "#89b4fa"))))  ; Blue
   '(rainbow-delimiters-depth-2-face ((t (:foreground "#cba6f7"))))  ; Mauve
   '(rainbow-delimiters-depth-3-face ((t (:foreground "#f9e2af"))))  ; Yellow
   '(rainbow-delimiters-depth-4-face ((t (:foreground "#89dceb"))))  ; Sky
   '(rainbow-delimiters-depth-5-face ((t (:foreground "#f38ba8"))))  ; Red
   '(rainbow-delimiters-depth-6-face ((t (:foreground "#a6e3a1"))))  ; Green
   '(rainbow-delimiters-depth-7-face ((t (:foreground "#fab387"))))  ; Peach
   '(rainbow-delimiters-depth-8-face ((t (:foreground "#cdd6f4"))))  ; Text
   '(rainbow-delimiters-depth-9-face ((t (:foreground "#bac2de")))))) ; Subtext1

(use-package rainbow-mode
  :defer t
  :hook ((prog-mode . rainbow-mode)
         (org-mode . rainbow-mode)))

(use-package buffer-terminator
  :defer t
  :custom
  (buffer-terminator-verbose nil)
  (buffer-terminator-inactivity-timeout (* 30 60)) ; 30 minutes

  (buffer-terminator-interval (* 10 60)) ; 10 minutes

  :config
  (buffer-terminator-mode 1))

(use-package helpful
  :defer t
  :commands (helpful-callable
             helpful-variable
             helpful-key
             helpful-command
             helpful-at-point
             helpful-function)
  :bind
  ([remap describe-command] . helpful-command)
  ([remap describe-function] . helpful-callable)
  ([remap describe-key] . helpful-key)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  :custom
  (helpful-max-buffers 7))

(use-package wgrep
  :defer t
  :commands (wgrep-change-to-wgrep-mode)
  :config
  ;; evil-collection provides bindings like :wq to save and :q! to abort.
  (setq wgrep-auto-save-buffer t))

(use-package indent-bars
  :custom
  (indent-bars-treesit-support t)
  (indent-bars-no-descend-string t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  (indent-bars-treesit-wrap '((python argument_list parameters
                      list list_comprehension
                      dictionary dictionary_comprehension
                      parenthesized_expression subscript)))
  (indent-bars-pattern "....")
  (indent-bars-width-frac 0.25)
  (indent-bars-pad-frac 0.2)
  :hook ((prog-mode yaml-mode) . indent-bars-mode))

(use-package jinx
  :defer t
  :custom
  (jinx-disabled-modes
   '(prog-mode           ; All programming modes
     conf-mode           ; All configuration file modes
     emacs-lisp-mode     ; Specifically for elisp
     dired-mode          ; File manager
     ibuffer-mode        ; Buffer list
     neotree-mode        ; File tree
     magit-status-mode   ; Magit UI
     magit-log-mode
     magit-diff-mode
     magit-branch-mode
     org-agenda-mode     ; Agenda view is not for writing
     org-src-mode        ; Don't check inside code blocks
     dashboard-mode      ; Startup dashboard
     which-key-mode      ; Keybinding helper
     help-mode           ; Help buffers
     Info-mode           ; Info documentation
     embark-collect-mode ; Embark's special buffer
     vterm-mode          ; Terminal emulator
     pdf-view-mode))     ; PDF viewer

    ;; Ensure the personal dictionary file exists, creating it if necessary.
  (let ((dict-file (expand-file-name "dict.txt" minimal-emacs-user-directory)))
    (unless (file-exists-p dict-file)
      (write-region "" nil dict-file)))

  (ar/global-leader
    "j" '(:ignore t :wk "jinx (spellcheck)")
    "j c" '(jinx-correct :wk "Correct word at point")
    "j n" '(jinx-next-error :wk "Go to next error")
    "j p" '(jinx-previous-error :wk "Go to previous error")
    "j s" '(jinx-suggest :wk "Show suggestions")
    "j a" '(jinx-add-word-to-personal-dictionary :wk "Add to dictionary")
    "j l" '(jinx-languages :wk "Select language")
    "j t" '(jinx-toggle-checking :wk "Toggle checking in buffer")))

(defun ar/deadgrep-fix-buffer-advice (fun &rest args)
  (let ((buf (apply fun args)))
    (with-current-buffer buf
      (toggle-truncate-lines 1))
    buf))

(use-package deadgrep
  :defer t
  :config
  (advice-add #'deadgrep--buffer :around #'ar/deadgrep-fix-buffer-advice))

;; Doom's comment continuation advice
(defun ar/newline-indent-and-continue-comments-a (orig-fn &rest args)
  "Continue comments when pressing RET with smartparens."
  (interactive "P")
  (if (and (sp-point-in-comment)
           comment-line-break-function)
      (funcall comment-line-break-function (car args))
    (apply orig-fn args)))

(use-package smartparens
  :defer t
  :hook ((prog-mode . smartparens-mode)
         (text-mode . smartparens-mode)
         (org-mode . smartparens-mode)
         (LaTeX-mode . smartparens-mode)
         (markdown-mode . smartparens-mode))
  :config
  (require 'smartparens-config)
  (show-paren-mode -1)
  (show-smartparens-global-mode +1)
  (sp-pair "<" ">" :actions '(:rem))

  ;; Re-enable only for C++ templates if needed
  (sp-local-pair 'c++-mode "<" ">"
                 :when '(sp-point-after-word-p)
                 :unless '(sp-point-before-same-p))

  ;; Doom's smartparens settings
  (setq sp-show-pair-delay 0.1
        sp-show-pair-from-inside t
        sp-cancel-autoskip-on-backward-movement nil
        sp-navigate-close-if-unbalanced t
        sp-message-width nil)

  ;; Smartparens interferes with evil replace mode
  (add-hook 'evil-replace-state-entry-hook #'turn-off-smartparens-mode)
  (add-hook 'evil-replace-state-exit-hook
            (lambda ()
              (when (and (bound-and-true-p smartparens-mode)
                         (not smartparens-mode))
                (turn-on-smartparens-mode))))

  ;; Doom's pair behaviors
  (sp-local-pair '(minibuffer-mode minibuffer-inactive-mode) "'" nil :actions nil)
  (sp-local-pair '(minibuffer-mode minibuffer-inactive-mode) "`" nil :actions nil)

  ;; Expand braces intelligently - CORRECTED SYNTAX
  (sp-pair "{" nil :post-handlers '(:add ("||\n[i]" "RET")))
  (sp-pair "(" nil :post-handlers '(:add ("||\n[i]" "RET")))
  (sp-pair "[" nil :post-handlers '(:add ("||\n[i]" "RET")))

  ;; Don't do square-bracket space-expansion for lisp modes
  (sp-local-pair '(emacs-lisp-mode lisp-mode clojure-mode racket-mode hy-mode)
                 "(" nil :post-handlers '(:add ("||\n[i]" "RET")))
  (sp-local-pair '(emacs-lisp-mode lisp-mode clojure-mode racket-mode hy-mode)
                 "[" nil :post-handlers nil)

  ;; Reasonable default pairs for org-mode
  (sp-local-pair 'org-mode "~" "~" :unless '(sp-point-before-word-p))
  (sp-local-pair 'org-mode "=" "=" :unless '(sp-point-before-word-p))
  (sp-local-pair 'org-mode "*" "*" :unless '(sp-point-before-word-p))

  (advice-add 'newline-and-indent :around #'ar/newline-indent-and-continue-comments-a)

  ;; Disable smartparens in some modes
  (dolist (mode '(erc-mode
                  gud-mode
                  inferior-emacs-lisp-mode
                  minibuffer-inactive-mode
                  debugger-mode))
    (add-to-list 'sp-ignore-modes-list mode))
  )

(use-package exec-path-from-shell
  :defer t
  :custom
  (exec-path-from-shell-variables
   '("PATH" "MANPATH"
     "OPENAI_API_KEY" "ANTHROPIC_API_KEY"
     "XAI_API_KEY" "DEEPSEEK_API_KEY"
     "OPENROUTER_API_KEY" "GEMINI_API_KEY"))
  :config
  (when (daemonp)
    (exec-path-from-shell-initialize)))

(use-package sudo-edit
  :defer t
  :commands (sudo-edit))

(unless *sys/win32*
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (set-language-environment "UTF-8")
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8))
;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

;; Smart trailing whitespace removal that preserves current line
(defun delete-trailing-whitespace-except-current-line ()
  "Delete trailing whitespace except on current line."
  (interactive)
  (let ((begin (line-beginning-position))
        (end (line-end-position)))
    (save-excursion
      (when (< (point-min) (1- begin))
        (save-restriction
          (narrow-to-region (point-min) (1- begin))
          (delete-trailing-whitespace)
          (widen)))
      (when (> (point-max) (+ end 2))
        (save-restriction
          (narrow-to-region (+ end 2) (point-max))
          (delete-trailing-whitespace)
          (widen))))))

(defun smart-delete-trailing-whitespace ()
  "Invoke trailing whitespace deletion on selected major modes only."
  (unless (member major-mode '(diff-mode))
    (delete-trailing-whitespace-except-current-line)))

;; Add to save hook
(add-hook 'before-save-hook #'smart-delete-trailing-whitespace)

(use-package symbol-overlay
  :defer t
  :hook (prog-mode . symbol-overlay-mode)
  :bind (("M-i" . symbol-overlay-put)
         ("M-n" . symbol-overlay-jump-next)
         ("M-p" . symbol-overlay-jump-prev)))

(use-package emacs
  :straight (:type built-in)
  :custom
  (completion-cycle-threshold 3)
  (text-mode-ispell-word-completion nil)
  (read-extended-command-predicate #'command-completion-default-include-p))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-pcm-leading-wildcard t)) ;; Emacs 31: partial-completion behaves like substring

(use-package vertico
  :demand t
  :hook (after-init . vertico-mode)
  :custom
  (vertico-resize nil)
  (vertico-cycle t)
  (vertico-count 10))

(use-package vertico-directory
  :straight (:type built-in)
  :after vertico
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package marginalia
  :defer 0.1
  :config
  (marginalia-mode)
  (setq marginalia-max-relative-age 0  ;; Always show absolute time
        marginalia-align 'right))
;; (use-package marginalia
;;   :hook (after-init . marginalia-mode))

(use-package nerd-icons-completion
  :demand t
  :after (nerd-icons marginalia)
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))
;; (use-package nerd-icons-completion
;;   :config
;;   (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup)
;;   (nerd-icons-completion-mode))

(use-package consult
  :defer 0.1
  :bind
  ([remap switch-to-buffer] . consult-buffer)
  ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
  ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
  ([remap bookmark-jump] . consult-bookmark)
  ([remap evil-show-marks] . consult-mark)
  ([remap evil-show-jumps] . consult-jump-list)
  ([remap goto-line] . consult-goto-line)
  ([remap imenu] . consult-imenu)
  ([remap load-theme] . consult-theme)
  ([remap recentf-open-files] . consult-recent-file)
  ([remap yank-pop] . consult-yank-pop)

  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  (setq consult-prompt-margin 0)
  (setq consult-preview-key 'any)

  :config
  ;; Define a custom buffer source that only shows file-visiting buffers
  ;; This mimics Doom Emacs behavior where only "real" file buffers are shown
  (defvar consult--source-file-visiting-buffer
    `(:name "Main"
      :narrow ?f
      :category buffer
      :face consult-buffer
      :history buffer-name-history
      :state ,#'consult--buffer-state
      :default t
      :items ,(lambda ()
                (consult--buffer-query
                 :sort 'visibility
                 :predicate (lambda (buf)
                              ;; Only show buffers that are visiting files
                              (buffer-file-name buf))
                 :as #'buffer-name)))
    "Buffer source for file-visiting buffers only.")

  (setq consult-buffer-sources
        '(consult--source-file-visiting-buffer))

  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file
   consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
   :preview-key "C-SPC"))

(use-package consult-dir
  :defer t
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

(use-package embark
  :defer 0.5
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))

  :init
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  (setq embark-indicators
        '(embark-minimal-indicator
          embark-highlight-indicator
          embark-isearch-highlight-indicator))

  (define-key embark-collect-mode-map (kbd "e") #'embark-export))
;; (use-package embark
;;   :bind
;;   (("C-." . embark-act)         ;; pick some comfortable binding
;;    ("C-;" . embark-dwim)        ;; good alternative: M-.
;;    ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

;;   :init
;;   (setq prefix-help-command #'embark-prefix-help-command)
;;   :config
;;   (define-key embark-collect-mode-map (kbd "e") #'embark-export)
;;   ;; Embark collect buffers display is now handled by shackle rules
;;   ;; See the shackle configuration in "Popup Management System" section
;;   )

(use-package embark-consult
  :defer t
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; (use-package corfu
;;   :hook (elpaca-after-init . global-corfu-mode)
;;   :config
;;   (corfu-history-mode)
;;   (corfu-popupinfo-mode) ; don't set delay or
;;   :bind
;;   (:map corfu-map
;;         ("TAB" . corfu-next)
;;         ([tab] . corfu-next)
;;         ("S-TAB" . corfu-previous)
;;         ([backtab] . corfu-previous)
;;         ("C-c h" . corfu-info-documentation))
;;   :custom
;;   (corfu-cycle t)
;;   (corfu-auto t)
;;   (corfu-auto-resize nil)
;;   (corfu-auto-delay 0.13)
;;   (corfu-preselect 'prompt)
;;   (corfu-quit-at-boundary 'separator) ; hecks if the current completion boundary has been left
;;   (corfu-quit-no-match 'separator) ; corfu completion will quit eagerly
;;   (corfu-on-exact-match nil))

;; (orderless-define-completion-style orderless-literal-only
;;   (orderless-style-dispatchers nil)
;;   (orderless-matching-styles '(orderless-literal)))

(use-package corfu
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous)
        ("M-h" . corfu-info-documentation))
  :config
  (corfu-history-mode)
  (corfu-popupinfo-mode)
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-preselect 'prompt)      ;; Preselect the prompt
  (corfu-on-exact-match 'insert) ;; Configure handling of exact matches

  :init
  (global-corfu-mode))

(use-package nerd-icons-corfu
  :after (corfu nerd-icons)
  :config (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-keyword)

  ;; Make these capfs composable.
  (advice-add 'lsp-completion-at-point :around #'cape-wrap-noninterruptible)
  (advice-add 'lsp-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add 'comint-completion-at-point :around #'cape-wrap-nonexclusive)
  ;; (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  ;; (advice-add 'eglot-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-nonexclusive))

(use-package dabbrev
  :straight (:type built-in)
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))

(use-package wgrep
  :defer t
  :config
  (setq wgrep-auto-save-buffer t))

(defvar ar/org-tangle-on-save-modes '(org-mode)
  "Major modes in which to enable auto-tangling on save.")

(defvar ar/org-tangle-silent t
  "If non-nil, suppress tangle messages.")

(defun ar/org-babel-tangle-on-save ()
  "Tangle current org file if it has #+auto_tangle: t property.
Only tangles if the file has been modified and saved."
  (when (and (member major-mode ar/org-tangle-on-save-modes)
             (buffer-file-name))
    ;; Check for #+auto_tangle: t in buffer
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^#\\+auto_tangle: *t" nil t)
        ;; Tangle with error handling
        (let ((inhibit-message ar/org-tangle-silent)
              (message-log-max (unless ar/org-tangle-silent message-log-max)))
          (condition-case err
              (progn
                (org-babel-tangle)
                (unless ar/org-tangle-silent
                  (message "Tangled %s" (buffer-file-name))))
            (error
             (message "Tangle error in %s: %s"
                      (buffer-file-name)
                      (error-message-string err)))))))))

;; Add to after-save-hook
(add-hook 'after-save-hook #'ar/org-babel-tangle-on-save)

;; Optional: Tangle before exiting Emacs
(defun ar/org-babel-tangle-all-on-exit ()
  "Tangle all org files with #+auto_tangle: t before exiting."
  (when (y-or-n-p "Tangle all modified org files before exit? ")
    (save-some-buffers t)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (eq major-mode 'org-mode)
                   (buffer-file-name)
                   (buffer-modified-p))
          (ar/org-babel-tangle-on-save))))))

;; Uncomment to enable exit tangling
(add-hook 'kill-emacs-query-functions #'ar/org-babel-tangle-all-on-exit)

;; Define base directory
(defvar my/org-directory (expand-file-name "~/org/")
  "Base directory for all org files.")

;; Define subdirectories relative to base
(defvar my/org-subdirs
  '("roam" "downloads" "noter" "archive"
    "roam/projects" "roam/literature" "roam/ideas" "roam/zettel"
    "attachments" "reviews" "backups")
  "List of subdirectories to create under `my/org-directory'.")

;; Helper function to get org subdirectory paths
(defun my/org-subdir (subdir)
  "Return full path for SUBDIR under `my/org-directory'."
  (expand-file-name subdir my/org-directory))

;; Lazy directory creation - only when needed
(defun my/ensure-org-dir (subdir)
  "Ensure SUBDIR exists under `my/org-directory'."
  (let ((dir (my/org-subdir subdir)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    dir))

;; Create all directories at startup
(mapc #'my/ensure-org-dir my/org-subdirs)

;; Define convenience variables
(defvar my/org-roam-directory (my/org-subdir "roam/"))
(defvar my/org-downloads-directory (my/org-subdir "downloads/"))
(defvar my/org-noter-directory (my/org-subdir "noter/"))
(defvar my/org-archive-directory (my/org-subdir "archive/"))

;; Improved project finder using projectile (with guard)
(defun my/find-org-projects ()
  "Return list of org files tagged as projects."
  (let* ((files (if (and (fboundp 'projectile-project-p)
                         (projectile-project-p))
                    (mapcar (lambda (f) (expand-file-name f (projectile-project-root)))
                            (projectile-current-project-files))
                  (directory-files-recursively
                   my/org-directory
                   "\\.org$"
                   nil
                   (lambda (dir)
                     (not (string-match-p "/\\(\\.git\\|archive\\|backups\\)/" dir)))))))
    (seq-filter
     (lambda (file)
       (with-temp-buffer
         (insert-file-contents file nil 0 2000)
         (goto-char (point-min))
         (re-search-forward "^#\\+filetags:.*:project:" nil t)))
     files)))

(defun ar/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.13)
                  (org-level-3 . 1.10)
                  (org-level-4 . 1.07)
                  (org-level-5 . 1.05)
                  (org-level-6 . 1.03)
                  (org-level-7 . 1.02)
                  (org-level-8 . 1)))
    (set-face-attribute (car face) nil :font "JetBrainsMono Nerd Font" :weight 'bold :height (cdr face))))

(use-package org
  :straight (:type built-in)
  :mode ("\\.org\\'" . org-mode)
  :hook
  ((org-mode . visual-line-mode)
   (org-mode . ar/org-font-setup)
   (org-mode . auto-fill-mode)
   (org-mode . (lambda () (setq-local yas-parents '(latex-mode))))
   (org-mode . (lambda ()
                 (setq-local electric-indent-local-mode -1) ; Prevent aggressive auto-indent; setting to true for now
                 (evil-define-key 'normal org-mode-map (kbd "TAB") 'org-cycle)))
   (org-agenda-mode . (lambda ()
                        (visual-line-mode -1)
                        (toggle-truncate-lines 1)
                        (display-line-numbers-mode -1)
                        (setq mode-line-format nil)
                        (setq header-line-format nil)))
   (org-capture-mode . (lambda ()
                         (setq mode-line-format nil)
                         (setq header-line-format nil))))

  :custom
  (org-directory my/org-directory)
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-return-follows-link t)
  (org-src-fontify-natively t)
  ;; (org-agenda-window-setup 'other-window)
  ;; (org-src-window-setup 'other-window)

  ;; Element cache - ESSENTIAL for large files
  (org-element-use-cache t)
  (org-element-cache-persistent t)

  ;; Disable expensive fontification
  (org-fontify-quote-and-verse-blocks nil)
  (org-fontify-whole-heading-line nil)
  (org-fontify-done-headline nil)
  (org-highlight-latex-and-related nil)  ;; MAJOR performance killer

  ;; Startup optimization
  (org-startup-folded 'showeverything)  ;; CHANGED: Fastest startup
  ;;(org-startup-indented nil)  ;; CHANGED: org-indent-mode is slow
  (org-startup-with-inline-images nil)  ;; CHANGED: Load images on demand
  (org-startup-with-latex-preview nil)  ;; CHANGED: Preview on demand
  (org-indent-mode -1)

  ;; Display settings
  (org-hide-leading-stars nil)  ;;  Faster with nil
  (org-hide-emphasis-markers nil)  ;; Faster with nil
  (org-pretty-entities nil)  ;; CHANGED: Disable for speed
  (org-cycle-separator-lines 2)

  ;; Agenda optimization
  (org-agenda-inhibit-startup t)  ;; CRITICAL: Don't run all hooks
  (org-agenda-dim-blocked-tasks nil)
  (org-agenda-use-tag-inheritance nil)
  (org-agenda-ignore-properties '(effort appt category))
  (org-agenda-span 'day)  ;; Show only today by default

  ;; Image settings
  (org-image-actual-width 600)

  ;; Babel settings
  (org-confirm-babel-evaluate nil)
  (org-src-tab-acts-natively t)
  (org-src-preserve-indentation t)

  ;; Tag settings
  (org-tag-alist '(("@work"      . ?w)
                   ("@home"      . ?h)
                   ("@computer"  . ?c)
                   ("@errands"   . ?e)
                   ("read"       . ?r)
                   ("meeting"    . ?m)
                   ("urgent"     . ?u)
                   ("someday"    . ?s)))

  ;; TODO keywords
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "PROG(p)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCEL(c@)")
     (sequence "PLAN(P)" "ACTIVE(A)" "PAUSED(x)" "|" "ACHIEVED(a)" "DROPPED(D)")))

  (org-todo-keyword-faces
   '(("TODO"      . (:foreground "#f38ba8" :weight bold))
     ("NEXT"      . (:foreground "#fab387" :weight bold))
     ("PROG"      . (:foreground "#89b4fa" :weight bold))
     ("WAIT"      . (:foreground "#f9e2af" :weight bold))
     ("DONE"      . (:foreground "#a6e3a1" :weight bold))
     ("CANCEL"    . (:foreground "#6c7086" :weight bold))
     ("PLAN"      . (:foreground "#94e2d5" :weight bold))
     ("ACTIVE"    . (:foreground "#cba6f7" :weight bold))
     ("PAUSED"    . (:foreground "#bac2de" :weight bold))
     ("ACHIEVED"  . (:foreground "#a6e3a1" :weight bold))
     ("DROPPED"   . (:foreground "#6c7086" :weight bold))))

  (org-babel-execution-completed-message nil)

  :config
  ;; Conditional feature loading based on file size
  (defun ar/org-conditional-features ()
    "Enable expensive features only for small files."
    (let ((size (buffer-size)))
      (if (< size 50000)  ;; Less than 50KB
          (progn
            (org-indent-mode 1)
            (setq-local org-hide-leading-stars t
                       org-pretty-entities t
                       org-hide-emphasis-markers t))
        ;; Large file - minimal features
        (progn
          (org-indent-mode -1)
          (setq-local org-hide-leading-stars nil
                     org-pretty-entities nil
                     org-hide-emphasis-markers nil)
          (message "Large org file detected - performance mode enabled")))))

  ;; Set language modes for babel
  (setq org-src-lang-modes
        (append org-src-lang-modes
                '(("python" . python-ts)
                  ("bash" . bash-ts)
                  ("sh" . bash-ts)
                  ("latex" . latex-ts)
                  ("typescript" . typescript-ts)))))

(with-eval-after-load 'org
  ;; Load languages for Org Babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (latex . t)
     (python . t)
     (shell . t)
     (sql . t)
     (gnuplot . t)))

  ;; Auto-display images after execution
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

  ;; Structure templates
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("jpy" . "src jupyter-python"))
  (add-to-list 'org-structure-template-alist '("tex" . "src latex")))

(use-package org-modern
  :hook (org-mode . org-modern-mode)
  :config
  (setq org-modern-hide-stars "· "
        org-modern-star '("◉" "○" "◈" "◇" "◆" "▷")
        org-modern-list '((43 . "➤") (45 . "–") (42 . "•"))
        org-modern-table-vertical 1
        org-modern-table-horizontal 0.1
        org-modern-block-name
        '(("src" "»" "«")
          ("example" "»" "«")
          ("quote" """ """))

        org-modern-todo-faces
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
          ("DROPPED"   . (:strike-through t :foreground "#6c7086")))

        org-modern-tag-faces
        `((:foreground ,(face-attribute 'default :foreground) :weight bold :box (:line-width (1 . -1) :color "#3b4261")))
        org-modern-checkbox '((todo . "☐") (done . "☑") (cancel . "☑") (priority . "⚑") (on . "◉") (off . "◯"))))

;; Enable org-modern only for small files or on demand
(defun ar/org-modern-toggle ()
  "Toggle org-modern-mode intelligently."
  (interactive)
  (if (< (buffer-size) 50000)
      (org-modern-mode 'toggle)
    (if (y-or-n-p "File is large. Enable org-modern anyway? ")
        (org-modern-mode 'toggle)
      (message "Use M-x org-modern-mode to enable manually"))))

;; Add command to check org performance
(defun ar/org-performance-report ()
  "Show performance-related settings for current org buffer."
  (interactive)
  (if (derived-mode-p 'org-mode)
      (message "Org Performance:\nSize: %d bytes\nIndent: %s\nPretty: %s\nFontify: %s\nCache: %s"
               (buffer-size)
               (if org-indent-mode "ON" "OFF")
               (if org-pretty-entities "ON" "OFF")
               (if org-src-fontify-natively "ON" "OFF")
               (if org-element-use-cache "ON" "OFF"))
    (message "Not in org-mode")))

(use-package org-roam
  :defer t
  :after org
  :init
  (setq org-roam-directory my/org-roam-directory)
  :custom
  (org-roam-completion-everywhere t)
  (org-roam-node-display-template
   (concat "${title:*} "
           (propertize "${tags:20}" 'face 'org-tag)))
  :config
  (org-roam-db-autosync-mode)

  ;; Org-roam backlinks display is now handled by shackle rules
  ;; See the shackle configuration in "Popup Management System" section

  ;; Templates for different kinds of notes (Zettelkasten).
  (setq org-roam-capture-templates
      '(("d" "default" plain "* %?"
         :target (file+head "${slug}.org"
                            "#+title: ${title}\n#+filetags: \n\n")
         :unnarrowed t)
        ("p" "project" plain "* Goal\n\n%?\n\n* Tasks\n\n* Notes\n\n* Log\n"
         :target (file+head "projects/${slug}.org"
                            "#+title: Project: ${title}\n#+filetags: project\n")
         :unnarrowed t)
        ("l" "literature note" plain "* Source\n  - Author: \n  - Title: \n  - Year: \n\n* Summary\n\n%?\n\n* Key Takeaways\n\n* Quotes\n"
         :target (file+head "literature/${slug}.org"
                            "#+title: ${title}\n#+filetags: literature\n")
         :unnarrowed t)
        ("i" "idea" plain "* %?"
         :target (file+head "ideas/${slug}.org"
                            "#+title: ${title}\n#+filetags: idea fleeting\n")
         :unnarrowed t)
        ("z" "zettel" plain "* %?\n\n* References\n\n"
         :target (file+head "zettel/${slug}.org"
                            "#+title: ${title}\n#+filetags: zettel permanent\n")
         :unnarrowed t)
        ("j" "journal" plain "* Log\n\n%?"
         :target (file+olp+datetree (expand-file-name "journal.org" my/org-roam-directory))
         :unnarrowed t))))

(use-package org-roam-ui
  :after org-roam
  :commands (org-roam-ui-mode org-roam-ui-open)
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start nil))

(use-package consult-org-roam
  :after (consult org-roam)
  :init (consult-org-roam-mode 1))

(use-package org-capture
  :straight (:type built-in)
  :after org
  :custom
  (org-capture-templates
   `(("t" "Task" entry
      (file+headline ,(expand-file-name "inbox.org" my/org-directory) "Tasks")
      "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n")

     ("n" "Note" entry
      (file+headline ,(expand-file-name "inbox.org" my/org-directory) "Notes")
      "* %? :note:\n:PROPERTIES:\n:CREATED: %U\n:SOURCE:\n:END:\n")

     ("j" "Journal" entry
      (file+olp+datetree ,(expand-file-name "journal.org" my/org-directory))
      "* %U %?\n")

     ("m" "Meeting" entry
      (file+headline ,(expand-file-name "inbox.org" my/org-directory) "Meetings")
      "* Meeting: %? :meeting:\n:PROPERTIES:\n:CREATED: %U\n:ATTENDEES:\n:END:\n** Agenda\n** Notes\n** Action Items\n")

     ("p" "Project" entry
      (file+headline ,(expand-file-name "projects.org" my/org-directory) "Projects")
      "* PLAN %? :project:\n:PROPERTIES:\n:CREATED: %U\n:GOAL:\n:DEADLINE:\n:END:\n** Goals\n** Tasks\n*** TODO Define project scope\n** Resources\n** Notes\n")

     ("P" "Project Task" entry
      (file (lambda ()
              (let* ((files (my/find-org-projects))
                     (file (completing-read "Select Project: "
                                           (mapcar #'file-name-nondirectory files)
                                           nil t)))
                (car (seq-filter
                      (lambda (f) (string= (file-name-nondirectory f) file))
                      files)))))
      "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n"
      :prepend t
      :headline "Tasks")

     ("b" "Book" entry
      (file+headline ,(expand-file-name "reading.org" my/org-directory) "Reading List")
      "* %? :book:read:\n:PROPERTIES:\n:CREATED: %U\n:AUTHOR:\n:GENRE:\n:PAGES:\n:STARTED:\n:FINISHED:\n:RATING:\n:END:\n** Summary\n** Key Takeaways\n** Quotes\n")

     ("h" "Habit" entry
      (file+headline ,(expand-file-name "habits.org" my/org-directory) "Habits")
      "* TODO %? :habit:\nSCHEDULED: %(format-time-string \"<%Y-%m-%d %a .+1d>\")\n:PROPERTIES:\n:CREATED: %U\n:STYLE: habit\n:END:\n")

     ("g" "Goal" entry
      (file+headline ,(expand-file-name "goals.org" my/org-directory) "Goals")
      "* GOAL %? :goal:\nDEADLINE: %(org-read-date nil nil \"+1y\")\n:PROPERTIES:\n:CREATED: %U\n:TYPE:\n:END:\n** Why this goal?\n** Success criteria\n** Action steps\n*** TODO Break down into smaller tasks\n** Resources needed\n** Potential obstacles\n** Progress tracking\n"))))

(use-package org-habit
  :straight (:type built-in)
  :defer t
  :after org
  :custom
  (org-habit-graph-column 60)
  (org-habit-show-habits-only-for-today t)
  (org-habit-preceding-days 21)
  (org-habit-following-days 7)
  (org-habit-completed-glyph
   (string-to-char (nerd-icons-codicon "nf-cod-check")))
  (org-habit-today-glyph
   (string-to-char (nerd-icons-codicon "nf-cod-circle_filled"))))

(use-package org-download
  :defer t
  :after org
  :custom
  (org-download-screenshot-method "grim -g \"$(slurp)\" - | swappy -f - -o -")
  (org-download-image-dir "assets")
  (org-download-method 'attach)
  (org-download-timestamp "%Y-%m-%d-%H%M%S_")
  (org-download-display-inline t)
  (org-image-actual-width 600)
  :config
  (advice-add 'org-download-dnd :after #'org-download-display-inline-images)

  (ar/global-leader
    "i" '(:ignore t :wk "insert")
    "i s" '(org-download-screenshot :wk "Screenshot")
    "i y" '(org-download-yank :wk "Yank Image from Clipboard")))

(use-package deft
  :commands deft
  :after org-roam
  :config
  ;; Point Deft to the org-roam directory and enable recursive search.
  (setq deft-directory org-roam-directory)
  (setq deft-recursive t)

  ;; Use the filter string to search for filenames as well as content.
  (setq deft-use-filter-string-for-filename t)

  ;; Clean up the summary by removing org properties and metadata.
  (setq deft-strip-summary-regexp
        (rx (or
             (: ":PROPERTIES:" (* anything) ":END:")
             (: "#+" (+ alnum) ":" (* nonl))
             (regexp "[\n\t]"))))

  ;; Clean up the UI in the deft buffer by hiding line numbers.
  (add-hook 'deft-mode-hook
            (lambda () (display-line-numbers-mode -1)))

  ;; --- Keybindings ---
  ;; Global leader key to open Deft, under the org-roam prefix.
  (ar/global-leader
    "o r d" '(deft :wk "Search notes (deft)"))

  ;; Local keybindings for navigating the deft buffer.
  (general-define-key
   :keymaps 'deft-mode-map
   :states '(normal motion)
   "q" #'quit-window
   "r" #'deft-refresh
   "s" #'deft-filter
   "d" #'deft-filter-clear
   "y" #'deft-filter-yank
   "t" #'deft-toggle-incremental-search
   "o" #'deft-toggle-sort-method))

;; --- Enhanced Org Mode Parsing for Deft ---
;; Helper function to improve summary display by formatting Org links.
(defun my/deft-parse-summary-around (fun contents title)
  (funcall fun (org-link-display-format contents) title))

;; Helper function to extract the #+title from an Org file.
(defun my/deft-parse-title (file contents)
  (with-temp-buffer
    (insert contents)
    (goto-char (point-min))
    (if (search-forward-regexp (rx (| "#+title:" "#+TITLE:")) nil t)
        (string-trim (buffer-substring-no-properties (point) (line-end-position)))
      file)))

;; Helper function to advise the original title parser.
(defun my/deft-parse-title-around (fun file contents)
  (or (my/deft-parse-title file contents)
      (funcall fun file contents)))

;; Use `with-eval-after-load` to ensure `deft` is loaded before we
;; modify its internal functions.
(with-eval-after-load 'deft
  (advice-add #'deft-parse-summary :around #'my/deft-parse-summary-around)
  (advice-add #'deft-parse-title :around #'my/deft-parse-title-around))

(use-package evil-org
  :hook (org-mode . evil-org-mode)
  :config
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme '(navigation insert textobjects additional calendar todo))))
  (add-to-list 'evil-emacs-state-modes 'org-agenda-mode)
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(ar/global-leader
  ;; Org-mode specific bindings
  "o" '(:ignore t :wk "org")
  "o a" '(org-agenda :wk "agenda")
  "o c" '(org-capture :wk "capture")
  "o s" '(org-schedule :wk "schedule")
  "o d" '(org-deadline :wk "deadline")
  "o t" '(org-set-tags-command :wk "set tags")

  ;; Org-roam specific bindings under "org roam"
  "o r" '(:ignore t :wk "roam")
  "o r f" '(org-roam-node-find :wk "find node")
  "o r i" '(org-roam-node-insert :wk "insert node")
  "o r c" '(org-roam-capture :wk "roam capture")
  "o r g" '(org-roam-graph :wk "show graph")
  "o r t" '(org-roam-tag-add :wk "add tag")

  "o n" '(:ignore t :which-key "org noter")
  "o n n" '(ar/org-noter-find-or-create-notes :wk "Open/Create PDF Notes")
  "o n i" '(org-noter-insert-note :wk "Insert Note"))

(use-package envrc
  :hook (after-init . envrc-global-mode))

;; This is the directory where you will store your personal snippets.
(defvar my/snippets-directory (expand-file-name "snippets" minimal-emacs-user-directory)
  "Directory for personal yasnippet snippets.")

;; Create the custom snippets directory if it doesn't exist.
(unless (file-directory-p my/snippets-directory)
  (make-directory my/snippets-directory t))

(use-package yasnippet-snippets :after yasnippet)

(use-package yasnippet
  :hook (after-init . yas-global-mode)
  :custom
  ;; Use a completing-read prompt for a better UI when multiple snippets match.
  (yas-prompt-functions '(yas-completing-prompt))
  ;; Only show important messages, hiding the "just-in-time loading" confirmation.
  :config

  ;; --- Add Personal Snippets Directory ---
  (add-to-list 'yas-snippet-dirs my/snippets-directory)

  ;; --- Robust, Save-Based Automatic Snippet Reloading ---
  (defun ar/yas-reload-snippets-on-save ()
    "Reload all snippets if a snippet file is being saved."
    (when (string-prefix-p my/snippets-directory (buffer-file-name))
      (yas-reload-all)
      (message "Yasnippet collection reloaded.")))

  (add-hook 'after-save-hook #'ar/yas-reload-snippets-on-save))

(use-package consult-yasnippet
  :after (consult yasnippet)
  :config
  ;; You can customize the preview behavior if desired.
  (consult-customize consult-yasnippet :preview-key 'any))

(ar/global-leader
  "s" '(:ignore t :wk "snippets")
  "s i" '(consult-yasnippet :wk "insert snippet (consult)")
  "s n" '(yas-new-snippet :wk "new snippet")
  "s v" '(yas-visit-snippet-file :wk "visit snippet file"))

(use-package markdown-mode
  :mode "\\.md\\'"
  :config
  (setq markdown-command
        (concat
         "pandoc"
         " --from=markdown --to=html"
         " --standalone --mathjax --highlight-style=pygments"
         " --css=pandoc.css"
         " --quiet"
         ))
  (setq markdown-live-preview-delete-export 'delete-on-export)
  (setq markdown-asymmetric-header t)
  ;;(setq markdown-open-command "/home/pavel/bin/scripts/chromium-sep")
  (add-hook 'markdown-mode-hook #'smartparens-mode)
  (general-define-key
   :keymaps 'markdown-mode-map
   "M-<left>" 'markdown-promote
   "M-<right>" 'markdown-demote))

(use-package lsp-mode
  :defer t
  :hook (prog-mode . lsp-deferred)
  :custom
  (lsp-semantic-tokens-enable t)
  (lsp-headerline-breadcrumb-enable t)
  (lsp-lens-enable t)
  (lsp-modeline-diagnostics-enable t)
  (lsp-diagnostics-provider :flycheck)
  (lsp-completion-provider :none)
  (lsp-eldoc-render-all nil)
  (lsp-idle-delay 0.2)
  (lsp-log-io nil)
  (lsp-signature-render-documentation nil))

(use-package lsp-ui
  :defer t
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-doc-show-with-mouse nil)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-delay 0.2)
  :custom-face
  (lsp-ui-doc-background ((t (:background "#1e2030"))))
  (lsp-ui-doc-header ((t (:foreground "#7aa2f7" :weight bold))))
  (lsp-ui-sideline-background ((t (:background "#1e2030")))))

(with-eval-after-load 'consult
  (setq xref-show-definitions-function #'consult-xref-show-definitions)
  (setq xref-show-references-function #'consult-xref-show-references))

(use-package consult-lsp
  :after (consult lsp-mode))

(use-package consult-flycheck
  :after (consult flycheck))

(use-package dap-mode
  :defer t
  :config
  ;; Set the breakpoint file location to be inside the var directory.
  (setq dap-breakpoints-file (expand-file-name "dap-breakpoints.json" user-emacs-directory))

  ;; --- Built-in UI Integration ---
  ;; Enable the automatic UI layout management.
  (dap-ui-mode 1)
  ;; Enable the floating controls for a richer debugging UI.
  (dap-ui-controls-mode 1)

  ;; Use GUD's tooltip mode for mouse-hover variable inspection in the UI buffers.
  (add-hook 'dap-ui-locals-mode-hook 'gud-tooltip-mode)
  (add-hook 'dap-ui-expressions-mode-hook 'gud-tooltip-mode)

  (ar/global-leader
   ;; Debugging Keybindings (DAP)
   "d" '(:ignore t :wk "debug (dap)")
   "d d" '(dap-debug :wk "Debug new")
   "d r" '(dap-debug-recent :wk "Debug recent")
   "d c" '(dap-continue :wk "Continue")
   "d q" '(dap-disconnect :wk "Quit")
   "d b" '(dap-toggle-breakpoint :wk "Breakpoint")
   "d n" '(dap-next :wk "Next")
   "d i" '(dap-step-in :wk "Step In")
   "d o" '(dap-step-out :wk "Step Out")
   "d u" '(:ignore t :wk "UI")
   "d u o" '(dap-ui-open :wk "Open UI")
   "d u c" '(dap-ui-close :wk "Close UI")
   "d u t" '(dap-ui-toggle :wk "Toggle UI")))

(use-package flycheck
  :defer t
  :hook (prog-mode . flycheck-mode)
  :custom
  (flycheck-check-syntax-automatically '(save mode-enabled))
  (flycheck-idle-change-delay 0.2)
  :custom-face
  (flycheck-error   ((t (:underline (:style wave :color "#f7768e") :inherit nil))))
  (flycheck-warning ((t (:underline (:style wave :color "#e0af68") :inherit nil))))
  (flycheck-info    ((t (:underline (:style wave :color "#73daca") :inherit nil)))))

(use-package sideline-flycheck
  :defer t
  :hook (flycheck-mode . sideline-mode)
  :init
  (setq sideline-flycheck-display-mode 'point)
  (setq sideline-backends-right '(sideline-flycheck)))

(use-package apheleia
  :defer t
  :config
  (apheleia-global-mode +1))

(with-eval-after-load 'tresit
  (setq treesit-font-lock-level 4)
  (setq major-mode-remap-alist
        '((typescript-mode . typescript-ts-mode)
          (js-mode . javascript-ts-mode)
          (latex-mode . latex-ts-mode)
          (elisp-mode . elisp-ts-mode)
          (python-mode . python-ts-mode)
          (json-mode . json-ts-mode))))

(use-package treesit-fold
  :hook (treesit-auto-mode-hook . treesit-fold-mode))

(use-package evil-textobj-tree-sitter
  :after evil
  :config
  ;; Goto start of next function
  (define-key evil-normal-state-map
              (kbd "]f")
              (lambda ()
                (interactive)
                (evil-textobj-tree-sitter-goto-textobj "function.outer")))

  ;; Goto start of previous function
  (define-key evil-normal-state-map
              (kbd "[f")
              (lambda ()
                (interactive)
                (evil-textobj-tree-sitter-goto-textobj "function.outer" t)))

  ;; Goto end of next function
  (define-key evil-normal-state-map
              (kbd "]F")
              (lambda ()
                (interactive)
                (evil-textobj-tree-sitter-goto-textobj "function.outer" nil t)))

  ;; Goto end of previous function
  (define-key evil-normal-state-map
              (kbd "[F")
            (lambda ()
              (interactive)
              (evil-textobj-tree-sitter-goto-textobj "function.outer" t t))))

(defun my/org-src-setup-lsp ()
  "Setup LSP for org source block edit buffers.
Sets buffer-file-name to tangle target and disables problematic LSP features."
  (when (and (boundp 'org-src-mode)
             org-src-mode
             (derived-mode-p 'python-ts-mode))
    (let* ((info (org-babel-get-src-block-info))
           (params (nth 2 info))
           (tangle-file (alist-get :tangle params)))

      ;; Only setup LSP for blocks with tangle files
      (when (and tangle-file
                 (not (string= tangle-file "no"))
                 (not (string= tangle-file "nil")))

        ;; Resolve tangle file path
        (let* ((org-buffer (marker-buffer org-src--beg-marker))
               (org-file (buffer-file-name org-buffer))
               (org-dir (file-name-directory org-file))
               (tangle-path (expand-file-name tangle-file org-dir)))

          ;; Create tangle file if it doesn't exist
          (unless (file-exists-p tangle-path)
            (let ((tangle-dir (file-name-directory tangle-path)))
              (unless (file-directory-p tangle-dir)
                (make-directory tangle-dir t)))
            (with-temp-file tangle-path
              (insert (format "# Tangled from: %s\n" org-file))))

          ;; Set buffer-file-name for LSP (CRITICAL)
          (setq-local buffer-file-name tangle-path)

          ;; Disable problematic LSP UI features in org-src buffers
          (setq-local lsp-headerline-breadcrumb-enable nil
                      lsp-modeline-code-actions-enable nil
                      lsp-modeline-diagnostics-enable nil
                      lsp-lens-enable nil
                      lsp-signature-auto-activate nil)

          ;; Start LSP
          (lsp-deferred))))))

;; Hook into org-src-mode
(add-hook 'org-src-mode-hook #'my/org-src-setup-lsp)

(use-package persp-mode
  :defer t
  :init
  (setq persp-auto-resume-time 0
        persp-auto-save-opt 1
        persp-auto-save-num-of-backups 3
        persp-nil-name "main"
        persp-reset-windows-on-nil-window-conf nil
        persp-set-last-persp-for-new-frames nil
        persp-switch-to-added-buffer nil
        persp-kill-foreign-buffer-behaviour 'kill
        persp-remove-buffers-from-nil-persp-behaviour nil
        persp-add-buffer-on-after-change-major-mode 'free
        persp-add-buffer-on-find-file 'if-not-autopersp
        persp-common-buffer-filter-functions
        (list #'(lambda (b) (or (string-prefix-p " " (buffer-name b))
                                (string-prefix-p "*" (buffer-name b))))))

  :config
  (persp-mode 1)

  ;; FIXED: Safe perspective save with initialization checks
  (defun ar/persp-save-state-quietly ()
    "Save perspective state only if persp-mode is properly initialized."
    (when (and (bound-and-true-p persp-mode)  ; Check if persp-mode is active
               (boundp 'persp-hash)             ; Check if hash table variable exists
               persp-hash                       ; Check if hash table is initialized
               (hash-table-p persp-hash)        ; Verify it's actually a hash table
               (> (hash-table-count persp-hash) 0)) ; Check if there's data to save
      (condition-case err
          (persp-save-state-to-file)
        (error (message "Error saving perspectives: %s" err)))))

  (add-hook 'kill-emacs-hook #'ar/persp-save-state-quietly)

  ;; Create default perspectives
  (with-eval-after-load 'persp-mode
    (persp-def-auto-persp "org"
      :mode 'org-mode
      :dyn-env '(org-directory my/org-directory)
      :hooks '(after-switch-to-buffer-functions)
      :switch 'window)

    (persp-def-auto-persp "code"
      :mode '(prog-mode)
      :hooks '(after-switch-to-buffer-functions)
      :switch 'window)))

(defvar +treemacs-git-mode 'simple
  "Type of git integration for treemacs.
Options: 'simple (default), 'extended, or 'deferred.
Extended and deferred require python3.")

(use-package treemacs
  :defer t
  :init
  (setq treemacs-follow-after-init t
        treemacs-is-never-other-window t
        treemacs-sorting 'alphabetic-case-insensitive-asc
        treemacs-persist-file (expand-file-name "treemacs-persist" user-emacs-directory)
        treemacs-last-error-persist-file (expand-file-name "treemacs-last-error-persist" user-emacs-directory)
        treemacs-width 35
        treemacs-width-is-initially-locked t
        treemacs-display-in-side-window t
        treemacs-position 'left
        treemacs-no-delete-other-windows t
        treemacs-show-hidden-files t
        treemacs-expand-after-init t
        treemacs-space-between-root-nodes t
        treemacs-silent-refresh t
        treemacs-silent-filewatch t
        treemacs-file-event-delay 2000
        treemacs-file-follow-delay 0.2)

  :config
  (treemacs-follow-mode -1)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode 'always)

  ;; Auto-add current project when opening treemacs
  (defun my/treemacs-projectile-setup ()
    "Automatically add current projectile project to treemacs."
    (when (and (projectile-project-p)
               (projectile-project-root))
      (let ((root (projectile-project-root)))
        (unless (treemacs-is-path root :in-workspace)
          (treemacs-do-add-project-to-workspace
           root
           (projectile-project-name))))))

  (add-hook 'treemacs-mode-hook #'my/treemacs-projectile-setup)

  (when +treemacs-git-mode
    (when (and (memq +treemacs-git-mode '(deferred extended))
               (not treemacs-python-executable)
               (not (executable-find "python3")))
      (setq +treemacs-git-mode 'simple))
    (treemacs-git-mode +treemacs-git-mode)
    (setq treemacs-collapse-dirs
          (if (memq +treemacs-git-mode '(extended deferred)) 3 0))))

(use-package treemacs-nerd-icons
  :after treemacs
  :config
  (treemacs-load-theme "nerd-icons"))

(use-package treemacs-evil
  :after (treemacs evil)
  :config
  (evil-define-key 'normal treemacs-mode-map (kbd "TAB") 'treemacs-TAB-action))

(use-package treemacs-projectile
  :after (treemacs projectile)
  :commands (treemacs-projectile))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package treemacs-persp
  :after (treemacs persp-mode)
  :config
  (treemacs-set-scope-type 'Perspectives))

(use-package fd-dired
  :defer t
  :config
  (setq fd-dired-use-gnu-find-syntax t))

(use-package dired-open
  :defer t
  :config
  (setq dired-open-extensions '(("png" . "imv")
                                ("jpg" . "imv")
                                ("mp4" . "mpv")
                                ("mkv" . "mpv")
                                ("pdf" . "zathura"))))

(use-package dired
  :straight (:type built-in)
  :commands (dired dired-jump)
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :custom
  (dired-listing-switches "-agho --group-directories-first")
  (dired-auto-revert-buffer t)
  (dired-dwim-target t)
  (dired-recursive-deletes 'always)
  (dired-recursive-copies 'always)
  (dired-kill-when-opening-new-dired-buffer t)

  :config
  ;; Evil keybindings for dired
  (evil-define-key 'normal dired-mode-map
    (kbd "h") 'dired-up-directory
    (kbd "l") 'dired-find-file
    (kbd "j") 'dired-next-line
    (kbd "k") 'dired-previous-line
    (kbd "G") 'dired-goto-file
    (kbd "gg") 'beginning-of-buffer
    (kbd "^") 'dired-up-directory
    (kbd "~") (lambda () (interactive) (dired "~"))
    (kbd "RET") 'dired-find-file
    (kbd "i") 'dired-maybe-insert-subdir
    (kbd "m") 'dired-mark
    (kbd "u") 'dired-unmark
    (kbd "U") 'dired-unmark-all-marks
    (kbd "t") 'dired-toggle-marks
    (kbd "C-n") 'dired-create-empty-file
    (kbd "C-d") 'dired-create-directory
    (kbd "R") 'dired-do-rename
    (kbd "D") 'dired-do-delete
    (kbd "C") 'dired-do-copy
    (kbd "X") 'dired-open-file
    (kbd "M") 'dired-do-chmod
    (kbd "O") 'dired-do-chown
    (kbd "s") 'dired-sort-toggle-or-edit
    (kbd "gh") 'dired-hide-dotfiles-mode
    (kbd "gr") 'revert-buffer))

(use-package dired-x
  :straight (:type built-in)
  :after dired
  :custom
  (dired-x-hands-off-my-keys nil)
  :config
  (setq dired-omit-files "^\\.[^.]\\|^#\\|^\\.$\\|^\\.\\.$\\|\\.pyc$\\|\\.o$")
  (setq dired-omit-verbose nil))

(use-package dired-git-info
  :after dired
  :hook (dired-mode . dired-git-info-mode)
  :config
  (setq dgi-auto-hide-details-p nil))

(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package dired-ranger
  :after dired
  :config
  (define-key dired-mode-map (kbd "y") 'dired-ranger-copy)
  (define-key dired-mode-map (kbd "p") 'dired-ranger-paste)
  (define-key dired-mode-map (kbd "x") 'dired-ranger-move))

;; Projects directory
(defcustom my/projects-directory (expand-file-name "~/Projects/")
  "Base directory for all projects."
  :type 'directory
  :group 'project)

(unless (file-directory-p my/projects-directory)
  (make-directory my/projects-directory t))

(use-package projectile
  :defer t
  :init
  (setq projectile-project-search-path (list my/projects-directory)
        projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" user-emacs-directory)
        projectile-cache-file (expand-file-name "projectile.cache" user-emacs-directory))

  :custom
  ;; === Performance Settings ===
  (projectile-enable-caching t)
  (projectile-indexing-method 'alien)  ; Use external tools (faster)
  (projectile-sort-order 'recentf)
  (projectile-file-exists-remote-cache-expire nil)

  ;; === Project Detection ===
  (projectile-project-root-files '(".projectile" "flake.nix" ".envrc"))
  (projectile-project-root-files-bottom-up '(".git" ".hg" ".svn" ".bzr" "_darcs"))
  (projectile-require-project-root t)

  ;; === Completion System ===
  (projectile-completion-system 'default)  ; Works with vertico/consult

  ;; === Behavior ===
  (projectile-auto-discover t)
  (projectile-switch-project-action 'projectile-dired)
  (projectile-per-project-compilation-buffer t)

  :config
  (projectile-mode +1)

  ;; === Projects Directory Setup ===
  (unless (file-directory-p my/projects-directory)
    (make-directory my/projects-directory t))

  ;; === Auto-discover Projects ===
  (defun my/projectile-discover-projects ()
    "Discover all projects in search path."
    (interactive)
    (projectile-discover-projects-in-search-path))

  (add-hook 'after-init-hook #'my/projectile-discover-projects)

  ;; === Helper Functions ===
  (defun my/projectile-bibliography-files ()
    "Get bibliography files for current project."
    (when-let ((root (projectile-project-root)))
      (let ((bib-files '()))
        ;; Search in project root
        (dolist (file (directory-files root t "\\.bib\\'"))
          (push file bib-files))
        ;; Search in common bibliography directories
        (dolist (subdir '("references/" "bib/" "bibliography/"))
          (let ((dir (expand-file-name subdir root)))
            (when (file-directory-p dir)
              (dolist (file (directory-files dir t "\\.bib\\'"))
                (push file bib-files)))))
        (delete-dups (nreverse bib-files)))))

  (defun my/projectile-find-file-by-extension (ext)
    "Find files with extension EXT in current project."
    (when (projectile-project-p)
      (let* ((files (projectile-current-project-files))
             (filtered (seq-filter (lambda (f) (string-suffix-p ext f)) files)))
        (find-file (expand-file-name
                   (completing-read (format "Select %s file: " ext) filtered)
                   (projectile-project-root))))))

  (defun my/projectile-find-bib-file ()
    "Open a .bib file in current project."
    (interactive)
    (if-let ((bib-files (my/projectile-bibliography-files)))
        (find-file (completing-read "Select bibliography: " bib-files))
      (message "No .bib files found in project")))

  (defun my/projectile-find-tex-file ()
    "Open a .tex file in current project."
    (interactive)
    (my/projectile-find-file-by-extension ".tex"))

  (defun my/projectile-find-py-file ()
    "Open a .py file in current project."
    (interactive)
    (my/projectile-find-file-by-extension ".py"))

  (defun my/setup-project-bibliography ()
    "Setup bibliography infrastructure for current project."
    (interactive)
    (if-let ((root (and (projectile-project-p) (projectile-project-root))))
        (let* ((bib-dir (expand-file-name "references/" root))
               (bib-file (expand-file-name "references.bib" bib-dir)))
          (unless (file-directory-p bib-dir)
            (make-directory bib-dir t))
          (unless (file-exists-p bib-file)
            (with-temp-file bib-file
              (insert "% Bibliography for "
                      (projectile-project-name) "\n")
              (insert "% Created: " (format-time-string "%Y-%m-%d") "\n\n")))
          (message "Bibliography setup complete: %s" bib-file)
          bib-file)
      (message "Not in a project"))))

(use-package proj-persp-extras
  :straight (:host github
           :repo "brandonwillard/proj-persp-extras")
  :after (persp-mode projectile)
  :config
  ;; Enable automatic project-perspective correspondence
  (proj-persp-tracking-mode +1)

  ;; Configure perspective naming
  (setq proj-persp-extras-project-name-function
        (lambda ()
          (when (projectile-project-p)
            (file-name-as-directory (expand-file-name (projectile-project-root))))))

  ;; Auto-switch to project's perspective when switching projects
  (defun my/proj-persp-switch-project-advice (orig-fun &rest args)
    "Switch to project's perspective before switching projects."
    (when (and (projectile-project-p)
               (bound-and-true-p persp-mode))
      (let* ((project-root (projectile-project-root))
             (persp-name (file-name-as-directory (expand-file-name project-root))))
        (unless (member persp-name (persp-names))
          (persp-switch persp-name))))
    (apply orig-fun args))

  (advice-add 'projectile-switch-project :around #'my/proj-persp-switch-project-advice))

(use-package bookmark
  :straight (:type built-in)
  :defer t
  :custom
  (bookmark-default-file (expand-file-name "bookmarks" user-emacs-directory))
  (bookmark-save-flag 1))

(defun ar/kill-other-buffers ()
  "Kill all buffers except the current one."
  (interactive)
  (let ((current (current-buffer)))
    (dolist (buf (buffer-list))
      (unless (or (eq buf current)
                  (string-prefix-p " " (buffer-name buf))
                  (string-prefix-p "*" (buffer-name buf)))
        (kill-buffer buf)))))

(defun ar/save-all-buffers ()
  "Save all file-visiting buffers without prompting."
  (interactive)
  (save-some-buffers t))

(defun ar/switch-to-scratch ()
  "Switch to *scratch* buffer, creating if needed."
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun ar/treemacs-toggle-and-follow ()
  "Toggle treemacs and follow current file."
  (interactive)
  (if (treemacs-is-treemacs-window-selected?)
      (delete-window)
    (progn
      (if (projectile-project-p)
          (treemacs-projectile)
        (treemacs))
      (treemacs-find-file))))

(ar/global-leader
 ;; File operations
 "f" '(:ignore t :wk "file")
 "f f" '(find-file :wk "find file")
 "f F" '(find-file-other-window :wk "find file (other window)")
 "f e" '(dired :wk "dired")
 "f E" '(dired-jump :wk "dired (jump to file)")
 "f r" '(consult-recent-file :wk "recent files")
 "f R" '(rename-file :wk "rename file")
 "f s" '(save-buffer :wk "save file")
 "f S" '(ar/save-all-buffers :wk "save all files")
 "f y" '((lambda () (interactive) (kill-new (buffer-file-name))) :wk "yank file path")
 "f t" '(treemacs :wk "toggle treemacs")
 "f T" '(ar/treemacs-toggle-and-follow :wk "treemacs find file")

 ;; Buffer operations
 "b" '(:ignore t :wk "buffers")
 "b b" '(consult-buffer :wk "switch buffer")
 "b B" '(switch-to-buffer-other-window :wk "switch (other window)")
 "b i" '(ibuffer :wk "ibuffer")
 "b k" '(kill-current-buffer :wk "kill buffer")
 "b K" '(ar/kill-other-buffers :wk "kill other buffers")
 "b n" '(next-buffer :wk "next buffer")
 "b p" '(previous-buffer :wk "previous buffer")
 "b r" '(revert-buffer :wk "revert buffer")
 "b R" '(rename-buffer :wk "rename buffer")
 "b s" '(ar/switch-to-scratch :wk "scratch buffer")
 "b S" '(save-buffer :wk "save buffer")
 "b m" '(bookmark-set :wk "set bookmark")
 "b M" '(consult-bookmark :wk "jump to bookmark")

 ;; Project operations
 "p" '(:ignore t :wk "project")
 "p p" '(projectile-switch-project :wk "switch project")
 "p f" '(projectile-find-file :wk "find file")
 "p F" '(projectile-find-file-in-known-projects :wk "find in all projects")
 "p d" '(projectile-find-dir :wk "find directory")
 "p D" '(projectile-dired :wk "dired root")
 "p b" '(projectile-switch-to-buffer :wk "switch buffer")
 "p k" '(projectile-kill-buffers :wk "kill buffers")
 "p c" '(projectile-compile-project :wk "compile")
 "p e" '(projectile-run-eshell :wk "eshell")
 "p s" '(projectile-grep :wk "grep")
 "p r" '(projectile-ripgrep :wk "ripgrep")
 "p R" '(projectile-replace :wk "replace")
 "p i" '(projectile-invalidate-cache :wk "invalidate cache")
 "p +" '(my/projectile-discover-projects :wk "discover projects")
 "p t" '(treemacs-projectile :wk "treemacs project")
 "p B" '(my/projectile-find-bib-file :wk "find .bib")
 "p T" '(my/projectile-find-tex-file :wk "find .tex")
 "p y" '(my/projectile-find-py-file :wk "find .py")
 "p !" '(projectile-run-async-shell-command-in-root :wk "async shell cmd")
 "p &" '(projectile-run-command-in-root :wk "shell cmd")

 ;; Workspace (Perspective) operations
 "TAB" '(:ignore t :wk "workspace")
 "TAB TAB" '(persp-switch :wk "switch workspace")
 "TAB n" '(persp-next :wk "next workspace")
 "TAB p" '(persp-prev :wk "previous workspace")
 "TAB d" '(persp-kill :wk "delete workspace")
 "TAB r" '(persp-rename :wk "rename workspace")
 "TAB s" '(persp-save-state-to-file :wk "save workspaces")
 "TAB l" '(persp-load-state-from-file :wk "load workspaces")
 "TAB b" '(persp-switch-to-buffer :wk "switch to workspace buffer")
 "TAB k" '(persp-kill-buffer :wk "kill buffer from workspace")
 "TAB a" '(persp-add-buffer :wk "add buffer to workspace")
 "TAB A" '(persp-set-buffer :wk "move buffer to workspace")
 "TAB i" '(persp-import-buffers :wk "import buffers")
 "TAB 0" '((lambda () (interactive) (persp-switch "main")) :wk "switch to main"))

(use-package magit
  :defer t
  :init
  (setq magit-auto-revert-mode nil)
  :commands (magit-status magit-blame)
  :custom
  ;; For a focused view, display the Magit status buffer in its own frame.
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  ;; Automatically save file-visiting buffers before staging changes.
  (magit-save-repository-buffers 'dont-confirmk)
  :config
  ;; When quitting Magit, this ensures the previous window configuration is restored.
  ;; The `magit-display-buffer-fullframe-status-v1` function saves the layout
  ;; to the `:magit-fullscreen` register, which we jump back to.
  (defun ar/magit-quit-and-restore-windows ()
    "Kill the Magit buffer and restore the previous window configuration."
    (interactive)
    (kill-buffer (current-buffer))
    (when (get-register :magit-fullscreen)
      (jump-to-register :magit-fullscreen)))

  ;; Bind "q" in the status buffer to our custom quitting function.
  (define-key magit-status-mode-map (kbd "q") #'ar/magit-quit-and-restore-windows))

(use-package forge :defer t :after magit)

(use-package magit-todos
  :hook (magit-mode . magit-todos-mode))

(use-package git-timemachine
  :after magit
  :config
  (evil-define-key 'normal git-timemachine-mode-map (kbd "C-j") 'git-timemachine-show-previous-revision)
  (evil-define-key 'normal git-timemachine-mode-map (kbd "C-k") 'git-timemachine-show-next-revision))

(use-package git-gutter
  :defer t
  :hook (prog-mode . git-gutter-mode)
  :custom
  ;; Only update the gutter when the buffer is saved, for performance.
  (git-gutter:update-on-save t)
  ;; Use a lighter touch for updates; avoids refreshing on every change.
  (git-gutter:update-method "idle")
  :config
  ;; Define keybindings for evil-mode for navigating between hunks.
  (with-eval-after-load 'evil
    (define-key evil-normal-state-map (kbd "]g") 'git-gutter:next-hunk)
    (define-key evil-normal-state-map (kbd "[g") 'git-gutter:previous-hunk)))

(ar/global-leader
 "g" '(:ignore t :wk "git")
 "g s" '(magit-status :wk "status")
 "g c" '(magit-commit :wk "commit")
 "g C" '(magit-commit-amend :wk "commit amend")
 "g p" '(magit-push-current-to-pushremote :wk "push")
 "g P" '(magit-pull-from-upstream :wk "pull")
 "g b" '(magit-branch :wk "branches")
 "g l" '(magit-log-buffer-file :wk "log current file")
 "g L" '(magit-log-current :wk "log current branch")
 "g d" '(magit-diff-unstaged :wk "diff")
 "g f" '(magit-fetch :wk "fetch")
 "g m" '(magit-merge :wk "merge")
 "g r" '(magit-rebase :wk "rebase")
 "g n" '(git-gutter:next-hunk :wk "next hunk")
 "g N" '(git-gutter:previous-hunk :wk "previous hunk")
 "g S" '(git-gutter:stage-hunk :wk "stage hunk"))

(use-package tex
  :straight auctex
  :defer t
  :hook ((LaTeX-mode . visual-line-mode)
         (LaTeX-mode . prettify-symbols-mode)
         (LaTeX-mode . TeX-fold-mode))
  :custom
  ;; === Tectonic Configuration (V2 CLI) ===
  ;; Based on: https://tectonic-typesetting.github.io/book/latest/howto/auctex-setup/
  (TeX-engine 'tectonic)
  (TeX-process-asynchronous t)  ; Don't hang Emacs during compilation
  (TeX-check-TeX nil)            ; Don't look for traditional TeX distributions

  ;; === Viewer Configuration ===
  (TeX-view-program-selection '((output-pdf "PDF Tools")))
  (TeX-source-correlate-mode t)  ; Enable forward/inverse search
  (TeX-PDF-mode t)               ; Always produce PDF output

  ;; === Parsing and Auto-save ===
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-save-query nil)           ; Don't ask to save before compiling

  ;; === Indentation ===
  (LaTeX-indent-level 2)
  (LaTeX-item-indent 0)

  :config
  ;; === Register Tectonic Engine ===
  (add-to-list 'TeX-engine-alist
               '(tectonic
                 "Tectonic"
                 "tectonic -X compile -f plain %T"
                 "tectonic -X watch"
                 nil))

  ;; === Simplify LaTeX command style for Tectonic ===
  (setq LaTeX-command-style '(("" "%(latex)")))

  ;; === Define Tectonic Commands ===
  (setq TeX-command-list
        (append TeX-command-list
                '(("Tectonic" "tectonic -X compile %s" TeX-run-TeX nil
                   (latex-mode LaTeX-mode) :help "Compile with Tectonic")
                  ("Tectonic Watch" "tectonic -X watch %s" TeX-run-TeX nil
                   (latex-mode LaTeX-mode) :help "Watch mode")
                  ("ChkTeX" "chktex -v0 -q -I %s" TeX-run-compile nil
                   (latex-mode LaTeX-mode) :help "Check with ChkTeX"))))

  ;; === Modify TeX and LaTeX commands for Tectonic ===
  (let ((tex-list (assoc "TeX" TeX-command-list))
        (latex-list (assoc "LaTeX" TeX-command-list)))
    (setf (cadr tex-list) "%(tex)"
          (cadr latex-list) "%l"))

  ;; === Project-aware output directory ===
  ;; For Tectonic projects (with Tectonic.toml)
  (add-hook 'after-change-major-mode-hook
            (lambda ()
              (when-let ((project (project-current))
                         (proot (project-root project)))
                (when (file-exists-p (expand-file-name "Tectonic.toml" proot))
                  (setq-local TeX-output-dir (expand-file-name "build/index" proot))))))

  ;; === Keybindings ===
  ;; Note: Most keybindings are set via :bind below to avoid loading issues
  )

;; LaTeX mode keybindings (set after mode is defined)
(with-eval-after-load 'latex
  (define-key LaTeX-mode-map (kbd "C-c C-c") #'TeX-command-master)
  (define-key LaTeX-mode-map (kbd "C-c C-v") #'TeX-view)
  (define-key LaTeX-mode-map (kbd "C-c C-k")
              (lambda () (interactive) (TeX-command "ChkTeX" 'TeX-master-file))))

(use-package evil-tex
  :after (tex evil)
  :hook (LaTeX-mode . evil-tex-mode))

(defun my/find-project-bibliographies ()
  "Find all .bib files in project (root, references/, bib/, bibliography/)."
  (when (and (fboundp 'projectile-project-p)
             (projectile-project-p))
    (when-let ((root (projectile-project-root)))
      (let ((bib-files '())
            (search-dirs (list root
                              (expand-file-name "references/" root)
                              (expand-file-name "bib/" root)
                              (expand-file-name "bibliography/" root))))
        (dolist (dir search-dirs)
          (when (file-directory-p dir)
            (dolist (file (directory-files dir t "\\.bib\\'"))
              (push file bib-files))))
        (delete-dups (nreverse bib-files))))))

(use-package citar
  :defer t
  :after oc
  :custom
  ;; === Global Fallback Bibliography ===
  (citar-bibliography '("~/references.bib"))
  (citar-library-paths '("~/Zotero/storage"))
  (citar-notes-paths (list my/org-roam-directory))

  ;; === Display Configuration ===
  (citar-symbols
   `((file ,(nerd-icons-mdicon "nf-md-file_document") . " ")
     (note ,(nerd-icons-mdicon "nf-md-note_text") . " ")
     (link ,(nerd-icons-mdicon "nf-md-link") . " ")))

  :config
  ;; === Dynamic Bibliography Function with Projectile Guard ===
  (defun my/citar-bibliography ()
    "Return project bibliography or global fallback."
    (or (my/find-project-bibliographies) citar-bibliography))

  ;; Override citar's bibliography detection
  (advice-add 'citar--bibliography-files :override #'my/citar-bibliography)

  ;; === Set Buffer-Local Bibliography with Projectile Guard ===
  (defun my/citar-set-local-bibliography ()
    "Set buffer-local bibliography for current buffer."
    (when (and (buffer-file-name)
               (or (derived-mode-p 'org-mode)
                   (derived-mode-p 'LaTeX-mode)))
      (setq-local citar-bibliography (my/citar-bibliography))
      ;; Also update org-cite
      (when (derived-mode-p 'org-mode)
        (setq-local org-cite-global-bibliography (my/citar-bibliography)))))

  ;; Apply to hooks - DEFERRED until after projectile is available
  ;; This prevents errors during startup before projectile is loaded
  (if (fboundp 'projectile-project-p)
      ;; Projectile already loaded, add hooks immediately
      (progn
        (add-hook 'org-mode-hook #'my/citar-set-local-bibliography)
        (add-hook 'LaTeX-mode-hook #'my/citar-set-local-bibliography)
        (add-hook 'buffer-list-update-hook
                  (lambda ()
                    (when (and (buffer-file-name)
                               (or (derived-mode-p 'org-mode)
                                   (derived-mode-p 'LaTeX-mode)))
                      (my/citar-set-local-bibliography)))))
    ;; Projectile not loaded yet, defer hooks
    (with-eval-after-load 'projectile
      (add-hook 'org-mode-hook #'my/citar-set-local-bibliography)
      (add-hook 'LaTeX-mode-hook #'my/citar-set-local-bibliography)
      (add-hook 'buffer-list-update-hook
                (lambda ()
                  (when (and (buffer-file-name)
                             (or (derived-mode-p 'org-mode)
                                 (derived-mode-p 'LaTeX-mode)))
                    (my/citar-set-local-bibliography)))))))

;; Citar keybindings (set after modes are loaded)
(with-eval-after-load 'latex
  (define-key LaTeX-mode-map (kbd "C-c b") #'citar-insert-citation)
  (define-key LaTeX-mode-map (kbd "C-c o") #'citar-open))

;; === Embark Integration ===
(use-package citar-embark
  :after (citar embark)
  :config
  (citar-embark-mode)
  (with-eval-after-load 'embark
    (add-to-list 'embark-keymap-alist '(citar-reference . citar-embark-map))))

;; === Org-Roam Literature Notes ===
(use-package citar-org-roam
  :after (citar org-roam)
  :config
  (citar-org-roam-mode 1)
  (setq citar-org-roam-subdir "literature"))

(with-eval-after-load 'oc
  (require 'citar)

  ;; === Set Citar as Processor ===
  (setq org-cite-insert-processor 'citar
        org-cite-follow-processor 'citar
        org-cite-activate-processor 'citar)

  ;; === Org-Cite Bibliography Function ===
  (defun my/org-cite-list-bibliography-files ()
    "List bibliography files for org-cite."
    (my/citar-bibliography))

  (advice-add 'org-cite-list-bibliography-files
              :override #'my/org-cite-list-bibliography-files)

  ;; === Org-Mode Citation Functions ===
  (defun my/org-insert-citation ()
    "Insert citation using citar in org-mode."
    (interactive)
    (if (fboundp 'citar-insert-citation)
        (citar-insert-citation)
      (message "Citar not available")))

  (defun my/org-verify-bibliography ()
    "Check if bibliography files exist and are readable."
    (interactive)
    (let ((bib-files (my/citar-bibliography)))
      (if bib-files
          (progn
            (message "Found %d bibliography file(s):" (length bib-files))
            (dolist (file bib-files)
              (message "  %s %s"
                      (if (file-readable-p file) "✓" "✗")
                      file)))
        (message "No bibliography files found. Using global fallback.")))))

;; Org-mode citation keybindings (set after org is loaded)
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c b") #'my/org-insert-citation)
  (define-key org-mode-map (kbd "C-c o") #'citar-open))

(defun my/setup-project-bibliography ()
  "Setup bibliography infrastructure for current project."
  (interactive)
  (if-let* ((project (project-current))
            (root (project-root project)))
      (let* ((bib-dir (expand-file-name "references/" root))
             (bib-file (expand-file-name "references.bib" bib-dir)))
        (unless (file-directory-p bib-dir)
          (make-directory bib-dir t))
        (unless (file-exists-p bib-file)
          (with-temp-file bib-file
            (insert "% Bibliography for "
                    (file-name-nondirectory (directory-file-name root)) "\n")
            (insert "% Created: " (format-time-string "%Y-%m-%d") "\n\n")))
        (message "Bibliography setup complete: %s" bib-file)
        bib-file)
    (message "Not in a project")))

(use-package reftex
  :straight (:type built-in)
  :after tex
  :hook (LaTeX-mode . reftex-mode)
  :custom
  (reftex-plug-into-AUCTeX t)
  (reftex-default-bibliography nil))  ; Let citar handle citations

(use-package cdlatex
  :defer t
  :hook ((LaTeX-mode . cdlatex-mode)
         (org-mode . org-cdlatex-mode))
  :custom
  ;; Custom math symbols
  (cdlatex-math-modify-alist
   '((?B "\\mathbb" nil t nil nil)
     (?C "\\mathcal" nil t nil nil)
     (?F "\\mathfrak" nil t nil nil)
     (?S "\\mathscr" nil t nil nil)))

  ;; Quick environment insertion
  (cdlatex-env-alist
   '(("equation*" "\\begin{equation*}\n?\n\\end{equation*}" nil)
     ("align*" "\\begin{align*}\n?\n\\end{align*}" nil)))

  :config
  ;; Ensure CDLaTeX tab works correctly
  (define-key cdlatex-mode-map (kbd "TAB") #'cdlatex-tab))

(use-package laas
  :defer t
  :hook ((LaTeX-mode . laas-mode)
         (org-mode . laas-mode))
  :config
  (aas-set-snippets 'laas-mode
    ;; === Math-only snippets ===
    :cond #'texmathp

    ;; Common operations
    "supp" "\\supp"
    "inf" "\\infty"
    "lim" "\\lim"

    ;; Big O notation
    "On" "O(n)"
    "O1" "O(1)"
    "Olog" "O(\\log n)"

    ;; Functions with yasnippet integration
    "Sum" (lambda () (interactive)
            (yas-expand-snippet "\\sum_{${1:i=1}}^{${2:n}} $0"))
    "Prod" (lambda () (interactive)
             (yas-expand-snippet "\\prod_{${1:i=1}}^{${2:n}} $0"))
    "Int" (lambda () (interactive)
            (yas-expand-snippet "\\int_{${1:a}}^{${2:b}} $0"))
    "Lim" (lambda () (interactive)
            (yas-expand-snippet "\\lim_{${1:x \\to \\infty}} $0"))

    ;; === Accent snippets (work on preceding object) ===
    :cond #'laas-object-on-left-condition
    "qq" (lambda () (interactive) (laas-wrap-previous-object "sqrt"))
    "vv" (lambda () (interactive) (laas-wrap-previous-object "vec"))
    ".." (lambda () (interactive) (laas-wrap-previous-object "dot"))
    "::" (lambda () (interactive) (laas-wrap-previous-object "ddot"))
    "~~" (lambda () (interactive) (laas-wrap-previous-object "tilde"))
    "^^" (lambda () (interactive) (laas-wrap-previous-object "hat"))
    "--" (lambda () (interactive) (laas-wrap-previous-object "bar"))))

(with-eval-after-load 'yasnippet
  (yas-define-snippets 'latex-mode
    '(;; === Environments ===
      ("beg" "\\begin{${1:env}}\n  $0\n\\end{$1}" "begin-end")

      ("eq" "\\begin{equation}\n  ${1:equation}\n  \\label{eq:${2:label}}\n\\end{equation}\n$0"
       "equation")

      ("eq*" "\\begin{equation*}\n  $0\n\\end{equation*}" "equation*")

      ("ali" "\\begin{align}\n  ${1:a} &= ${2:b} \\\\\n  ${3:c} &= ${4:d}\n  \\label{eq:${5:label}}\n\\end{align}\n$0"
       "align")

      ("ali*" "\\begin{align*}\n  ${1:a} &= ${2:b} \\\\\n  $0\n\\end{align*}" "align*")

      ;; === Figures ===
      ("fig" "\\begin{figure}[${1:htbp}]\n  \\centering\n  \\includegraphics[width=${2:0.8}\\textwidth]{${3:path}}\n  \\caption{${4:caption}}\n  \\label{fig:${5:label}}\n\\end{figure}\n$0"
       "figure")

      ;; === Tables ===
      ("tab" "\\begin{table}[${1:htbp}]\n  \\centering\n  \\caption{${2:caption}}\n  \\label{tab:${3:label}}\n  \\begin{tabular}{${4:lll}}\n    \\toprule\n    ${5:header} \\\\\n    \\midrule\n    ${6:data} \\\\\n    \\bottomrule\n  \\end{tabular}\n\\end{table}\n$0"
       "table")

      ;; === Math ===
      ("frac" "\\frac{${1:num}}{${2:denom}}$0" "fraction")
      ("pdv" "\\frac{\\partial ${1:f}}{\\partial ${2:x}}$0" "partial derivative")
      ("dv" "\\frac{d ${1:f}}{d ${2:x}}$0" "derivative")

      ;; === Citations & References ===
      ("cite" "\\cite{${1:key}}$0" "cite")
      ("ref" "\\ref{${1:label}}$0" "reference")
      ("eqref" "\\eqref{${1:eq:label}}$0" "equation reference")

      ;; === Sectioning ===
      ("sec" "\\section{${1:title}}\n\\label{sec:${2:label}}\n$0" "section")
      ("sub" "\\subsection{${1:title}}\n\\label{sub:${2:label}}\n$0" "subsection")
      ("ssub" "\\subsubsection{${1:title}}\n\\label{ssub:${2:label}}\n$0" "subsubsection"))))

;; === Prettify Symbols ===
(defun my/latex-prettify-symbols-setup ()
  "Enable prettify-symbols-mode with LaTeX ligatures."
  (prettify-symbols-mode 1)
  (setq prettify-symbols-alist
        '(;; Greek letters
          ("\\alpha" . "α")   ("\\beta" . "β")     ("\\gamma" . "γ")
          ("\\delta" . "δ")   ("\\epsilon" . "ε")  ("\\lambda" . "λ")
          ("\\mu" . "μ")      ("\\pi" . "π")       ("\\sigma" . "σ")
          ("\\phi" . "φ")     ("\\theta" . "θ")    ("\\omega" . "ω")

          ;; Math operators
          ("\\sum" . "∑")     ("\\int" . "∫")      ("\\prod" . "∏")
          ("\\infty" . "∞")   ("\\partial" . "∂")

          ;; Set theory
          ("\\in" . "∈")      ("\\subset" . "⊂")   ("\\subseteq" . "⊆")
          ("\\cup" . "∪")     ("\\cap" . "∩")      ("\\emptyset" . "∅")

          ;; Logic & relations
          ("\\leq" . "≤")     ("\\geq" . "≥")      ("\\neq" . "≠")
          ("\\approx" . "≈")  ("\\equiv" . "≡")

          ;; Arrows
          ("\\rightarrow" . "→")  ("\\Rightarrow" . "⇒")
          ("\\leftarrow" . "←")   ("\\Leftarrow" . "⇐")
          ("\\leftrightarrow" . "↔")

          ;; Others
          ("\\times" . "×")   ("\\cdot" . "·")     ("\\forall" . "∀")
          ("\\exists" . "∃")  ("\\neg" . "¬"))))

(add-hook 'LaTeX-mode-hook #'my/latex-prettify-symbols-setup)

(with-eval-after-load 'org
  (require 'ox-latex)

  ;; === Use Tectonic for Exports ===
  (setq org-latex-compiler "tectonic")
  (setq org-latex-pdf-process '("tectonic -X compile %f"))

  ;; === Preview Configuration ===
  (add-to-list 'org-preview-latex-process-alist
               '(tectonic
                 :programs ("tectonic" "convert")
                 :description "pdf > png"
                 :message "you need to install: tectonic and imagemagick"
                 :image-input-type "pdf"
                 :image-output-type "png"
                 :image-size-adjust (1.0 . 1.0)
                 :latex-compiler
                 ("tectonic -Z shell-escape-cwd=%o --outfmt pdf --outdir %o %f")
                 :image-converter
                 ("convert -density %D -trim -antialias %f -quality 100 %O")))

  (setq org-preview-latex-default-process 'tectonic)

  ;; === LaTeX Classes ===
  (add-to-list 'org-latex-classes
               '("article"
                 "\\documentclass[11pt,a4paper]{article}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")) t)

  (add-to-list 'org-latex-classes
               '("report"
                 "\\documentclass[11pt,a4paper]{report}"
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")) t)

  (add-to-list 'org-latex-classes
               '("beamer"
                 "\\documentclass{beamer}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")) t))

;; === Toggle LaTeX fragment previews automatically ===
(use-package org-fragtog
  :defer t
  :hook (org-mode . org-fragtog-mode))

;; === Show markup when cursor is on it ===
(use-package org-appear
  :defer t
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-autolinks t)
  (org-appear-autosubmarkers t)
  (org-appear-autoentities t))

(ar/global-leader
  ;; === LaTeX Compilation ===
  "l" '(:ignore t :wk "latex")
  "l c" '(TeX-command-master :wk "Compile")
  "l v" '(TeX-view :wk "View PDF")
  "l l" '((lambda () (interactive) (TeX-command "LaTeX" 'TeX-master-file)) :wk "LaTeX")
  "l t" '((lambda () (interactive) (TeX-command "Tectonic" 'TeX-master-file)) :wk "Tectonic")
  "l w" '((lambda () (interactive) (TeX-command "Tectonic Watch" 'TeX-master-file)) :wk "Watch")

  ;; === Bibliography ===
  "l b" '(:ignore t :wk "bibliography")
  "l b i" '(citar-insert-citation :wk "Insert Citation")
  "l b o" '(citar-open :wk "Open Entry")
  "l b n" '(citar-create-note :wk "Create Note")
  "l b s" '(my/setup-project-bibliography :wk "Setup Project Bib")
  "l b v" '(my/org-verify-bibliography :wk "Verify Bibliography")

  ;; === Org LaTeX ===
  "l p" '(:ignore t :wk "preview")
  "l p p" '(org-latex-preview :wk "Toggle Preview")
  "l p e" '(org-latex-export-to-pdf :wk "Export to PDF")
  "l p f" '(org-fragtog-mode :wk "Toggle Fragtog"))

(use-package lsp-pyright
  :after lsp-mode
  :custom
  (lsp-pyright-langserver-command "basedpyright")
  (lsp-pyright-type-checking-mode "standard")
  (lsp-pyright-diagnostic-mode "openFilesOnly")
  :hook (python-ts-mode . (lambda ()
                            (require 'lsp-pyright)
                            (lsp-deferred))))

;; Flycheck 35.0+ has built-in ruff support
;; Simply ensure ruff is installed: pip install ruff

(with-eval-after-load 'flycheck
  ;; Add python-ruff to the list of checkers for python-ts-mode
  (add-to-list 'flycheck-checkers 'python-ruff)

  ;; Set it as the preferred checker for python-ts-mode
  (defun my/flycheck-python-setup ()
    "Setup Flycheck for Python with Ruff."
    (flycheck-select-checker 'python-ruff))

  (add-hook 'python-ts-mode-hook #'my/flycheck-python-setup))

;; For older flycheck versions without built-in ruff, use this:
;; (with-eval-after-load 'flycheck
;;   (flycheck-define-checker python-ruff
;;     "A Python syntax and style checker using Ruff."
;;     :command ("ruff" "check"
;;               "--output-format=concise"
;;               "--stdin-filename" source-original
;;               "-")
;;     :standard-input t
;;     :error-patterns
;;     ((error line-start
;;             (or "-" (file-name)) ":" line ":" (optional column ":") " "
;;             (id (one-or-more (any alpha digit))) " "
;;             (message (one-or-more not-newline))
;;             line-end))
;;     :modes (python-mode python-ts-mode))
;;
;;   (add-to-list 'flycheck-checkers 'python-ruff))

(with-eval-after-load 'apheleia
  (setf (alist-get 'ruff apheleia-formatters)
        '("ruff" "format" "--stdin-filename" filepath "-"))
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(ruff)))

(use-package jupyter
  :defer t
  :after org
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((jupyter . t)))

  ;; Set default header args for jupyter-python blocks
  (setq org-babel-default-header-args:jupyter-python
        '((:async . "yes")
          (:session . "py")
          (:kernel . "python3")
          (:tangle . "no")))

  ;; Override python blocks to use jupyter automatically
  (with-eval-after-load 'ob-jupyter
    (org-babel-jupyter-override-src-block "python")
    (add-to-list 'org-src-lang-modes '("jupyter-python" . python-ts)))

  ;; Keybindings
  (ar/global-leader
    "j" '(:ignore t :wk "jupyter")
    "j c" '(jupyter-connect-repl :wk "Connect REPL")
    "j r" '(jupyter-run-repl :wk "Run REPL")
    "j k" '(jupyter-shutdown-kernel :wk "Shutdown kernel")
    "j i" '(jupyter-inspect-at-point :wk "Inspect")))

(use-package ob-async
  :after org
  :config
  (setq ob-async-no-async-languages-alist '("python" "jupyter-python")))

;; Function to dynamically set tangle target for current block
(defun my/jupyter-set-tangle-file ()
  "Dynamically set tangle file for current jupyter-python block."
  (interactive)
  (let* ((default-dir (file-name-directory (buffer-file-name)))
         (python-dir (expand-file-name "python/" default-dir))
         (filename (read-string "Python filename (without .py): "
                               (file-name-base (buffer-file-name)))))
    (unless (file-directory-p python-dir)
      (make-directory python-dir t))
    ;; Set property for current subtree
    (org-set-property "header-args:jupyter-python"
                     (format ":session py :async yes :kernel python3 :tangle python/%s.py"
                            filename))
    (message "Set tangle to: python/%s.py" filename)))

;; Alternative: Use file-level property dynamically
(defun my/jupyter-setup-project-tangle ()
  "Setup tangle target at file level for jupyter-python blocks."
  (interactive)
  (let* ((default-dir (file-name-directory (buffer-file-name)))
         (python-dir (expand-file-name "python/" default-dir))
         (default-file (file-name-base (buffer-file-name)))
         (filename (read-string "Default Python filename: " default-file)))
    (unless (file-directory-p python-dir)
      (make-directory python-dir t))
    (save-excursion
      (goto-char (point-min))
      ;; Add or update PROPERTY line
      (if (re-search-forward "^#\\+PROPERTY: header-args:jupyter-python" nil t)
          (progn
            (beginning-of-line)
            (kill-line)
            (insert (format "#+PROPERTY: header-args:jupyter-python :session py :async yes :kernel python3 :tangle python/%s.py"
                          filename)))
        (goto-char (point-min))
        (when (re-search-forward "^#\\+title:" nil t)
          (end-of-line)
          (newline)
          (insert (format "#+PROPERTY: header-args:jupyter-python :session py :async yes :kernel python3 :tangle python/%s.py"
                        filename)))))
    (message "Set default tangle to: python/%s.py" filename)))

;; Helper to use subtree's tangle-dir property
(defun my/org-jupyter-tangle-to-subdir (filename)
  "Tangle to python/ subdirectory, respecting tangle-dir property."
  (let ((tangle-dir (or (org-entry-get nil "tangle-dir" t) "python/")))
    (expand-file-name filename (expand-file-name tangle-dir default-directory))))

;; Keybindings
(ar/global-leader
  "o j" '(:ignore t :wk "jupyter tangle")
  "o j t" '(my/jupyter-set-tangle-file :wk "Set tangle file (subtree)")
  "o j T" '(my/jupyter-setup-project-tangle :wk "Setup file-level tangle"))

(use-package ein
  :commands (ein:run ein:login ein:notebooklist-open ein:ipynb-mode)
  :config
  ;; Configuration
  (setq ein:output-area-inlined-images t
        ein:slice-image t
        ein:query-timeout 1000
        ein:default-url-or-port "http://localhost:8888"
        ein:completion-backend 'ein:use-none-backend
        ein:use-auto-complete-superpack nil)

  ;; Keybindings
  (ar/global-leader
    "e" '(:ignore t :wk "ein (notebooks)")
    "e l" '(ein:login :wk "Login")
    "e r" '(ein:run :wk "Run server")
    "e o" '(ein:notebooklist-open :wk "Open list")
    "e s" '(ein:stop :wk "Stop server"))

  ;; Evil integration
  (with-eval-after-load 'evil
    (evil-define-key 'normal ein:notebook-mode-map
      (kbd "RET") 'ein:worksheet-execute-cell-and-goto-next
      (kbd "C-<return>") 'ein:worksheet-execute-cell
      (kbd "S-<return>") 'ein:worksheet-execute-cell-and-insert-below)

    (evil-define-key 'insert ein:notebook-mode-map
      (kbd "C-<return>") 'ein:worksheet-execute-cell
      (kbd "S-<return>") 'ein:worksheet-execute-cell-and-insert-below)))

;; Auto-open .ipynb files with ein
(add-to-list 'auto-mode-alist '("\\.ipynb\\'" . ein:ipynb-mode))

(defun my/babel-ansi ()
  "Apply ANSI color codes to the result of an Org Babel block."
  (when-let ((beg (org-babel-where-is-src-block-result nil nil)))
    (save-excursion
      (goto-char beg)
      (when (looking-at org-babel-result-regexp)
        (let ((end (org-babel-result-end))
              (ansi-color-context-region nil))
          (ansi-color-apply-on-region beg end))))))

(define-minor-mode org-babel-ansi-colors-mode
  "Apply ANSI color codes to Org Babel results globally."
  :global t
  :init-value t
  (if org-babel-ansi-colors-mode
      (add-hook 'org-babel-after-execute-hook #'my/babel-ansi)
    (remove-hook 'org-babel-after-execute-hook #'my/babel-ansi)))

(defun my/jupyter-org-scalar (value)
  (cond
   ((stringp value) value)
   (t (jupyter-org-scalar value))))

(define-minor-mode my/emacs-jupyter-raw-output
  "Make emacs-jupyter do raw output")

(defun my/jupyter-org-scalar-around (fun value)
  (if my/emacs-jupyter-raw-output
      (my/jupyter-org-scalar value)
    (funcall fun value)))

(with-eval-after-load 'jupyter
  (advice-add 'jupyter-org-scalar :around #'my/jupyter-org-scalar-around))

(defun my/org-strip-results (data)
  (replace-regexp-in-string ":\\(RESULTS\\|END\\):\n" "" data))

(with-eval-after-load 'org
  ;; Project structure helper
  (defun my/org-setup-project-structure ()
    "Create project subfolder structure for tangling."
    (interactive)
    (let* ((org-dir (file-name-directory (buffer-file-name)))
           (python-dir (expand-file-name "python/" org-dir))
           (tex-dir (expand-file-name "tex/" org-dir))
           (output-dir (expand-file-name "output/" org-dir)))
      (dolist (dir (list python-dir tex-dir output-dir))
        (unless (file-directory-p dir)
          (make-directory dir t)))
      (message "Created: python/, tex/, output/ directories")))

  ;; Improved template with proper #+PROPERTY syntax
  (defun my/org-insert-scientific-project-template ()
    "Insert header template for scientific org document."
    (interactive)
    (goto-char (point-min))
    (insert "#+TITLE: Scientific Analysis\n")
    (insert "#+AUTHOR: " user-full-name "\n")
    (insert "#+DATE: " (format-time-string "%Y-%m-%d") "\n")
    (insert "#+OPTIONS: toc:nil\n")
    (insert "#+STARTUP: overview indent\n")
    (insert "# -*- org-src-preserve-indentation: t; -*-\n\n")

    (insert "# LaTeX configuration for export\n")
    (insert "#+LATEX_CLASS: article\n")
    (insert "#+LATEX_HEADER: \\usepackage{amsmath}\n")
    (insert "#+LATEX_HEADER: \\usepackage{graphicx}\n\n")

    (insert "# Language-specific header arguments\n")
    (insert "# LaTeX: never evaluate, export code for PDF\n")
    (insert "#+PROPERTY: header-args:latex :exports code :eval never\n\n")

    (insert "# Python: export results, tangle to python/, evaluate on demand\n")
    (insert "#+PROPERTY: header-args:python :session py :exports results :eval never-export :tangle python/analysis.py\n\n")

    (insert "# Jupyter-Python: same as Python but async\n")
    (insert "#+PROPERTY: header-args:jupyter-python :session py :async yes :exports results :eval never-export :tangle python/jupyter.py :kernel python3\n\n")

    (insert "# To toggle Python export, use my/org-toggle-python-export\n")
    (insert "# To change tangle file, use my/org-set-python-tangle-file\n\n")
    (message "Inserted scientific project template"))

  ;; Toggle Python export: results vs none
  (defun my/org-toggle-python-export ()
    "Toggle Python/Jupyter blocks between exporting results vs. none."
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (let ((changed 0))
        ;; Toggle Python
        (when (re-search-forward
               "^#\\+PROPERTY: header-args:python.*:exports \\(results\\|none\\)" nil t)
          (let ((current (match-string 1)))
            (replace-match (if (string= current "results") "none" "results")
                          t t nil 1)
            (setq changed (1+ changed))))
        ;; Toggle Jupyter-Python
        (goto-char (point-min))
        (when (re-search-forward
               "^#\\+PROPERTY: header-args:jupyter-python.*:exports \\(results\\|none\\)" nil t)
          (let ((current (match-string 1)))
            (replace-match (if (string= current "results") "none" "results")
                          t t nil 1)
            (setq changed (1+ changed))))
	;;<
        (if (> changed 0)
            (progn
              (org-mode-restart)
              (message "Toggled %d language export setting(s). Press C-c C-c on #+PROPERTY lines to refresh." changed))
          (message "No Python/Jupyter property lines found")))))

  ;; Set tangle file for Python blocks
  (defun my/org-set-python-tangle-file ()
    "Set tangle file for Python and Jupyter-Python blocks."
    (interactive)
    (let* ((default-dir (file-name-directory (buffer-file-name)))
           (python-dir (expand-file-name "python/" default-dir))
           (filename (read-string "Python filename (without .py): "
                                 (file-name-base (buffer-file-name)))))
      (unless (file-directory-p python-dir)
        (make-directory python-dir t))
      (save-excursion
        (goto-char (point-min))
        (let ((changed 0))
          ;; Update Python tangle
          (when (re-search-forward
                 "^#\\+PROPERTY: header-args:python.*:tangle \\([^ \n]+\\)" nil t)
            (replace-match (concat "python/" filename ".py") t t nil 1)
            (setq changed (1+ changed)))
          ;; Update Jupyter-Python tangle
          (goto-char (point-min))
          (when (re-search-forward
                 "^#\\+PROPERTY: header-args:jupyter-python.*:tangle \\([^ \n]+\\)" nil t)
            (replace-match (concat "python/" filename ".py") t t nil 1)
            (setq changed (1+ changed)))
          (if (> changed 0)
              ;; <
              (progn
                (org-mode-restart)
                (message "Updated %d tangle path(s) to python/%s.py. Press C-c C-c on #+PROPERTY lines to refresh."
                        changed filename))
            (message "No Python/Jupyter property lines found"))))))

  ;; Set export type for current subtree
  (defun my/org-set-subtree-export (lang export-type)
    "Set export type for LANG in current subtree. EXPORT-TYPE: results, code, both, or none."
    (interactive
     (list (completing-read "Language: " '("python" "jupyter-python" "latex") nil t)
           (completing-read "Export type: " '("results" "code" "both" "none") nil t)))
    (org-set-property (format "header-args:%s" lang)
                     (format ":exports %s" export-type))
    (message "Set %s exports to %s for current subtree" lang export-type))

  ;; Quick insertion with property awareness
  (defun my/org-insert-src-block (lang)
    "Insert a source block for LANG. Properties are applied automatically."
    (interactive
     (list (completing-read "Language: "
                           '("python" "jupyter-python" "latex" "emacs-lisp" "shell")
                           nil t)))
    (insert (format "#+begin_src %s\n\n#+end_src\n" lang))
    (forward-line -2)
    (message "Inserted %s block. Header args from #+PROPERTY will be applied automatically." lang))

  ;; Keybindings
  (ar/global-leader
    "o p" '(:ignore t :wk "properties")
    "o p s" '(my/org-insert-scientific-project-template :wk "Scientific template")
    "o p d" '(my/org-setup-project-structure :wk "Setup project dirs")
    "o p t" '(my/org-toggle-python-export :wk "Toggle Python export")
    "o p f" '(my/org-set-python-tangle-file :wk "Set tangle file")
    "o p e" '(my/org-set-subtree-export :wk "Set subtree export")
    "o i s" '(my/org-insert-src-block :wk "Insert source block")))

(use-package popwin
  :init
  (setq popwin:popup-window-height 0.35
        popwin:popup-window-width 0.35
        popwin:popup-window-position 'bottom)

  :config
  ;; Enable popwin mode
  (popwin-mode 1)

  ;; Set display-buffer to use popwin
  (setq display-buffer-function 'popwin:display-buffer)

  ;; Define popup rules
  (setq popwin:special-display-config
        '(;; === Help and Documentation ===
          ("*Help*" :height 0.35 :position bottom :noselect nil)
          ("*info*" :height 0.4 :position bottom :noselect nil)
          ("*Man*" :height 0.45 :position bottom :noselect nil)
          ("*woman*" :height 0.45 :position bottom :noselect nil)
          (help-mode :height 0.35 :position bottom :noselect nil)
          (helpful-mode :height 0.35 :position bottom :noselect nil)

          ;; === Compilation and Messages ===
          ("*compilation*" :height 0.3 :position bottom :noselect t)
          ("*Compile-Log*" :height 0.3 :position bottom :noselect t)
          ("*Messages*" :height 0.3 :position bottom :noselect t)
          ("*Warnings*" :height 0.3 :position bottom :noselect t)
          ("*Backtrace*" :height 0.4 :position bottom :noselect nil)
          (compilation-mode :height 0.3 :position bottom :noselect t)

          ;; === Shell and Terminal ===
          ("*shell*" :height 0.35 :position bottom :noselect nil)
          ("*eshell*" :height 0.35 :position bottom :noselect nil)
          ("*term*" :height 0.35 :position bottom :noselect nil)
          ("^\\*vterm" :regexp t :height 0.35 :position bottom :noselect nil)

          ;; === Org Mode ===
          ("*Org Src*" :width 0.7 :position right :noselect nil)
          ("*Org Agenda*" :height 0.4 :position bottom :noselect nil)
          ("*Agenda Commands*" :height 0.25 :position bottom :noselect nil)
          ("^CAPTURE-.*\\.org" :regexp t :height 0.4 :position bottom :noselect nil)
          ("*Org Select*" :height 0.3 :position bottom :noselect nil)
          (org-agenda-mode :height 0.4 :position bottom :noselect nil)

          ;; === Magit (minimal - Magit has its own display logic) ===
          ("^magit-revision:" :regexp t :height 0.6 :position bottom :noselect nil)
          ("^magit:" :regexp t :height 0.7 :position bottom :noselect nil)

          ;; === Debugger (dape) ===
          ("^\\*dape-repl" :regexp t :height 0.35 :position bottom :noselect t)
          ("^\\*dape-info" :regexp t :width 0.35 :position right :noselect t)

          ;; === Grep, Occur, Search ===
          ("*grep*" :height 0.4 :position bottom :noselect nil)
          ("*Occur*" :height 0.4 :position bottom :noselect nil)
          ("^\\*deadgrep" :regexp t :height 0.4 :position bottom :noselect nil)
          (occur-mode :height 0.4 :position bottom :noselect nil)

          ;; === Completion ===
          ("*Completions*" :height 0.3 :position bottom :noselect t)
          ("*Embark Actions*" :height 0.3 :position bottom :noselect nil)
          ("^\\*Embark Collect" :regexp t :height 0.5 :position bottom :noselect nil)
          (completion-list-mode :height 0.3 :position bottom :noselect t)

          ;; === Miscellaneous ===
          ("*Apropos*" :height 0.4 :position bottom :noselect nil)
          ("*Bookmark List*" :height 0.4 :position bottom :noselect nil)
          ("*Calculator*" :height 0.4 :position bottom :noselect nil)
          ("^\\*Calc" :regexp t :height 0.4 :position bottom :noselect nil)
          ("*Colors*" :height 0.3 :position bottom :noselect nil)
          ("*undo-tree*" :width 0.3 :position right :noselect nil)

          ;; === PDF Tools and Org Noter ===
          ("^\\*Outline" :regexp t :width 0.3 :position right :noselect t)

          ;; === Jupyter and EIN ===
          ("^\\*ein:notebooklist" :regexp t :height 0.5 :position bottom :noselect nil)
          ("^\\*ob-async" :regexp t :height 0.3 :position bottom :noselect t)

          ;; === Org-roam ===
          ("^\\*org-roam\\*" :regexp t :width 0.33 :position right :noselect t)

          ;; === Deft ===
          (deft-mode :height 0.5 :position bottom :noselect nil)

          ;; === Proced ===
          (proced-mode :height 0.5 :position bottom :noselect nil))))

(use-package popper
  :bind (("C-`" . popper-toggle)
         ("M-`" . popper-cycle)
         ("C-M-`" . popper-toggle-type))

  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "\\*Warnings\\*"
          "\\*Backtrace\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          "^\\*eshell.*\\*$" eshell-mode
          "^\\*shell.*\\*$" shell-mode
          "^\\*term.*\\*$" term-mode
          "^\\*vterm.*\\*$" vterm-mode
          help-mode
          helpful-mode
          compilation-mode
          "^\\*Completions\\*"
          "^\\*Occur\\*"
          "^\\*grep\\*"
          "^\\*deadgrep"
          "^\\*Flycheck errors\\*"
          "^\\*eldoc"
          "^\\*xref\\*"
          "^\\*org-roam\\*"
          "^\\*lsp-help\\*"
          "^\\*lsp-bridge"
          "^\\*dape-repl"
          "^\\*Embark"
          "^\\*Org Agenda\\*"))

  (setq popper-group-function #'popper-group-by-project)

  ;; Let popwin handle display rules, popper handles access
  (setq popper-display-control nil)

  :config
  (popper-mode 1)
  (popper-echo-mode 1))

(defun ar/popup-close-all ()
  "Close all popup windows."
  (interactive)
  ;; Close popwin windows
  (when (bound-and-true-p popwin-mode)
    (popwin:close-popup-window))
  ;; Also close popper popups
  (when (bound-and-true-p popper-mode)
    (dolist (buf popper-buried-popup-buffers)
      (when (get-buffer-window buf)
        (delete-window (get-buffer-window buf))))))

(defun ar/popup-raise ()
  "Raise (show) the last popup window."
  (interactive)
  (if (bound-and-true-p popwin-mode)
      (popwin:popup-last-buffer)
    (when (bound-and-true-p popper-mode)
      (popper-toggle))))

(defun ar/popup-toggle-messages ()
  "Toggle the *Messages* buffer as a popup."
  (interactive)
  (let ((buf (get-buffer "*Messages*")))
    (if buf
        (if (bound-and-true-p popwin-mode)
            (popwin:display-buffer buf)
          (display-buffer buf))
      (message "No *Messages* buffer"))))

(use-package org-pdftools
  :defer t
  :hook (org-load . org-pdftools-setup-link))

(use-package pdf-tools
  :defer t
  :magic ("%PDF" . pdf-view-mode)
  :hook (pdf-view-mode . pdf-view-midnight-minor-mode)
  :custom
  (pdf-view-midnight-colors '("#1e1e2e" . "#cdd6f4"))
  (pdf-view-continuous t)
  ;; Point to system-installed epdfinfo
  (pdf-info-epdfinfo-program "/usr/bin/epdfinfo")

  :config
  (custom-set-faces
   '(pdf-view-highlight-face ((t (:background "#f9e2af" :foreground "#1e1e2e"))))
   '(pdf-view-link-face ((t (:foreground "#89b4fa"))))
   '(pdf-view-active-link-face ((t (:foreground "#cba6f7")))))
  ;; Ensure Org mode integration is set up after Org itself is loaded.
  (with-eval-after-load 'org
    (add-to-list 'org-open-at-point-functions 'org-pdftools-open-link)
    (setq org-pdftools-link-prefix "pdf")))

(use-package gnuplot
  :defer t
  :mode (("\\.gp\\'" . gnuplot-mode)
         ("\\.gnuplot\\'" . gnuplot-mode))
  :custom
  ;; Tell Emacs where to find the gnuplot executable.
  ;; This should be in your PATH if installed via home-manager.
  (gnuplot-program "gnuplot")
  (gnuplot-default-term 'pdfcairo))

(ar/global-leader
  ;; --- Window Management ---
  "w" '(:ignore t :wk "windows")
  "w /" '(evil-window-vsplit :wk "Split Vertically")
  "w -" '(evil-window-split :wk "Split Horizontally")
  "w d" '(delete-window :wk "Delete Window")
  "w c" '(delete-other-windows :wk "Delete Other Windows")
  "w w" '(other-window :wk "Other Window")
  "w h" '(ar/evil-window-move-left :wk "Window Left")
  "w j" '(ar/evil-window-move-down :wk "Window Down")
  "w k" '(ar/evil-window-move-up :wk "Window Up")
  "w l" '(ar/evil-window-move-right :wk "Window Right")
  "w s" '(evil-window-swap :wk "Swap Windows")
  "w n" '(next-buffer :wk "Next Buffer")
  "w p" '(previous-buffer :wk "Previous Buffer")

  ;; --- Quality-of-Life & Navigation ---
  "/" '(:ignore t :wk "search")
  "/ /" '(consult-line :wk "Search Line in Buffer")
  "/ p" '(consult-ripgrep :wk "Search Project (Ripgrep)")

  "." '(:ignore t :wk "jump")
  ". l" '(consult-goto-line :wk "Jump to Line")
  ". m" '(consult-mark :wk "Jump to Mark")
  ". j" '(consult-jump-list :wk "Jump List (Evil)")

  "t" '(:ignore t :wk "toggle")
  "t n" '(display-line-numbers-mode :wk "Toggle Line Numbers")
  "t t" '(toggle-truncate-lines :wk "Toggle Truncate Lines")
  "t f" '(display-fill-column-indicator-mode :wk "Toggle Fill Column Indicator"))
