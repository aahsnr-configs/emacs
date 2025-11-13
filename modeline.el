(defvar catppuccin-mocha-colors
  '((base       . "#1e1e2e")
    (mantle     . "#181825")
    (crust      . "#11111b")
    (surface0   . "#313244")
    (surface1   . "#45475a")
    (surface2   . "#585b70")
    (overlay0   . "#6c7086")
    (overlay1   . "#7f849c")
    (text       . "#cdd6f4")
    (subtext0   . "#a6adc8")
    (subtext1   . "#bac2de")
    (blue       . "#89b4fa")
    (lavender   . "#b4befe")
    (sapphire   . "#74c7ec")
    (sky        . "#89dceb")
    (teal       . "#94e2d5")
    (green      . "#a6e3a1")
    (yellow     . "#f9e2af")
    (peach      . "#fab387")
    (maroon     . "#eba0ac")
    (red        . "#f38ba8")
    (mauve      . "#cba6f7")
    (pink       . "#f5c2e7")
    (flamingo   . "#f2cdcd")
    (rosewater  . "#f5e0dc"))
  "Catppuccin Mocha color palette.")

(defun catppuccin-get (color)
  "Get COLOR from Catppuccin Mocha palette."
  (alist-get color catppuccin-mocha-colors))

;; --- Package Integration Setup ---

;; Minions for minor mode management
(use-package minions
  :ensure nil
  :hook (after-init . minions-mode)
  :custom
  (minions-mode-line-lighter "")
  (minions-prominent-modes '(envrc-mode
                             flycheck-mode
                             flymake-mode
                             lsp-bridge-mode
                             evil-snipe-local-mode)))

;; Which-function for function name display
(use-package which-func
  :ensure nil
  :hook (prog-mode . which-function-mode)
  :custom
  (which-func-unknown "")
  (which-func-format '(:propertize which-func-current face which-func)))

;; Anzu configuration
(with-eval-after-load 'anzu
  (setq anzu-cons-mode-line-p nil))

;; Display time
(use-package time
  :ensure nil
  :custom
  (display-time-24hr-format t)
  (display-time-default-load-average nil)
  (display-time-format " %H:%M")
  (display-time-interval 60))

;; Battery status
(use-package battery
  :ensure nil
  :custom
  (battery-mode-line-format " %b%p%%")
  (battery-update-interval 60))

;; --- Custom Modeline Faces ---
(defface custom-modeline-evil-normal
  `((t (:foreground ,(catppuccin-get 'base) 
        :background ,(catppuccin-get 'blue) 
        :weight bold
        :box (:line-width 4 :color ,(catppuccin-get 'blue)))))
  "Face for evil normal state.")

(defface custom-modeline-evil-insert
  `((t (:foreground ,(catppuccin-get 'base) 
        :background ,(catppuccin-get 'green) 
        :weight bold
        :box (:line-width 4 :color ,(catppuccin-get 'green)))))
  "Face for evil insert state.")

(defface custom-modeline-evil-visual
  `((t (:foreground ,(catppuccin-get 'base) 
        :background ,(catppuccin-get 'mauve) 
        :weight bold
        :box (:line-width 4 :color ,(catppuccin-get 'mauve)))))
  "Face for evil visual state.")

(defface custom-modeline-evil-replace
  `((t (:foreground ,(catppuccin-get 'base) 
        :background ,(catppuccin-get 'red) 
        :weight bold
        :box (:line-width 4 :color ,(catppuccin-get 'red)))))
  "Face for evil replace state.")

(defface custom-modeline-evil-emacs
  `((t (:foreground ,(catppuccin-get 'base) 
        :background ,(catppuccin-get 'yellow) 
        :weight bold
        :box (:line-width 4 :color ,(catppuccin-get 'yellow)))))
  "Face for evil emacs state.")

(defface custom-modeline-evil-operator
  `((t (:foreground ,(catppuccin-get 'base) 
        :background ,(catppuccin-get 'peach) 
        :weight bold
        :box (:line-width 4 :color ,(catppuccin-get 'peach)))))
  "Face for evil operator state.")

(defface custom-modeline-evil-motion
  `((t (:foreground ,(catppuccin-get 'base) 
        :background ,(catppuccin-get 'pink) 
        :weight bold
        :box (:line-width 4 :color ,(catppuccin-get 'pink)))))
  "Face for evil motion state.")

(defface custom-modeline-buffer-name
  `((t (:foreground ,(catppuccin-get 'text) 
        :weight bold)))
  "Face for buffer name.")

(defface custom-modeline-buffer-modified
  `((t (:foreground ,(catppuccin-get 'red) 
        :weight bold)))
  "Face for modified buffer indicator.")

(defface custom-modeline-buffer-read-only
  `((t (:foreground ,(catppuccin-get 'yellow) 
        :weight bold)))
  "Face for read-only buffer indicator.")

(defface custom-modeline-remote
  `((t (:foreground ,(catppuccin-get 'pink) 
        :weight bold)))
  "Face for remote file indicator.")

(defface custom-modeline-project
  `((t (:foreground ,(catppuccin-get 'sapphire) 
        :weight bold)))
  "Face for project name.")

(defface custom-modeline-git-branch
  `((t (:foreground ,(catppuccin-get 'mauve) 
        :weight normal)))
  "Face for git branch.")

(defface custom-modeline-git-added
  `((t (:foreground ,(catppuccin-get 'green))))
  "Face for git additions.")

(defface custom-modeline-git-modified
  `((t (:foreground ,(catppuccin-get 'yellow))))
  "Face for git modifications.")

(defface custom-modeline-git-deleted
  `((t (:foreground ,(catppuccin-get 'red))))
  "Face for git deletions.")

(defface custom-modeline-major-mode
  `((t (:foreground ,(catppuccin-get 'blue) 
        :weight bold)))
  "Face for major mode.")

(defface custom-modeline-function
  `((t (:foreground ,(catppuccin-get 'flamingo) 
        :slant italic)))
  "Face for current function.")

(defface custom-modeline-position
  `((t (:foreground ,(catppuccin-get 'peach))))
  "Face for position info.")

(defface custom-modeline-selection
  `((t (:foreground ,(catppuccin-get 'mauve)
        :weight bold)))
  "Face for selection info.")

(defface custom-modeline-encoding
  `((t (:foreground ,(catppuccin-get 'teal))))
  "Face for encoding info.")

(defface custom-modeline-diagnostics-error
  `((t (:foreground ,(catppuccin-get 'red) 
        :weight bold)))
  "Face for errors.")

(defface custom-modeline-diagnostics-warning
  `((t (:foreground ,(catppuccin-get 'yellow) 
        :weight bold)))
  "Face for warnings.")

(defface custom-modeline-diagnostics-info
  `((t (:foreground ,(catppuccin-get 'sky))))
  "Face for info.")

(defface custom-modeline-lsp
  `((t (:foreground ,(catppuccin-get 'green))))
  "Face for LSP status.")

(defface custom-modeline-anzu
  `((t (:foreground ,(catppuccin-get 'lavender)
        :weight bold)))
  "Face for anzu search info.")

(defface custom-modeline-macro
  `((t (:foreground ,(catppuccin-get 'red)
        :weight bold)))
  "Face for macro recording.")

(defface custom-modeline-narrow
  `((t (:foreground ,(catppuccin-get 'yellow)
        :weight bold)))
  "Face for narrowing indicator.")

(defface custom-modeline-separator
  `((t (:foreground ,(catppuccin-get 'surface1))))
  "Face for separators.")

(defface custom-modeline-time
  `((t (:foreground ,(catppuccin-get 'overlay1))))
  "Face for time display.")

(defface custom-modeline-battery
  `((t (:foreground ,(catppuccin-get 'green))))
  "Face for battery.")

(defface custom-modeline-battery-warning
  `((t (:foreground ,(catppuccin-get 'yellow)
        :weight bold)))
  "Face for low battery.")

(defface custom-modeline-battery-critical
  `((t (:foreground ,(catppuccin-get 'red)
        :weight bold)))
  "Face for critical battery.")

;; --- Cache Variables ---
(defvar custom-modeline--git-branch-cache nil)
(defvar custom-modeline--git-branch-cache-time 0)
(defvar custom-modeline--project-cache nil)
(defvar custom-modeline--project-cache-buffer nil)

;; --- Helper Functions ---
(defun custom-modeline--icon (icon-func icon-name &optional fallback)
  "Safely get icon, with fallback for terminal mode."
  (if (and (display-graphic-p) (fboundp icon-func))
      (funcall icon-func icon-name)
    (or fallback "")))

(defun custom-modeline--evil-state ()
  "Return formatted evil state indicator with icon."
  (when (bound-and-true-p evil-mode)
    (let* ((state evil-state)
           (icon (custom-modeline--icon 
                  'nerd-icons-codicon
                  (pcase state
                    ('normal "nf-cod-circle_filled")
                    ('insert "nf-cod-edit")
                    ('visual "nf-cod-selection")
                    ('replace "nf-fa-exchange")
                    ('operator "nf-cod-symbol_operator")
                    ('motion "nf-cod-move")
                    ('emacs "nf-custom-emacs")
                    (_ "nf-cod-circle_outline"))
                  "●"))
           (tag (pcase state
                  ('normal "NORMAL")
                  ('insert "INSERT")
                  ('visual "VISUAL")
                  ('replace "REPLACE")
                  ('operator "OPERATOR")
                  ('motion "MOTION")
                  ('emacs "EMACS")
                  (_ (upcase (symbol-name state)))))
           (face (pcase state
                   ('normal 'custom-modeline-evil-normal)
                   ('insert 'custom-modeline-evil-insert)
                   ('visual 'custom-modeline-evil-visual)
                   ('replace 'custom-modeline-evil-replace)
                   ('operator 'custom-modeline-evil-operator)
                   ('motion 'custom-modeline-evil-motion)
                   ('emacs 'custom-modeline-evil-emacs)
                   (_ 'mode-line))))
      (propertize (format " %s %s " icon tag) 'face face))))

(defun custom-modeline--selection-info ()
  "Return selection info in visual state."
  (when (and (bound-and-true-p evil-mode)
            (eq evil-state 'visual)
            (region-active-p))
    (let* ((beg (region-beginning))
           (end (region-end))
           (lines (count-lines beg end))
           (chars (- end beg)))
      (propertize 
       (format " %s %dL %dC" 
               (custom-modeline--icon 'nerd-icons-codicon "nf-cod-selection" "Ⓥ")
               lines 
               chars)
       'face 'custom-modeline-selection))))

(defun custom-modeline--macro-recording ()
  "Return macro recording indicator."
  (when (and defining-kbd-macro (display-graphic-p))
    (propertize 
     (format " %s REC" 
             (custom-modeline--icon 'nerd-icons-mdicon "nf-md-record_circle" "●"))
     'face 'custom-modeline-macro)))

(defun custom-modeline--narrow-indicator ()
  "Return narrowing indicator."
  (when (buffer-narrowed-p)
    (propertize 
     (format " %s NARROW" 
             (custom-modeline--icon 'nerd-icons-mdicon "nf-md-arrow_collapse_horizontal" "↔"))
     'face 'custom-modeline-narrow)))

(defun custom-modeline--recursive-edit ()
  "Return recursive edit depth indicator."
  (when (> (recursion-depth) 0)
    (propertize (format " [%d]" (recursion-depth))
               'face 'custom-modeline-narrow)))

(defun custom-modeline--remote-indicator ()
  "Return remote file indicator."
  (when (file-remote-p default-directory)
    (propertize 
     (format " %s" 
             (custom-modeline--icon 'nerd-icons-mdicon "nf-md-cloud" "☁"))
     'face 'custom-modeline-remote
     'help-echo (format "Remote: %s" default-directory))))

(defun custom-modeline--project-name ()
  "Return project name with caching."
  (when (fboundp 'project-current)
    (when (or (not (eq custom-modeline--project-cache-buffer (current-buffer)))
             (null custom-modeline--project-cache))
      (setq custom-modeline--project-cache-buffer (current-buffer))
      (setq custom-modeline--project-cache
            (when-let* ((project (project-current))
                       (name (file-name-nondirectory 
                             (directory-file-name 
                              (project-root project)))))
              (let ((icon (custom-modeline--icon 'nerd-icons-octicon "nf-oct-repo" "⚑")))
                (propertize (format " %s %s" icon name)
                           'face 'custom-modeline-project
                           'help-echo (project-root project))))))
    custom-modeline--project-cache))

(defun custom-modeline--buffer-info ()
  "Return buffer name and status with appropriate icon."
  (let* ((icon (if (display-graphic-p)
                  (condition-case nil
                      (concat (nerd-icons-icon-for-buffer) " ")
                    (error ""))
                ""))
         (name (propertize 
                (if (> (length (buffer-name)) 40)
                    (concat (substring (buffer-name) 0 37) "...")
                  (buffer-name))
                'face 'custom-modeline-buffer-name))
         (modified (if (and (buffer-modified-p) (not buffer-read-only))
                      (propertize 
                       (concat " " (custom-modeline--icon 'nerd-icons-faicon "nf-fa-circle" "●"))
                       'face 'custom-modeline-buffer-modified)
                    ""))
         (readonly (if buffer-read-only
                      (propertize 
                       (concat " " (custom-modeline--icon 'nerd-icons-faicon "nf-fa-lock" "🔒"))
                       'face 'custom-modeline-buffer-read-only)
                    "")))
    (concat " " icon name modified readonly " ")))

(defun custom-modeline--file-size ()
  "Return file size if buffer is visiting a file."
  (when buffer-file-name
    (let* ((size (file-attribute-size (file-attributes buffer-file-name)))
           (size-str (cond
                      ((> size (* 1024 1024)) (format "%.1fM" (/ size (* 1024.0 1024.0))))
                      ((> size 1024) (format "%.1fK" (/ size 1024.0)))
                      (t (format "%dB" size)))))
      (propertize size-str
                 'face 'custom-modeline-encoding))))

(defun custom-modeline--git-branch ()
  "Return git branch with caching (5 second cache)."
  (when (and (fboundp 'magit-get-current-branch)
            (> (- (float-time) custom-modeline--git-branch-cache-time) 5))
    (setq custom-modeline--git-branch-cache
          (when-let ((branch (magit-get-current-branch)))
            (let ((icon (custom-modeline--icon 'nerd-icons-devicon "nf-dev-git_branch" "")))
              (propertize (format "%s %s" icon branch)
                         'face 'custom-modeline-git-branch))))
    (setq custom-modeline--git-branch-cache-time (float-time)))
  custom-modeline--git-branch-cache)

(defun custom-modeline--git-diff ()
  "Return git diff statistics using git-gutter."
  (when (and (bound-and-true-p git-gutter-mode)
            (display-graphic-p)
            (fboundp 'git-gutter:statistic))
    (let* ((stats (git-gutter:statistic))
           (added (car stats))
           (modified (cadr stats))
           (deleted (caddr stats))
           (result ""))
      (when (and added (> added 0))
        (setq result 
              (concat result
                     (propertize (format " +%d" added)
                               'face 'custom-modeline-git-added))))
      (when (and modified (> modified 0))
        (setq result 
              (concat result
                     (propertize (format " ~%d" modified)
                               'face 'custom-modeline-git-modified))))
      (when (and deleted (> deleted 0))
        (setq result 
              (concat result
                     (propertize (format " -%d" deleted)
                               'face 'custom-modeline-git-deleted))))
      (when (not (string-empty-p result))
        result))))

(defun custom-modeline--lsp-status ()
  "Return LSP Bridge status."
  (when (and (bound-and-true-p lsp-bridge-mode)
            (display-graphic-p))
    (propertize 
     (format " %s LSP" 
             (custom-modeline--icon 'nerd-icons-mdicon "nf-md-code_braces" "{}"))
     'face 'custom-modeline-lsp)))

(defun custom-modeline--diagnostics ()
  "Return diagnostics from flymake."
  (when (and (bound-and-true-p flymake-mode)
            (display-graphic-p))
    (let* ((known (hash-table-keys flymake--state))
           (running (flymake-running-backends))
           (disabled (flymake-disabled-backends))
           (reported (flymake-reporting-backends))
           (all-disabled (and (null running) (null reported)))
           (some-waiting (cl-set-difference running reported))
           (warning-count 0)
           (error-count 0)
           (note-count 0)
           (result ""))
      
      (when (and (not all-disabled) reported)
        (cl-loop
         with warning-level = (warning-numeric-level :warning)
         with error-level = (warning-numeric-level :error)
         for diag in (flymake-diagnostics)
         do (let ((severity (flymake-diagnostic-type diag)))
              (cond ((eq severity :error) (cl-incf error-count))
                    ((eq severity :warning) (cl-incf warning-count))
                    (t (cl-incf note-count)))))
        
        (when (> error-count 0)
          (setq result 
                (concat result
                       (propertize 
                        (format " %s %d" 
                               (custom-modeline--icon 'nerd-icons-codicon "nf-cod-error" "E")
                               error-count)
                        'face 'custom-modeline-diagnostics-error))))
        (when (> warning-count 0)
          (setq result 
                (concat result
                       (propertize 
                        (format " %s %d" 
                               (custom-modeline--icon 'nerd-icons-codicon "nf-cod-warning" "W")
                               warning-count)
                        'face 'custom-modeline-diagnostics-warning))))
        (when (> note-count 0)
          (setq result 
                (concat result
                       (propertize 
                        (format " %s %d" 
                               (custom-modeline--icon 'nerd-icons-codicon "nf-cod-info" "I")
                               note-count)
                        'face 'custom-modeline-diagnostics-info)))))
      result)))

(defun custom-modeline--anzu ()
  "Return anzu search information."
  (when (and (bound-and-true-p anzu--state)
            (display-graphic-p))
    (let ((here anzu--current-position)
          (total anzu--total-matched))
      (when (and here total)
        (propertize (format " %s %d/%d" 
                           (custom-modeline--icon 'nerd-icons-codicon "nf-cod-search" "?")
                           here 
                           total)
                   'face 'custom-modeline-anzu)))))

(defun custom-modeline--org-info ()
  "Return org-mode specific information."
  (when (and (derived-mode-p 'org-mode) (display-graphic-p))
    (let ((result ""))
      (when (and (fboundp 'org-clocking-p) (org-clocking-p))
        (setq result
              (concat result
                     (propertize 
                      (format " %s %s"
                             (custom-modeline--icon 'nerd-icons-mdicon "nf-md-clock_outline" "⏱")
                             (org-duration-from-minutes
                              (floor (org-clock-get-clocked-time))))
                      'face 'custom-modeline-diagnostics-warning))))
      result)))

(defun custom-modeline--major-mode ()
  "Return major mode with icon."
  (let* ((icon (if (display-graphic-p)
                  (condition-case nil
                      (concat (nerd-icons-icon-for-mode major-mode) " ")
                    (error ""))
                ""))
         (mode-name (propertize (format-mode-line mode-name)
                               'face 'custom-modeline-major-mode)))
    (concat icon mode-name)))

(defun custom-modeline--which-function ()
  "Return current function name."
  (when (and (bound-and-true-p which-function-mode)
            which-func-current
            (not (string= which-func-current which-func-unknown))
            (not (string-empty-p which-func-current)))
    (let ((fn-name (if (> (length which-func-current) 30)
                      (concat (substring which-func-current 0 27) "...")
                    which-func-current)))
      (propertize (format " %s %s" 
                         (custom-modeline--icon 'nerd-icons-codicon "nf-cod-symbol_method" "ƒ")
                         fn-name)
                 'face 'custom-modeline-function
                 'help-echo which-func-current))))

(defun custom-modeline--position ()
  "Return cursor position information with icons."
  (let ((line (format-mode-line "%l"))
        (col (format-mode-line "%c"))
        (percent (format-mode-line "%p"))
        (line-icon (custom-modeline--icon 'nerd-icons-codicon "nf-cod-list_selection" "↕"))
        (col-icon (custom-modeline--icon 'nerd-icons-codicon "nf-cod-arrow_right" "→")))
    (propertize (format "%s %s %s %s  %s%%" 
                       line-icon line col-icon col percent)
               'face 'custom-modeline-position)))

(defun custom-modeline--encoding ()
  "Return file encoding and line ending information."
  (when buffer-file-coding-system
    (let* ((sys (coding-system-plist buffer-file-coding-system))
           (eol (pcase (plist-get sys :eol-type)
                  (0 "LF")
                  (1 "CRLF")
                  (2 "CR")
                  (_ "")))
           (charset (upcase (symbol-name (plist-get sys :charset))))
           (icon (custom-modeline--icon 'nerd-icons-codicon "nf-cod-file_code" "")))
      (when (not (string-empty-p eol))
        (propertize (format "%s %s %s" icon charset eol)
                   'face 'custom-modeline-encoding)))))

(defun custom-modeline--time ()
  "Return current time if display-time-mode is active."
  (when (bound-and-true-p display-time-mode)
    (let ((icon (custom-modeline--icon 'nerd-icons-mdicon "nf-md-clock_outline" "🕐"))
          (time (format-time-string display-time-format)))
      (propertize (format "%s%s" icon time)
                 'face 'custom-modeline-time))))

(defun custom-modeline--battery ()
  "Return battery status if battery-mode is active."
  (when (bound-and-true-p display-battery-mode)
    (let* ((status (and (fboundp battery-status-function)
                       (funcall battery-status-function)))
           (percentage (and status (string-to-number 
                                   (or (battery-format "%p" status) "0"))))
           (charging (and status (string= (battery-format "%B" status) "AC")))
           (icon (cond
                  ((null status) "")
                  (charging (custom-modeline--icon 'nerd-icons-mdicon "nf-md-battery_charging" "⚡"))
                  ((>= percentage 90) (custom-modeline--icon 'nerd-icons-mdicon "nf-md-battery" "🔋"))
                  ((>= percentage 70) (custom-modeline--icon 'nerd-icons-mdicon "nf-md-battery_80" "🔋"))
                  ((>= percentage 50) (custom-modeline--icon 'nerd-icons-mdicon "nf-md-battery_50" "🔋"))
                  ((>= percentage 30) (custom-modeline--icon 'nerd-icons-mdicon "nf-md-battery_30" "🔋"))
                  ((>= percentage 10) (custom-modeline--icon 'nerd-icons-mdicon "nf-md-battery_10" "🔋"))
                  (t (custom-modeline--icon 'nerd-icons-mdicon "nf-md-battery_alert" "!"))))
           (face (cond
                  ((null status) 'mode-line)
                  (charging 'custom-modeline-battery)
                  ((< percentage 20) 'custom-modeline-battery-critical)
                  ((< percentage 40) 'custom-modeline-battery-warning)
                  (t 'custom-modeline-battery))))
      (when status
        (propertize (format "%s %d%%" icon percentage)
                   'face face
                   'help-echo (battery-format "%B %p%%" status))))))

(defun custom-modeline--separator ()
  "Return a subtle separator."
  (propertize " │ " 'face 'custom-modeline-separator))

(defun custom-modeline--space ()
  "Return a simple space."
  " ")

(defun custom-modeline--right-side-width ()
  "Calculate the width needed for right-aligned elements."
  (+ 4
     (string-width (format-mode-line (custom-modeline--major-mode)))
     (if minions-mode 4 0)
     (string-width (format-mode-line (or (custom-modeline--encoding) "")))
     (if (and (custom-modeline--file-size) buffer-file-name) 6 0)
     (if (bound-and-true-p display-time-mode) 8 0)
     (if (and (bound-and-true-p display-battery-mode) (custom-modeline--battery)) 10 0)
     (string-width (format-mode-line (custom-modeline--position)))
     20))

;; --- Modeline Format ---
(setq-default mode-line-format
              '((:eval (custom-modeline--evil-state))
                (:eval (custom-modeline--separator))
                
                ;; Left side - buffer and project info
                (:eval (when (custom-modeline--project-name)
                         (concat (custom-modeline--project-name)
                                (custom-modeline--separator))))
                (:eval (custom-modeline--buffer-info))
                (:eval (custom-modeline--remote-indicator))
                
                ;; Git information
                (:eval (when (custom-modeline--git-branch)
                         (concat (custom-modeline--separator)
                                (custom-modeline--git-branch))))
                (:eval (when (custom-modeline--git-diff)
                         (custom-modeline--git-diff)))
                
                ;; Status indicators
                (:eval (when (custom-modeline--lsp-status)
                         (custom-modeline--lsp-status)))
                (:eval (when (custom-modeline--diagnostics)
                         (custom-modeline--diagnostics)))
                (:eval (when (custom-modeline--anzu)
                         (custom-modeline--anzu)))
                (:eval (when (custom-modeline--macro-recording)
                         (concat (custom-modeline--separator)
                                (custom-modeline--macro-recording))))
                (:eval (when (custom-modeline--narrow-indicator)
                         (concat (custom-modeline--separator)
                                (custom-modeline--narrow-indicator))))
                (:eval (when (custom-modeline--selection-info)
                         (concat (custom-modeline--separator)
                                (custom-modeline--selection-info))))
                (:eval (when (custom-modeline--recursive-edit)
                         (custom-modeline--recursive-edit)))
                
                ;; Org-mode specific
                (:eval (when (custom-modeline--org-info)
                         (concat (custom-modeline--separator)
                                (custom-modeline--org-info))))
                
                ;; Current function
                (:eval (when (custom-modeline--which-function)
                         (concat (custom-modeline--separator)
                                (custom-modeline--which-function))))
                
                ;; Right-align the rest
                (:eval (propertize 
                        " " 
                        'display 
                        `((space :align-to (- right-fringe 
                                             ,(custom-modeline--right-side-width))))))
                
                ;; Right side - mode and system info
                (:eval (custom-modeline--major-mode))
                (:eval (custom-modeline--space))
                (:eval minions-mode-line-modes)
                (:eval (when (custom-modeline--encoding)
                         (concat (custom-modeline--separator)
                                (custom-modeline--encoding))))
                (:eval (when (and buffer-file-name (custom-modeline--file-size))
                         (concat (custom-modeline--separator)
                                (custom-modeline--file-size))))
                (:eval (when (custom-modeline--time)
                         (concat (custom-modeline--separator)
                                (custom-modeline--time))))
                (:eval (when (custom-modeline--battery)
                         (concat (custom-modeline--separator)
                                (custom-modeline--battery))))
                (:eval (custom-modeline--separator))
                (:eval (custom-modeline--position))))

;; --- Configuration ---
(setq mode-line-compact nil)
(setq mode-line-position-column-line-format '(" %l:%c"))

;; --- Update hooks ---
(defun custom-modeline--update ()
  "Force modeline update."
  (force-mode-line-update t))

(add-hook 'find-file-hook #'custom-modeline--update)
(add-hook 'after-save-hook #'custom-modeline--update)
(add-hook 'after-revert-hook #'custom-modeline--update)
(add-hook 'evil-visual-state-entry-hook #'custom-modeline--update)
(add-hook 'evil-visual-state-exit-hook #'custom-modeline--update)

;; Delayed updates for git info
(run-with-idle-timer 5 t #'custom-modeline--update)

;; Enable time and battery display (optional)
(display-time-mode 1)

;; Only enable battery if available
(when (and (fboundp 'battery-status-function)
          battery-status-function
          (ignore-errors 
            (not (string-match-p "N/A\\|unknown"
                                (battery-format "%B" 
                                              (funcall battery-status-function))))))
  (display-battery-mode 1))

;; Enable flymake for diagnostics
(add-hook 'prog-mode-hook #'flymake-mode)

;; Ensure proper updates from other modes
(with-eval-after-load 'git-gutter
  (add-hook 'git-gutter:update-hooks #'custom-modeline--update))

(with-eval-after-load 'magit
  (add-hook 'magit-post-refresh-hook 
            (lambda () 
              (setq custom-modeline--git-branch-cache-time 0)
              (custom-modeline--update))))
