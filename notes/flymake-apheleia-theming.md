Of course. To visually integrate `flymake` and `apheleia` with your `doom-tokyo-night` theme, we need to customize the faces they use to match the theme's color palette.

For `flymake`, this means coloring the underlines for errors, warnings, and notes. For `apheleia`, which is a background process, we can provide a themed success message to make it feel more integrated.

### **Required Changes**

Here are the specific changes for your `config.txt` file.

#### 1. Theming Flymake Diagnostics

In the **Syntax Checking** section, replace your existing `(use-package flymake ...)` block with the following. This adds a `:custom-face` section that defines wavy underline colors pulled directly from the `doom-tokyo-night` palette.

```emacs-lisp
;; Use the built-in flymake for syntax checking.
(use-package flymake
  :ensure nil
  :hook (prog-mode . flymake-mode)
  :custom
  (flymake-check-syntax-automatically '(save mode-enabled idle-change))
  (flymake-idle-change-delay 0.4)
  ;; Define theme-aware faces for diagnostics to match doom-tokyo-night.
  :custom-face
  (flymake-error   ((t (:underline (:style wave :color "#f7768e") :inherit nil)))) ; Tokyonight Red
  (flymake-warning ((t (:underline (:style wave :color "#e0af68") :inherit nil)))) ; Tokyonight Yellow
  (flymake-note    ((t (:underline (:style wave :color "#73daca") :inherit nil)))) ; Tokyonight Cyan
  )
```

#### 2. Theming Apheleia User Feedback

In the **Formatting** section, add the new helper function *before* your `(use-package apheleia ...)` block. Then, modify the `apheleia` block to use this function in a hook.

```emacs-lisp
;; Themed success message function for Apheleia
(defun ar/apheleia-format-message (formatter)
  "Display a themed success message in the echo area after formatting."
  (message (propertize (format "Apheleia formatted with %s" formatter)
                       'face '(:foreground "#9ece6a" :weight bold)))) ; Tokyonight Green for success

(use-package apheleia
  :defer t
  :init (apheleia-global-mode +1)
  :config
  ;; Add a themed hook to provide clear, theme-consistent feedback on success.
  (add-hook 'apheleia-post-format-hook #'ar/apheleia-format-message))
```

With these changes, syntax errors will now be highlighted with colors that match your theme, and you will get a pleasant, green confirmation message in the echo area whenever `apheleia` successfully formats your code, making both tools feel like a natural part of your themed environment.
