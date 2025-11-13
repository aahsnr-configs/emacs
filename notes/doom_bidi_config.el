;;; config.el -*- lexical-binding: t; -*-

;; Global setting (affects all buffers)
(setq-default bidi-display-reordering nil)


;; Buffer-local optimizations
(defun +bidi/optimize-for-ltr ()
  "Apply bidi optimizations for current buffer."
  (setq-local bidi-paragraph-direction 'left-to-right
              bidi-inhibit-bpa t))


;; Apply to appropriate modes using Doom's add-hook! macro
(add-hook! '(org-mode-hook text-mode-hook prog-mode-hook)
  #'+bidi/optimize-for-ltr)