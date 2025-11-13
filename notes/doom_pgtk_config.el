;;; config.el -*- lexical-binding: t; -*-

;; Disable PGTK input method context for all frames
(when (fboundp 'pgtk-use-im-context)
  (add-hook! 'after-make-frame-functions
    (defun +pgtk/disable-im-context (frame)
      "Disable PGTK input method context for FRAME."
      (with-selected-frame frame
        (pgtk-use-im-context nil)))))