;; This function provides a more robust way to quit the minibuffer.
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

;; This custom escape command also clears search highlights for evil-mode.
(defun my/escape-key ()
  "Clear evil search highlight and then quit."
  (interactive)
  (when (fboundp 'evil-ex-nohighlight)
    (evil-ex-nohighlight))
  (keyboard-quit))

;; --- Apply the Custom Escape Key Behavior ---
;; Make ESC quit everything in normal states
(general-define-key
 :keymaps '(normal visual global)
 [escape] #'my/escape-key)

;; Make ESC quit the minibuffer with a single press
(general-define-key
 :keymaps '(minibuffer-local-map
            minibuffer-local-ns-map
            minibuffer-local-completion-map
            minibuffer-local-must-match-map
            minibuffer-local-isearch-map)
 [escape] 'minibuffer-keyboard-quit)
