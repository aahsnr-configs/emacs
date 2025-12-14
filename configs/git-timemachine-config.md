```emacs-lisp
** Git Timemachine
#+begin_src emacs-lisp
(use-package git-timemachine
  :defer t
  :commands (git-timemachine git-timemachine-toggle)
  :custom
  ;; Abbreviate commit hashes to 12 characters
  (git-timemachine-abbreviation-length 12)
  
  ;; Show commit details in minibuffer by default
  (git-timemachine-show-minibuffer-details t)
  
  ;; Control what details to show: 'commit for hash, 'subject for message
  (git-timemachine-minibuffer-detail 'subject)
  
  :custom-face
  ;; Tokyo Night theme colors for git-timemachine faces
  (git-timemachine-commit ((t (:foreground "#7aa2f7" :weight bold))))
  (git-timemachine-minibuffer-author-face ((t (:foreground "#bb9af7"))))
  (git-timemachine-minibuffer-detail-face ((t (:foreground "#9ece6a"))))
  
  :config
  ;; Ensure lsp-mode doesn't interfere with git-timemachine
  (with-eval-after-load 'lsp-mode
    (add-to-list 'lsp-disabled-clients 'git-timemachine-mode))
  
  ;; Helper function to format revision info
  (defun ar/git-timemachine-show-selected-revision ()
    "Show details of the current revision in a popup."
    (interactive)
    (when (bound-and-true-p git-timemachine-revision)
      (let* ((commit (car git-timemachine-revision))
             (date-relative (nth 1 git-timemachine-revision))
             (date-full (nth 2 git-timemachine-revision))
             (subject (nth 5 git-timemachine-revision))
             (author (nth 6 git-timemachine-revision)))
        (message "Commit: %s\nAuthor: %s\nDate: %s (%s)\nSubject: %s"
                 (propertize commit 'face 'git-timemachine-commit)
                 (propertize author 'face 'font-lock-variable-name-face)
                 date-full
                 date-relative
                 (propertize subject 'face 'font-lock-comment-face)))))
  
  ;; Keybindings in git-timemachine-mode
  (with-eval-after-load 'git-timemachine
    (define-key git-timemachine-mode-map (kbd "i") #'ar/git-timemachine-show-selected-revision)))
#+end_src
```
