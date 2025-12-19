```emacs-lisp
** Version Control Hydra
#+begin_src emacs-lisp
(with-eval-after-load 'pretty-hydra
  (pretty-hydra-define hydra-version-control
    (:title (pretty-hydra-title "Version Control" 'octicon "nf-oct-git_branch" :face 'nerd-icons-green)
     :color blue
     :quit-key ("q" "C-g"))
    
    ("Magit"
     (("s" magit-status "status")
      ("c" magit-commit "commit")
      ("C" magit-commit-amend "commit amend")
      ("p" magit-push-current-to-pushremote "push")
      ("P" magit-pull-from-upstream "pull")
      ("f" magit-fetch "fetch")
      ("b" magit-branch "branch")
      ("m" magit-merge "merge")
      ("r" magit-rebase "rebase")
      ("l" magit-log-buffer-file "log file")
      ("L" magit-log-current "log current")
      ("d" magit-diff-unstaged "diff"))
     
     "Git Gutter"
     (("n" git-gutter:next-hunk "next hunk")
      ("N" git-gutter:previous-hunk "previous hunk")
      ("S" git-gutter:stage-hunk "stage hunk")
      ("R" git-gutter:revert-hunk "revert hunk")
      ("h" git-gutter:popup-hunk "show hunk")
      ("t" git-gutter-mode "toggle gutter" :toggle t))
     
     "Timemachine"
     (("T" git-timemachine-toggle "toggle timemachine")
      ("j" git-timemachine-show-previous-revision "previous revision")
      ("k" git-timemachine-show-next-revision "next revision")
      ("g" git-timemachine-show-nth-revision "goto revision")
      ("w" git-timemachine-kill-revision "copy hash")
      ("W" git-timemachine-kill-abbreviated-revision "copy short hash")
      ("b" git-timemachine-blame "show blame")
      ("?" ar/git-timemachine-show-selected-revision "show info"))
     
     "Actions"
     (("B" magit-blame "blame")
      ("F" magit-find-file "find file")
      ("D" magit-diff-range "diff range")
      ("i" magit-init "init repo")
      ("C-c" magit-clone "clone")
      ("!" magit-run "run command")))))

;; Bind the hydra to a key in version control contexts
(with-eval-after-load 'magit
  (define-key magit-mode-map (kbd "C-c v") #'hydra-version-control/body))

(with-eval-after-load 'git-timemachine
  (define-key git-timemachine-mode-map (kbd "?") #'hydra-version-control/body))
#+end_src
```
