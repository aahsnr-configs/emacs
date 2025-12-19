(use-package org
  :ensure nil
  :pretty-hydra
  ;; See `org-structure-template-alist'
  ((:title (pretty-hydra-title "Org Template" 'sucicon "nf-custom-orgmode" :face 'nerd-icons-green)
    :color blue :quit-key ("q" "C-g"))
   ("Basic"
    (("a" (hot-expand "<a") "ascii")
     ("c" (hot-expand "<c") "center")
     ("C" (hot-expand "<C") "comment")
     ("x" (hot-expand "<e") "example")
     ("E" (hot-expand "<E") "export")
     ("h" (hot-expand "<h") "html")
     ("l" (hot-expand "<l") "latex")
     ("n" (hot-expand "<n") "note")
     ("o" (hot-expand "<q") "quote")
     ("v" (hot-expand "<v") "verse"))
    "Head"
    (("i" (hot-expand "<i") "index")
     ("A" (hot-expand "<A") "ASCII")
     ("I" (hot-expand "<I") "INCLUDE")
     ("H" (hot-expand "<H") "HTML")
     ("L" (hot-expand "<L") "LaTeX"))
    "Source"
    (("s" (hot-expand "<s") "src")
     ("e" (hot-expand "<s" "emacs-lisp") "emacs-lisp")
     ("y" (hot-expand "<s" "python") "python")
     ("w" (hot-expand "<s" "powershell") "powershell")
     ("r" (hot-expand "<s" "ruby") "ruby")
     ("S" (hot-expand "<s" "sh") "sh")
     ("g" (hot-expand "<s" "go :imports '\(\"fmt\"\)") "golang"))
    "Misc"
    (("m" (hot-expand "<s" "mermaid :file chart.png") "mermaid")
     ("u" (hot-expand "<s" "plantuml :file chart.png") "plantuml")
     ("Y" (hot-expand "<s" "ipython :session :exports both :results raw drawer\n$0") "ipython")
     ("P" (progn
            (insert "#+HEADERS: :results output :exports both :shebang \"#!/usr/bin/env perl\"\n")
            (hot-expand "<s" "perl")) "Perl tangled")
     ("<" self-insert-command "ins"))))
  :bind (:map org-mode-map
         ("<" . (lambda ()
                  "Insert org template."
                  (interactive)
                  (if (or (region-active-p) (looking-back "^\s*" 1))
                      (org-hydra/body)
                    (self-insert-command 1)))))

  :hook ((org-mode . visual-line-mode)
         (org-mode . ar/org-font-setup)
         (org-mode . auto-fill-mode)
         (org-mode . (lambda () (setq-local yas-parents '(latex-mode))))
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
  (org-modules nil)
  (org-directory my/org-directory)
  (org-log-done 'time)
  (org-pretty-entities t)
  (org-log-into-drawer t)
  (org-return-follows-link t)
  (org-src-fontify-natively t)
  (org-element-use-cache t)
  (org-hide-leading-stars t)
  (org-hide-emphasis-markers t)
  (org-element-cache-persistent t)
  (org-fontify-whole-heading-line t)
  ;; (org-fontify-quote-and-verse-blocks nil)
  ;; (org-fontify-whole-heading-line nil)
  ;; (org-fontify-done-headline nil)
  (org-highlight-latex-and-related nil)
  (org-src-preserve-indentation t)
  (org-edit-src-content-indentation 0)
  (org-startup-folded 'overview)
  (org-startup-with-inline-images nil)
  (org-startup-with-latex-preview nil)
  (org-cycle-separator-lines 2)

  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "PROG(p)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCEL(c@)")
	 (sequence "PLAN(P)" "ACTIVE(A)" "PAUSED(x)" "|" "ACHIEVED(a)" "DROPPED(D)")))

  (org-todo-keyword-faces
   '(("TODO"      . (:foreground "#f7768e" :weight bold))
     ("NEXT"      . (:foreground "#ff9e64" :weight bold))
     ("PROG"      . (:foreground "#7aa2f7" :weight bold))
     ("WAIT"      . (:foreground "#e0af68" :weight bold))
     ("DONE"      . (:foreground "#9ece6a" :weight bold))
     ("CANCEL"    . (:foreground "#565f89" :weight bold))
     ("PLAN"      . (:foreground "#7dcfff" :weight bold))
     ("ACTIVE"    . (:foreground "#bb9af7" :weight bold))
     ("PAUSED"    . (:foreground "#a9b1d6" :weight bold))
     ("ACHIEVED"  . (:foreground "#9ece6a" :weight bold))
     ("DROPPED"   . (:foreground "#565f89" :weight bold))))

  (org-tag-alist '(("@work"      . ?w)
		           ("@home"      . ?h)
		           ("@computer"  . ?c)
		           ("@errands"   . ?e)
		           ("read"       . ?r)
		           ("meeting"    . ?m)
		           ("urgent"     . ?u)
		           ("someday"    . ?s)))


  (org-priority-faces '((?A . error)
                        (?B . warning)
                        (?C . success)))

  ;; Agenda styling
  (org-agenda-files (list my/org-directory))
  (org-agenda-block-separator ?─)
  (org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
  (org-agenda-current-time-string
   "⭠ now ─────────────────────────────────────────────────")
  (org-agenda-inhibit-startup t)
  (org-agenda-dim-blocked-tasks nil)
  (org-agenda-use-tag-inheritance nil)
  (org-agenda-ignore-properties '(effort appt category))
  (org-agenda-span 'day)
  (org-tags-column -80)

  :config
  (defun hot-expand (str &optional mod)
    (let (text)
      (when (region-active-p)
        (setq text (buffer-substring (region-beginning) (region-end)))
        (delete-region (region-beginning) (region-end)))
      (insert str)
      (if (fboundp 'org-try-structure-completion)
          (org-try-structure-completion) ; < org 9
        (progn
          ;; New template expansion since org 9
          (require 'org-tempo nil t)
          (org-tempo-complete-tag)))
      (when mod (insert mod) (forward-line))
      (when text (insert text))))

  ;; Babel
  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t)

  (defconst load-language-alist
    '((emacs-lisp . t)
      (perl       . t)
      (python     . t)
      (ruby       . t)
      (js         . t)
      (css        . t)
      (sass       . t)
      (C          . t)
      (java       . t)
      (shell      . t)
      (plantuml   . t))
    "Alist of org ob languages.")

  (org-babel-do-load-languages 'org-babel-load-languages
                               load-language-alist)

  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("jpy" . "src jupyter-python"))
  (add-to-list 'org-structure-template-alist '("tex" . "src latex")))
