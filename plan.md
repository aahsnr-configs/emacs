# PLAN for custom centuar emacs fork
**Important** When writing all the contents of the org file and lisp folder files, make sure to compare it without the original centaur emacs config

- [x] place the following files in lisp subfolder:
   - [x] init-const
   - [x] init-custom
   - [x] init-func

- [ ] Place all the code of `init.el` inside an `config.org` file and make sure the order of configs is same as `init.el`

- [ ] The following files are being completely placed in `config.org`
  - [x] init-package
  - [x] init-base.el
  - [x] init-hydra.el

  - [x] init-ui.el
  - [x] init-edit.el
  - [x] init-completion.el
  - [x] init-snippet.el

  - [x] init-bookmark.el
  - [x] init-dashboard.el
  - [x] init-dired.el
  - [x] init-highlight.el
  - [x] init-ibuffer.el
  - [x] init-kill-ring.el
  - [x] init-workspace.el
  - [x] init-window.el
  - [x] init-treemacs.el

  - [ ] init-eshell.el
  - [ ] init-shell.el

  - [x] init-markdown.el
  - [x] init-org.el
  - [x] init-reader.el

  - [ ] init-dict.el
  - [ ] init-docker.el
  - [ ] init-player.el
  - [x] init-utils.el

  - [ ] init-vcs.el
  - [x] init-check.el
  - [x] init-lsp.el
  - [x] init-dap.el
  - [ ] init-ai.el

  - [x] init-prog.el
  - [x] init-elisp.el
  - [ ] init-c.el
  - [ ] init-go.el
  - [ ] init-rust.el
  - [ ] init-python.el
  - [ ] init-ruby.el
  - [ ] init-elixir.el
  - [ ] init-web.el

- [ ] leaving out `(eval-when-compile)` from the following files.
  - [x] init-package.el
  - [x] init-base.el
  - [x] init-ui.el
  - [x] init-edit.el
  - [x] init-completion.el
- [ ] Compare and contrast all the sections and subsections in `config.org` for the following commented out code:
```elisp
(eval-when-compile
  (require 'init-const)
  (require 'init-custom))
```

- [ ] Set general and evil keybindings for centaur keybindings
- [ ] Remove diminish from everywhere
- [ ] Keep all the lisp files from original centaur emacs config to keep track of
- [ ] Learn how to fork a project in github within a specific branch my repo
- [ ] Repeat the init-lsp.el integration with init-lsp-modified.el

***
# Issue
**Some of the integrations from centaur emacs do not work well for evil integrations**

# New Plan - Integrate between emacs-configv1.org and emacs-configv2.org  in emacs-mod.org

- [x] setup elpaca, evil, general and org mode at the very beginning, then use this config as the current config for further editing
- [x] don't use evil-org
- [ ] add some subsections of the highlight subsection at the end
- [ ] add easysession from minimal-emacs.d to session management section
- [x] add the following code to org-mode
```elisp
  ;; (org-fontify-done-headline t)
  ;; (org-fontify-todo-headline t)
  ;; (org-fontify-whole-heading-line t)
  ;; (org-fontify-quote-and-verse-blocks t)
```
- [ ] add treesit-auto from minimal-emacs.d
- [ ] add evil alternative drag-stuff
- [ ] determine if Optimization, Frame, Suppress GUI features, and Transient Posframe  subsections in UI and Theming is needed.
- [ ] don't add any frame-padding until the end
- [ ] combine treesit configs from both files
- [ ] replace hidewhow in emacs-configv2.org with treesit-fold along with pretty-hydra setup

***
## Integrating scimax modules
- [ ] scimax-autoformat-abbrev.el [Provides some flyspell stuff] - Also determine is flyspell is needed if jinx from below is used.
- [ ] scimax-ob.el
- [ ] scimax-jinx.el [Remove ivy and avy integrations]
- [ ] scimax-jupyter.el [Remove hydra stuff for now]
- [ ] scimax-latex.el


