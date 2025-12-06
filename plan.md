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
  - [ ] init-ibuffer.el
  - [ ] init-kill-ring.el
  - [ ] init-workspace.el
  - [ ] init-window.el
  - [ ] init-treemacs.el

  - [ ] init-eshell.el
  - [ ] init-shell.el

  - [ ] init-markdown.el
  - [ ] init-org.el
  - [ ] init-reader.el

  - [ ] init-dict.el
  - [ ] init-docker.el
  - [ ] init-player.el
  - [ ] init-utils.el

  - [ ] init-vcs.el
  - [ ] init-check.el
  - [ ] init-lsp.el
  - [ ] init-dap.el
  - [ ] init-ai.el

  - [ ] init-prog.el
  - [ ] init-elisp.el
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
