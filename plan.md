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

## Integrating scimax modules
- [ ] scimax-autoformat-abbrev.el [Provides some flyspell stuff] - Also determine is flyspell is needed if jinx from below is used.
- [ ] scimax-ob.el
- [ ] scimax-jinx.el [Remove ivy and avy integrations]
- [ ] scimax-jupyter.el [Remove hydra stuff for now]
- [ ] scimax-latex.el

I haved attached my emacs configuration files, mainly the Emacs.org file containing the bulk of my emacs configuration and the early-init.el. Perform the following tasks for my emacs configuration:

Your first task is to set doom emacs style keybindings and vim-style keybindings (where appropriate) for the following emacs packages and replacing the existing packages general: 
- [ ] transwin
- [ ] undo-fu
- [ ] macro
- [ ] helpful
- [ ] avy
- [ ] avy-zap
- [ ] ace-link
- [ ] iedit
- [ ] expand-region
- [ ] multiple-cursors
- [ ] consult-dir
- [ ] consult-yasnippet
- [ ] dired and dired-related packages
- [ ] hl-todo
- [ ] diff-hl
- [ ] browse-kill-ring
- [ ] popper
- [ ] treemacs
- [ ] org-roam
- [ ] org-roam-ui
- [ ] consult-org-roam
- [ ] olivetti
- [ ] casual-suite
- [ ] pdf-view
- [ ] lsp-ui
- [ ] consult-lsp
- [ ] lsp-treemacs
- [ ] xref
- [ ] quickrun

Keep in mind that you can keep any keybindings that you deem not needed to be changed for the 1st task. Also make sure there are no conflicting keybindings for this task as well. The general emacs package use-package and all the keybindings set using the general package should be at the end of the emacs config file in a single use-package block

Also replace the following keybindings with doom emacs style keybindings using general:
```el
(bind-keys ("s-r"     . revert-buffer-quick)
           ("C-x K"   . delete-this-file)
           ("C-c C-l" . reload-init-file))

```

Only use consult-recent-file to open recent files using `SPC f r`. All the replaced or the new keybindings must be mnemonic

Do not touch any keybindings related to hydra and pretty-hydra throughout the emacs configuration. In other words, don't touch any keybindings under the :pretty-hydra block as well as any keybindings that map to -hydra/body. This is the end of the 1st task. And point what keybindings need to removed as well. 

For the 2nd task, in the hydra Setup section, rewrite the hydra use-package config so that it does not need childframe-completion-workable-p. This is the end of the 2nd task.

The whole emacs configuration was previously using the built-in emacs package management and now it uses elpaca. Make sure the emacs configuration correctly use elpaca. Do not write any changes into the emacs config file itself. This is your 3rd task.

For the 4th task, remove any unnecessary or not that useful evil configurations and evil packages. But make sure to keep the following config:
```el
(with-eval-after-load 'evil-maps
  (evil-define-key '(normal visual) 'global "gc" 'evilnc-comment-or-uncomment-lines))
```

For your 5th task, also make sure all the custom color settings match the colors of doom-themes tokyo-night theme. Also set tokyo-night theming for pdf-tools.

For your 6th task, make sure to include any buffer or modes that have not already been include in the popper configuration.

After performing all the above tasks, only write out the changes needed in the left side of the window.
