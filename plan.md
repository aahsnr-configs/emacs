# PLAN for custom centuar emacs fork
**Important** When writing all the contents of the org file and lisp folder files, make sure to compare it without the original centaur emacs config

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
- [x] add easysession from minimal-emacs.d to session management section
- [x] add the following code to org-mode
```elisp
  ;; (org-fontify-done-headline t)
  ;; (org-fontify-todo-headline t)
  ;; (org-fontify-whole-heading-line t)
  ;; (org-fontify-quote-and-verse-blocks t)
```
- [x] add treesit-auto from minimal-emacs.d
- [ ] add evil alternative drag-stuff
- [ ] determine if Optimization, Frame, Suppress GUI features, and Transient Posframe  subsections in UI and Theming is needed.
- [ ] don't add any frame-padding until the end
- [x] combine treesit configs from both files
- [x] for the folding behaviour use https://github.com/doomemacs/doomemacs/blob/master/modules/editor/fold/config.el as a guide and use hide-show, evil-vimish-fold and treesit fold
- [x] list-environment is not necessary
- [ ] Remap C-g to escape
- [x] Write a git-gutter config and make it work in org files as well as other files. And instead of using the use git markers like + and - to indicate changes, use a more aesthetic approach to indicate changes that match the tokyonight colors. Write a sample guide to using git-gutter in a wide variety of files.
- [x] Using general.el setup C-- for text-scale-decrease and C-= for text-scale-increase
- [x] In my updated emacs config, does the posframe window (childframe) for opening hydra keys change in dimensions based on the contents of the hydra body?
- [ ] Make magit utilize hydra and pretty-hydra instead of using transient for its usage. Also make the magit window open as a minibuffer in the bottom instead of a new buffer.

- [ ] **Fix dired/dirvish before proceeding** Doom emacs uses projectile for it project related functionalities. One of its functionalities is to open project directory using SPC p p. But I am using project.el that is built-in to emacs. In my emacs config I want similar behaviour using project.el instead of using projectile, but having the same keybinding. The other functionality doom emacs utilizes is using the SPC f p keybinding to open the doom emacs config directory using projectile. I want the same functionality using the same keybinding but using project.el instead as well, and also since I am using vanilla emacs and I don't have a doom emacs config directory, I want SPC f p to open my emacs config directory which is typically in either `~/.emacs.d` or
`~/.config/emacs` directory.

- [ ] There is another buffer I like about the workspaces behaviour from the doom emacs project. The doom emacs project uses persp-mode to manage workspaces, but it has an interesting behaviour when allocating buffers and projects using projectile. It only shows the projects and buffer specific to the current emacsclient frame. Say you initiate an emacsclient A and in A, you open project 1 and buffer 1 from project 1. Then you initiate emacsclient B, and in B, you open project 2 and buffer 2 from project 2. Assuming at all these projects and buffers are distinct from one another, you won't see project 2 and buffer 2 in emacsclient A when executing consult-buffer. Similarly you won't see project 1 and buffer 1 in emacsclient B. As well, you will never see all these projects and buffers at the same time in a different emacsclient. Using the existing emacs packages I use, specifically using the built-in project.el package to manage projects and tabspaces to manage workspaces, I want a similar workflow in my own vanilla emacs configuration.

- [ ] make sure projects and recent files are detected immediately after opening a project directory or after opening a file.

- [ ] close dired minibuffer without entering a file

- [ ] I usually lose the list of recently opened files or buffers after I restart my emacs daemon. But the recentf file and the projects file are not deleted in between emacs daemon restarts. Shouldn't the recent files list still exist in between the restarts.

- [ ] Hide emphasis markers in markdown files like how my org-mode config does.

- [x] Assuming that I have made the changes you suggested, how do I make the area below the doom modeline more useful like doom emacs does?  I want it to show matching parenthesis that are not visible in the buffer. I want it show org headlines and subheadlines info. I want this area to show other useful stuff as well that you think might be useful. Search the web to determine this. And only write out the changes needed. Think for a while for this task as well.

- [ ] using evil pressing g g takes a while to go to the top of the buffer while pressing G go to the bottom immediately

- [ ] Sometimes search by evil using / is weird. It does not match searchs when words are separated by - 

- [ ] general keybindings shows that SPC s i is bound consult-imenu but M-x shows that SPC s i is bound to imenu instead. M-x also shows that SPC s i is bound to consult-imenu But when pressing SPC s i consult-imenu is correctly executed.  

- [ ] Add a hydra body/pretty-hydra setting for lsp-mode/lsp-ui. Make sure it includes toggling the lsp features like breadcrumbs using keybindings set in hydra/pretty-hydra. All lsp related keybindings should use hydra/pretty-hydra instead of other keybindings


***
## Integrating scimax modules
*Determine the order in which these files should be added*
- [ ] scimax-ob.el
- [ ] scimax-jupyter.el [Remove hydra stuff for now]
- [ ] scimax-latex.el
