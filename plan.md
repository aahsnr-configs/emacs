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
- [x] Remove diminish from everywhere
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
- [x] Make magit utilize hydra and pretty-hydra instead of using transient for its usage. Also make the magit window open as a minibuffer in the bottom instead of a new buffer.

- [ ] My  attached emacs config emacs.txt has two issue now. When I press enter inside an org-mode source code block, instead of moving the cursor to the next line, pressing enter moves the cursor to the beginning of the source code block but 1 line after the #+begin_src "org-babel language" in the source code block. Pressing the tab key also exhibits the exact same behavior. But I would also like have an evil method of getting to this position without conflicting other keybindings or similarly to the end of the source code block before #+end_src as well. There is another weird: when g c to comment or uncomment lines a formatter window is opened. Make sure the provided apheleia confi is correct. First analyze the attached emacs.txt file carefully and then fix the two issues for me. Search the web and think longer for task before proceeding. Then only write out the changes needed.

- [ ] How and where does cape-tex and cape-dict get its sources. For cape-dict can I use en-UK spellings instead of en-US

- [ ] **Fix dired/dirvish before proceeding** Doom emacs uses projectile for it project related functionalities. One of its functionalities is to open project directory using SPC p p. But I am using project.el that is built-in to emacs. In my emacs config I want similar behaviour using project.el instead of using projectile, but having the same keybinding. The other functionality doom emacs utilizes is using the SPC f p keybinding to open the doom emacs config directory using projectile. I want the same functionality using the same keybinding but using project.el instead as well, and also since I am using vanilla emacs and I don't have a doom emacs config directory, I want SPC f p to open my emacs config directory which is typically in either `~/.emacs.d` or
`~/.config/emacs` directory.

- [X] There is another buffer I like about the workspaces behaviour from the doom emacs project. The doom emacs project uses persp-mode to manage workspaces, but it has an interesting behaviour when allocating buffers and projects using projectile. It only shows the projects and buffer specific to the current emacsclient frame. Say you initiate an emacsclient A and in A, you open project 1 and buffer 1 from project 1. Then you initiate emacsclient B, and in B, you open project 2 and buffer 2 from project 2. Assuming at all these projects and buffers are distinct from one another, you won't see project 2 and buffer 2 in emacsclient A when executing consult-buffer. Similarly you won't see project 1 and buffer 1 in emacsclient B. As well, you will never see all these projects and buffers at the same time in a different emacsclient. Using the existing emacs packages I use, specifically using the built-in project.el package to manage projects and tabspaces to manage workspaces, I want a similar workflow in my own vanilla emacs configuration.

- [ ] make sure projects and recent files are detected immediately after opening a project directory or after opening a file.

- [ ] close dired minibuffer without entering a file

- [x] I usually lose the list of recently opened files or buffers after I restart my emacs daemon. But the recentf file and the projects file are not deleted in between emacs daemon restarts. Shouldn't the recent files list still exist in between the restarts.

- [x] Hide emphasis markers in markdown files like how my org-mode config does.

- [x] Assuming that I have made the changes you suggested, how do I make the area below the doom modeline more useful like doom emacs does?  I want it to show matching parenthesis that are not visible in the buffer. I want it show org headlines and subheadlines info. I want this area to show other useful stuff as well that you think might be useful. Search the web to determine this. And only write out the changes needed. Think for a while for this task as well.

- [ ] When I press SPC p p to open projects and select a project from the projects menu (project menu is display in one of the attached screenshots), I get an actions menu with letters attached to each of the actions as seen in one of the other screenshots. I don't want to see this menu. I just to find a file directly in dired without pressing f as seen in the menu of actions. Then perform a similar task: I want to use SPC f p to directly to open the emacs config directory using project.el in the backend. But now I want to bypass both the projects menu and the actions menu. In other words, when I press SPC f p, the keybinding directly bypasses the projects menu so that the only project in this scenario is `~/.config/emacs` and then without pressing f from the actions menu, SPC f p has direct access to the find file action so that find file only finds the `~/.config/emacs/` directory. Both these keybindings perform the same task in doom emacs using projectile, but here we use the project.el in emacs.

- [ ] How to jump between main buffer and the open minibuffer. Find a elegant solution to do and make sure to use to set keybindings using general.el package

- [ ] using evil pressing g g takes a while to go to the top of the buffer while pressing G go to the bottom immediately

- [ ] Sometimes search by evil using / is weird. It does not match searchs when words are separated by -

- [ ] general keybindings shows that SPC s i is bound consult-imenu but M-x shows that SPC s i is bound to imenu instead. M-x also shows that SPC s i is bound to consult-imenu But when pressing SPC s i consult-imenu is correctly executed.

- [ ] Add a hydra body/pretty-hydra setting for lsp-mode/lsp-ui. Make sure it includes toggling the lsp features like breadcrumbs using keybindings set in hydra/pretty-hydra. All lsp related keybindings should use hydra/pretty-hydra instead of other keybindings

- [x] add the updated markdown configuration from markdown-updated.md in guide folder.

- [ ] in an org-mode file, when cursor moves to the end of the file, the buffer itself does not show the end of the file. Instead I have to invoke the scrollbar to go to the end. Right now this issue is only limited to org files as far as I have experienced. Also keep in mind that, at the end of the org file, many of the headlines are folded, but I am not sure if that factor has anything to do with this particular behavior.

- [ ] elpaca-manager should always open as a full window instead of a minibuffer
***
## Integrating scimax modules
*Determine the order in which these files should be added*
- [ ] scimax-ob.el
- [ ] scimax-jupyter.el [Remove hydra stuff for now]
- [ ] scimax-latex.el

- [ ] `I have updated the language check to use org-element-at-point which is technically more robust than context for retrieving block properties in complex documents.` in cape config. Will this interfere with org-src-context file
