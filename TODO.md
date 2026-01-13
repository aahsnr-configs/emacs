# TODOS:

[NOTE]: Return to ahsan_05.rahman@gmail.com in gemini to enhance the documentation in org-src-context.el file and apply the notes given by flymake. Also fix some stuttering for python completions in org-edit-special buffers.

[NOTE]: All mentioned documents in aahsnr-common/guides repository

- [ ] [Urgent] First study Zettelkasten vs. GTD in zettelkasten-gtd.md file. Then study the gtg-guide.md file. Then combine the two workflow approaches to create a unified workflow experience for the vanilla emacs config in the attached config.org file. You may remove any config options that are no longer needed. Only write out the changes needed but make sure the whole workflow guide is optimized for my personal emacs configuration and seamlessly integrated with my personal emacs configuration as well.

- [ ] [Urgent] Use the resulting guide to make changes to my emacs config
- [ ] [important] add org-landing-page.md configs
- [ ] [Next]: after completing the above 3 tasks, add/remove org files from org directory. org directory should be in Documents folder
- [ ] add evil alternative drag-stuff

- [ ] How and where does cape-tex and cape-dict get its sources. For cape-dict can I use en-UK spellings instead of en-US

- [ ] Setup org-mode for productivity with org todos and org-pomodoro. Use doom emacs' config for org-pomodoro

- [ ] Doom emacs uses projectile for it project related functionalities. One of its functionalities is to open project directory using SPC p p. But I am using project.el that is built-in to emacs. In my emacs config I want similar behaviour using project.el instead of using projectile, but having the same keybinding. The other functionality doom emacs utilizes is using the SPC f p keybinding to open the doom emacs config directory using projectile. I want the same functionality using the same keybinding but using project.el instead as well, and also since I am using vanilla emacs and I don't have a doom emacs config directory, I want SPC f p to open my emacs config directory which is typically in either ~/.config/emacs` directory.

- [ ] There is another buffer I like about the workspaces behaviour from the doom emacs project. The doom emacs project uses persp-mode to manage workspaces, but it has an interesting behaviour when allocating buffers and projects using projectile. It only shows the projects and buffer specific to the current emacsclient frame. Say you initiate an emacsclient A and in A, you open project 1 and buffer 1 from project 1. Then you initiate emacsclient B, and in B, you open project 2 and buffer 2 from project 2. Assuming at all these projects and buffers are distinct from one another, you won't see project 2 and buffer 2 in emacsclient A when executing consult-buffer. Similarly you won't see project 1 and buffer 1 in emacsclient B. As well, you will never see all these projects and buffers at the same time in a different emacsclient. Using the existing emacs packages I use, specifically using the built-in project.el package to manage projects and tabspaces to manage workspaces, I want a similar workflow in my own vanilla emacs configuration.

- [ ] make sure projects and recent files are detected immediately after opening a project directory or after opening a file.

- [ ] close dired minibuffer without entering a file

- [ ] When I press SPC p p to open projects and select a project from the projects menu (project menu is display in one of the attached screenshots), I get an actions menu with letters attached to each of the actions as seen in one of the other screenshots. I don't want to see this menu. I just to find a file directly in dired without pressing f as seen in the menu of actions. Then perform a similar task: I want to use SPC f p to directly to open the emacs config directory using project.el in the backend. But now I want to bypass both the projects menu and the actions menu. In other words, when I press SPC f p, the keybinding directly bypasses the projects menu so that the only project in this scenario is `~/.config/emacs` and then without pressing f from the actions menu, SPC f p has direct access to the find file action so that find file only finds the `~/.config/emacs/` directory. Both these keybindings perform the same task in doom emacs using projectile, but here we use the project.el in emacs.

- [ ] How to jump between main buffer and the open minibuffer. Find a elegant solution to do and make sure to use to set keybindings using general.el package

- [ ] using evil pressing g g takes a while to go to the top of the buffer while pressing G go to the bottom immediately

- [ ] Sometimes search by evil using / is weird. It does not match searchs when words are separated by -

- [ ] general keybindings shows that SPC s i is bound consult-imenu but M-x shows that SPC s i is bound to imenu instead. M-x also shows that SPC s i is bound to consult-imenu But when pressing SPC s i consult-imenu is correctly executed.

- [ ] in an org-mode file, when cursor moves to the end of the file, the buffer itself does not show the end of the file. Instead I have to invoke the scrollbar to go to the end. Right now this issue is only limited to org files as far as I have experienced. Also keep in mind that, at the end of the org file, many of the headlines are folded, but I am not sure if that factor has anything to do with this particular behavior.

- [ ] Using the evil `g g` binding in normal mode does not go to the top of the buffer. Need to press `0` again to go to the start of the line. Similar behaviour with `G` such that this keybinding does not entirely go to the end of the file, it just goes to the first word of the last line. Need `$` to go the very end. [Note]: Need to investigate further.

- [ ] _Replace all hydra configs with transient configs in its own main heading_

- [ ] Can recentf update recent files list when an existing file in its list changes name and/or directory

- [ ] **Important** After adding jupyter config, make sure it does not conflict with org-src-context.el

- [ ] Bind corfu-documentation to a more practical keybind so that it does not disturb my flow of typing but still allows me to see documentation with the keybind

- [ ] [Claude] Further study emacs configuration in the attached config.org file. Search the web and think longer to answer these 2 questions: Is 128MB gc-cons-threshold size needed for eglot? Determine if lsp-mode will be generally faster than elgot for my emacs configuration. Make sure the information you gather is up-to-date when you search the web

- [ ] Fix vertical and horizontal split keybindings and the window management settings
