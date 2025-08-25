# EMACS PLAN

## INSPIRATIONS

- [Quark Emacs](https://github.com/PythonNut/quark-emacs)

- Make sure to look at [Scimax](https://github.com/jkitchin/scimax) at the following files:
  - [ ] scimax/scimax-literate-programming.el
  - [ ] scimax/scimax-org-src-blocks.el

- Make sure to look at the following files from [Centaur Emacs](https://github.com/seagle0128/.emacs.d)
  - [ ] [init-base](https://github.com/seagle0128/.emacs.d/blob/master/lisp/init-base.el)
  - [ ] [init-completion](https://github.com/seagle0128/.emacs.d/blob/master/lisp/init-completion.el)
  - [ ] [init-edit](https://github.com/seagle0128/.emacs.d/blob/master/lisp/init-edit.el)
  - [ ] [init-highlight](https://github.com/seagle0128/.emacs.d/blob/master/lisp/init-highlight.el)
  - [ ] [init-lsp](https://github.com/seagle0128/.emacs.d/blob/master/lisp/init-lsp.el)
  - [ ] [init.org](https://github.com/seagle0128/.emacs.d/blob/master/lisp/init-org.el)

## OPTIMIZATIONS

- [ ] Headling folding/unfolding can introduce lag
- [ ] Input Delay when using pgtk

## QUESTIONS

- [ ] Orderless,vertico, marginalia, nerd-icons-completion, consult, embark, embark-consult, corfu, nerd-icons-corfu, cape, dabbrev. What are some issues that these package face with eglot
- [ ] Does corfu have native integration with eldoc
- [ ] How does lsp-mode view documentation.
- [ ] What will happen when cape-company-to-capf backend is provided to cape.
- [ ] Will treesit work with lsp-mode and/or dap-mode
- [ ] How does paredit work?
- [ ] Are additional backends needed for cape and eglot
- [ ] Can I use a formatter with texlab
- [ ] Determine if I need flycheck-posframe
- [ ] Make apheleia autoformats buffers on have
- [ ] Will AucTeX completion work with corfu?

## LaTeX tasks

- [ ] For the attached emacs configuration, setup yasnippet snippet inheritance, so that org-mode inheritaces the snippets from yas-define-snippets, keeping in mind the function yas-parent-mode does not exist
- [ ] Export org files to latex and pdf files
- [ ] Determine if texpresso-tonic can be used

## Programming modes

- LaTeX-mode
- python-ts-mode
- markdown-ts-mode

---

## TASKS

- [ ] Integrate <https://github.com/jamescherti/minimal-emacs.d>
- [ ] Use projectile and persp-mode, along with their integration packages
- [ ] Make sure the selected candidate in corfu popups are automatically selected
- [ ] Make sure the documentation corfu generated is only available on key press events
- [ ] integrate org-mode and org-roam from the base emacs configuration into the writing environment
- [ ] setup ligatures for LaTeX with additional math ligaturs
- [ ] setup cdlatex for quick math insertions, and setup laas and auto-activating-snippets.
- [ ] setup custom snippets that might be useful to quickly format and write LaTeX documents
- [ ] Setup completion setup .tex and .org files like company-auctex
- [ ] Add evil surround
- [ ] Set gc-cons-threshold to 100mb
- [x] Set corfu to display documentation only on key input
- [ ] Add whitespace-cleanup-mode
- [ ] Use Scimax to setup many things
- [ ] Add combobulate
- [x] Write a more comprehensive shackles configuration
- [ ] Make package-install load faster
- [ ] EasySession
- [ ] Make sure pdf-tools install epdinfo automatically
- [ ] Heavily borrow from <https://github.com/seagle0128/.emacs.d>
- [ ] Org-mode export support or bibliography note management with Org-roam later.
- [ ] Winner-Mode
- [ ] **Indent Bars**

# Emacs tasks for later

- [ ] Setup transient, casual and crux once using emacs full time
- [ ] add Deadgrep later on
- [ ] add vim-tab-bar
- [ ] Integrate ripgrep and fd throughout the whole configuration
- [ ] Setup calendar, diary-lib, appt (appointments)
- [ ] ZZZ-to-char
- [ ] tldr
- [ ] only use hydra with dap-mode
- [ ] helpful
- [ ] avy
- [ ] Move-Text
- [ ] Pulsar
- [ ] Colorful Mode

# **_Inspirations_**

- [ ] Emacs Writing Studio
- [ ] Doom Emacs
- [ ] Scimax
- [ ] SqrtMinusOne
- [ ] <https://github.com/emacs-tw/awesome-emacs>
- [ ] progfolio setup
