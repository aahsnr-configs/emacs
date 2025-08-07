# EMACS PLAN

## INSPIRATIONS

## OPTIMIZATIONS
- [ ] Headling folding/unfolding can introduce lag
- [ ] Input Delay when using pgtk

## QUESTIONS 
---
- [ ] Orderless,vertico, marginalia, nerd-icons-completion, consult, embark, embark-consult, corfu, nerd-icons-corfu, cape, dabbrev. What are some issues that these package face with lsp-mode or dap-mode
- [ ] Does corfu have native integration with eldoc
- [ ] How does lsp-mode view documentation.
- [ ] What will happen when cape-company-to-capf backend is provided to cape.
- [ ] Will treesit work with lsp-mode and/or dap-mode
- [ ] How does paredit work?

---
## TASKS


- [x] Integrate https://github.com/jamescherti/minimal-emacs.d
- [x] Add lexical binding to all .el files
- [x] Add early-init.el contents.
- [x] Add init.el contents for referring to config.org
- [x] Kill all buffers with \* right after emacs starts
- [x] Setup elpaca package manager
- [x] Add optimizations and sane defaults from doom emacs
- [x] User Information
- [-] Refactor the code from aashnr/emacs from the built-in branch
- [x] General.el
- [-] Separate out corfu-popuinfo and corfu-history [See if necessary later]
- [x] Update consult configuration from consult-doom.el
- [x] Check init-completion.el from https://github.com/seagle0128/.emacs.d
- [x] UI and Theming
- [x] Evil Mode
- [x] Setup Magit
- [ ] Add evil surround
- [x] Org Mode
- [x] Integrate org settings from elken/doom
- [x] Borrow modern emacs from my doom branch of aahsnr/emacs
- [x] Setup projectile, ibuffer, treemacs, persp-mode: **There are errors in this configuration**
- [x] Setup eglot and dape
- [x] Integrate the completion framework with eglot and dape
- [ ] For corfu, print the current selection candidate to buffer.
- [x] Set gc-cons-threshold to 100mb
- [x] Set corfu to display documentation only on key input 
- [ ] Add cape-company-to-capf backend to cape
- [ ] Use Scimax to setup many things
- [ ] Dired/Dirvish: Copy doom emacs's setup; [Note]: Don't use gemini
- [x] For LSP, use eglot and related packages;
- [ ] For eglot configuration, add a configuration option so that emacs has the latest eglot version.
- [ ] Make sure embark integrates with eglot
- [ ] For debugging, use dape and related packages
- [ ] For syntax-checking, use flymake and related packages
- [ ] Add flymake-posfrome into the syntax-checking
- [ ] For formatting apheleia is fine
- [ ] For spell-checking, use jinx with hunspell; make sure it is selectively on and off for certain modes
- [ ] Setup programming for python as IDE and org-mode
  - Use sqrtminuz for jupyter and python configuration.
- [ ] Add combobulate
  - Check seagle0128/.emacs.d for lsp-mode completion in source code blocks
- [ ] Setup LaTeX typesetting as IDE and org-mode
- [x] Refine configuration with emacs-modded.md
- [ ] Setup transient and crux once using emacs full time
- [ ] Add Deadgrep later on
- [ ] Add vim-tab-bar
- [ ] Add smartparens and paredit
- [x] Write a more comprehensive shackles configuration
- [ ] Make package-install load faster
- [ ] Make sure pdf-tools install epdinfo automatically
- [x] Emulate ivy, ivy-rich and ivy-rich-nerd-icons features into my configuration; not installing these packages, just emulating them
- [ ] Heavily borrow from https://github.com/seagle0128/.emacs.d
- [ ] Emulate completion framework for:
  - company-box
  - company-math
  - company-auctex
  - company-bibtex
  - company-reftex
  - company-maxima

  [NOTES]:
  - [ ] I don't need vertico multiform mode
