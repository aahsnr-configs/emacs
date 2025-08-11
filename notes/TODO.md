# EMACS PLAN

## INSPIRATIONS

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

## COMPLETION/DEVELOPMENT

- `Desired Corfu Behaviour`:
  - [x] Buffer-local/Corfu-only completion styles [Corfu Github]
  - [x] TAB-and-Go completion [Corfu Github] working
  - [x] Disable popupifo and use corfu-show-info-documentation to setup a keybinding to display documentation

- Desired Flymake Behaviour
  - [ ] Remove flymake posframe and add flymake-sideline

- [ ] Install tree-sitter grammaers directly using arch linux

---

## TASKS

- [ ] **Make sure the following packages are working org source code blocks, first by manually checking, then adding if neccessary**
  - Completion System
  - Eglot, eldoc, dape, flymake
  - Alphaelia

- [x] **LaTeX**: [Add laas, auto-activating-snippets, cdlatex]
      Using the attached emacs configuration files as the base emacs configuration, write a comprehensive LaTeX writing environment in emacs 30 with the following features and integrations:
- integrate tectonic from <https://github.com/tectonic-typesetting/tectonic>
- integrate texlab from <https://github.com/latex-lsp/texlab> as the lsp backend for eglot.
- texlab and tectonic should work interchangeably wherever possible
- integrate org-mode and org-roam from the base emacs configuration into the writing environment.
- ability to write LaTeX documents both in org mode files and separate LaTeX files
- mimic the setup from doom emacs setup as closely as possible while not adding any redundant configuration options
- setup ligatures for LaTeX with additional math ligaturs
- setup cdlatex for quick math insertions, and setup laas and auto-activating-snippets.
- setup custom snippets that might be useful to quickly format and write LaTeX documents
- setup TeX-fold if writing separate LaTeX files
- integrate parsebib
- use pdf-tools as the default pdf viewer when compiling LaTeX files
- implement a robust citation and reference environment using citar-embark, citar-capf, org-roam-bibtex and citar-org-roam.

Make sure the LateX writing environment is integrated with the rest of the emacs configuration in the attached files. Make sure to search the web before writing anything. Do not introduce errors. You do not need to rewrite the whole emacs configuration. Only writet out latex writing environment and other needed changes in a nicely formatted and readble markdown output.

- [x] Integrate <https://github.com/jamescherti/minimal-emacs.d>
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
- [x] Check init-completion.el from <https://github.com/seagle0128/.emacs.d>
- [x] UI and Theming
- [x] Evil Mode
- [x] Setup Magit
- [ ] Make sure the selected candidate in corfu popups are automatically selected
- [ ] Make sure the documentation corfu generated is only available on key press events
- [ ] Setup completion setup for the auctex package like company-auctex
- [ ] Add evil surround
- [x] Org Mode
- [x] Integrate org settings from elken/doom
- [x] Borrow modern emacs from my doom branch of aahsnr/emacs
- [x] Setup projectile, ibuffer, treemacs, persp-mode: **There are errors in this configuration**
- [x] Setup eglot and dape
- [x] Integrate the completion framework with eglot and dape
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
- [ ] Use sqrtminuz for jupyter and python configuration.
- [ ] Add combobulate
- [ ] Check seagle0128/.emacs.d for lsp-mode completion in source code blocks
- [x] Setup LaTeX typesetting as IDE and org-mode
- [x] Refine configuration with emacs-modded.md
- [ ] Setup transient and crux once using emacs full time
- [ ] Add Deadgrep later on
- [ ] Add vim-tab-bar
- [ ] Add smartparens and paredit
- [x] Write a more comprehensive shackles configuration
- [ ] Make package-install load faster
- [ ] Make sure pdf-tools install epdinfo automatically
- [ ] Heavily borrow from <https://github.com/seagle0128/.emacs.d>
- [ ] Org-mode export support or bibliography note management with Org-roam later.
- [ ] Winner-Mode
- [ ] avy
- [ ] **Move-Text**
- [ ] **Aggressive Indent**: [Make sure enabled in org source code blocks. Does it provide extra functionality than evil indent plus? Maybe integrate with indent plus]
- [ ] **Helpful**
- [ ] **Pulsar**
- [ ] **Casual**
- [ ] **Indent Bars**
- [ ] **prettify-symbols-mode**
- [ ] **Crux**
- [ ] **Colorful Mode**
- [ ] **EasySession**
- [ ] **ZZZ-to-char**
- [ ] **tldr**
- [ ] **add hydra config after everything**
      Set a separate transient menu for magit
- [ ] Integrate ripgrep and fd throughout the whole configuration
- [ ] **Setup calendar, diary-lib, appt (appointments) later**

## The following packages must be working in org source code blocks

- [ ] **Aggressive Indent**
- [ ] **Move Text**
- [ ] **Pulsar**
- [ ] **Indent Bars**
- [ ] **Origami**
- [ ] **Rainbow Delimiters**
- [ ] **Smartparens**
- [ ] **Aggressive Indent**

# **_Inspirations_**

- [ ] Emacs Writing Studio
- [ ] Doom Emacs
- [ ] Scimax
- [ ] SqrtMinusOne
- [ ] <https://github.com/emacs-tw/awesome-emacs>
- [ ] progfolio setup
