# All tasks must be done in the given order
1. [NOTE]: Add the following code when setting up org-agenda
  (setq org-agenda-start-with-log-mode t) ; Show log entries in agenda
  (setq org-agenda-span 'day) ; Default agenda span to day
  (setq org-agenda-time-grid '((daily today require-timed)
                               (800 1000 ".-")
                               (1200 1400 ".-")
                               (1600 1800 ".-")
                               (2000 2200 ".-")))
  (setq org-agenda-compact-blocks t) ; Compact agenda blocks

2. [NOTE]: Add the following code when setting up Latex export
  ;; Org Export settings (basic HTML export)
  (setq org-html-validation-link nil) ; Don't include W3C validation link
  (setq org-html-head-include-default-style nil) ; Don't include default CSS
  (setq org-html-head-include-scripts nil) ; Don't include default JS
  (setq org-html-head "<link rel=\"stylesheet\" href=\"https://cdn.jsdelivr.net/npm/water.css@2/out/water.css\">") ; Simple CSS for exports

- [ ] **Use the org-modern and org-appear settings from my doom setup**

- [ ] __Replace lsp-mode with eglot & eglot-booster__

- [ ] __Replace dap-mode with dape__

- [ ] __Replace flycheck with flymake__

- [ ] __Add consult-flymake__

- [ ] __Fix dired/dirvish__

- [ ] __Setup global word-wrap__

- [ ] __Evil__

- [ ] __Make sure the following packages are working org source code blocks, first by manually checking, then adding if neccessary__
  - Completion System
  - Eglot, eldoc, dape, flymake
  - Alphaelia



- [ ] __Buffer Management with ibuffer and bufler__

- [ ] __Project__ [Integrate project with completion system and ibuffer and perspective.el]

- [x] __Completion System__ [ Further add dabbrev and integrate the completion system with org-src-code blocks. Check when coding in jupyter ]

- [ ] __Keybindings__

- [ ] __Delimiters with smartparens and delimiters__

- [x] __Treemacs__

- [ ] __Git with treemacs__

- [x] __Treemacs toggle__

- [x] __Dired__

- [ ] __PDF Tools__

- [x] __Winner-Mode__

- [x] __Which-Key__

- [x] __Solaire Mode__

- [x] __Alphaelia as the Formatter__

- [x] __Doom Modeline__

- [x] __Recent Files__

- [x] __Savehist__

- [ ] __Window Management__

- [x] __Replace Projectile with Project.el__

- [x] __Replace Bufler with ibuffer__ [Integrate ibuffer with perspective.el and project.el and possibly completion system]

- [x] __Perspective__ [Integrate with ibuffer, project.el and possibly completion system]

Write comprehensive perspective.el configuration from https://github.com/nex3/perspective-el for emacs 30 using use-package format, and keybindings follow spacemacs-like keybindings using SPC as leader. The persective.el configuration must have the following integrations: ibuffer, project.el, treemacs and vertico, assuming that the user has setup configurations for the latter 4 packages. Do not introduce errors and redundant configurations.

- [ ] __Avy: Complete the configuration from avy.md__ 

- [ ] __Snippets: yasnippet + yasnippet-capf + consult-yasnippet__

- [x] __Flyspell with hunspell backend__ [ Make sure flyspell is disabled for certain unnecessay parts of the config. In the org-mode, disable it in the headlines and inside the org source code blocks ]

- [x] __Move-Text__

- [ ] __Aggressive Indent__: [Make sure enabled in org source code blocks. Does it provide extra functionality than evil indent plus? Maybe integrate with indent plus]

- [ ] __Helpful__

- [ ] __Pulsar__

- [ ] __Casual__

- [ ] __AutoSave__

- [ ] __Indent Bars__

- [ ] __Org__
  - [ ] __flyspell__
  - [ ] __cdlatex__
  - [ ] __fill column__
  - [ ] __display no. of words__
  - [x] __integrate org-roam with org-roam-ql, org-roam-ql-ql, embark-org-roam, org-roam-timestamps__
  - [ ] __setup padding from gemini (located in generated-configs)__
  - [ ] __setup org-roam general keybindings from incomplete claude.ai configuration in email unbent-crib-onyx@duck.com. Also complete org-roam from org-roam.md and compared it with the incomplete claude.ai configuration__

- [ ] __LaTeX__: [Add laas, auto-activating-snippets, cdlatex]
Using the attached emacs configuration files as the base emacs configuration, write a comprehensive LaTeX writing environment in emacs 30 with the following features and integrations:
 - integrate tectonic from https://github.com/tectonic-typesetting/tectonic
 - integrate texlab from https://github.com/latex-lsp/texlab as the lsp backend for eglot.
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

- [ ] __Python__

- [x] __Imenu__

- [ ] __Org__:Check DT's doom config for color-coded todo keywords

- [ ] __Ligatures and Unicode that mimics doom emacs__

- [ ] __prettify-symbols-mode__

- [ ] __Crux__

- [x] __Colorful Mode__

- [ ] __Rainbow-Delimiters__

- [ ] __Smartparens__

- [ ] __Setup anzu and evil-anzu__

- [ ] __EasySession__

- [x]  __ZZZ-to-char__

- [ ] __Rearrange settings from early-init.el using emacs-from-scratch

- [ ] __tldr__

- [ ] __add hydra config after everything__
Set a separate transient menu for magit

- [ ] Integrate ripgrep and fd throughout the whole configuration

- [ ] Improve existing vertico by adding the extensions from github

- [ ] Line Numbers from emacs-config.org

- [ ] Org-mode export support or bibliography note management with Org-roam later.

- [ ] Get jupyter config from python-dev-env.md

- [ ] __Setup calendar, diary-lib, appt (appointments) later__


## The following packages must be working in org source code blocks
- [ ] __Aggressive Indent__
- [ ] __Move Text__
- [ ] __Pulsar__
- [ ] __Indent Bars__
- [ ] __Origami__
- [ ] __Rainbow Delimiters__
- [ ] __Smartparens__
- [ ] __Aggressive Indent__


------------------------

# EMACS/DOOM EMACS

- [ ] Use crafted-emacs and use deepseek to create emacs config files
- [ ] Decide whether to use personal config instead of doom emacs
- [x] Install hyprlang-ts-mode for emacs
- [ ] Add color support to doom emacs
- [ ] add scripts directory to path
- [ ] add lisp code to path for use in configuration
- [ ] highlight matching parenthesis and use rainbow brackets
- [ ] integrate better defaults from emacs-config.org into fresh-emacs.org
- [ ] line numbering support inside org-src-code blocks
- [ ] borrow line numbers setting and minibuffer escape from emacs-config.org
- [ ] borrow zooming config from emacs-config.org
- [ ] org-capture binary from the doom emacs project
- [ ] setup a doom doctor-like setup and binary from the doom emacs project
- [ ] jupyter and latex integration inside org-babel
- [ ] Use emacs to setup systemd files
- [ ] after finishing emacs-config.org add features from from doom emacs init.el into the deepseek command
- [ ] Setup Org-mode from tmp-org5.el and org-roam from deepseek to setup. Turn on org-modern todo check
- [ ] Optimize org-mode scrolling using the deepseek setup
- [ ] Add origami, drag-stuff support
- [ ] Test tecosaur doom eamcs setup [TODO]
- [ ] Add solaire-mode
- [ ] Use evil keybindings wherever possible even in general
- [ ] Look for overlapping keybindings using gemini
- [ ] Train how to use all evil modules
- [ ] Chat for Gemini/Deepseek: Optimize the emacs 30 configuration using the attached file. Make sure all the respective modules are loaded in the correct order. Make sure all the components work well with each other. Defer any component if necessary. Optimize the whole configuration as well. Then rewrite the whole configuration.

------------------------

# ___Inspirations___

- [ ] Emacs Writing Studio
- [ ] Doom Emacs
- [ ] Scimax
- [ ] SqrtMinusOne
- [ ] <https://github.com/emacs-tw/awesome-emacs>
- [ ] progfolio setup

-------------------------

# Integrate the following comments into doom emacs

1. For the attached init.el file containing the base emacs 30 configuration, write a comprehensive org-mode configuration using the built-in org-mode with org-mode optimizations and the following features and integration:
    - org file directory is in $home/org along with all other org-mode related files;
    - extensive org headlines configuration with variable font size with each level of header, fonts using jetbrainsmono nerd font bold fonts for all headlines;
    - prettify source code blocks with ligatures, icons and prettify-symbols-mode;
    - all org files start in the overview mode;
    - comprehesive org-agenda setup including integrations with org-super-agenda;
    - comprehensive org-modern configuration with optimizations and integrations throughout the whole configuration and also include integrations with org-super-agenda, org-fragtog, org-download, org headlines and have custom org stars; use org-modern-table instead of a custom configuration;
    - org-fancy-priorities configuration with integration into org-modern
    - comprehensive org-todo configuration with ligatures and unicode integrated into org todo keywords
    - make sure org-ellipses integrates well with org-modern stars
    - have support for pretty tables in org files
    - seamless integrations writing in bold, italic and underline texts
    - have gruvbox dark theme integration throughtout the org configuration
    - org-roam v2 configuration with the following features and integrations: features that are inspired by The Brain in <https://thebrain.com/>; keybindings that follow doom-emacs-like bindings; quality-of-life features and improvements; advance visualizations with org-roam-ui; obsidian-like features; don't follow obsidian keybindings;; org-roam v2 related files are inside the org directory; additional quality-of-life features and improvements
    - comprehensive org-noter configuration that integrates well with org-roam v2 that integrates with pdf-tools
    - integrate any other missing features for quality-of-life improvements for org-mode configuration

----------------------------------
[!Note]: All steps must contain the following replies:

- enhance the above configuration
- find and fix any errors and issues in the above configuration including syntax errors, brackets mismatch, etc. Remove any redundant configuration options. Then rewrite the whole configuration
- further enhance the above configuration
- find and fix any further errors and issues in the above configuration. Then rewrite the whole configuration

[!Note] Add org-mode after seeing crafted-emacs config, write part about org configuration with org-appear, org-modern, denote, org-fragtog
Write a state-of-the-art emacs 30 configuration in org-mode that will be tangled to init.el with the following features, properties and integrations:

- divide the whole org file into sensible titles and respectives emacs-lisp org source code blocks with integration between the source code blocks for their respective configurations
- optimize emacs startup time and optimize the whole configuration where possible. all packages must be lazy loaded like neovim wherever possible.  
- use both elpaca and straight.el as package manager but only use-package format to install packages using elpaca package manager. straight.el is used to manage packages from git repos
- aggressive emacs optimizations to the configuration wherever possible
- comprehensive doom tokyo-night theme integration throughout the configuration and wherever possible
- setup automatic package update
- comprehensive keybindings configuration with doom emacs-like and spacemacs-like bindings and vim bindings integration using the general emacs package. Vim keybindings must not clash with the doom emacs-like or spacemacs-like keybindings
- minimal ui along with zen mode integration
- relace yes/no prompts for y/n
- disable automatically starting the splash screen, startup message, scratch message on startup
- comprehensive lsp-mode configuration for all the major programming mode  
- comprehensive tree-sitter support with treesitter integration for any part of the emacs 30 config that needs it.
- comprehensive editorconfig configuration to have cross-editor/ide like features
- comprehensive ibuffer configuration following keybindings from doom emacs project and integrating the ibuffer-project emacs package
- color and emojis support for emacs 30 as well as rainbow-mode integration

- comprehensive evil configuration following the evil configuration from the doom emacs project and including setups for the following packages: emacs-collection, evil-nerd-commenter and evil-goggles
- comprehensive completion system using extensive configurations for cape, consult, corfu, corfu-terminal, embark embark-consult, marginalia, orderless, and vertico. Have nerd-icons and tokyonight night theme integration wherever possible
- comprehensive lisp configuration for lisp modes including emacs-lisp, sly, clojure and guile. All lisp modes must have aggressive indent integration. The following emacs packages will be setup: package-lint, package-lint-flymake, sly, sly-asdf, sly-quicklisp, sly-repl-ansi-color, cider, clj-refactor, clojure-mode, flycheck-clojure, geiser, geiser-guile and geiser-racket.
- comprehensive speedbar configuration. speedbar is built into emacs 30. Don't pull from package manager sources or git sources.
- keep folders clean by no littering emacs package and disable nativecomp warnings
- set default fonts as JetBrainsMono Nerd Font and Ubuntu Nerd Font for variable pitch fonts
- comprehensive and state-of-the-art dired configuration with the following features and integrations: ranger integration; keybindings must follow the keybindings from the doom emacs project; file preview for various types of files; files and folders must only show the icon and title of the respective file and/or folder in that particular order; nerd icons integration; tokyonight night theme integration; hidden files must be shown with distinction from regular files; folders must be shown first then files are shown; respective files and/or folders for hidden files must be shown first before their regular counterpars

- comprehensive setup for looking up documentation for all common programming languages
- all-the-icons and nerd-icons integration throughout the configuration where needed. Don't use both. Mainly have nerd icons integration for the whole configuration and all the icons where nerd icons use is not available
- extensive dabbrev integration throughout the configuration
- add the ability to drag stuff (words, region, lines) around in Emacs using drag-stuff emacs package
- add the ability to format the a file on save using format-all package for the available file types
- setup comprehensive flycheck configuration using the flycheck package
- setup comprehensive magit configuration that includes git-timemachine package
- extensive helpful configuration for the package helpful from <https://github.com/Wilfred/helpful> instead of the built-in emacs 30 one
- extensive indent guides highlighting setup using highlight-indent-guides package
- comprehensive dap-mode setup integrated with lsp-mode
- comprehensive ligature configuration using the ligature emacs package
- comprehensive emacs modeline configuration using doom-modeline package inspired by the doom emacs project
- comprehensive treemacs configuration following keybindings from doom emacs project, integration with tokyo-night night theme and nerd icons theming
- support for editing nix files in emacs
- comprehensive prettiy-symbols configuration with integration for all programming modes
- support for re-opening all open buffers and files if emacs crashes for any reason
- add quality of life features for delimites including highlighting for matching parenthesis and extensive rainbow-delimiters integration
- comprehensive liguratures configuration
- comprehensive centaur-tabs configuration including features and keybindings from the doom emacs project
- comprehensive vterm configuration with optimizations and vterm toggle integration. Disable the use of eshell in emacs
- comprehensive which-key configuration with which-key being at the bottom and having idle delay of 0.1 and using  → as the separator
- comprehensive snippets configuration using yasnippet and the snippets are integrated throughout the whole configuration
- comprehensive dashboard configuration using the emacs-dashboard packages with quality of life improvements emacs and following the theming and features from the doom emacs dashboard

- comprehensive and state-of-the-art configuration for org-mode using the built-in org-mode in emacs with org-mode optimizations and the following features and integrations: do not pull org package from any emacs sources, instead use the built-in org-mode that comes with emacs 30; quality of features and improvements; do not setup org-roam(v1,2), org-noter or org-brain throughout the emacs 30 configuration; org file directory is in $HOME/org along with all other org-mode related files; extensive org headlines configuration with variable font size with each level of header, tokyonight night theme integrations, fonts using JetBrainsMono Nerd Font bold fonts for all headlines; prettify source code blocks with ligatures and icons; all org files start in the overview mode; comprehensive org babel configuration with support for python, shell, emacs-lisp and conf-unix; comprehensive org structure templates configuration with support for python, shell, emacs-lisp and conf-unix; comprehesive org-agenda setup including integrations with org-super-agenda; comprehensive org-todo configuration following exactly from the doom emacs project including the default keybindings, and default keywords, and integrates with org-super-agenda and org-modern; comprehensive org-fragtog and org-download integration; pomodoro integration for tasks; doom emacs-like capture templates; comprehensive org-modern configuration with optimizations and integrations throughout the whole configuration and also include integrations with org-super-agenda, org-fragtog, org-download, org headlines and have custom org stars; org-fancy-priorities configuration with integration into org-modern; setup a binary or python script for org-capture similar to the doom emacs project; seamless integrations writing in bold, italic and underline texts, as well as url and highlighted texts in org mode

- extensive hl-todo configuration that integration with org-modern and keybindings follows spacemacs like format
- autopair setup for types of brackets except for the delimiter "<" inside a org file
- org-roam v2 configuration with the following features and integrations: features that are inspired by The Brain in <https://thebrain.com/>; keybindings that follow doom-emacs-like bindings; quality-of-life features and improvements; advance visualizations; obsidian-like features; don't follow obsidian keybindings;; org-roam v2 related files are inside the org directory; additional quality-of-life features and improvements
- setup a binary or python script for having features like doom doctor and doom sync from the doom emacs project for the emacs 30 configuration
- comprehensive and state-of-the-art python programming configuration that includes lsp-mode integration, treesitters, formatters, linters, and dap and integrates all these features into org-mode so that python programming in an org file has support for lsp-mode, treesitters, formatters, linters, dap and all other features inside org source code blocks so that python programming in org-mode is seamless. The configuration must include comprehensive support for programming in jupyter using the emacs-jupyter package inside org mode. Org's structure templates for python must include the shebang argument '#!/usr/bin/env python'
- comprehensive projectile configuration with support for various programming languages and integrates into ibuffer if possible
- take inspiration from the doom emacs project for any missing features that may quality of life improvements
