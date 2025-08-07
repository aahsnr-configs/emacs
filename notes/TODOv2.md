# All tasks must be done in the given order
- [ ] __Make sure the following packages are working org source code blocks, first by manually checking, then adding if neccessary__
  - Completion System
  - Eglot, eldoc, dape, flymake
  - Alphaelia

- [ ] __Buffer Management with ibuffer and bufler__

- [x] __Winner-Mode__

- [x] __Alphaelia as the Formatter__

- [ ] __Window Management__

- [x] __Replace Projectile with Project.el__

- [x] __Replace Bufler with ibuffer__ [Integrate ibuffer with perspective.el and project.el and possibly completion system]

- [x] __Perspective__ [Integrate with ibuffer, project.el and possibly completion system]
- [ ] __Avy: Complete the configuration from avy.md__ 

- [ ] __Snippets: yasnippet + yasnippet-capf + consult-yasnippet__

- [ ] __Move-Text__

- [ ] __Aggressive Indent__: [Make sure enabled in org source code blocks. Does it provide extra functionality than evil indent plus? Maybe integrate with indent plus]

- [ ] __Helpful__

- [ ] __Pulsar__

- [ ] __Casual__

- [ ] __AutoSave__

- [ ] __Indent Bars__

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

- [ ] __prettify-symbols-mode__

- [ ] __Crux__

- [ ] __Colorful Mode__

- [ ] __EasySession__

- [ ]  __ZZZ-to-char__

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

# ___Inspirations___

- [ ] Emacs Writing Studio
- [ ] Doom Emacs
- [ ] Scimax
- [ ] SqrtMinusOne
- [ ] <https://github.com/emacs-tw/awesome-emacs>
- [ ] progfolio setup
