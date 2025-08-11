# Python

- Setup pyright manually in lsp-mode instead of lsp-pyright to have better optimization. Definitely use dap-python

- **Python** --> eglot --> pyright: disable linting and formatting selectively
  dape --> debugpy
  flymake --> flake8 --> checking styling guide violations
  --> mypy --> static type checking
  apheleia --> black
- emacs pkgs:

I will use nix-shell with direnv
Is eldoc already setup with lsp-mode or is it needed or not needed?

**LaTeX** : Setup texlab manually in lsp-mode instead of lsp-latex.

eglot: texlab
flymake: chktex
apheleia: latexindent

- Can I use a formatter with texlab
- Determine if I need flycheck-posframe
- Make apheleia autoformats buffers on have
- Setup Automatic Save

For the emacs configuration in the attached init.txt file, integrate auctex's completion into the existing completion framework. And add texlab as lsp server to eglot. Make sure eglot's completion using texlab works together with auctex's implementation. Then integrate auctex's error checking system into the existing flymake configuration. Also make sure flymake also uses chktex for syntax checking and works together with auctex's error-checking system as well. For formatting configure apheleia to use latexindent. Check if auctex supports formatting. If yes, then make sure it is integrated into apheleia and works together with latexindent. Setup all the above configurations in the LaTeX section. Make sure these configurations are available in individual tex files as well org-mode files. Furthermore, write a comprehensive configuration for jinx from https://github.com/minad/jinx. And make sure it is disabled for LaTeX syntax and inside org source code blocks in org-mode files Write the output in a nicely formatted and readable markdown output. Do not introduce errors. Do not hallucinate emacs packages and configuration options. And most importantly, do not rewrite the whole emacs configuration. Only write out the changes that are needed. After that is done,
