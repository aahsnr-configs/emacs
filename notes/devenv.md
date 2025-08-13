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

Write the output in a nicely formatted and readable markdown output. Do not introduce errors. Do not hallucinate emacs packages and configuration options. And most importantly, do not rewrite the whole emacs configuration. Only write out the changes that are needed.
