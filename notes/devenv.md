# Python
- Setup pyright manually in lsp-mode instead of lsp-pyright to have better optimization. Definitely use dap-python

- __Python__ --> lsp      --> pyright
             dap      --> debugpy
             flycheck --> flake8 --> checking styling guide violations
                      --> mypy   --> static type checking
- emacs pkgs: 


I will use nix-shell with direnv
Is eldoc already setup with lsp-mode or is it needed or not needed?

o __LaTeX__ : Setup texlab manually in lsp-mode instead of lsp-latex. Don't need flycheck-chktex since its bundled with flycheck
- Can I use a formatter with texlab
- Determine if I need flycheck-posframe
- Make apheleia autoformats buffers on have
- Setup Automatic Save
