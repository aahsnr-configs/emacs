## Installation Instructions

### 1. System Dependencies

```bash
# Install basedpyright (via pip, NOT npm)
pip install basedpyright

# Install debugpy
pip install debugpy

# Install ruff
pip install ruff

# Verify installations
basedpyright-langserver --version
ruff --version
python3 -m debugpy --version
```

### 2. Tree-sitter Grammar

In Emacs:
```
M-x treesit-install-language-grammar RET python RET
```

### 3. Project Configuration Files

Create `pyproject.toml` in your project root:

```toml
[tool.ruff]
line-length = 88
target-version = "py311"

[tool.ruff.lint]
select = [
    "E",   # pycodestyle errors
    "W",   # pycodestyle warnings
    "F",   # pyflakes
    "I",   # isort
]

[tool.basedpyright]
typeCheckingMode = "standard"
reportMissingImports = true
pythonVersion = "3.11"
```

## Usage Workflow

### 1. Create an Org File

```org
#+auto_tangle: t
#+TITLE: Data Analysis
#+PROPERTY: header-args:jupyter-python :session py :async yes :kernel python3 :tangle ./analysis.py

* Setup
#+begin_src jupyter-python
import pandas as pd
import numpy as np
#+end_src

* Analysis
#+begin_src jupyter-python
def analyze_data(df: pd.DataFrame) -> pd.DataFrame:
    """Analyze the dataframe."""
    return df.describe()
#+end_src
```

### 2. Edit with LSP Support

1. Position cursor in a source block
2. Press `C-c '` (org-edit-special)
3. LSP activates automatically with full IDE features
4. Edit code with completions, diagnostics, and hover docs
5. Press `C-c '` again to return to org buffer
6. Save to auto-tangle

### 3. Debug Tangled File

1. Open the tangled file: `analysis.py`
2. Set breakpoints: `M-x dap-breakpoint-toggle`
3. Start debugging: `M-x dap-debug`
4. Select "Python :: Run Current File"

## Key Features

✅ **Full LSP in org-edit-special** with context from tangled file  
✅ **No crashes** - problematic UI features disabled in org-src buffers  
✅ **Basedpyright** - modern, strict type checking  
✅ **Ruff** - fast linting and formatting  
✅ **Debugpy** - modern Python debugging in tangled files  
✅ **python-ts-mode** - tree-sitter syntax everywhere  
✅ **Jupyter async** - rich output with async execution  
✅ **Auto-tangle** - seamless org-mode → Python workflow  

## Troubleshooting

### LSP not starting in org-src buffers?

1. Verify tangle file exists
2. Check basedpyright installation: `basedpyright-langserver --version`
3. Check `*lsp-log*` buffer for errors
4. Try `M-x lsp-workspace-restart`

### Jupyter kernel issues?

```elisp
;; Refresh kernelspecs after environment changes
(defun my/jupyter-refresh-kernelspecs ()
  "Refresh Jupyter kernelspecs."
  (interactive)
  (jupyter-available-kernelspecs t))
```

### Flycheck not using ruff?

```
M-x flycheck-verify-setup
```
Check that ruff is detected and enabled.
