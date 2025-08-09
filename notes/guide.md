## Project-specific .bib files
### 3. Update Your Project `.dir-locals.el`

To ensure the new packages can find your project-specific bibliographies, you need to add the `biblio-bibtex-files` variable to your directory-local settings.

**Action:** For each of your LaTeX projects, update the `.dir-locals.el` file.

**Example `.dir-locals.el`:**

```el
;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((latex-mode
  ;; This tells biblio (and thus consult-bibtex) where to find the .bib file
  (biblio-bibtex-files . ("references.bib"))

  ;; Your existing variables for citar and reftex should remain
  (citar-bibliography . ("references.bib"))
  (reftex-default-bibliography . ("references.bib"))))
```

**Explanation:**

*   By adding `biblio-bibtex-files`, you are telling the `biblio` package which bibliography files to manage for this specific project. `consult-bibtex` will then automatically use these files.
*   This approach maintains your current, robust system of managing bibliographies on a per-project basis without hardcoding paths in your main configuration.



Of course. Here are template files for `.dir-locals.el` and `.envrc` to facilitate Python development with Jupyter within Emacs. These templates are based on common practices and assume a project structure where a virtual environment is managed within the project directory.

Since no Emacs configuration was provided, these files are designed to be general and compatible with modern Emacs Python setups, including those using `lsp-mode` (for code intelligence) and `ein` or the built-in `python-mode` for Jupyter kernel interaction.

### `.envrc` for Direnv

This file should be placed in the root directory of your Python project. It uses `direnv` to automatically create and manage a project-specific Python virtual environment.

When you first `cd` into the directory, `direnv` will prompt you for authorization. Once you allow it, it will create a virtual environment inside a `.direnv` directory in your project root and activate it for your shell session.

**.envrc**
```bash
### direnv configuration for a Python project ###

# Use the 'python' layout, which creates and manages a virtual environment.
# This will use python3 by default. If you need a specific Python version
# that is on your PATH (e.g., python3.9), you can use 'layout python python3.9'.
layout python

# You can add other project-specific environment variables here.
# For example:
# export DJANGO_SETTINGS_MODULE="myproject.settings"
# export FLASK_APP="myapp.py"
```

**To make this work, you need to:**
1.  Install `direnv`.
2.  Hook `direnv` into your shell (e.g., by adding `eval "$(direnv hook bash)"` to your `.bashrc`).
3.  Navigate to your project directory and run `direnv allow`.

For seamless integration with graphical Emacs, it is highly recommended to install the `emacs-direnv` package, which will ensure Emacs is aware of the environment variables set by `direnv`.

### `.dir-locals.el` for Emacs

This file should also be placed in the root of your project. Emacs will automatically detect this file and apply the specified settings to all files opened within the project directory.

This configuration tells `python-mode` where to find the virtual environment managed by `direnv`. This helps Emacs locate the correct Python interpreter and, by extension, the Jupyter kernel associated with your project's dependencies.

**.dir-locals.el**
```elisp
;;; .dir-locals.el --- Directory-Local Variables for this Python Project

((python-mode
  ;; Point Emacs to the virtual environment managed by direnv.
  ;; The `layout python` command in .envrc creates the venv in .direnv/python-X.X.
  ;; This ensures that M-x run-python, Jupyter kernels, and other tools
  ;; use the project's isolated environment.
  (python-shell-virtualenv-root . ".direnv"))

 ;; For LSP (Language Server Protocol) integration with eglot or lsp-mode.
 ;; By using the emacs-direnv package, Emacs will automatically pick up the
 ;; virtual environment's PATH. This ensures the language server (like pylsp or pyright)
 ;; is launched from within the correct environment and can see your installed packages.
 ;; No explicit configuration is usually needed here if emacs-direnv is active.

 ;; If you use EIN (Emacs IPython Notebook)
 (ein:notebook-mode
  ;; EIN typically discovers kernelspecs automatically. When a python
  ;; process is started from the direnv-activated environment, it will
  ;; register its kernelspec correctly. You can ensure this by running
  ;; `python -m ipykernel install --user --name=<project-name>` from your
  ;; activated shell once. After that, EIN's `ein:notebook-open-kernel`
  ;; should find it.
  )

 ;; General setting for all modes in this project
 (nil
  ;; Example: Set indentation style for all files in this project.
  (indent-tabs-mode . nil)
  (tab-width . 4)))
```

When you first open a file in this directory, Emacs will ask for confirmation before applying these local variables for security reasons. You can choose to apply them for the current session or trust them permanently.

By using these two files together, you create a robust, project-specific environment that is automatically managed by `direnv` and seamlessly integrated with Emacs.
