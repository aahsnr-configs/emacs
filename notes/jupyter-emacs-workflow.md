Of course. Here is the comprehensive, unified guide for advanced literate Python programming in Emacs, incorporating all of our corrected configurations and detailed explanations.

---

# The Definitive Guide to Advanced Literate Python Programming in Emacs

## Introduction

This guide details how to configure Emacs for a state-of-the-art literate programming workflow with Python and Jupyter. The resulting environment is exceptionally powerful because it seamlessly integrates two different programming paradigms into a single, cohesive experience:

1.  **Live Runtime Introspection:** Through `emacs-jupyter`, you get intelligent, data-aware completion and interaction from a live, running Python kernel.
2.  **Robust Static Analysis:** Through Eglot and `basedpyright`, you get professional-grade, real-time diagnostics, type-checking, and refactoring tools.

By following this guide, you will build an environment that provides the right tool for every task, whether you are interactively exploring a dataset or engineering production-quality code, all from within a single Org Mode document.

## Prerequisites

Before configuring Emacs, ensure the following packages are installed in your Python environment:

- **Jupyter Ecosystem:** `pip install jupyterlab notebook ipykernel`
- **LSP Server:** `pip install 'python-basedpyright[all]'`
- **Linters & Formatters:** `pip install ruff mypy`
- **Debugger:** `pip install debugpy`

## The Definitive Configuration

Place the following Emacs Lisp code blocks into your configuration (`init.el` or equivalent). These snippets contain all the necessary logic to stabilize the environment and enable the advanced, dual-mode workflow.

#### 1. Stabilize Inline Completion & Enable a Smart `TAB`

This block resolves the core conflict between Corfu and `emacs-jupyter` and establishes a robust manual completion trigger.

**Location:** Add to your **Completion Framework** or **Corfu** section.

```emacs-lisp
;; --- Stabilize Jupyter and Corfu Integration ---

;; 1. Fix Corfu auto-completion error in Org-mode Jupyter blocks.
;;    This disables the auto-timer but allows manual completion.
(add-hook 'jupyter-org-interaction-mode-hook
          (lambda ()
            (setq-local corfu-auto nil)))

;; 2. Create a smart TAB command for manual completion.
;;    It tries to complete; if it can't, it indents. This is our manual trigger.
(defun ar/corfu-complete-or-indent ()
  "Try to complete with Corfu, otherwise indent."
  (interactive)
  (or (corfu-complete)
      (indent-for-tab-command)))

;; 3. Bind this smart TAB command in Evil's insert mode.
(define-key evil-insert-state-map (kbd "TAB") #'ar/corfu-complete-or-indent)
```

#### 2. Create the Unified Edit Buffer (`C-c '`)

This block contains the logic to create the ultimate editing experience, combining LSP and live kernel features inside the special `org-edit` buffer.

**Location:** Add to your **Jupyter Notebooks** or a new **Literate Programming** section.

```emacs-lisp
;; --- Create a Unified Editing Experience in C-c ' ---

;; 1. Automatically start the LSP client in the special edit buffer.
(add-hook 'org-babel-edit-prep-hook #'eglot-ensure)

;; 2. Define the function to connect the edit buffer to the live Jupyter kernel.
(defun ar/connect-org-edit-buffer-to-jupyter (&rest _)
  "Connect the special org-edit buffer to a live Jupyter kernel."
  ;; Information is fetched from the *original* Org buffer.
  (let* ((src-info (org-babel-get-src-block-info '(".")))
         (lang (nth 0 src-info))
         (header-args (nth 2 src-info))
         (session (cdr (assoc :session header-args))))

    ;; Only proceed if we are in a jupyter-python block with a session.
    (when (and (string-equal lang "python") session)
      ;; Switch context to the newly created special edit buffer.
      (with-current-buffer (org-babel-get-special-edit-buffer)
        ;; Find the running kernel associated with the session name.
        (when-let ((kernel (jupyter-get-running-kernel-from-session session)))
          ;; Connect this buffer to that kernel.
          (jupyter-connect-to-kernel kernel)

          ;; Most importantly, add Jupyter's live completion function to this
          ;; buffer's list of completion providers.
          (add-to-list 'completion-at-point-functions #'jupyter-completion-at-point nil t))))))

;; 3. Use `advice-add` to run our function *after* `org-edit-special` has finished.
;;    This is the robust and correct way to implement this feature.
(advice-add 'org-edit-special :after #'ar/connect-org-edit-buffer-to-jupyter)
```

## Deep Dive: How the Configuration Works

Understanding _why_ these pieces work is key to mastering your environment.

- **Fixing the `corfu-auto` Conflict:** The original `(wrong-type-argument markerp nil)` error occurs because Corfu's automatic completion timer calls `emacs-jupyter`'s completion function in a context it isn't prepared for. By using the `jupyter-org-interaction-mode-hook` to set `(setq-local corfu-auto nil)`, we disable this problematic timer _only_ when inside a Jupyter block, preventing the error without disabling auto-completion elsewhere.

- **The Necessity of a Smart `TAB`:** With auto-completion turned off, we need a reliable way to manually trigger completions. The `ar/corfu-complete-or-indent` function makes `TAB` intelligent: it first tries to initiate or cycle through completions via `corfu-complete`. If and only if there are no completions available does it fall back to its standard behavior of indenting the line.

- **The Power of `advice-add`:** The best way to modify the behavior of a built-in command like `org-edit-special` is with advice. The `(advice-add 'org-edit-special :after ...)` form ensures our custom function runs reliably _after_ Emacs has fully created the special edit buffer. This is far more robust than using `org-edit-src-code-hook`, which runs _before_ the buffer exists. Our advice function then inspects the original Org block's header to find the `:session` name, allowing it to connect the new buffer to the correct, already-running Jupyter kernel.

## Conceptual Foundation: Runtime vs. Static Analysis

Your new environment gives you the power of two different but complementary paradigms.

#### Runtime Introspection (Jupyter)

This is a **live, stateful** analysis. The Jupyter kernel has executed your code and knows the _actual_ objects in memory. It is essential for data exploration because it provides **data-aware completion**.

#### Static Analysis (LSP / `basedpyright`)

This is a **stateless** analysis. The LSP server reads your code's text but does not run it. It excels at finding errors, enforcing type safety, and understanding code structure based on function signatures and type hints.

#### The Definitive Example

Consider this code, where `my_data.csv` contains a column named `user_id`.

```python
import pandas as pd
df = pd.read_csv('my_data.csv')
df. # <-- Your cursor is here
```

- **Jupyter sees:** `head()`, `describe()`, and `user_id`. It has the real DataFrame object in memory.
- **The LSP sees:** `head()` and `describe()`. It knows the methods of the `DataFrame` class but has never read the CSV file, so it cannot know the column names.

## The Dual-Workflow User Guide

You now have two distinct, powerful workflows for any situation.

#### Workflow A: The Inline Exploratory Loop

This is your "notebook-style" workflow for rapid iteration and data visualization.

- **How:** Edit Python code directly inside the `#+begin_src...` block.
- **Execution:** Press `C-c C-c` or `SPC j e` to run the block.
- **Completion:** Press **`TAB`** to manually request **runtime-aware** completions from the live Jupyter kernel.

#### Workflow B: The Focused Development Loop (The Unified Buffer)

This is your professional environment for writing robust, production-quality code.

- **How:** With your cursor in a source block, press **`C-c '`**. This command, `org-edit-special`, opens the block's code in a new, fully-featured `python-ts-mode` buffer.
- **The Environment:** Our configuration automatically connects this buffer to **both** the LSP and the live Jupyter kernel.
- **Completion:** Auto-completion is enabled here. The Corfu popup will display a **single, merged list of suggestions from both sources**:
  - **Live completions** from Jupyter (DataFrame columns, variable attributes).
  - **Static completions** from `basedpyright` (library functions, type information, diagnostics).

This unified buffer is the pinnacle of this setup, offering unparalleled insight by combining what your code _is_ (from Jupyter) with what your code _should be_ (from the LSP).

## Key Jupyter Keybindings Reference

| Keybinding    | Action            | Description                                      |
| :------------ | :---------------- | :----------------------------------------------- |
| **`SPC j n`** | New src block     | Inserts a new Python source block.               |
| **`SPC j e`** | Eval src block    | Executes the code in the current block.          |
| **`SPC j v`** | View REPL         | Switches to the interactive Jupyter REPL.        |
| **`SPC j C`** | Clear all results | Removes all `#+RESULTS:` blocks in the buffer.   |
| **`SPC j R`** | Restart & Run All | Restarts the kernel and re-evaluates all blocks. |
| **`SPC j r`** | Restart kernel    | Restarts the kernel, clearing its state.         |
| **`SPC j i`** | Interrupt kernel  | Stops a long-running computation.                |
| **`SPC j k`** | Shutdown kernel   | Shuts down the current Jupyter kernel.           |
| **`SPC j X`** | Export to .ipynb  | Exports your Org file to a Jupyter Notebook.     |
