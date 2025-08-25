Excellent question. Since `eglot` and `lsp-mode` have fundamentally different design philosophies, there isn't a single, monolithic package like `lsp-ui` for `eglot`.

Instead of providing its own UI, **`eglot`'s philosophy is to integrate with existing, standard Emacs features** (`xref`, `eldoc`, `flymake`, `completing-read`). Therefore, enhancing `eglot`'s UI means enhancing those standard Emacs features.

Here are the best alternatives for each of the main features `lsp-ui` provides, tailored for an `eglot` setup.

### 1. For Documentation on Hover/at Point

- **LSP-UI Feature:** `lsp-ui-doc` shows documentation in a floating child-frame.
- **Eglot's Native Way:** `eglot` uses the built-in **`eldoc`** system, which displays information in the echo area (the minibuffer). This is the most minimal approach.
- **Recommended Eglot Alternative:**
  - **`eldoc-box`**: This is the closest you'll get to `lsp-ui-doc`. It's a lightweight package that takes the information from `eldoc` and displays it in a pop-up box near the point, very similar to `lsp-ui`.
    ```emacs-lisp
    (use-package eldoc-box
      :hook (eglot-managed-mode . eldoc-box-hover-mode))
    ```

### 2. For "Peeking" Definitions and References

- **LSP-UI Feature:** `lsp-ui-peek` shows definitions or references in an inline, temporary window without leaving your current buffer.
- **Eglot's Native Way:** `eglot` uses the built-in **`xref`** system. When you use `xref-find-definitions` (`M-.`), it shows the results in a separate `*xref*` buffer, which you can navigate.
- **Recommended Eglot Alternative:**
  - **`consult-xref`** (part of the `consult` package): This is the most popular and powerful solution. Instead of opening a dedicated `*xref*` buffer, it displays the locations in the minibuffer with live previews. It's not an "inline peek" but is arguably even more efficient for navigating multiple references.
    ```emacs-lisp
    ;; If you use consult, this is the best way.
    (with-eval-after-load 'xref
      (setq xref-show-xrefs-function #'consult-xref
            xref-show-definitions-function #'consult-xref))
    ```

### 3. For Displaying Diagnostics (Errors/Warnings)

- **LSP-UI Feature:** `lsp-ui-sideline` displays diagnostics on the right-hand side of the window, next to the code.
- **Eglot's Native Way:** `eglot` uses the built-in **`flymake`** diagnostics engine. By default, Flymake just adds underlines (squiggles) to the code. You can see the error message by hovering the mouse or using a command like `flymake-show-diagnostics-at-point`.
- **Recommended Eglot Alternative:**
  - **Configure Flymake's UI**: You can make Flymake display diagnostics in the margin or fringe without any extra packages.
    ```emacs-lisp
    ;; Show diagnostics in the fringe (the space to the left of the line numbers)
    (with-eval-after-load 'flymake
      (define-fringe-bitmap 'flymake-fringe-warning-face-bitmap [16] nil nil '(center))
      (set-face-attribute 'flymake-warning nil :fringe 'flymake-fringe-warning-face-bitmap)
      (set-face-attribute 'flymake-note nil :fringe 'flymake-fringe-warning-face-bitmap))
    ```
  - **`flymake-collection`**: A third-party package that provides more advanced UI configurations for Flymake, including sideline-like displays.

### 4. For Code Actions

- **LSP-UI Feature:** `lsp-ui-sideline-show-code-actions` shows a lightbulb or icon in the sideline when actions are available.
- **Eglot's Native Way:** `eglot` uses the standard `completing-read` interface. You run the `eglot-code-actions` command (bound to `C-c .` in `eglot-mode-map`), and it presents the available actions in the minibuffer for you to select.
- **Recommended Eglot Alternative:**
  - There isn't a direct visual replacement because this is core to `eglot`'s minimal philosophy. However, you can make the indicator more visible in your modeline.
    ```emacs-lisp
    ;; Add eglot's status to your modeline to see when actions are available
    (add-to-list 'mode-line-format '(:eval (eglot-mode-line-format)))
    ```

### Summary Table

| LSP-UI Feature            | Eglot's Emacs-Native Way     | Recommended Eglot Alternative/Enhancement                              |
| :------------------------ | :--------------------------- | :--------------------------------------------------------------------- |
| **Doc Popups**            | `eldoc` (in echo area)       | **`eldoc-box`** (for floating popups)                                  |
| **Peek Definition**       | `xref` (in `*xref*` buffer)  | **`consult-xref`** (for minibuffer list with preview)                  |
| **Sideline Diagnostics**  | `flymake` (underlines)       | **Configure `flymake` UI** or use a package like `flymake-collection`. |
| **Headerline Breadcrumb** | None (intentionally omitted) | Use a separate package like `breadcrumb` or `imenu-list`.              |
| **Code Action Lightbulb** | `eglot-code-actions` command | Add **`eglot-mode-line-format`** to your modeline.                     |

In short, to get a feature-rich `eglot` experience that feels like `lsp-ui`, you would assemble a stack of smaller, more focused packages:

- **`eglot`** for the LSP connection.
- **`consult`** for enhanced `xref` and symbol navigation.
- **`eldoc-box`** for documentation popups.
- And potentially some custom **`flymake`** configuration for diagnostics.
