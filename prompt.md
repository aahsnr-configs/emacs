## **Python Prompt**

**Objective:** To conduct a thorough review, refactoring, and optimization of the provided Doom Emacs configuration files (`init.el`, `packages.el`, `config.el`). The goal is to create a highly efficient, stable, and powerful development environment that is idiomatic to the Doom Emacs framework, with a special focus on creating a state-of-the-art Python IDE experience.

---

### **Key Areas of Focus:**

**1. Streamlining and Decluttering (Redundancy Elimination)**

- **Analyze Core Modules:** Scrutinize the `(doom! ...)` block in `init.el`. Cross-reference the enabled modules and flags (e.g., `(lsp +eglot)`, `(python +lsp +tree-sitter)`) with the custom configurations in `packages.el` and `config.el`.
- **Identify Overlaps:** Pinpoint and remove any manual package installations (`package!`) or configurations (`use-package!`, `add-hook!`) for features that are already provided, configured, or autoloaded by a Doom module. For instance, if a module's default setup suffices, custom code for it should be removed.
- **Consolidate Package Management:** Ensure all non-Doom packages are explicitly declared in `packages.el` and that any `:ignore t` or `:disable t` declarations are justified and necessary.

**2. Python IDE Overhaul: A Unified, High-Performance Environment**

This is the central focus. The aim is to integrate a specific, modern toolchain for an unparalleled IDE experience for `.py` files, ensuring each component has a distinct and complementary role.

- **A. Language Server (Code Intelligence):**
  - **Client:** Solidify `eglot` as the LSP client, as enabled by the `(:tools lsp +eglot)` module.
  - **Server:** Explicitly configure `eglot` to use `basedpyright` as the Python language server. This server is chosen for its performance and comprehensive features, including precise type inference and code navigation. Ensure Doom's LSP setup correctly identifies and launches it for Python projects.

- **B. Code Formatting (On-Save Consistency):**
  - **Engine:** Leverage `apheleia`, Doom's modern, asynchronous formatting engine provided by the `:editor (format +onsave)` module.
  - **Tool:** Configure `ruff` as the _sole_ formatting tool. Utilize its powerful capabilities to replace the need for separate formatters. Specifically, configure it to handle both general code formatting (equivalent to Black) and import sorting (replacing `isort`). This unification under a single, high-speed Rust-based tool is a key optimization. The `set-formatter!` macro should be used for this integration.

- **C. Linting & Diagnostics (Real-time Feedback):**
  - **Engine:** Utilize `flymake` for on-the-fly syntax checking, as enabled by the `(:checkers syntax +flymake)` module.
  - **Toolchain:** Implement a dual-backend approach for `flymake`:
    - **`ruff`:** For instantaneous linting of a wide range of stylistic and logical errors.
    - **`mypy`:** For deeper, more comprehensive static type analysis.
  - Ensure both backends can run concurrently without conflict, providing immediate feedback from `ruff` and more thorough type-checking from `mypy`.

- **D. Debugging (Interactive Analysis):**
  - **Client:** Configure `dap-mode`, enabled via the `(:tools debugger)` module (with the `+lsp` flag for better integration).
  - **Server:** Critically, ensure `dap-mode` is configured to use `debugpy` as the Python debug adapter server. This is non-negotiable for a modern debugging experience, enabling full support for breakpoints, variable inspection, stepping, and a debug console. The configuration should set `dap-python-debugger` to `'debugpy`.

**3. Seamless Jupyter Integration within Org Mode**

- **Review and Refine:** Examine the existing configuration for `:lang (org +jupyter)`. While the current setup is a good start, ensure it aligns perfectly with Doom's conventions.
- **Optimization:** Confirm that kernel management is efficient and that the interactive experience within Org source blocks is fluid. Remove any workarounds or settings that are now handled by default in the latest versions of Doom's Org and Jupyter modules.

**4. Global Performance and Efficiency Tuning**

- **Startup Time:** Analyze the configuration for any potential bottlenecks affecting startup speed. This includes reviewing the use of `after!` and `with-eval-after-load` to ensure packages are loaded as lazily as possible.
- **UI Responsiveness:** While the existing UI choices are good, double-check for known performance-intensive features. Suggest alternatives if any part of the UI configuration is known to cause sluggishness (e.g., certain modes or graphical embellishments).
- **Garbage Collection:** Introduce a standard Doom Emacs garbage collection tuning snippet to minimize pauses during interactive use, especially during demanding tasks like LSP communication and linting.

**5. Adherence to Doom Emacs Idioms and Best Practices**

- **Syntax and Style:** All revised code must strictly adhere to Doom Emacs's established configuration syntax (e.g., `doom!`, `package!`, `use-package!`, `map!`, `setq-default`, `add-hook!`).
- **Error Prevention:** The final configuration must be free of syntax errors and load cleanly after running `doom sync`.
- **Clarity and Readability:** The final `config.el` (or `config.org` if literate) should remain well-organized and commented, explaining the "why" behind significant configurations, especially for the new Python toolchain.

---

## ** LaTeX Prompt**

You are an expert in configuring Doom Emacs. First study the attached doom emacs configuration files carefully. Your task is to provide a comprehensive configuration guide for writing scientific documents. This guide will modify the standard Doom Emacs configuration files (`init.el`, `packages.el`, and `config.el`) to create a powerful and efficient environment for authors working with Org mode and `.tex` files.

The configuration should be modular and well-documented. Please provide the necessary Elisp code snippets for each section and specify which file they should be placed in.

Here are the detailed requirements:

**1. `init.el` Configuration**

Ensure the following Doom Emacs modules are enabled to support the required features. Please provide the correct `:lang` and `:tools` entries.

- **Essential Modules:** Enable the `org` module with the `+dragndrop` and `+present` flags for enhanced usability.
- **LaTeX Support:** Enable the `latex` module (`:lang latex`) with the `+lsp` flag to integrate the LSP server.
- **Spell Checking:** Enable the `spell` module (`:checkers spell`) and specify the `+jinx` flag to use Jinx as the backend.
- **LSP:** Ensure the core `lsp` module (`:tools lsp`) is enabled.

**2. `packages.el` Configuration**

The following packages are required. Please provide the correct `package!` declarations for `~/.doom.d/packages.el`.

- **Texpresso:** Add the `texpresso` package, installed directly from its GitHub repository (`let-def/texpresso`). Use the `:recipe` keyword to specify the source.

**3. `config.el` Configuration**

This is where the main customization will happen. Please provide Elisp code to achieve the following:

- **LSP for LaTeX (Texlab):**
  - Configure the `texlab` LSP server to work with the `+lsp` flag in the `latex` module.
  - Ensure that `texlab` is automatically started for `.tex` files.

- **Texpresso Integration:**
  - Create a custom interactive function, for example, `my/open-texpresso`, that launches the `texpresso` live preview.
  - Bind this function to a convenient keymap in `tex-mode` and `org-mode`, such as `C-c t p`.

- **Formatting and Linting:**
  - **Linters:** Configure `chktex` as the default linter for LaTeX files.
  - **Formatters:** Configure `latexindent` as the code formatter for `.tex` files. Set it to format on save.

- **Jinx Spell Checker:**
  - Configure `jinx` to be the spell-checking engine for `org-mode` and `latex-mode`.
  - Set up `jinx-mode` to start automatically in these modes.
  - Provide guidance on how a user would add words to their personal dictionary.

- **Enhanced Org Mode for Scientific Writing:**
  - **Inline LaTeX Previews:** Enable and configure `org-fragtog` to automatically preview LaTeX fragments within Org mode buffers.
  - **Citation Management:** Configure `org-cite` to work with a user-specified BibTeX file (e.g., `~/references.bib`). Show how to define a standard citation export style for LaTeX output.
  - **Exporting:** Briefly explain how an Org file configured this way would be exported to a PDF, leveraging the LaTeX backend.

- **General Enhancements:**
  - **PDF Viewer:** Provide instructions on how to set up forward and inverse search between Emacs and a PDF viewer. Give examples for popular viewers like Skim (macOS) or Zathura (Linux).
  - **Keybindings:** Suggest a few quality-of-life keybindings for common scientific writing tasks, such as inserting citations or cross-references.
