# LaTeX Writing

## `requirements`

Of course. Based on the provided `init.txt` file, here is a comprehensive list of all the external requirements needed to make your LaTeX writing environment fully functional.

I have broken them down by category to clarify what each component does and why it is necessary.

### Summary of Requirements

| Tool                       | Purpose                    | Required by Emacs Package(s) |
| :------------------------- | :------------------------- | :--------------------------- |
| **Tectonic**               | LaTeX Compiler             | `tex` (AUCTeX)               |
| **Texlab**                 | Language Server (LSP)      | `eglot`                      |
| **ChkTeX**                 | Style & Syntax Linter      | `flymake` (via AUCTeX)       |
| **latexindent**            | Code Auto-formatter        | `apheleia`                   |
| **Perl**                   | Runtime for `latexindent`  | `apheleia`                   |
| **Zotero**                 | Reference Manager          | `citar`                      |
| **Better BibTeX (BBT)**    | Zotero Extension           | `citar`                      |
| **PDF-Tools Dependencies** | C/C++ Compiler & Libraries | `pdf-tools`                  |

---

### Detailed Breakdown

#### 1. Core Compilation System

This is the engine that turns your `.tex` source files into a PDF.

- **Tectonic**
  - **What it is:** A modern, all-in-one LaTeX compiler. It automatically downloads required packages and runs as many times as needed to resolve all references.
  - **Why it's needed:** Your configuration explicitly sets Tectonic as the default TeX engine with `(setq TeX-engine 'tectonic)` and defines commands like `"Tectonic"` and `"Tectonic Watch"`. Without this, you cannot compile any documents.
  - **How to get it:** It is typically installed via your system's package manager or with `cargo` (the Rust package manager).
    ```bash
    # Example with cargo
    cargo install tectonic
    ```

#### 2. Language Server & Diagnostics

These tools provide live feedback, intelligent completions, and error checking as you type.

- **Texlab**
  - **What it is:** A Language Server Protocol (LSP) implementation for LaTeX.
  - **Why it's needed:** Your Eglot configuration `(add-to-list 'eglot-server-programs ... '("texlab"))` tells Emacs to run `texlab` in the background to provide code actions, smart completions, and diagnostics.
  - **How to get it:**
    ```bash
    # Example with cargo
    cargo install texlab
    ```

- **ChkTeX**
  - **What it is:** A command-line tool that checks for common stylistic errors and typographic mistakes in LaTeX source code.
  - **Why it's needed:** The line `(flymake-add-checker 'tex-chktex)` in your configuration activates Flymake's built-in support for ChkTeX, providing on-the-fly linting.
  - **How to get it:** It is available in most system package managers.

    ```bash
    # Example for Debian/Ubuntu
    sudo apt-get install chktex

    # Example for macOS (Homebrew)
    brew install chktex
    ```

#### 3. Code Formatting

This tool keeps your source code clean and consistently formatted.

- **latexindent**
  - **What it is:** A powerful and highly configurable Perl script for formatting LaTeX code.
  - **Why it's needed:** Your `apheleia` configuration is set up to use `"latexindent"` as the formatter for both `.tex` files and Org LaTeX blocks.
  - **How to get it:** It is usually installed as part of a complete TeX Live distribution or can be installed from CTAN. Since it is a Perl script, it also requires a working **Perl** installation with specific modules. The easiest way is often via your TeX distribution's package manager.
    ```bash
    # Example with TeX Live's manager
    tlmgr install latexindent
    ```

#### 4. Citation Management

This ecosystem connects your reference manager (Zotero) to Emacs for seamless citation.

- **Zotero Desktop Application**
  - **What it is:** Your reference management software.
  - **Why it's needed:** Your `citar` configuration points to `~/Zotero/storage` to find attached PDF files. This entire workflow depends on having a Zotero library.

- **Better BibTeX for Zotero (BBT) Extension**
  - **What it is:** A crucial third-party extension for the Zotero application.
  - **Why it's needed:** Your workflow relies on having `.bib` files for each project. BBT is the standard tool for automating the export of your Zotero library into `.bib` files. It generates stable citation keys and can be configured to keep the `.bib` files automatically in sync with your library. You would use it to export collections for each of your projects.

#### 5. PDF Viewing

This is required to view the compiled PDF output and for SyncTeX (clicking in the PDF to jump to the source code, and vice-versa).

- **PDF-Tools Dependencies**
  - **What they are:** The `pdf-tools` package in Emacs is not self-contained; it works by compiling a server program on your machine. This compilation process has its own dependencies.
  - **Why it's needed:** To build the server executable, you need:
    1.  A **C/C++ compiler** (like `gcc` or `clang`).
    2.  The **Poppler** library development files (e.g., `libpoppler-glib-dev`).
  - **How to get them:**

    ```bash
    # Example for Debian/Ubuntu
    sudo apt-get install build-essential libpoppler-glib-dev

    # Example for macOS (Homebrew)
    brew install poppler
    ```

Once these external programs and tools are installed and available in your system's `PATH`, your Emacs LaTeX environment should work exactly as configured.

## `.dir-locals.el`

You need to tell Emacs the path to that project's `.bib` file.

The standard and most robust Emacs method for this is to place a file named `.dir-locals.el` in the root directory of your project.

> **Note:** This **complements** `direnv`, it does not conflict with it. `direnv` is for setting shell environment variables, while `.dir-locals.el` is for setting Emacs-specific variables for buffers within a project. They work great together.

For each project, create a `.dir-locals.el` file with the following content. Just change the name of the `.bib` file to match your project's bibliography.

```emacs-lisp
;; .dir-locals.el
((latex-mode . ((citar-bibliography . ("my-project-bibliography.bib"))))
 (org-mode   . ((citar-bibliography . ("my-project-bibliography.bib")))))
```

When you open any `.tex` or `.org` file in that project directory, Emacs will automatically read this file and set the `citar-bibliography` variable for that buffer only. `citar` will instantly know which bibliography to use for that specific project.
