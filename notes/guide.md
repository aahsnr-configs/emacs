
### Project-Based Bibliography Management

Instead of a single global bibliography, we use Emacs's built-in `.dir-locals.el` feature. This allows you to specify different `.bib` files for each writing project, making your setup modular and portable.

**How to Use:**

In the root directory of your LaTeX project, create a file named `.dir-locals.el` with the following content. Emacs will automatically apply these settings to all files within that directory.

**Example `~/Projects/MyPaper/.dir-locals.el`:**
```emacs-lisp
;; This is a sample .dir-locals.el file.

((latex-mode . ((citar-bibliography . ("references.bib" "extra-notes.bib"))
                 (reftex-default-bibliography . ("references.bib" "extra-notes.bib"))))

 (org-mode . ((citar-bibliography . ("references.bib" "extra-notes.bib")))))
```
*   This example tells `citar` and `reftex` to use `references.bib` and `extra-notes.bib` for this specific project.
*   You can list as many `.bib` files as you need.

