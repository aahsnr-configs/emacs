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
