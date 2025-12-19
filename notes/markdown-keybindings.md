```emacs-lisp
;; Add this to the General Keybindings section under Local Leader

(ar/local-leader
  :keymaps 'markdown-mode-map
  "," '(markdown-do :wk "Do")
  "m" '(markdown-toggle-markup-hiding :wk "Toggle markup")
  "p" '(markdown-preview :wk "Preview")
  "e" '(markdown-export :wk "Export")
  "i" '(:ignore t :wk "insert")
  "i l" '(markdown-insert-link :wk "Link")
  "i i" '(markdown-insert-image :wk "Image")
  "i c" '(markdown-insert-code :wk "Code")
  "i b" '(markdown-insert-bold :wk "Bold")
  "i e" '(markdown-insert-italic :wk "Italic")
  "i t" '(markdown-insert-table :wk "Table")
  "t" '(markdown-toc-generate-toc :wk "Generate TOC")
  "c" '(markdown-check-refs :wk "Check refs"))
```
