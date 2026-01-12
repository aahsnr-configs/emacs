  ;; EGLOT & MARKSMAN SETUP
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '(markdown-mode . ("marksman"))))

  ;; FORMATTING INTEGRATION
  (with-eval-after-load 'apheleia
    (setf (alist-get 'markdown-mode apheleia-mode-alist) '(eglot)))
