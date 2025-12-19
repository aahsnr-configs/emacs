For Test Case 3.1, now when I execute C-c ' inside

```org
#+begin_src python
import sys
print(sys.path)
#+end_src
```

eglot connects to pyright language server correctly as seen in the 1st screenshot and M-: buffer-file-name RET gives the output in the 2nd screenshots and the M-x eglot-events-buffer gives the output in the 3rd screenshot. But when I create the other python source code block with

```org
#+begin_src python :tangle ./script.py
x = 10
#+end_src
```

I get the error in the 4th screenshot when I press C-c ' and so eglot does not connect server to that code block.
