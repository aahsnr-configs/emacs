Here is the updated testing guide. Since Phase 1 and Phase 2 are now verified successes, you can move on to verifying the actual features (LSP and Context).

### **Phase 1: Basic Sanity Checks**
**Status: ✅ COMPLETE SUCCESS**
*Verified via previous screenshots.*
*   `org-src-context-mode` is active (`t`).
*   `org-edit-src-code` is correctly advised.
*   `org-edit-special` is correctly advised.

---

### **Phase 2: Regression Testing (The "Weird" Bugs)**
**Status: ✅ COMPLETE SUCCESS**
*Verified by user.*
*   **Test 2.1 (Ghost Newline):** Pressing `RET` inside a block in the main buffer creates a newline correctly without cursor jumps.
*   **Test 2.2 (Evil Open):** Pressing `o` inside a block in the main buffer opens a new line immediately, enters Insert mode, and places the cursor correctly with zero lag.

---

### **Phase 3: LSP & Eglot Integration**
*Goal: Ensure Eglot connects and finds the project root despite the buffer being temporary.*

#### **Test Case 3.1: The "Mock File" Strategy (No Tangle)**
1.  Create a block **without** a `:tangle` header:
    ```org
    #+begin_src python
    import sys
    print(sys.path)
    #+end_src
    ```
2.  Place cursor inside the block.
3.  Press `C-c '` to open the edit buffer.
4.  **Run Check:** `M-: buffer-file-name RET`.
5.  **Pass Criteria:**
    *   The output matches: `".../your-project-dir/org-src-babel-python.tmp"`.
    *   **Eglot Status:** The modeline should show the LSP server name (e.g., `Pyright` or `Jedi`).
    *   **Verify:** Run `M-x eglot-events-buffer` to see the handshake log.

#### **Test Case 3.2: Real Tangle File**
1.  Create a block **with** a tangle header:
    ```org
    #+begin_src python :tangle ./script.py
    x = 10
    #+end_src
    ```
2.  Press `C-c '`.
3.  **Run Check:** `M-: buffer-file-name RET`.
4.  **Pass Criteria:** output is `".../your-project-dir/script.py"`.

---

### **Phase 4: Context Awareness**
*Goal: Verify that Block B can "see" code in Block A.*

1.  **Block A (Definition):**
    ```org
    #+begin_src python
    my_global_var = "Hello LSP"
    def my_helper():
        pass
    #+end_src
    ```
2.  **Block B (Usage):**
    ```org
    #+begin_src python
    print(my_global_var)
    my_helper()
    #+end_src
    ```
3.  Place cursor on `my_global_var` in **Block B**.
4.  Press `C-c '` to open the edit buffer.
5.  **Action:** Invoke `xref-find-definitions` (usually `M-.` or `gd`).
6.  **Pass Criteria:** The cursor jumps to the definition in Block A. (Note: Block A will appear as read-only text at the top of this temporary edit buffer).

---

### **Phase 5: Performance Benchmarking**
*Goal: Scientifically confirm that the "Allowlist" logic prevents lag.*

Paste this code into your `*scratch*` buffer and run it with `M-x eval-buffer`:

```elisp
(defun my/benchmark-org-indent ()
  "Measure the time taken to indent a line in a source block."
  (interactive)
  (with-temp-buffer
    (org-mode)
    (insert "* Header\n")
    (dotimes (_ 5000) (insert "Some content filler line.\n"))
    (insert "#+begin_src python\ndef test():\n    pass\n#+end_src")
    (search-backward "pass")
    (garbage-collect)
    (let ((start (float-time)))
      (org-indent-line) ;; This triggers the advice
      (message "Indentation took: %.4f seconds" (- (float-time) start)))))
```

*   **Pass Criteria:** The output should be **< 0.01 seconds** (indicating the expensive logic was skipped).
*   **Fail Criteria:** If it takes > 0.1 seconds.
