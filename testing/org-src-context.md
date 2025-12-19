# Org-Src-Context: Comprehensive Testing Protocol

**Objective:** Verify that the patched `org-src-context.el` eliminates performance regressions (lag, cursor jumping) while successfully providing context to LSP servers (Eglot) in Org Mode.

---

### **Phase 1: Installation & Sanity Checks**
*Goal: Confirm that the local, patched version is loaded and the advice framework is active.*

#### **Test 1.1: Verify Package Activation**
1.  Restart Emacs.
2.  Open any `.org` file.
3.  Run `M-x describe-variable RET org-src-context-mode RET`.
4.  **Pass Criteria:**
    *   Value is `t` (true).
    *   Documentation string matches the local file (e.g., mentions "Performance Optimized").

#### **Test 1.2: Verify Advice Injection**
1.  Run `M-x describe-function RET org-edit-src-code RET`.
2.  **Pass Criteria:**
    *   The help buffer must state: `"This function has :around advice: org-src-context--advice"`.
3.  Run `M-x describe-function RET org-edit-src-exit RET`.
4.  **Pass Criteria:**
    *   The help buffer must state: `"This function has :before advice: org-src-context--cleanup"`.

---

### **Phase 2: Regression Testing (Stability & Input)**
*Goal: Verify that the "Weird Bugs" (cursor jumping, ghost newlines, lag) caused by the upstream package are fixed.*

> **CRITICAL:** Perform these tests in the **Main Org Buffer**, NOT the special edit buffer.

#### **Test 2.1: The "Rapid Newline" Test (Cursor Jumping)**
1.  Create a dummy source block:
    ```org
    #+begin_src python
    def test():
        return True
    #+end_src
    ```
2.  Place your cursor at the end of `def test():`.
3.  Press `RET` (Enter) rapidly 3-5 times.
4.  **Pass Criteria:**
    *   The cursor moves down 1 line per keystroke.
    *   The cursor **does not** jump to the top of the file or random locations.
    *   There is zero perceptible lag.

#### **Test 2.2: The "Ghost Newline" Test (Boundary Stickiness)**
1.  Place your cursor at the very end of the last line of code (right before `#+end_src`).
2.  Press `RET`.
3.  Type `print("end")`.
4.  **Pass Criteria:**
    *   A new line is visually created.
    *   The text `print("end")` appears on the new line.
    *   The text **does not** disappear or get "swallowed" into the `#+end_src` line.

#### **Test 2.3: The "Evil Open" Test (Lag Check)**
1.  Ensure you are in Evil Normal state.
2.  Place cursor inside a source block.
3.  Press `o` (Open new line below).
4.  **Pass Criteria:**
    *   A new line opens immediately.
    *   Editor enters Insert state immediately.
    *   There is no "hiccup" or freeze.

---

### **Phase 3: LSP & Eglot Integration**
*Goal: Ensure Eglot connects and finds the project root despite the buffer being temporary.*

#### **Test 3.1: The "Mock File" Strategy (No Tangle)**
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
    *   The output path ends in `.tmp` (e.g., `.../project/org-src-babel-python.tmp`).
    *   The directory path matches your actual project root.
    *   **Eglot Status:** The modeline shows the LSP server name (e.g., `Pyright` or `Jedi`).

#### **Test 3.2: Real Tangle File**
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
*Goal: Verify that Block B can "see" code in Block A via LSP.*

#### **Test 4.1: Definition Jumping**
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
6.  **Pass Criteria:**
    *   The cursor jumps to the definition of `my_global_var`.
    *   *Note:* Since context injection is read-only, you cannot edit Block A from here, but you should be able to see it and jump to it.

---

### **Phase 5: Performance Benchmarking**
*Goal: Scientifically confirm that the "Allowlist" logic prevents lag.*

Paste this code into your `*scratch*` buffer and run it with `M-x eval-buffer`:

```elisp
(defun my/benchmark-org-indent ()
  "Measure the time taken to indent a line in a source block.
  This replicates the specific call that caused lag in the upstream package."
  (interactive)
  (with-temp-buffer
    (org-mode)
    (insert "* Header\n")
    ;; Create a large enough buffer to force parsing lag if bug exists
    (dotimes (_ 5000) (insert "Some content filler line.\n"))
    (insert "#+begin_src python\ndef test():\n    pass\n#+end_src")
    (search-backward "pass")
    (garbage-collect)
    (let ((start (float-time)))
      ;; This function calls org-edit-src-code internally with a 'code' argument.
      ;; Our patch should detect that argument and SKIP the heavy lifting.
      (org-indent-line)
      (message "Indentation took: %.4f seconds" (- (float-time) start)))))
```

1.  Check the `*Messages*` buffer for the result.
2.  **Pass Criteria:** The output should be **< 0.01 seconds**.
3.  **Fail Criteria:** If it takes > 0.1 seconds, the patch is not ignoring transient calls correctly.

******
***
***
# Phase 4: Advanced Context Awareness Testing

**Objective:** Verify that `org-src-context.el` correctly stitches together split source blocks into a single logical unit for the LSP, regardless of whether you are using "Literate" mode (no tangle) or "Project" mode (tangle).

---

#### **Test 4.1: The "Literate Notebook" (No Tangle Headers)**
*Goal: Verify context works for quick scripts without file generation.*

1.  Create a new Org file (e.g., `test_context.org`).
2.  **Setup:** Create three Python blocks.
   ```org

* Setup
#+begin_src python
class DataProcessor:
    def __init__(self, value):
        self.value = value

    def process(self):
        return self.value * 2
#+end_src

* Analysis
#+begin_src python
# This block initiates the data
my_data = DataProcessor(10)
#+end_src

* Result
#+begin_src python
# We want to use 'my_data' from the block above
final_result = my_data.process()
print(final_result)
#+end_src
    ```
3.  **Action:** Place cursor inside the **3rd block** (Result) and press `C-c '` to open the edit buffer.
4.  **Check A (Completion):**
    *   Type `my_d`.
    *   **Pass Criteria:** `completion-at-point` (Corfu/Company) should suggest `my_data` (variable defined in Block 2).
5.  **Check B (Type Inference):**
    *   Place cursor on `my_data` or `process`.
    *   **Action:** Trigger `eldoc` (or wait for hover).
    *   **Pass Criteria:** The echo area or popup should identify `my_data` as type `DataProcessor` (or `(variable) my_data: DataProcessor`).
6.  **Check C (Definition Jump):**
    *   Place cursor on `process`.
    *   **Action:** Press `M-.` (xref-find-definitions).
    *   **Pass Criteria:** Cursor jumps to `def process(self):` in the **read-only header** of the current edit buffer.

---

#### **Test 4.2: The "Inherited Property" (Project Workflow)**
*Goal: Verify your specific requirement regarding `#+PROPERTY` headers works.*

1.  **Reset Environment:**
    *   You must clear the previous test content to ensure no interference.
    *   **Action:** Press `C-x h` (select all) then `DEL` (backspace/delete) to wipe the buffer clean. Alternatively, kill the current buffer and create a new file named `test_tangle.org`.
2.  **Setup:** Paste the following. Note the Property header at the top.
    ```org
    #+TITLE: Property Test
    #+PROPERTY: header-args :tangle my_script.py

    * Global Config
    #+begin_src python
    GLOBAL_TIMEOUT = 5000
    #+end_src

    * Subroutine
    #+begin_src python
    def connect():
        print(f"Connecting with timeout {GLOBAL_TIMEOUT}")
    #+end_src
    ```
3.  **Action:** Place cursor inside the **Subroutine** block (the second one) and press `C-c '`.
4.  **Check A (Filename mocking):**
    *   Run `M-: (file-name-nondirectory buffer-file-name)`.
    *   **Pass Criteria:** Output is `"my_script.py"` (inherited from the property).
5.  **Check B (Context Injection):**
    *   Scroll to the top of the edit buffer.
    *   **Pass Criteria:** You should see `GLOBAL_TIMEOUT = 5000` grayed out (read-only).
6.  **Check C (LSP Error Check):**
    *   **Action:** Change `GLOBAL_TIMEOUT` to `MISSING_VAR` inside the `print` statement.
    *   **Pass Criteria:** Flymake/Flycheck should underline `MISSING_VAR` as undefined.

---

#### **Test 4.3: The "Broken Chain" (Diagnostics)**
*Goal: Verify that deleting a definition in the Org file immediately breaks the code in the Edit buffer (after refresh).*

1.  Use the setup from **Test 4.1**. (If you are still in Test 4.2 state, wipe the buffer and paste the content from Test 4.1 again).
2.  Open the **3rd block** (Result) with `C-c '`. Verify no errors.
3.  **Action:**
    *   Exit the edit buffer (`C-c '`).
    *   **Delete** the entire 1st block (The `DataProcessor` class definition) in the Org file.
    *   Go back to the 3rd block and press `C-c '` again.
4.  **Pass Criteria:**
    *   The Edit buffer opens.
    *   The `DataProcessor` definition is gone from the top context.
    *   LSP (Eglot) should now flag `my_data.process()` as an error (e.g., "reportUndefinedVariable" or similar), because the class no longer exists in the context.

---

#### **Test 4.4: Polyglot Safety (Language Isolation)**
*Goal: Ensure Python blocks don't try to include Emacs Lisp blocks as context.*

1.  **Reset Environment:** `C-x h` -> `DEL`.
2.  **Setup:** Mix languages.
    ```org
    * Elisp Config
    #+begin_src emacs-lisp
    (defvar my-elisp-var 123)
    #+end_src

    * Python Script
    #+begin_src python
    x = 10
    #+end_src
    ```
3.  **Action:** Open the **Python** block with `C-c '`.
4.  **Pass Criteria:**
    *   Scroll to the top.
    *   You should **NOT** see `(defvar my-elisp-var 123)`.
    *   The context should be empty (or only contain other Python blocks if you added them).
    *   If you see Lisp code inside the Python edit buffer, the language filtering logic is broken.
