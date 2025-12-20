Here are the results of the two tasks.

### Task 1: Updated Testing Guidelines

--- START OF FILE org-src-context-test.md ---

# Org-Src-Context: Comprehensive Testing Protocol (v0.9)

**Objective:** Verify that `org-src-context.el` (Version 0.9) eliminates performance regressions while successfully providing context to LSP servers.

---

### **Phase 1: Installation & Sanity Checks**
**Status: ⚠️ REQUIRES RETESTING (Verify v0.9 Load)**

*Goal: Confirm that the local v0.9 is loaded and advice is active.*

#### **Test 1.1: Verify Package Version & Description**
1.  Restart Emacs.
2.  Open any `.org` file.
3.  Run `M-x describe-function RET org-src-context-mode RET`.
4.  **Pass Criteria:**
    *   The documentation string must contain: `**Performance Optimized** version by Ahsanur Rahman.`
    *   Confirm variables match v0.9 defaults (e.g., `org-src-context-max-filesize`).

#### **Test 1.2: Verify Advice Injection**
1.  Run `M-x describe-function RET org-edit-src-code RET`.
2.  **Pass Criteria:**
    *   Output: `This function has :around advice: org-src-context--advice`.
3.  Run `M-x describe-function RET org-edit-src-exit RET`.
4.  **Pass Criteria:**
    *   Output: `This function has :before advice: org-src-context--cleanup`.

---

### **Phase 2: Regression Testing (Stability & Input)**
**Status: ⚠️ REQUIRES RETESTING (Verify v0.9 Stability)**

*Goal: Verify that the "Weird Bugs" (cursor jumping, ghost newlines, lag) remain fixed in v0.9.*

> **CRITICAL:** Perform these tests in the **Main Org Buffer**.

#### **Test 2.1: The "Rapid Newline" Test (Cursor Jumping)**
1.  Create a dummy source block (`def test(): return True`).
2.  Place cursor at the end of the code.
3.  Press `RET` rapidly 5 times.
4.  **Pass Criteria:** Cursor moves down 1 line per keystroke; no jumping to top of file; zero lag.

#### **Test 2.2: The "Ghost Newline" Test (Boundary Stickiness)**
1.  Place cursor at the very end of the last line of code (right before `#+end_src`).
2.  Press `RET`, then type `print("end")`.
3.  **Pass Criteria:**
    *   A new line is visually created.
    *   The text appears correctly and does not get "swallowed" into the `#+end_src` line.
    *   The cursor does not snap back to the previous line.

#### **Test 2.3: The "Evil Open" Test (Lag Check)**
1.  In Evil Normal state, place cursor inside a source block.
2.  Press `o`.
3.  **Pass Criteria:** Immediate entry into Insert mode on a new line with no freeze.

---

### **Phase 3: LSP & Eglot Integration**
**Status: ⚠️ REQUIRES RETESTING (Verify v0.9 File Mocking)**

*Goal: Ensure Eglot connects and finds the project root.*

#### **Test 3.1: The "Mock File" Strategy (No Tangle)**
1.  Create a block without `:tangle`.
2.  Open edit buffer (`C-c '`).
3.  **Pass Criteria:**
    *   `M-: buffer-file-name` returns a path ending in the correct extension (e.g., `.py` for Python), **not** `.txt`.
    *   Eglot connects successfully.

#### **Test 3.2: Real Tangle File**
1.  Create a block with `#+begin_src python :tangle ./script.py`.
2.  Open edit buffer (`C-c '`).
3.  **Pass Criteria:** `buffer-file-name` is exactly `.../script.py`.

---

### **Phase 4: Advanced Context Awareness**
**Status: ✅ PASSED (Verified on v0.9)**

*Goal: Verify that `org-src-context.el` correctly stitches together split source blocks into a single logical unit.*

#### **Test 4.1: The "Literate Notebook" (No Tangle Headers)**
1.  **Setup:**
    *   Block 1: Define `class DataProcessor`.
    *   Block 2: Instantiate `my_data = DataProcessor(10)`.
    *   Block 3: Call `my_data.process()`.
2.  **Action:** Edit Block 3 (`C-c '`).
3.  **Pass Criteria:**
    *   Completion (`my_d...`) works.
    *   Eldoc/Hover shows correct type signature for `process()`.
    *   `M-.` jumps to definition in the read-only header.

#### **Test 4.2: The "Inherited Property" (Project Workflow)**
1.  **Setup:** Use `#+PROPERTY: header-args :tangle my_script.py` at top of file. Define `GLOBAL_TIMEOUT` in Block 1, use it in Block 2.
2.  **Action:** Edit Block 2.
3.  **Pass Criteria:**
    *   `buffer-file-name` matches `my_script.py`.
    *   LSP identifies `GLOBAL_TIMEOUT` as a known constant/variable.

#### **Test 4.3: The "Broken Chain" (Diagnostics)**
1.  **Action:** Delete Block 1 (Definition) in the Org file. Re-open Block 3.
2.  **Pass Criteria:** LSP marks the usage of the deleted class/variable as an **Error** or **Undefined**.

#### **Test 4.4: Polyglot Safety (Language Isolation)**
1.  **Setup:** Create an Elisp block defining a var, and a Python block.
2.  **Action:** Edit Python block.
3.  **Pass Criteria:** The Elisp code is **NOT** present in the injected context header.

---

### **Phase 5: Performance Benchmarking**
**Status: ⚠️ REQUIRES RETESTING (Verify v0.9 Efficiency)**

*Goal: Confirm that the "Allowlist" logic prevents lag.*

Run the following benchmark in `*scratch*`:
```elisp
(defun my/benchmark-org-indent ()
  (interactive)
  (with-temp-buffer
    (org-mode)
    (insert "* Header\n")
    (dotimes (_ 5000) (insert "filler.\n"))
    (insert "#+begin_src python\ndef test():\n    pass\n#+end_src")
    (search-backward "pass")
    (let ((start (float-time)))
      (org-indent-line) 
      (message "Time: %.4f" (- (float-time) start)))))
```
*   **Pass Criteria:** Time < 0.01 seconds.
