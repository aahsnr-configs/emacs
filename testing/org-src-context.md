Based on the analysis of `org-src-contextv0.9.1.txt`, here is a comprehensive testing guide. This guide covers installation, stability, performance, and the specific architectural logic (unified collection, marker tracking, and LSP mocking) implemented in the package.

---

# Org-Src-Context (v0.9.1) Testing Protocol

**Objective:** Verify that `org-src-context.el` correctly stitches disjointed source blocks into a cohesive unit for LSP servers without introducing editor instability or performance regressions.

**Prerequisites:**
1.  Emacs 30.1+ and Org Mode 9.6+.
2.  **Eglot** configured and working.
3.  **Python** installed (with a language server like `pyright` or `pylsp`) for the sample code.

---

## **Phase 1: Installation & Hooks Verification**
*Goal: Confirm the package is loaded and has successfully hooked into Org Mode's edit cycle.*

### **Test 1.1: Mode Activation**
1.  Open any `.org` file.
2.  Run `M-x org-src-context-mode` to ensure it is enabled.
3.  Run `M-x describe-variable RET org-src-context-mode RET`.
4.  **Pass Criteria:**
    *   Value is `t`.
    *   Documentation string mentions: `**Performance Optimized** version`.

### **Test 1.2: Advice Registry**
The package relies on Emacs' advice system to intercept buffer creation.
1.  Run `M-x describe-function RET org-edit-src-code RET`.
    *   **Criteria:** Output includes: `This function has :around advice: org-src-context--advice`.
2.  Run `M-x describe-function RET org-edit-src-exit RET`.
    *   **Criteria:** Output includes: `This function has :before advice: org-src-context--cleanup`.

---

## **Phase 2: Editor Stability (The "Weird Bugs" Check)**
*Goal: Verify that the Marker-based tracking and text properties (stickiness) correctly prevent cursor glitches.*

### **Test 2.1: The "Ghost Newline" & Boundary Stickiness**
*Logic Tested:* The code sets `front-sticky nil` on the footer and `rear-nonsticky (read-only)` on the header. This test ensures you don't get stuck in read-only zones.

1.  Create a block:
    ```org
    #+begin_src python
    print("start")
    print("end")
    #+end_src
    ```
2.  Enter the block (`C-c '`).
3.  Move cursor to the **very last character** (after `")` on the second line).
4.  Press `RET`.
5.  Type `print("new line")`.
6.  **Pass Criteria:**
    *   A new line is created visually.
    *   The cursor stays on the new line (does not snap back up).
    *   The text is editable (not read-only).

### **Test 2.2: Rapid Entry (Marker Stability)**
*Logic Tested:* Markers should update automatically. If the logic used integers, rapid typing would cause the view to jump.

1.  Enter a block (`C-c '`).
2.  Hold down `RET` to insert 20 new lines rapidly.
3.  **Pass Criteria:**
    *   The editor should scroll smoothly.
    *   The view should **not** jump to the top of the buffer.
    *   The surrounding context (header/footer) should remain hidden/greyed out.

---

## **Phase 3: Context & LSP Logic**
*Goal: Verify the "Unified Collector Strategy" and "Extension Mapping".*

### **Test 3.1: Literate Notebook (Untangled Context)**
*Logic Tested:* Blocks with no `:tangle` header should still see each other if they share a language.

1.  **Setup:** Create a new file `test_literate.org`:
    ```org
    * Setup
    #+begin_src python
    class Dog:
        def bark(self):
            return "Woof"
    #+end_src

    * Implementation
    #+begin_src python
    my_dog = Dog()
    #+end_src
    ```
2.  **Action:** Edit the **Implementation** block (`C-c '`).
3.  **Action:** Place cursor on `Dog` and press `M-.` (Go to Definition).
4.  **Pass Criteria:**
    *   Cursor jumps to `class Dog` in the (read-only) header section of the same buffer.
    *   Eldoc/Hover on `my_dog` shows type `Dog`.

### **Test 3.2: Tangle & Property Inheritance**
*Logic Tested:* The code resolves `#+PROPERTY` headers to find the tangle target.

  1.  **Setup:** Create `test_project.org`:
      ```org
      #+PROPERTY: header-args :tangle main.py

      * Config
      #+begin_src python
      TIMEOUT = 5000
      #+end_src

      * Main
      #+begin_src python
      print(TIMEOUT)
      #+end_src
      ```
2.  **Action:** Edit the **Main** block.
3.  **Check A (Mocking):** Run `M-: (file-name-nondirectory buffer-file-name)`.
    *   **Criteria:** Output is `"main.py"` (Derived from the property).
4.  **Check B (Context):** Verify `TIMEOUT` is recognized (not flagged as undefined).

### **Test 3.3: Polyglot Isolation**
*Logic Tested:* Python blocks should not see Elisp context.

1.  **Setup:**
    ```org
    #+begin_src emacs-lisp
    (defvar my-secret-var 123)
    #+end_src

    #+begin_src python
    x = 1
    #+end_src
    ```
2.  **Action:** Edit the **Python** block.
3.  **Action:** Scroll to the top of the edit buffer (you may need to `M-x widen` if narrowing is enabled).
4.  **Pass Criteria:** The Elisp block `(defvar my-secret-var 123)` must **NOT** be present in the buffer.

---

## **Phase 4: Deduplication Logic**
*Logic Tested:* The code explicitly compares `blk-start` vs `current-blk-start` to prevent the code you are editing from appearing twice (once as context, once as editable).

1.  **Setup:**
    ```org
    #+begin_src python
    def duplicate_test():
        pass
    #+end_src
    ```
2.  **Action:** Edit the block.
3.  **Action:** Disable narrowing temporarily (`M-x widen`).
4.  **Pass Criteria:**
    *   You should see the mock header (if any previous blocks existed).
    *   You should see your editable code.
    *   You should **NOT** see a grayed-out, read-only copy of `def duplicate_test():` immediately above your editable code.

---

## **Phase 5: Performance Benchmarking**
*Goal: Verify the "Lazy Execution" check.*
*Logic Tested:* `(if (car args) (apply orig-fn args) ...)` ensures context logic is skipped during automated operations.

1.  **Setup:** Copy this elisp into your `*scratch*` buffer.
    ```elisp
    (defun benchmark-context-overhead ()
      (interactive)
      (with-temp-buffer
        (org-mode)
        ;; Create a "large" buffer
        (dotimes (_ 2000) (insert "filler text\n"))
        (insert "#+begin_src python\n  print('hello')\n#+end_src\n")
        (search-backward "print")

        ;; Measure indentation time.
        ;; org-indent-line calls org-edit-src-code with a 'code' argument.
        ;; This should trigger the "Lazy Execution" bypass.
        (let ((start (float-time)))
          (org-indent-line)
          (message "Operation took: %.6f seconds" (- (float-time) start)))))
    ```
2.  **Action:** `M-x eval-buffer`, then `M-x benchmark-context-overhead`.
3.  **Pass Criteria:** Output time should be **< 0.01 seconds**.
    *   *Note:* If the bypass failed and it tried to parse the 2000 lines for context, it would likely take > 0.05s or > 0.1s.
