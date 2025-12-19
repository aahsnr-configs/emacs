# Markdown Configuration Test

This guide provides a structured protocol to verify that your **Marksman LSP**, **Flymake Syntax Checking**, and **Apheleia Formatting** are working in harmony.

### Phase 1: Environment & Binary Verification
Before opening Emacs, ensure the language server is accessible to your system.

1.  **Check Marksman Binary:** Open your terminal and run:
    ```bash
    marksman --version
    ```
    *If this fails, the LSP will not start. Ensure it is in your `$PATH`.*
2.  **Verify Emacs Environment:** Inside Emacs, run `M-x exec-path-from-shell-initialize` (or check the variable `exec-path`) to ensure Emacs sees the same binary.

---

### Phase 2: LSP Connectivity (Eglot)
1.  **Open a Test File:** Create a new file named `test_pivotal.md`.
2.  **Verify Activation:** Look at your modeline. You should see `[Eglot:marksman]`.
    *   *Debug:* If you see `[Eglot: (no-server)]`, run `M-x eglot` manually and check the `*Eglot events*` buffer for errors.
3.  **Test Imenu/Symbols:** Press `SPC s i` (Consult Imenu). You should see a list of headers provided by Marksman.
4.  **Test Cross-References:** Create two files. In `file1.md`, create a header `# Section 1`. In `file2.md`, type `[[file1#Section 1]]`.
    *   Put your cursor on the link and press `M-.` (or `g d`). Eglot should jump you to the specific header in the other file.

---

### Phase 3: Syntax Checking (Flymake & Popon)
Marksman is unique because it validates **wiki-links** and **header references**.

1.  **Trigger a Tooltip:** Type a broken link: `[Broken Link](#non-existent-header)`.
2.  **Verify Fringe:** A **Doom-style double arrow** icon should appear in the left fringe.
3.  **Verify Popon:** Hover your cursor over the broken link. After 0.2 seconds, a floating `posframe` tooltip should appear stating: *'Header not found: non-existent-header'*.
4.  **Test Navigation:** Move your cursor to the top of the file and press `] e`. The cursor should jump directly to the broken link. Press `[ e` to jump back.
5.  **Test Dashboard:** Press `SPC c B` to open the Flymake Diagnostics buffer. Verify that the error is listed with a **Nerd Icon** (the `nf-fa-times_circle`).

---

### Phase 4: Formatting (Apheleia & Eglot Bridge)
This tests the "pivotal" asynchronous bridge we built.

1.  **Mess up Indentation:** Create a nested list with inconsistent spacing:
    ```markdown
    - Item 1
       - Broken Indent
      - Another Item
    ```
2.  **Save the Buffer:** Press `SPC f s`.
    *   **Success Criteria:** The list should instantly snap to correct 2-space indentation.
    *   **The "No-Jump" Test:** While the file is saving, keep your cursor in the middle of a word. If the formatting happens without your cursor jumping to the top of the file, **Apheleia** is working correctly.
3.  **Manual Force:** Delete the space between a header and its text: `#Header`. Press `SPC c F` (Manual LSP Format). Marksman should fix it to `# Header`.

---

### Phase 5: UI & Rendering (Markdown-mode)
1.  **Markup Hiding:** Type `**This is bold**`. Move your cursor away. The `**` should disappear. Move your cursor back onto the word; the `**` should reappear.
2.  **Checkbox Interaction:** Type `- [ ] Task 1`. Move to the next line and type `- [X] Task 2`.
    *   Verify that `[ ]` looks like an empty square `☐` and `[X]` looks like a filled square `☑`.
3.  **Table of Contents:** Press `M-x markdown-toc-generate-toc`. A "Table of Contents" section should appear.

---

### Troubleshooting Checklist
| Issue | Potential Cause | Fix |
| :--- | :--- | :--- |
| **No "marksman" in modeline** | Binary missing or hook failed. | Check `M-x eglot-events-buffer`. |
| **Formatting jumps the cursor** | Apheleia is bypassed; Eglot is formatting natively. | Ensure `apheleia-mode` is enabled in the buffer. |
| **No floating tooltips** | `flymake-popon` not active. | Run `M-x flymake-popon-mode`. |
| **Markup won't hide/show** | `post-command-hook` conflict. | Check if `markdown-hide-markup` is `t`. |
| **LSP formatting fails** | Marksman doesn't support it. | Run `M-: (eglot-server-capable :documentFormattingProvider)`; should return `t`. |
