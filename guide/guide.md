# Mastering Project Navigation & Refactoring: A Practical Guide for Your Emacs Config

This guide demonstrates how to leverage the specific combination of tools currently active in your configuration: **Evil Mode**, **Consult**, **Embark**, **Wgrep**, and **HL-Todo**.

By following these workflows, you can navigate large codebases and perform complex refactoring without ever leaving your keyboard or opening multiple buffers manually.

---

## 1. Setup: Create the Sample Environment

To practice these workflows effectively, create a temporary directory (e.g., `~/tmp/demo/`) and create the following two Python files inside it.

**File 1: `processor.py`**
```python
def calculate_metrics(data):
    """
    Calculates core business metrics.
    FIXME: This function crashes if data is None.
    """
    if not data:
        return 0

    result = 0
    for item in data:
        # TODO: Optimize this loop for large datasets
        result += item.value * 1.5

    return result

def process_batch():
    raw_data = [1, 2, 3]
    # We need to calculate_metrics before export
    final_score = calculate_metrics(raw_data)
    print(f"Batch score: {final_score}")
```

**File 2: `main.py`**
```python
import processor

def main():
    print("Starting system...")
    # Run the processor
    processor.process_batch()

    # Direct call for testing
    test_val = processor.calculate_metrics([10, 20])
    print(f"Test run: {test_val}")

if __name__ == "__main__":
    main()
```

---

## 2. Workflow A: Managing Technical Debt (HL-Todo)

**The Goal:** You want to identify, prioritize, and jump to tasks (TODOs, FIXMEs) scattered across your project without manually opening every file.

**Prerequisite:** Your config automatically highlights keywords like `TODO` (Yellow) and `FIXME` (Red) using `hl-todo`.

### Step 1: Search the Project
Press **`SPC s T`**.
*   **What happens:** This triggers your custom function `hl-todo-rg-project`.
*   **Behind the scenes:** It invokes `consult-ripgrep` but pre-fills the input with a regex matching your defined keywords: `\b(TODO|FIXME|BUG|ISSUE|...)\b`.

### Step 2: Live Filtering
The Minibuffer opens showing every issue in the project.
*   **Action:** Type `FIXME` after the regex.
*   **Result:** The list narrows down instantly. You will see the entry in `processor.py`.

### Step 3: Preview and Jump
*   **Action:** Press **`C-j`** (or `down` arrow).
*   **Result:** Emacs shows a live preview of the code in the main window without closing the search.
*   **Action:** Press **`RET`**.
*   **Result:** You jump directly to the line in `processor.py` to fix the crash.

---

## 3. Workflow B: Project-Wide Refactoring (Consult + Wgrep)

**The Goal:** The function name `calculate_metrics` is vague. You want to rename it to `compute_stats` across the entire project safely and efficiently.

### Step 1: Initiate the Search
Open `processor.py` and place your cursor anywhere on the word `calculate_metrics`.
*   Press **`SPC s p`** (Your binding for `consult-ripgrep`).

### Step 2: Smart Input (The "Future History" Trick)
The Minibuffer opens empty. **Do not re-type the function name.**
*   **Action:** Press **`M-n`** (Meta+n).
*   **Result:** Consult instantly pulls the symbol under your cursor (`calculate_metrics`) into the search bar.
*   **Visual:** You will immediately see 3 matches: the definition in `processor.py` and the usages in `main.py`.

### Step 3: Export Results to a Buffer
You cannot edit the Minibuffer results directly. You need to export them.
*   **Action:** Trigger **Embark Act** (Standard binding is usually `C-.`, `C-;`, or `C-h B`).
*   **Action:** Press **`E`** (Export).
*   **Result:** Emacs creates a new buffer called `*grep*` containing your search results.

### Step 4: Make Editable (Wgrep)
The grep buffer is read-only by default.
*   **Action:** Press **`w`**.
*   **Result:** The buffer header turns green (indicating `wgrep-mode`). You can now edit this buffer as if it were a normal text file.

### Step 5: Mass Replace (Evil Mode)
Since you are in a text buffer, use your Evil powers.
*   **Action:** Type **`:%s/calculate_metrics/compute_stats/g`** and press **`RET`**.
*   **Result:** You will see the text update in the lines for both `processor.py` and `main.py` right there in the grep buffer.

### Step 6: Commit to Disk
*   **Action:** Press **`C-c C-c`**.
*   **Result:**
    1.  `wgrep` applies the changes to the actual files on your hard drive.
    2.  Because you set `(setq wgrep-auto-save-buffer t)`, Emacs saves the modified buffers immediately.
    3.  The refactoring is complete.

---

## Summary of Key Bindings

| Keybinding | Command | Description |
| :--- | :--- | :--- |
| **`SPC s T`** | `hl-todo-rg-project` | Search all TODO/FIXME items in the project. |
| **`SPC s p`** | `consult-ripgrep` | Interactive project-wide text search. |
| **`M-n`** | `next-history-element` | (Inside Minibuffer) Insert the symbol at point. |
| **`C-.`** (or similar) | `embark-act` | Open the context menu for the current item. |
| **`w`** | `wgrep-change-to-wgrep-mode` | (Inside Grep buffer) Make search results writable. |
| **`C-c C-c`** | `wgrep-finish-edit` | Save changes made in the grep buffer to disk. |

## Using Git Gutter

**1. Visuals**
*   Look at the left fringe (margin).
*   **Green Bar:** Added lines.
*   **Orange Bar:** Modified lines.
*   **Red Triangle:** Deleted lines.

**2. Navigation**
*   **`SPC g n`**: Jump to the next change.
*   **`SPC g N`**: Jump to the previous change.

**3. Actions**
*   **Preview (`SPC g p`):** You changed a line in `config.org` but forgot the original value. Press `SPC g p`. A popup shows the diff. Press `q` to close.
*   **Revert (`SPC g R`):** You deleted a TODO item by mistake. Go to the Red marker and press `SPC g R`. It returns immediately.
*   **Stage (`SPC g S`):** You are writing a feature but also fixed a typo. Move to the typo line, press `SPC g S`. That single change is staged. You can now commit just the typo fix via Magit.

**4. Org Mode Specifics**
*   This config forces `git-gutter-mode` on in Org files.
*   **Note:** The file *must* be tracked by git.
*   **Usage:** Useful for seeing which tasks you've added or modified today before you commit your daily journal or todo list.

# Contextual Awareness: A Guide to Your Enhanced Echo Area

This configuration transforms the area at the bottom of Emacs (the Echo Area) into a context-aware heads-up display. It adapts automatically to **any** file type you are editing.

### 1. Global Context Awareness (Any File)
**Scenario:** You are editing a large config file (Apache, XML, TOML) or a text file. You are inside a specific section block but the header is scrolled off-screen.

**How to use:**
*   Place your cursor anywhere inside the section.
*   **Look at the bottom:** You will see a gear icon and the name of the current tag/section:
    `⚙ [Database]` or `⚙ <CustomerData>`
*   **Benefit:** Works everywhere, keeping you oriented in any structured text.

### 2. Navigating Deep Org Hierarchies
**Scenario:** You are editing a large project file (`work.org`) and are deep inside a task list.

**How to use:**
*   Place your cursor on an item.
*   **Look at the bottom:** You see the specific path:
    `⮕ Work Projects › Backend API › v2.0 Migration › Update API`

### 3. Integrated Feedback (LSP + Errors)
**Scenario:** You are writing code with LSP enabled. You have a syntax error, but also need to see the function signature.

**How to use:**
*   **Look at the bottom:** Emacs composes the information together:
    `def process_data(data: dict) -> None`
    `[Error] Unexpected indentation`

### 4. Reading Full Documentation (Fixed)
**Scenario:** The documentation in the echo area is truncated (ends with `...`).

**How to use:**
*   Press **`C-h .`**
*   **Result:**
    *   If docs exist: A separate buffer opens with the full text.
    *   If no docs exist: A message "No full documentation available" appears (instead of an error).

### 5. Mouse Hover (Global Tooltip)
**Scenario:** You want to check an item without moving your text cursor.

**How to use:**
*   **Hover your mouse** over the item.
*   **Result:** A small popup appears with context info.
