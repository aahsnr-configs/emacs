Based on my comprehensive research, I can now provide you with a detailed explanation of why those LSP features are problematic for org-src buffers:

## Why LSP Features Are Problematic in Org-Src Buffers

### 1. **`lsp-headerline-breadcrumb-mode` - Critical Crash Issue**

The headerline breadcrumb mode causes crashes in org-edit-special buffers because org-mode sets `header-line-format` to a string rather than a list, but `lsp-headerline-breadcrumb-mode` expects it to be a list and tries to use `add-to-list` on it, resulting in a "wrong-type-argument listp" error.

**Technical Details:**
- Org-src buffers have a special header line that displays: "Edit, then exit with 'C-c '' or abort with 'C-c C-k'"
- This is set as a **string**, not a list
- LSP's breadcrumb code assumes `header-line-format` is always a list
- When it tries to append to it, Emacs crashes with type error

### 2. **`lsp-ui-sideline` - Performance and Display Issues**

LSP-UI sideline causes performance problems during completion because it receives diagnostics and related results while performing completion, forcing Emacs to spend time updating the sideline, flycheck, lenses, and links.

**Why it's worse in org-src buffers:**
- Org-src buffers are **temporary** - they're created and destroyed frequently
- The sideline tries to update on every cursor movement
- In a temporary buffer with no persistent state, this creates unnecessary overhead
- The sideline can interfere with the visible code area in smaller org-src buffers

### 3. **`lsp-modeline` Features - Redundant in Temporary Buffers**

**Issues:**
- **Code actions in modeline**: Org-src buffers already have their own modeline showing the source language and edit status
- **Diagnostics in modeline**: Creates visual clutter when the buffer is temporary
- **Performance**: These features poll the LSP server continuously, which is wasteful for ephemeral buffers

### 4. **`lsp-lens-enable` - Context Issues**

**Problems:**
- Code lenses (like "X references" above functions) query the entire workspace
- In org-src buffers, the context is artificial (we're pretending the buffer-file-name is the tangle target)
- This can cause lenses to show incorrect or misleading information
- Adds unnecessary network traffic to LSP server

### 5. **`lsp-ui-doc` - Popup Conflicts**

**Issues:**
- Documentation popups can conflict with org-mode's own documentation/preview systems
- The child-frame documentation can appear outside the org-src window boundary
- Creates confusion about which "context" you're in (org file vs. source block)

### 6. **`lsp-signature-auto-activate` - Intrusive in Small Buffers**

**Problems:**
- Signature help popups are designed for full-window editing
- In smaller org-src windows, they can cover significant portions of code
- The auto-activation on cursor movement is distracting when you're just reviewing code

### 7. **General Performance Concerns**

The headerline breadcrumb feature can cause huge CPU load and freezing when enabled, with users reporting that disabling breadcrumbs made the problem disappear.

**Why temporary buffers amplify these issues:**
- Org-src buffers are created/destroyed frequently during editing sessions
- Each creation triggers LSP initialization, feature activation, and workspace queries
- The buffers don't benefit from caching that normal file buffers enjoy
- Memory isn't freed immediately when closing the org-src buffer

## The Solution: Selective Disabling

The configuration I provided disables these problematic features **only in org-src buffers** while keeping them active in regular Python files:

```elisp
;; These settings are applied locally only in org-src-mode
(setq-local lsp-headerline-breadcrumb-enable nil        ; Prevents crashes
            lsp-modeline-code-actions-enable nil        ; Reduces clutter
            lsp-modeline-diagnostics-enable nil         ; Reduces clutter
            lsp-lens-enable nil                         ; Prevents incorrect info
            lsp-ui-sideline-enable nil                  ; Improves performance
            lsp-ui-doc-enable nil                       ; Prevents popup conflicts
            lsp-signature-auto-activate nil             ; Reduces intrusion
            lsp-eldoc-enable-hover nil                  ; Keep eldoc minimal
            lsp-completion-show-detail nil              ; Simpler completions
            lsp-completion-show-kind nil)               ; Simpler completions
```

## What You Keep

Even with these disabled, you **still get** the essential LSP features in org-src buffers:
- ✅ **Completions** (via Corfu)
- ✅ **Diagnostics** (via Flycheck)
- ✅ **Go to definition** (via xref)
- ✅ **Find references**
- ✅ **Hover information** (on demand with `lsp-describe-thing-at-point`)
- ✅ **Formatting** (via Apheleia)
- ✅ **Code actions** (via `lsp-execute-code-action`)

The key difference is these features work **on-demand** rather than automatically, which is perfect for the focused, temporary nature of org-src editing buffers.

This approach gives you a fully functional IDE experience for the actual code editing while avoiding the crashes and performance issues that come from features designed for persistent, full-window file buffers.
