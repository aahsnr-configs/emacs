I'll analyze your configuration and search for best practices on using shackle and popper to replace display-buffer-alist entries.After analyzing your configuration and researching how shackle and popper work together, I can confirm that **yes, you can replace display-buffer-alist entries with shackle rules**. Shackle handles window placement rules while popper manages quick access to popups, and they work together harmoniously - shackle respects popper's rules and vice versa.

Here are the specific changes needed:

## 1. Remove Org-Roam display-buffer-alist Entry

**Current code in Org Roam section:**
```elisp
;; Configure the backlinks buffer to appear in a right-hand sidebar.
(add-to-list 'display-buffer-alist
             '("\\*org-roam\\*"
               (display-buffer-in-direction)
               (direction . right)
               (window-width . 0.33)
               (window-height . fit-window-to-buffer)))
```

**Replace with:**
```elisp
;; Org-roam backlinks display is now handled by shackle rules
;; See the shackle configuration in "Popup Management System" section
```

## 2. Remove Embark display-buffer-alist Entry

**Current code in Embark section:**
```elisp
;; Hide the mode line of the Embark live/completions buffers
(add-to-list 'display-buffer-alist
             '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
               nil
               (window-parameters (mode-line-format . none))))
```

**Replace with:**
```elisp
;; Embark collect buffers display is now handled by shackle rules
;; See the shackle configuration in "Popup Management System" section
```

## 3. Update Shackle Rules

**Add to your existing shackle-rules** (in the Popup Management System section):

```elisp
:config
  ;; Define popup rules matching Doom Emacs patterns
  (setq shackle-rules
        '(;; ... existing rules ...

          ;; Org-roam backlinks sidebar
          ("^\\*org-roam\\*" :select nil :size 0.33 :align right :popup t)

          ;; Embark Collect - hide modeline
          ("^\\*Embark Collect \\(Live\\|Completions\\)\\*" :regexp t :select t :size 0.5 :align below :popup t)
          
          ;; ... rest of existing rules ...
          ))
```

## 4. Optional: Add Org-Roam to Popper References

If you want to quickly toggle the org-roam buffer with popper, add it to `popper-reference-buffers`:

```elisp
:init
  (setq popper-reference-buffers
        '(;; ... existing entries ...
          "^\\*org-roam\\*"
          ;; ... rest of entries ...
          ))
```

## 5. Verify Shackle Window Parameters

Your current shackle configuration already sets default window parameters. If you need the mode-line removed from Embark buffers specifically, you can modify the rule:

```elisp
;; Alternative: Embark with explicit window parameters
;; #TODO Apply similar method below to remove modeline format from Agenda Commands
("^\\*Embark Collect" :regexp t :select t :size 0.5 :align below :popup t 
 :window-parameters ((mode-line-format . none)))
```

**Note:** The `:window-parameters` key in individual shackle rules will override `shackle-default-window-parameters` for that specific buffer.

## Summary

The key insight is that shackle works by adding a single entry to display-buffer-alist that intercepts all buffer display requests and applies your rules. By consolidating all your display rules in `shackle-rules`, you get:

1. **Centralized configuration** - all popup rules in one place
2. **Simpler syntax** - shackle's plist format is easier than display-buffer-alist
3. **Better integration** - shackle and popper work together seamlessly
4. **Debugging support** - shackle provides tracing functions to troubleshoot display issues

Your existing shackle and popper configuration is already well-structured, so these changes simply move the remaining `display-buffer-alist` entries into the shackle system you're already using.
