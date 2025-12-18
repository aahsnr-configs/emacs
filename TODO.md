---
---
---
[**NOTE**]
**Urgency/Importance Scale**:
  - *High*
  - *Medium*
  - *Low*
[NOTE]: No apparent performance gains from minimal-emacs.d


---
---
---

- [ ] Use https://github.com/MatthewZMD/.emacs.d to optimize my emacs configuration


# Fixes in order
- [ ] Apply **display-buffer-alist-fix.md** first
- [ ] Then apply **treemacs-fix.md** next

# Add to Emacs
- [x] Exec From Path Shell
- [x] Sudo Edit
- [x] Optimize Editing Experience
- [x] UTF-8 Coding System
- [x] Small Configs
- [ ] Functions - *Top Level Header* & *General keybindings*
- [ ] Maybe the comprehensive escape key behaviour is not necessary if I have replaced the `C-g` keybinding with `[escape]`?
- [x] Replace the whole Session Management content with that from minimal-emacs.d
- [ ] Add outline indent from minimal-emacs.d. Make sure it works with lsp-bridge
- [x] Replace the custom remove strip whitespace function with with stripspace from minimal-emacs.d. Make sure this works in lsp-bridge as well
- [x] Add easysession from minimal-emacs.d
- [x] Test treemacs configuration from minimal-emacs.d
- [x] Partially replace snippets configuration with that from minimal-emacs.d
- [x] **Important** Re-add my snippets config
- [x] Switch to Catppuccin Mocha Theme
- [ ] **Important** Add projectile-escape-key.md and then python-final-config.md, and then issue the remaining prompts
- [ ] **Medium** Add some emacs packages listed in awesome emacs
- [ ] **Medium** Later modify jinx config to accomodate :defer t better
- [ ] **High** Integrate back to minimal-emacs.d and add many of their recommended packages

# Issues
- [ ] **High** There is a lag when cursor highlights the next matching parenthesis
- [ ] **High** There is still a lag/stutter when lsp-bridge completion opens while I am typing
- [ ] **Hight** There is a lag in typing while a completion candidate is printed to the buffer

[NOTE]: __Query for the folowing tasks after applying the above fixes__
- [ ] **Medium** When I yank a word/phrase using the `y` key and paste it over another word/phrase using the `p` key, the latter word/phrase gets copied by evil. I am doing this in visual mode and I don't that behaviour. Instead I want the former word/phrase to be still be able to be pasted.
- [ ] **Medium** When I yank and paste something in visual mode, I want to visual flash as feedback to confirm that yank and paste have worked using the `y` and `p` keys from evil

- [ ] **Return to aahsnr@duck**
