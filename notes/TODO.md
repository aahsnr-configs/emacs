# EMACS PLAN

# **Main ACTION**

- [ ] Switch to lsp-mode and dap-mode

## QUESTIONS

- [ ] Orderless,vertico, marginalia, nerd-icons-completion, consult, embark, embark-consult, corfu, nerd-icons-corfu, cape, dabbrev. What are some issues that these package face with eglot
- [ ] How does lsp-mode view documentation.
- [ ] add cape-company-to-capf backend anyways
- [ ] How does paredit work?
- [ ] Are additional backends needed for cape and eglot
- [x] Can I use a formatter with texlab
- [x] Determine if I need flycheck-posframe
- [x] Make apheleia autoformats buffers on have
- [ ] Will AucTeX completion work with corfu?

---

## LaTeX tasks

- [ ] For the attached emacs configuration, setup yasnippet snippet inheritance, so that org-mode inheritaces the snippets from yas-define-snippets, keeping in mind the function yas-parent-mode does not exist
- [ ] Export org files to latex and pdf files
- [ ] Determine if texpresso-tonic can be used

---

## TASKS

---

- [ ] Corfu completion now should have the following behaviour:
  1. It should only show completion candidates on pressing _TAB_ and not _autocomplete_.
  2. I should be able to cycle through the available candidates list using `jk` keys where `j` is down and `k` is up.
  3. When I have highlighted a candidate from the list, I must be able to print that candidate onto my file/buffer only by pressing _Return_
  4. If there is only one candidate I must be able to print that candidate only my file only by pressing _Return_.
- [ ] Implement lsp-mode and related packages; implement dap-mode and related packages
- [ ] Study and research the doom emacs project in [doom emacs](https://github.com/doomemacs/doomemacs) carefully. Then implement the configurations for org-agenda, projectile, and persp-mode. Make sure to only integrate the appropriate parts of the doom emacs' configuration that is suitable for my vanilla emacs configuration. Then also implement a suitable [org-super-agenda](https://github.com/alphapapa/org-super-agenda) configuration. Make sure these configuration are well integrated with my emacs configuration
- [ ] Write a comprehensive [bufler](https://github.com/alphapapa/bufler.el) that is suitable for my emacs configuration and well integrated with as well
- [ ] Set gc-cons-threshold to 100mb

---

- [ ] integrate org-mode and org-roam from the base emacs configuration into the writing environment
- [ ] setup ligatures for LaTeX with additional math ligaturs
- [ ] setup cdlatex for quick math insertions, and setup laas and auto-activating-snippets.
- [ ] setup custom snippets that might be useful to quickly format and write LaTeX documents
- [ ] Setup completion setup .tex and .org files like company-auctex
- [ ] Add evil surround
- [ ] Set gc-cons-threshold to 100mb
- [x] Set corfu to display documentation only on key input
- [ ] Add whitespace-cleanup-mode
- [x] Write a more comprehensive shackles configuration
- [ ] EasySession
- [ ] Make sure pdf-tools install epdinfo automatically
- [ ] Org-mode export support or bibliography note management with Org-roam later.
- [ ] Winner-Mode
- [ ] **Indent Bars**

# Emacs tasks for later

- [ ] Setup transient, casual and crux once using emacs full time
- [ ] add Deadgrep later on
- [ ] add vim-tab-bar
- [ ] Integrate ripgrep and fd throughout the whole configuration
- [ ] Setup calendar, diary-lib, appt (appointments)
- [ ] ZZZ-to-char
- [ ] tldr
- [ ] only use hydra with dap-mode
- [ ] helpful
- [ ] avy
- [ ] Move-Text
- [ ] Pulsar
- [ ] Colorful Mode

# **_Inspirations_**

- [ ] Emacs Writing Studio
- [ ] Doom Emacs
- [ ] Scimax
- [ ] SqrtMinusOne
- [ ] <https://github.com/emacs-tw/awesome-emacs>
- [ ] progfolio setup
