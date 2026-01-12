I'll help you integrate org-gtd.el into your configuration. Let me first search for information about org-gtd to understand it thoroughly, then provide you with the integration.Based on my thorough analysis of both org-gtd.el and your configuration, I'll provide you with the necessary changes to integrate org-gtd seamlessly. First, let me give you the comprehensive GTD guide, then the configuration changes.

---

# **COMPREHENSIVE GUIDE TO GTD AND ORG-GTD**

## **Part 1: Understanding GTD Methodology**

### **Core GTD Philosophy**

Getting Things Done (GTD) is based on one fundamental truth: **"Your mind is for having ideas, not holding them."**

The human brain is excellent at processing information but terrible at storing it. Every uncompleted task, commitment, or idea bouncing around in your head creates "mental overhead" that drains your cognitive energy and causes stress.

### **The Five GTD Workflows**

#### **1. CAPTURE**

Everything that has your attention goes into an **inbox** - a trusted collection system outside your brain.

**What to Capture:**

- Tasks ("Reply to Sarah's email")
- Projects ("Redesign company website")
- Ideas ("Blog post about productivity")
- Commitments ("Pick up dry cleaning")
- Someday/Maybe ("Learn Italian")

**Key Principle:** Capture EVERYTHING. Don't filter, don't organize, just collect. Trust that you'll process it later.

#### **2. CLARIFY**

Process inbox items to determine what they mean and what action is required.

**The Clarify Decision Tree:**

```
Is it actionable?
├─ NO
│  ├─ Trash it
│  ├─ Reference it (someday you might need it)
│  └─ Someday/Maybe (might do it later)
└─ YES
   ├─ Can you do it in <2 minutes? → DO IT NOW
   ├─ Can you delegate it? → DELEGATE (add to Waiting For)
   └─ Defer it
      ├─ Single action? → Add to Next Actions
      └─ Multiple steps? → Create a PROJECT
```

**The Two-Minute Rule:** If an action takes less than 2 minutes, do it immediately. It takes longer to organize and track it than to just do it.

#### **3. ORGANIZE**

Sort clarified items into the appropriate lists/categories.

**GTD's Core Lists:**

- **Next Actions:** Concrete, physical actions you can take (by context: @computer, @home, @phone)
- **Projects:** Outcomes requiring >1 action step
- **Waiting For:** Items blocked by someone else
- **Someday/Maybe:** Things you might want to do but not now
- **Calendar:** Time-specific or date-specific items ONLY

**Projects vs Next Actions:**

- Project: "Redesign website" (outcome)
- Next Actions: "Email designer for quotes", "Review competitor websites", "Draft content outline"

Every project MUST have at least one next action or it's "stuck."

#### **4. REFLECT**

Regularly review your system to keep it current and trustworthy.

**Daily Review (5-10 min):**

- Check calendar for today's commitments
- Review Next Actions list
- Process new items in inbox to zero

**Weekly Review (1-2 hours) - CRITICAL:**

1. **Get Clear:**
   - Collect loose papers and materials
   - Process inbox to zero
   - Empty your head (mind sweep)

2. **Get Current:**
   - Review Next Actions lists
   - Review previous calendar week
   - Review upcoming calendar (next 2-3 weeks)
   - Review Waiting For list (follow up as needed)
   - Review Projects list (ensure each has a next action)
   - Review Someday/Maybe list

3. **Get Creative:**
   - Review life goals and vision
   - Identify new projects or opportunities

#### **5. ENGAGE**

Choose what to work on based on:

1. **Context:** Where are you? What tools do you have?
2. **Time available:** 5 minutes? 2 hours?
3. **Energy level:** High? Low? Creative vs. administrative tasks
4. **Priority:** Given the above, what's most important?

### **The Six Horizons of Focus**

GTD works bottom-up, but you should regularly review from all levels:

- **Horizon 5 (Life Purpose):** Why do you exist?
- **Horizon 4 (Vision):** 3-5 year goals
- **Horizon 3 (Goals):** 1-2 year objectives
- **Horizon 2 (Areas of Focus):** Key responsibilities (work, health, relationships)
- **Horizon 1 (Projects):** Current multi-step outcomes
- **Horizon 0 (Next Actions):** Immediate physical tasks

---

## **Part 2: org-gtd.el Mastery**

### **How org-gtd Implements GTD**

org-gtd.el automates the GTD workflow in Emacs by:

1. Providing a structured inbox processing interface
2. Auto-filing items into appropriate GTD lists
3. Managing project dependencies with org-edna
4. Creating custom agenda views for engagement

### **The org-gtd Workflow**

#### **1. Capture (C-c g d c)**

Press `SPC g d c` → Type your thought → `C-c C-c` to save.

Items go into `inbox.org`. Don't overthink it—just capture.

#### **2. Process Inbox (C-c g d p)**

Press `SPC g d p` to enter processing mode.

**You'll see each inbox item one at a time with options:**

- **Edit (RET):** Clarify what the item actually means
- **Done editing (C-c c):** Opens organization menu

**Organization Menu:**

- `q` - **Quick Action:** 1-step task you'll do soon
- `s` - **Single Action:** 1-step task (with context tags)
- `p` - **Project:** Multi-step outcome
- `c` - **Calendar:** Time/date-specific item
- `d` - **Delegate:** Assign to someone (tracked in Waiting For)
- `r` - **Reference:** Not actionable, save for later
- `t` - **Trash:** Delete it
- `h` - **Habit:** Recurring behavior pattern
- `m` - **Someday/Maybe:** Might do it eventually

#### **3. Engage (C-c g d e)**

Press `SPC g d e` to see your daily agenda.

This shows:

- Today's calendar items
- All NEXT actions (ready to work on)
- Active projects

**What Makes a Task "NEXT"?**

- It's the next physical action you can take on a project
- All dependencies (if any) are completed
- It's not blocked by anyone/anything

#### **4. Project Management**

**Creating Projects:**
When you choose "Project" during inbox processing:

1. Create a heading with the desired outcome as the title
2. Add sub-headings for each action step (sequential by default)
3. Mark them as TODO
4. org-edna automatically marks the first as NEXT

**Example:**

```org
* proj Redesign company website
** NEXT Email designer for quotes
** TODO Review competitor sites
** TODO Draft content outline
** TODO Approve mockups
```

When you mark "Email designer" as DONE, org-edna **automatically** changes "Review competitor sites" to NEXT.

**Parallel Tasks (Version 4.0+):**
You can now have multiple NEXT actions in a project:

```org
* proj Website Project
:PROPERTIES:
:BLOCKER: task("Review design") task("Get budget approval")
:END:
** NEXT Research hosting options
** NEXT Interview designers
** WAIT Review design
** TODO Get budget approval
```

#### **5. Custom Views**

Create context-specific views:

**See all @computer tasks:**

```elisp
(defun my-gtd-computer-actions ()
  "Show all @computer next actions."
  (interactive)
  (org-ql-search (org-gtd-directory)
    '(and (todo "NEXT") (tags "@computer"))
    :title "@Computer Actions"))
```

---

### **Best Practices for org-gtd**

1. **Process Inbox to Zero Daily:** Don't let it pile up
2. **Weekly Review is Sacred:** Schedule it, protect it, do it
3. **Keep Next Actions Atomic:** "Email designer" not "Work on project"
4. **One Next Action Per Project Minimum:** If a project has no NEXT, it's stuck
5. **Trust Your System:** Only check your system, not your memory
6. **Use Contexts Wisely:** Tag by where/how you can do the task (@computer, @phone, @home, @errands)

---

Now, let me provide the configuration changes:

---

# **CONFIGURATION CHANGES FOR EMACS**

## **1. New Package Installations**

Add these to your **Org Mode** section in `config.org`:

```org
** Org GTD Dependencies
#+begin_src emacs-lisp
(use-package org-edna
  :defer t
  :after org
  :commands (org-edna-mode org-edna-load)
  :custom
  (org-edna-use-inheritance t)
  :config
  (org-edna-mode 1))

(use-package org-agenda-property
  :defer t
  :after org
  :custom
  (org-agenda-property-list '("DELEGATED_TO" "CONTEXT"))
  (org-agenda-property-position 'next-line))

(use-package org-gtd
  :after (org org-edna)
  :commands (org-gtd-capture
             org-gtd-process-inbox
             org-gtd-engage
             org-gtd-show-all-next
             org-gtd-show-stuck-projects
             org-gtd-organize)
  :init
  ;; Suppress upgrade warnings
  (setq org-gtd-update-ack "4.0.0")

  ;; Set GTD directory (integrated with your org structure)
  (setq org-gtd-directory (expand-file-name "gtd/" my/org-directory))

  ;; Ensure directory exists
  (unless (file-directory-p org-gtd-directory)
    (make-directory org-gtd-directory t))

  :custom
  ;; Configure TODO keywords - ADD a separate GTD sequence
  ;; This preserves your existing sequences
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "PROG(p)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCEL(c@)")
     (sequence "PLAN(P)" "ACTIVE(A)" "PAUSED(x)" "|" "ACHIEVED(a)" "DROPPED(D)")
     ;; GTD-specific sequence (required by org-gtd)
     (sequence "GTD-TODO(T)" "GTD-NEXT(N)" "GTD-WAIT(W)" "|" "GTD-DONE(D)" "GTD-CNCL(C)")))

  ;; Map GTD semantic states to your keywords
  (org-gtd-keyword-mapping
   '((todo . "GTD-TODO")
     (next . "GTD-NEXT")
     (wait . "GTD-WAIT")
     (done . "GTD-DONE")
     (canceled . "GTD-CNCL")))

  ;; Per-type refile prompting (recommended)
  (org-gtd-refile-to-any-target nil)

  ;; Enable mode-line inbox count
  (org-gtd-mode t)

  :config
  ;; Add GTD files to agenda
  (add-to-list 'org-agenda-files org-gtd-directory)

  ;; Define custom GTD agenda views
  (setq org-agenda-custom-commands
        (append org-agenda-custom-commands
                '(("g" . "GTD contexts")
                  ("gn" "All Next Actions"
                   ((todo "GTD-NEXT")))
                  ("gc" "Context: Computer"
                   ((tags-todo "@computer/GTD-NEXT")))
                  ("gh" "Context: Home"
                   ((tags-todo "@home/GTD-NEXT")))
                  ("gp" "Context: Phone"
                   ((tags-todo "@phone/GTD-NEXT")))
                  ("gw" "Waiting For"
                   ((todo "GTD-WAIT")))
                  ("gs" "Stuck Projects"
                   ((org-gtd-stuck-projects))))))

  ;; Weekly Review template
  (defun ar/gtd-weekly-review ()
    "Launch GTD weekly review checklist."
    (interactive)
    (let ((review-file (expand-file-name "weekly-review.org" org-gtd-directory)))
      (unless (file-exists-p review-file)
        (with-temp-file review-file
          (insert "#+TITLE: GTD Weekly Review
#+STARTUP: overview

* Weekly Review Checklist
** Get Clear
- [ ] Collect all loose papers and materials
- [ ] Process inbox to zero
- [ ] Empty email inbox
- [ ] Empty physical inbox
- [ ] Mind sweep - capture any thoughts

** Get Current
- [ ] Review Next Actions lists
- [ ] Review previous calendar week
- [ ] Review upcoming calendar (2-3 weeks)
- [ ] Review Waiting For list
- [ ] Review all Projects (each has next action?)
- [ ] Review Someday/Maybe

** Get Creative
- [ ] Review Areas of Focus
- [ ] Review Goals (1-2 year)
- [ ] Review Vision (3-5 year)
- [ ] Any new projects or opportunities?

* Review Log
")))
      (find-file review-file))))

#+end_src
```

## **2. Modified Org Configuration**

**Update your existing org configuration's TODO keywords:**

```org
#+begin_src emacs-lisp
  ;; UPDATED: Modified to include GTD sequence
  (setq org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "PROG(p)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCEL(c@)")
     (sequence "PLAN(P)" "ACTIVE(A)" "PAUSED(x)" "|" "ACHIEVED(a)" "DROPPED(D)")
     ;; GTD-specific sequence for org-gtd
     (sequence "GTD-TODO(T)" "GTD-NEXT(N)" "GTD-WAIT(W)" "|" "GTD-DONE(D)" "GTD-CNCL(C)")))

  ;; UPDATED: Add GTD keyword faces
  (setq org-todo-keyword-faces
   (append org-todo-keyword-faces
           '(("GTD-TODO" . (:foreground "#e0af68" :weight bold))
             ("GTD-NEXT" . (:foreground "#7aa2f7" :weight bold))
             ("GTD-WAIT" . (:foreground "#ff9e64" :weight bold))
             ("GTD-DONE" . (:foreground "#9ece6a" :weight bold))
             ("GTD-CNCL" . (:foreground "#565f89" :weight bold)))))
#+end_src
```

## **3. Update General Keybindings**

Add GTD keybindings to your **General Keybindings** section:

```org
#+begin_src emacs-lisp
  ;; GTD operations (SPC g d)
  (ar/global-leader
    "g d" '(:ignore t :wk "gtd")
    "g d c" '(org-gtd-capture :wk "Capture to inbox")
    "g d p" '(org-gtd-process-inbox :wk "Process inbox")
    "g d e" '(org-gtd-engage :wk "Engage (daily view)")
    "g d n" '(org-gtd-show-all-next :wk "Show all NEXT")
    "g d s" '(org-gtd-show-stuck-projects :wk "Stuck projects")
    "g d w" '(ar/gtd-weekly-review :wk "Weekly review")
    "g d a" '((lambda () (interactive)
                (org-agenda nil "g")) :wk "GTD agenda menu"))

  ;; Update Notes operations to include GTD
  (ar/global-leader
    "n g" '(:ignore t :wk "gtd")
    "n g c" '(org-gtd-capture :wk "GTD capture")
    "n g p" '(org-gtd-process-inbox :wk "Process inbox")
    "n g e" '(org-gtd-engage :wk "Daily engage")
    "n g w" '(ar/gtd-weekly-review :wk "Weekly review"))
#+end_src
```

## **4. Configurations to REMOVE**

**Remove or comment out these from your Org Capture section:**

```org
;; REMOVE: These are now handled by org-gtd
;;("t" "Task" entry
;; (file+headline ,(expand-file-name "inbox.org" my/org-directory) "Tasks")
;; "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n")

;;("p" "Project" entry
;; (file+headline ,(expand-file-name "projects.org" my/org-directory) "Projects")
;; "* PLAN %? :project:\n...")

;; KEEP: These are complementary to GTD, not duplicates
;; Journal, Book, Habit, Goal captures can stay
```

**Keep but modify your org-agenda-files:**

```org
#+begin_src emacs-lisp
  ;; UPDATED: Include both regular org and GTD directories
  (setq org-agenda-files
        (list my/org-directory org-gtd-directory))
#+end_src
```

## **5. Integration with Org-Roam**

Add to your **Org Roam** section:

```org
#+begin_src emacs-lisp
  ;; Integration: Link GTD projects to Roam notes
  (defun ar/gtd-link-to-roam ()
    "Create or link to an org-roam note from current GTD project."
    (interactive)
    (when (derived-mode-p 'org-mode)
      (let* ((project-title (org-get-heading t t t t))
             (node (org-roam-node-create :title project-title)))
        (org-roam-capture- :node node
                          :props '(:immediate-finish nil)))))

  ;; Add to GTD clarify mode
  (with-eval-after-load 'org-gtd
    (define-key org-gtd-clarify-mode-map (kbd "C-c r") #'ar/gtd-link-to-roam))
#+end_src
```

## **6. Hydra for GTD (Optional)**

Add to your **Hydra Setup** section:

```org
#+begin_src emacs-lisp
(pretty-hydra-define gtd-hydra
  (:title (pretty-hydra-title "GTD System" 'mdicon "nf-md-checkbox_marked_circle" :face 'nerd-icons-green)
   :color amaranth :quit-key ("q" "C-g"))
  ("Capture & Process"
   (("c" org-gtd-capture "capture")
    ("p" org-gtd-process-inbox "process inbox")
    ("o" org-gtd-organize "organize item"))

   "Engage"
   (("e" org-gtd-engage "daily engage")
    ("n" org-gtd-show-all-next "all next actions")
    ("s" org-gtd-show-stuck-projects "stuck projects")
    ("a" org-agenda "agenda menu"))

   "Review"
   (("w" ar/gtd-weekly-review "weekly review")
    ("d" org-gtd-directory-show "show GTD directory"))

   "Navigate"
   (("i" (find-file (expand-file-name "inbox.org" org-gtd-directory)) "inbox")
    ("N" (find-file (expand-file-name "next.org" org-gtd-directory)) "next actions")
    ("P" (find-file (expand-file-name "projects.org" org-gtd-directory)) "projects")
    ("S" (find-file (expand-file-name "someday.org" org-gtd-directory)) "someday"))))

;; Bind to SPC g h
(ar/global-leader
  "g h" '(gtd-hydra/body :wk "GTD hydra"))
#+end_src
```

---

## **7. Migration Strategy**

**For existing projects and tasks:**

1. **Don't migrate everything at once.** Start fresh with GTD for new items.

2. **For critical current projects:**

   ```org
   ;; In your existing projects.org, add this function:
   (defun ar/migrate-project-to-gtd ()
     "Migrate current project to GTD system."
     (interactive)
     (when (org-entry-get nil "TODO")
       (let* ((heading (org-get-heading t t t t))
              (content (org-get-entry))
              (target-file (expand-file-name "projects.org" org-gtd-directory)))
         ;; Copy project structure
         (org-copy-subtree)
         (find-file target-file)
         (goto-char (point-max))
         (org-paste-subtree)
         ;; Update TODO keywords
         (org-todo "GTD-TODO")
         (message "Project migrated to GTD: %s" heading))))
   ```

3. **Archive old inbox items:**
   Move items from old inbox.org to archive or process them through GTD.

---

## **8. Directory Structure After Integration**

```
~/org/
├── roam/                    # Org-roam knowledge base (KEEP)
├── gtd/                     # org-gtd managed (NEW)
│   ├── inbox.org           # Captured items (org-gtd creates)
│   ├── next.org            # Next actions (org-gtd creates)
│   ├── projects.org        # Active projects (org-gtd creates)
│   ├── calendar.org        # Time-specific (org-gtd creates)
│   ├── someday.org         # Someday/Maybe (org-gtd creates)
│   ├── references.org      # Reference material (org-gtd creates)
│   └── weekly-review.org   # Review checklist (you create)
├── journal.org              # Personal journaling (KEEP)
├── habits.org               # Habit tracking (KEEP - but can migrate to GTD habits)
├── goals.org                # Long-term goals (KEEP - Horizon 3-5)
├── reading.org              # Book tracking (KEEP)
└── meetings.org             # Meeting notes (KEEP or capture via GTD)
```

---

## **9. Workflow After Integration**

**Daily:**

1. **Morning:** `SPC g d e` → Check daily engage view
2. **Throughout day:** `SPC g d c` → Capture anything that comes up
3. **End of day:** `SPC g d p` → Process inbox to zero (10-15 min)

**Weekly:**

1. `SPC g d w` → Run weekly review (Friday afternoon or Sunday evening)
2. Go through checklist systematically
3. Ensure every project has a next action

**Monthly/Quarterly:**

1. Review Someday/Maybe list
2. Review Goals against Projects
3. Archive completed projects

---

## **Summary of Changes**

**Added:**

- org-edna (dependency management)
- org-agenda-property (delegation tracking)
- org-gtd (complete GTD system)
- GTD-specific TODO keywords (separate sequence)
- GTD keybindings under `SPC g d`
- Weekly review function
- GTD hydra
- Integration with org-roam

**Removed:**

- Generic task capture template (replaced by org-gtd-capture)
- Generic project template (replaced by org-gtd project workflow)

**Kept:**

- Journal, reading, goals, habits (complementary to GTD)
- Org-roam (knowledge management separate from task management)
- All Evil, completion, and editor configurations
- Custom org-agenda views (extended with GTD views)

This integration gives you the best of both worlds: **structured GTD workflow** for tasks and projects, while keeping **org-roam for knowledge management** and your other org files for specific purposes.
