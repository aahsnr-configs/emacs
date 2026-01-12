;; ============================================================================
;; USAGE GUIDE
;; ============================================================================

;; STANDARD PACKAGES (MELPA/ELPA):
;; --------------------------------
;; (use-package package-name)

;; PACKAGES FROM GITHUB:
;; ---------------------
;; (use-package package-name
;;   :quelpa (package-name :fetcher github :repo "user/repo"))
;;
;; Example:
;; (use-package copilot
;;   :quelpa (copilot :fetcher github :repo "copilot-emacs/copilot.el"))

;; PACKAGES FROM GITLAB:
;; ---------------------
;; (use-package package-name
;;   :quelpa (package-name :fetcher gitlab :repo "user/repo"))

;; PACKAGES FROM GIT URL:
;; ----------------------
;; (use-package package-name
;;   :quelpa (package-name :fetcher git :url "https://example.com/repo.git"))

;; SPECIFIC BRANCH/COMMIT/TAG:
;; ---------------------------
;; (use-package package-name
;;   :quelpa (package-name
;;            :fetcher github
;;            :repo "user/repo"
;;            :branch "develop"))     ; or :commit "abc123" or :tag "v1.0"

;; CUSTOM FILES PATTERN:
;; ---------------------
;; (use-package package-name
;;   :quelpa (package-name
;;            :fetcher github
;;            :repo "user/repo"
;;            :files ("*.el" "subdir/*.el")))

;; FORCE UPGRADE:
;; --------------
;; (use-package package-name
;;   :quelpa ((package-name :fetcher github :repo "user/repo")
;;            :upgrade t))

;; AVAILABLE FETCHERS:
;; -------------------
;; git, github, gitlab, bzr, hg, svn, cvs, darcs, fossil, wiki

;; MANUAL COMMANDS:
;; ----------------
;; M-x quelpa                  - Install package interactively
;; M-x quelpa-upgrade          - Upgrade specific package
;; M-x quelpa-upgrade-all      - Upgrade all Quelpa packages

;; BEST PRACTICES:
;; ---------------
;; 1. Use MELPA/ELPA packages when available
;; 2. Use Quelpa for:
;;    - Packages not on MELPA
;;    - Development versions
;;    - Personal/forked packages
;; 3. Pin critical packages to specific commits for stability
;; 4. Keep Quelpa package list in version control

;; ============================================================================

;; installs abc-mode with quelpa
(use-package abc-mode :quelpa)

;; does the same (`t' is optional)
(use-package abc-mode :quelpa t)

;; again... (if the package would have another name)
(use-package abc-mode :quelpa abc-mode)

;; passes upgrade parameter to quelpa
(use-package abc-mode :quelpa (:upgrade t))

;; uses the given recipe
(use-package abc-mode
  :quelpa (abc-mode :fetcher github :repo "mkjunker/abc-mode"))

;; recipe with plist arguments
(use-package abc-mode
  :quelpa ((abc-mode :fetcher github :repo "mkjunker/abc-mode") :upgrade t))
