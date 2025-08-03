;; First, ensure Elpaca is installed and configured
;; This should go in your init.el before any other Elpaca calls

(defvar elpaca-installer-version 0.5)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                     (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                     (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                     "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install and configure use-package for Elpaca
(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :elpaca t unless otherwise specified.
  (setq elpaca-use-package-by-default t))

;; Block until current queue processed.
(elpaca-wait)

;; AUCTeX configuration with Elpaca
(use-package tex
  :ensure (auctex :pre-build (("./autogen.sh")
                              ("./configure"
                               "--without-texmf-dir"
                               "--with-packagelispdir=./"
                               "--with-packagedatadir=./")
                              ("make"))
                  :build (:not elpaca--compile-info) ;; Make handles compilation
                  :files ("*.el" "doc/*.info*" "etc" "images" "latex" "style")
                  :version (lambda (_) 
                             (require 'tex-site) 
                             AUCTeX-version))
  :init
  ;; Basic AUCTeX settings
  (setq TeX-parse-self t              ; Parse documents automatically
        TeX-auto-save t               ; Save parsed information
        TeX-master nil                ; Query for master file
        reftex-plug-into-AUCTeX t)    ; Enable RefTeX integration
  
  ;; PDF viewer configuration
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view)))
  
  ;; Enable source correlation (SyncTeX)
  (setq TeX-source-correlate-mode t
        TeX-source-correlate-method 'synctex
        TeX-source-correlate-start-server t)
  
  ;; Enable automatic math mode for $...$
  (setq TeX-electric-math '("$" . "$"))
  
  :config
  ;; Enable LaTeX-math-mode for convenient math symbol insertion
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  
  ;; Enable RefTeX for better reference handling
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  
  ;; Enable auto-fill-mode for automatic line breaking
  (add-hook 'LaTeX-mode-hook 'auto-fill-mode)
  
  ;; Enable outline-minor-mode for better document navigation
  (add-hook 'LaTeX-mode-hook 'outline-minor-mode)
  
  ;; Enable flyspell for spell checking
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  
  ;; Better folding support
  (add-hook 'LaTeX-mode-hook 'TeX-fold-mode)
  
  ;; Enable prettify-symbols-mode for better symbol display
  (add-hook 'LaTeX-mode-hook 'prettify-symbols-mode))

;; Optional: CDLaTeX for fast LaTeX input
(use-package cdlatex
  :hook (LaTeX-mode . turn-on-cdlatex)
  :config
  (setq cdlatex-use-dollar-to-ensure-math nil)) ; Don't interfere with AUCTeX's electric math

;; Optional: Company mode for auto-completion
(use-package company
  :hook (LaTeX-mode . company-mode)
  :config
  (setq company-idle-delay 0.3
        company-minimum-prefix-length 2))

;; Optional: Company-AUCTeX for LaTeX-specific completions
(use-package company-auctex
  :after (company tex)
  :config
  (company-auctex-init))

;; Optional: PDF Tools for better PDF viewing
(use-package pdf-tools
  :init
  (pdf-tools-install :no-query)
  :config
  (setq-default pdf-view-display-size 'fit-width)
  ;; Enable synctex correlation
  (setq pdf-sync-forward-display-action
        '(display-buffer-reuse-window (reusable-frames . t)))
  (setq pdf-sync-backward-display-action
        '(display-buffer-reuse-window (reusable-frames . t))))

;; Optional: RefTeX configuration for better reference management
(use-package reftex
  :after tex
  :config
  (setq reftex-cite-prompt-optional-args t
        reftex-cite-cleanup-optional-args t
        reftex-default-bibliography '("~/bibliography.bib") ; Set your bib file path
        reftex-bibliography-commands '("bibliography" "nobibliography" "addbibresource")))

;; Optional: BibTeX mode enhancements
(use-package bibtex
  :config
  (setq bibtex-align-at-equal-sign t
        bibtex-autokey-year-length 4
        bibtex-autokey-name-year-separator ""
        bibtex-autokey-year-title-separator ""
        bibtex-autokey-titleword-separator ""
        bibtex-autokey-titlewords 1
        bibtex-autokey-titlewords-stretch 0
        bibtex-autokey-titleword-length 5))

;; Ensure packages are processed
(elpaca-wait)
