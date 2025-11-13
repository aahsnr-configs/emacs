;;; config.el -*- lexical-binding: t; -*-

;; First, declare the package in packages.el:
;; (package! lsp-bridge)


(use-package! lsp-bridge
  :init
  ;; Global mode activation
  (global-lsp-bridge-mode)

  :config
  ;; LSP server configuration
  (setq lsp-bridge-python-multi-lsp-server "basedpyright_ruff"
        lsp-bridge-tex-lsp-server "texlab"
        lsp-bridge-nix-lsp-server "nil")

  ;; Disable search words globally (will be selectively enabled)
  (setq lsp-bridge-enable-search-words nil)

  ;; ACM (Asynchronous Completion Menu) configuration
  (setq acm-enable-doc nil
        acm-enable-jupyter t
        acm-enable-capf t
        acm-enable-doc-markdown-render 'async
        acm-enable-icon t
        acm-candidate-match-function 'orderless-literal
        acm-backend-search-file-words-enable-fuzzy-match t)

  ;; LSP Bridge features
  (setq lsp-bridge-enable-hover-diagnostic t
        lsp-bridge-enable-auto-format-code t
        lsp-bridge-enable-with-tramp t
        lsp-bridge-enable-inlay-hint t
        lsp-bridge-signature-show-function 'lsp-bridge-signature-show-with-frame
        lsp-bridge-enable-org-babel t
        lsp-bridge-org-babel-lang-list nil)

  ;; Semantic tokens configuration
  (setq lsp-bridge-semantic-tokens t)
  (setq-default lsp-bridge-semantic-tokens-ignore-modifier-limit-types ["variable"]))


;; Selectively enable search words in specific modes
(defun +lsp-bridge/enable-search-words-for-text ()
  "Enable search words for text-based modes only."
  (setq-local lsp-bridge-enable-search-words
              (memq major-mode '(text-mode markdown-mode))))


(add-hook! 'lsp-bridge-mode-hook #'+lsp-bridge/enable-search-words-for-text)