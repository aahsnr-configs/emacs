;;; post-init.el --- Load custom tangled configuration -*- no-byte-compile: t; lexical-binding: t; -*-

(let ((custom-config (expand-file-name "config.el" minimal-emacs-user-directory)))
  (when (file-exists-p custom-config)
    (load-file custom-config)))
