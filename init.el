;;; init.el --- loads main config -*- no-byte-compile: t; lexical-binding: t; -*-
(let ((custom-config (expand-file-name "config.el" user-emacs-directory)))
  (when (file-exists-p custom-config)
    (load-file custom-config)))
;;; init.el ends here
