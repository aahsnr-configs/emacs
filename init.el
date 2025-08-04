;;; init.el --- loads main config -*- no-byte-compile: t; lexical-binding: t; -*-
(setq custom-file (locate-user-emacs-file "custom.el")) 

(let ((custom-config (expand-file-name "config.el" user-emacs-directory)))
  (when (file-exists-p custom-config)
    (load-file custom-config)))
;;; init.el ends here

