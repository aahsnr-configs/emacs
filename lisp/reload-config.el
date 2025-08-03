;;; -*- lexical-binding: t; -*-
;;; -*- reload-config.el: t; -*-

;;; Code
(defun ar/reload-config ()
  "Reload the Emacs configuration."
  (interactive)
  ;; Assuming config.org is the main configuration file and this config.el is tangled from it.
  ;; If config.el is the primary config, change to: (load-file (expand-file-name "config.el" user-emacs-directory))
  (let ((config-file "~/.emacs.d/post-init.el"))
    (if (file-exists-p config-file)
        (progn
          (message "Reloading Emacs configuration from config.org...")
          (org-babel-load-file config-file)
          (message "Configuration reloaded successfully!"))
      (error "Configuration file %s not found" config-file))))

;; Bind ar/reload-config globally
(global-set-key (kbd "C-c r") 'ar/reload-config)

(provide 'reload-config)
;;; reload-config.el ends here
