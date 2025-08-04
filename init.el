;;; init.el --- loads main config -*- no-byte-compile: t; lexical-binding: t; -*-
(let ((custom-config (expand-file-name "config.el" user-emacs-directory)))
  (when (file-exists-p custom-config)
    (load-file custom-config)))
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(eglot)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bold ((t (:foreground "#7aa2f7" :weight bold))))
 '(italic ((t (:foreground "#bb9af7" :slant italic)))))
