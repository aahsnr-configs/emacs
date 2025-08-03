;;; -*- lexical-binding: t; -*-
;;; -*- update-packages.el: t; -*-

;;; Code
(defun my-enhanced-update-packages ()
  "Refresh package list, upgrade installed packages, and remove obsolete ones.
Bound to C-c u."
  (interactive)
  ;; Step 1: Refresh package contents from archives
  (message "Refreshing package archives...")
  (package-refresh-contents)
  (message "Package archives refreshed.")

  ;; Step 2: Identify and upgrade packages
  (let* ((installed-packages package-installed-packages)
         (upgradable-packages (cl-remove-if-not #'package-can-upgrade-p installed-packages)))
    (if upgradable-packages
        (progn
          (message "Found %d package(s) to upgrade: %s"
                   (length upgradable-packages)
                   (mapconcat #'package--symbol upgradable-packages ", "))
          (when (y-or-n-p "Upgrade these packages now? ")
            ;; Mark packages for installation and install them
            (dolist (pkg upgradable-packages)
              (package-install (car pkg)))
            (message "Package upgrade process complete.")))
      (message "All installed packages are up-to-date.")))

  ;; Step 3: Clean up obsolete packages
  (let ((obsolete (package-obsolete-packages)))
    (if (and obsolete (not (zerop (length obsolete))))
        (when (y-or-n-p (format "Delete %d obsolete package(s)?" (length obsolete)))
          (message "Removing obsolete packages...")
          (package-autoremove)
          (message "Obsolete packages removed."))
      (message "No obsolete packages found."))))

(global-set-key (kbd "C-c u") 'my-enhanced-update-packages)

(provide 'update-packages)
;;; update-packages.el ends here
