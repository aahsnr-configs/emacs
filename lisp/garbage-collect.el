;;; -*- lexical-binding: t; -*-
;;; -*- garbage-collect.el: t; -*-

;;; Code
(defun ar/garbage-collect ()
  "Run garbage collection and report the amount of memory freed."
  (interactive)
  (let ((before-gc-info (memory-info)))
    (message "Triggering garbage collection...")
    (garbage-collect)
    (let* ((after-gc-info (memory-info))
           (reclaimed-memory (- (car before-gc-info) (car after-gc-info))))
      (message "Garbage collection complete. Reclaimed %s."
               (file-size-human-readable reclaimed-memory)))))

(defun memory-info ()
  "Return a list of memory usage statistics."
  (list (memory-to-string (memory-use-counts))
        (memory-to-string (garbage-collect-heap-size))))

(global-set-key (kbd "C-c g") 'ar/garbage-collect)

(provide 'garbage-collect)
;;; garbage-collect.el ends here
