;;; graphite-cheatsheet.el --- Toggle a Graphite layout popup under the cursor  -*- lexical-binding: t; -*-

(require 'posframe)

(defvar my/graphite-image
  (expand-file-name "graphite-angle-mod.png" user-emacs-directory)
  "Path to the Graphite layout picture.")

(defvar my/graphite--shown nil
  "Non-nil while the popup is visible.")

(defun my/graphite-toggle ()
  "Show the Graphite layout just below the cursor, or hide it if shown."
  (interactive)
  (if my/graphite--shown
      (posframe-hide " *graphite*")
    (with-current-buffer (get-buffer-create " *graphite*")
      (erase-buffer)
      (insert-image (create-image my/graphite-image nil nil :max-width 400)))
    (posframe-show " *graphite*"
                   :poshandler #'posframe-poshandler-point-bottom-left-corner
                   :override-parameters '((alpha . 80))))
  (setq my/graphite--shown (not my/graphite--shown)))

(global-set-key (kbd "C-c k") #'my/graphite-toggle)

(provide 'graphite-cheatsheet)
;;; graphite-cheatsheet.el ends here
