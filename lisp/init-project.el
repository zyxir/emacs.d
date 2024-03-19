;;; init-project.el --- Project management.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(eval-and-compile (require 'init-basic))

;; "<leader> p" for project-related operations.
(general-def
  :keymaps 'project-prefix-map
  "g" #'magit-project-status
  "/" #'project-find-regexp)
(zy/leader-def "p" project-prefix-map)

(after-or-now! project
  (setq
   ;; Customized commands upon switching to another project.
   project-switch-commands '((project-find-file "Find file")
                             (project-find-dir "Find directory")
                             (project-find-regexp "Find regexp")
                             (magit-project-status "Magit")
                             (project-vc-dir "VC-Dir"))))

(provide 'init-project)

;;; init-project.el ends here
