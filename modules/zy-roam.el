;;; zy-roam.el --- PKM via Org-roam. -*- lexical-binding: t -*-

;;; Commentary:

;; This file provides the `+roam' module of the configuration.

;; It sets up a PKM (personal knowledge management) system via the package
;; Org-roam. The note-taking method named Zettelkasten is the core of this
;; system. All Org-roam-related commands are prefixed with "<leader> r".

;;; Code:

(require 'zylib)

(pkg! 'org-roam)
(pkg! 'org-roam-ui)

(after! 'org-roam
  ;; Automatically sync the database.
  (org-roam-db-autosync-mode 1)

  ;; Ignore all headlines with the ATTACH tag.
  (setq org-roam-db-node-include-function
        (defun +roam-no-attach-node-fn (&rest _)
          (not (member "ATTACH" (org-get-tags))))))

;; Global keys.
(after! '+leader
  (keybind! nil +leader-r-map
    "f" (cons "Find Node" #'org-roam-node-find)
    "i" (cons "Ins Node" #'org-roam-node-insert)
    "c" (cons "Capture" #'org-roam-capture)
    "s" (cons "Sync" #'org-roam-db-sync)
    "u" (cons "UI" #'org-roam-ui-mode)))

;; Keys inside an Org-roam buffer.
(after! '(+leader org-roam)
  (defprefix! +roam-map "Roam"
              nil org-roam-mode-map "<localleader>"
    "a" (cons "Add Alias" #'org-roam-alias-add)
    "l" (cons "Toggle Buffer" #'org-roam-buffer-toggle)))

(provide 'zy-roam)

;;; zy-roam.el ends here
