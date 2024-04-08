;;; zy-project.el --- Project management. -*- lexical-binding: t -*-

;;; Commentary:

;; This file provides the `+project' module of the configuration.

;; Project.el is the built-in project management utility of Emacs. It is younger
;; and has less features than Projectile, but I have found it less buggy than
;; Projectile. Since Project.el and Projectile each has its pros and cons, it is
;; adviced to make a module for each of them, and choose one of them manually.

;;; Code:

(require 'zylib)

(defvar +project-dominating-files '("shell.nix")
  "List of dominating files that define a project.
This is useful to recognize a non-version-controlled project or a
sub-project inside another project.")

(after! 'project
  ;; Customize commands used upon switching to another project.
  (setq project-switch-commands '((project-find-file "Find file" ?f)
                                  (project-find-dir "Find directory" ?d)
                                  (project-find-regexp "Find regexp" ?/)
                                  (magit-project-status "Magit" ?g)
                                  (project-vc-dir "VC-Dir" ?v)))

  (add-hook! 'project-find-functions
    (defun +project-find-dominating-file (dir)
      (let ((root (cl-some
                   (lambda (file) (locate-dominating-file dir file))
                   +project-dominating-files)))
        (and root (cons 'transient root))))))

(provide 'zy-project)

;;; zy-project.el ends here
