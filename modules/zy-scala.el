;;; zy-scala.el --- Scala development. -*- lexical-binding: t -*-

;;; Commentary:

;; This file provides the `+scala' module of the configuration.

;; Emacs has no built-in support for Scala. Therefore, I choose to only install
;; the tree-sitter version of Scala mode.

;;; Code:

(require 'zylib)

(pkg! 'scala-mode)
(pkg! 'scala-ts-mode)
(pkg! 'sbt-mode)

(defvar +scala-disable-treesit nil
  "Use `scala-mode' instead of `scala-ts-mode' for Scala files.

`scala-ts-mode' fits Scala 3 better in my opinion. In my work
with Scala 2 I prefer `scala-mode'.")

(add-hook! '(scala-mode-hook scala-ts-mode-hook)
  (setq-local fill-column 100))

(after! 'scala-ts-mode
  ;; Indent 4 spaces for function arguments and class parameters, as the scala
  ;; formatter does.
  (after! 'scala-ts-mode
    (setf (alist-get
           '(parent-is "^class_parameters$")
           (alist-get 'scala scala-ts--indent-rules)
           nil nil #'equal)
          '(parent-bol 4)
          (alist-get
           '(parent-is "^parameters$")
           (alist-get 'scala scala-ts--indent-rules)
           nil nil #'equal)
          '(parent-bol 4))))

(after! 'scala-mode
  ;; Indent run-on lines according to operators.
  (setq scala-indent:default-run-on-strategy scala-indent:operator-strategy))

(after! '(:or scala-mode scala-ts-mode)
  ;; Command to start and switch to the sbt shell.
  (eval-and-compile
    (defun +scala-switch-to-sbt-shell ()
      "Switch to the sbt buffer.
When there is no sbt process running, start one before
switching to it."
      (interactive)
      (eval-and-compile (require 'sbt-mode))
      (unless (and (get-buffer (sbt:buffer-name))
                   (get-buffer-process (sbt:buffer-name)))
        (sbt:run-sbt nil nil))
      (sbt-switch-to-active-sbt-buffer)))

  (defprefix! +scala-map "Scala"
              nil (scala-mode-map scala-ts-mode-map) "<localleader>")

  (defprefix! +scala-s-map "Sbt"
              nil +scala-map "s"
    "s" (cons "Shell" #'+scala-switch-to-sbt-shell)
    "c" (cons "Command" #'sbt-command)
    "C" (cons "Run Last Command" #'sbt-run-previous-command)))

(provide 'zy-scala)

;;; zy-scala.el ends here
