;;; init-scala.el --- Scala development.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'scala-ts-mode)
(require-package 'sbt-mode)

(add-hook! (scala-mode scala-ts-mode)
  (setq-local fill-column 100))

(provide 'init-scala)

;;; init-scala.el ends here
