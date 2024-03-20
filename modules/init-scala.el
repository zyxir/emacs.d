;;; init-scala.el --- Scala development.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(eval-and-compile (require 'init-basic))

(pkg! 'scala-ts-mode)
(pkg! 'sbt-mode)

(add-hook! (scala-mode scala-ts-mode)
  (setq-local fill-column 100))

(provide 'init-scala)

;;; init-scala.el ends here
