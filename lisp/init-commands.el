;;; -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:

;; Useful M-x commands.

;;; Code:

;; Run command in new frame.

(defun zy/execute-in-new-frame (prefixarg command-name)
  (interactive (list current-prefix-arg (read-extended-command)))
  (let ((command (intern-soft command-name)))
    (unless command
      (error "%s is not a valid command name" command-name))
    (select-frame (make-frame))
    (let ((prefix-arg prefixarg))
      (command-execute command))))
(general-define-key "C-c M-x" 'zy/execute-in-new-frame)

;; End of config

(provide 'init-commands)
