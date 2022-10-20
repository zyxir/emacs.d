;;; init-benchmark.el --- Startup benchmarking -*- lexical-binding: t -*-


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; Measure the time took to load each part of the configuration.

;;; Code:

;;;; Startup Message

(defun display-startup-echo-area-message ()
  "Display startup time after init."
  (message
   (format "ZyEmacs ready in %.2f seconds."
           (float-time
            (time-subtract (current-time) before-init-time)))))


;;;; The Benchmark Tree
;; ZyEmacs uses a tree structure to represent the loading of files.  Each node
;; of the tree stores some benchmarked information.

(defun zy/time-subtract-millis (time1 &optional time2)
  "Get the time from TIME1 to TIME2 in milliseconds.

If TIME2 is nil or omitted, use the current time instead."
  (* 1000 (float-time (if time2
			  (time-subtract time2 time1)
			(time-since time1)))))

(defvar zy/benchmark-tree (cons
			   `(name root
				  start ,(current-time)
				  since ,(zy/time-subtract-millis
					  before-init-time))
			   nil)
  "Benchmark results stored in a tree data structure.

Each node of the tree is a cons cell of the form:

  (ENTRY . CHILDREN)

ENTRY is a property list of benchmarking results.  Available
properties include:

`name' A feature symbol or a filename.

`type' Function used to load `name', such as `require' or `load'.

`start' Time when loading starts.  This property is only used
internally.

`since' Time since `before-init-time' when loading starts, in
milliseconds.

`taken' Time taken to load `name', in milliseconds.

`taken-adjusted' Like `taken', but with time taken by children
nodes removed.

CHILDREN is a list of the node's children nodes.")

(defvar zy/benchmark-current-node zy/benchmark-tree
  "The currently measured benchmark node.")


;;;; Benchmark Advices

(defun zy/benchmark-start (name type)
  "Start measuring NAME of TYPE."
  (let ((parent zy/benchmark-current-node)
	(node (cons `(name ,name type ,type start ,(current-time))
		    nil)))
    (setq zy/benchmark-current-node node)
    parent))

(defun zy/benchmark-stop (parent recordp)
  "Stop measuring, and store node to PARENT if RECORDP is non-nil."
  (let* ((node zy/benchmark-current-node)
	 (start (plist-get (car node) 'start)))
    (when recordp
      (plist-put (car node) 'since (zy/time-subtract-millis before-init-time start))
      (plist-put (car node) 'taken (zy/time-subtract-millis start))
      (setcdr parent (cons node (cdr parent))))
    (setq zy/benchmark-current-node parent)))

(defun zy/benchmark-around-require (require-func feature &rest args)
  "Benchmark the running of REQUIRE-FUNC on FEATURE.

REQUIRE-FUNC is a function like `require', and FEATURE is its
first argument.  ARGS is the list of remaining arguments."
  (if (featurep feature)
      feature
    (let ((parent (zy/benchmark-start feature 'require)))
      (prog1
	  (apply require-func feature args)
	(zy/benchmark-stop parent (featurep feature))))))

(defun zy/benchmark-around-load (load-func file &rest args)
  "Benchmark the running of LOAD-FUNC on FILE.

LOAD-FUNC is a function like `load', and FILE is its first
argument.  ARGS is the list of remaining arguments."
  (let ((parent (zy/benchmark-start file 'load)))
    (prog1
	(apply load-func file args)
      (zy/benchmark-stop parent t))))

(advice-add 'require :around 'zy/benchmark-around-require)
(advice-add 'load :around 'zy/benchmark-around-load)
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (advice-remove 'require 'zy/benchmark-around-require)
	    (advice-remove 'load 'zy/benchmark-around-load)))


(provide 'init-benchmark)

;;; init-benchmark.el ends here
