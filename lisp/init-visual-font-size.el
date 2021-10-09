;;; -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:

;; Font size adjustion utilities.

;;; Code:

(defvar zy/current-font-size zy/default-font-size
  "Current font size.

Should be one of `zy/font-size-choices'.")

(defconst zy/font-size-choices
  '(6 8 10 12 14 16 20 24 28 34 40 50 60 70 80)
  "Possible values of font sizes.

Even numbers make sure Chinese and Latin scripts always align.")

(defun zy:set-font-size (size)
  "Set all font size to SIZE."
  (zy:set-font zy/current-font size)
  (zy:set-vpfont zy/current-vpfont size)
  (setq zy/current-font-size size))

(defun zy:inc-or-dec-font-size (&optional dec)
  "Increase font size for one step.

If DEC is non-nil, decrease one step instead."
  (let* ((choices (or (and dec
			   (reverse zy/font-size-choices))
		      zy/font-size-choices))
	 (target-size
	  (cadr
	   (member zy/current-font-size choices))))
    (when target-size
      (zy:set-font-size target-size))))

(defun zy:reset-font-size ()
  "Reset font size to default.

If the current font size is already default, do nothing."
  (unless (eq zy/current-font-size zy/default-font-size)
    (zy:set-font-size zy/default-font-size)))

(defun zy:font-size-adjust-action (key)
  "Adjust font according to KEY.

Different actions are conducted with different ARG:

  ?+, ?=   Increase font size by one step
  ?-       Decrease font size by one step
  ?0       Reset font size
  others   Do nothing

Font size can only be set to one of `zy/font-size-choices'"
  (pcase key
   ((or ?+ ?=)
    (zy:inc-or-dec-font-size))
   (?-
    (zy:inc-or-dec-font-size 'dec))
   (?0
    (zy:reset-font-size))))

(defun zy/font-size-adjust ()
  "Adjust the font size globally.

The actual adjustment made depends on the final component of the
key-binding used to invoke the command, with all modifiers removed:

  +, =   Increase font size by one step
  -      Decrease font size by one step
  0      Resset font size to default

After adjusting, continue to read input events and further adjust
font size as long as the input event read \(with all modifiers
removed) is one of the above characters.

Font size can only be one value of `zy/font-size-choices'. The
default font size is `zy/default-font-size'."
  (interactive)
  (let* ((echo-keystrokes nil)
	 (ev last-command-event)
	 (key (event-basic-type ev)))
    (zy:font-size-adjust-action key)
    (message "[%s] Use +,-,0 for further adjustment."
	     (propertize
	      (int-to-string zy/current-font-size)
	      'face
	      'info-title-2))
    (set-transient-map
     (let ((map (make-sparse-keymap)))
       (dolist (mods '(() (control)))
	 (dolist (key '(?- ?+ ?= ?0))
	   (define-key map (vector (append mods (list key)))
	     #'zy/font-size-adjust)))
       map))))

;; End of config.

(provide 'init-visual-font-size)
