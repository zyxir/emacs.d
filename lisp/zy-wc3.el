;;; zy-wc3.el --- WC3 map-making helper functions.

(defun zy-wc3colorize (text color)
  "Wrap TEXT in COLOR code."
  (concat "|cff" color text "|r"))

(defun zy-wc3data (text maxlvl)
  "Generate data text TEXT with MAXLVL."
  (let (result
	tag)
    (dotimes (lvl maxlvl)
      (if result
	  (setq result (concat result "/"))
	(setq result ""))
      (setq tag (replace-regexp-in-string
		 ""
		 (int-to-string (+ lvl 1))
		 text
		 t 'literal))
      (setq result (concat result tag)))
    result))

;;;###autoload
(defun zy-wc3ability (desc alt params comment &optional maxlvl)
  "Generate WC3 ability description.

DESC is the main description in normal color.

ALT is alternative description in green (#CFF21BF).

PARAMS is a list of (PARAM VALUE UNIT) where PARAM is an ability
parameter, VALUE is the value specifier with  replacing the
actual level, and UNIT is the unit for the value.

COMMENT is the comment of the ability.

MAXLVL is the maximum level of the ability."
  (let ((result desc)
	(color-alt "bfff82")
	(color-param "ecce87")
	(color-comment "00ebff"))
    (setq result (concat result
			 (zy-wc3colorize alt color-alt)
			 "|n|n"))
    (dolist (pvu params)
      (let ((param (car pvu))
	    (value (cadr pvu))
	    (unit (caddr pvu)))
	(setq result
	      (concat result
		      (zy-wc3colorize (concat
				       param
				       ":")
				      color-param)
		      " "
		      (zy-wc3data value maxlvl)
		      unit
		      "|n"))))
    (setq result (concat result
			 "|n"
			 (zy-wc3colorize comment color-comment)))))

;;; end of zy-wc3.el
