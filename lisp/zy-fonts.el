;;; zy-fonts.el --- ZyEmacs font utilities.

(defvar zf--fs-counter 0
  "Number of fontsets created by ZyEmacs.")


;; Bottom-level mechanism.

;;;###autoload
(defun zf--set-main-font (font &optional face frame)
  "Set the default font for all character sets.

FONT is a font-spec object or a font name string.

FACE is the face to set, 'default' by default.

FRAME is the frame to set, 'nil' by default."
  (set-face-attribute (if face face 'default) frame :font font)
  (unless (font-get font :size)
    (set-face-attribute (if face face 'default) frame :height 'unspecified)))

;;;###autoload
(defun zf--set-charset-font (charset font &optional face frame
				     append)
  "Set the font used for CHARSET.

CHARSET could be a character set, or a list of character sets.
If CHARSET is'cjk', it represents all CJK-related character set.

FONT is a font-spec object or a font name string.

FACE is the face to set, 'default' by default.

FRAME is the frame to set, 'nil' by default.

If APPEND is non-nil, the font should be appended to the existing
config."
  (let* ((face (if face face 'default))
	 (charset (if (equal charset 'cjk)
		      '(han cjk-misc bopomofo kana hangul)
		    charset))
	 fontset
	 fontset-new-p)
    (setq fontset (face-attribute face :fontset frame))
    (when (equal fontset 'unspecified)
      (setq fontset
	    (new-fontset
               (format "-*-*-*-*-*--*-*-*-*-*-*-fontset-zfs%d"
                       zf--fs-counter)
	       nil)
	    zf--fs-counter (+ zf--fs-counter 1)
	    fontset-new-p t))
    (if (listp charset)
	(dolist (c charset)
	  (set-fontset-font fontset c font frame append))
      (set-fontset-font fontset charset font frame append))
    (when fontset-new-p
      (set-face-attribute face frame :fontset fontset))))


;; User level font setter.

;;;###autoload
(defun zf-set-font-for (target font &optional face frame append)
  "Set font for TARGET according to arguments.

if TARGET is 'main', set the main font; if it is 'cjk', set font
for all CJK-related charsets; if it is a charset or a list of
charsets, set font for them.  TARGET is 'main' by default.

FONT is a font-spec object or a font name string.

FACE is the face to set, 'default' by default.

FRAME is the frame to set, 'nil' by default.

If APPEND is non-nil, the font should be appended to existing
config. If TARGET is 'main', APPEND is ignored."
  (cond
   ((or (equal target 'main) (equal target nil))
    (zf--set-main-font font face frame))
   (t
    (zf--set-charset-font target font face frame append))))

;;;###autoload
(defun zf-set-font (main-font &optional size cjk-font face frame)
  "A simple but less flexible function to set font.

Set main font to MAIN-FONT, optionally with pixel size SIZE, and
optionally set CJK font to CJK-FONT.

FACE is the face to set, 'default' by default.

FRAME is the frame to set, 'nil' by default."
  (zf--set-main-font (font-spec :family main-font :size size)
		     face frame)
  (when cjk-font
    (zf--set-charset-font 'cjk (font-spec :family cjk-font)
			  face frame nil)))


;; Font availability utility.

;;;###autoload
(defmacro zf-font-available-p (font)
  "Return FONT if FONT is available, or nil if not.

FONT is a font-spec object or a font string."
  `(if ,(if (stringp font)
	    `(x-list-fonts ,font)
	  `(find-font ,font))
       ,font
     nil))

;;;###autoload
(defmacro zf-pick-font (&rest fonts)
  "Get the first available font in FONTS.

Each element of FONTS is a font-spec object or a font string."
  (let* ((expr '(or)))
    (dolist (font fonts)
      (add-to-list
       'expr
       (macroexpand `(zf-font-available-p ,font))
       'append))
    expr))



(provide 'zy-fonts)
