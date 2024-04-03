;;; zylib-key.el --- Keybinding utility. -*- lexical-binding: t -*-
;;; Commentary:

;; This file defines the keybinding framework of the configuration. It provides
;; two simple macros, `keybind!' and `defprefix!', for an efficient and
;; consistent keybinding interface and automatic loading management.
;;
;; Due to the deep coupling of the keybinding framework with Evil, it is hard to
;; implement these utilities without Evil. Therefore Evil is the only package
;; that is required in Zylib. Nevertheless, this file trys to only use the
;; utility functions provided to Evil, and leave all customizations of Evil to
;; the `evil' module, including choosing the (local) leader key.

;;; Code:

(require 'zylib-pkg)

(pkg! 'evil)

(defun zy--feature-p (maybe-feature)
  "Return t if MAYBE-FEATURE is a loadable feature.
This will load MAYBE-FEATURE if it is indeed a feature."
  (condition-case nil
      (require maybe-feature)
    (error nil)
    (:success t)))

(defun zy--try-suffix (keymap-symbol suffix)
  "Try to find the feature by which KEYMAP-SYMBOL is defined.
SUFFIX is used to guess the name of the feature.

First ensure that the KEYMAP-SYMBOL with SUFFIX removed is indeed
a feature via `zy--feature-p', then check if KEYMAP-SYMBOL is a
defined variable after the feature is loaded. If all results are
true, return the feature. Otherwise return nil."
  (let ((name (symbol-name keymap-symbol)))
    (and
     (string-suffix-p suffix name)
     (let ((feature
            (intern (substring name 0 (- (length suffix))))))
       (and (zy--feature-p feature)
            (boundp keymap-symbol)
            feature)))))

(defun zy--get-keymap-feature (keymap-symbol)
  "Try to get the feature by which KEYMAP-SYMBOL is defined.
KEYMAP-SYMBOL is a symbol representing a keymap.

Return nil if no feature could be found."
  (cl-some (lambda (suffix) (zy--try-suffix keymap-symbol suffix))
           '("-map" "-mode-map" "-command-map" "-keymap")))

(defmacro keybind! (state keymap key def &rest bindings)
  "Create a STATE binding from KEY to DEF for KEYMAP.

STATE is one of the Evil states, or a list of one or more of
them. Omitting a state by using nil corresponds to a standard
Emacs binding using ‘define-key’.

KEYMAP is a keymap to define the binding in. If KEYMAP is the
quoted symbol `global', the global evil keymap corresponding to
the state(s) is used.

If KEYMAP is not quoted, whether it is already loaded or not, try
to find the feature that defines it, and load the keybinding(s)
only after that feature is available.

KEY and DEF are like those in `define-key', except that if KEY is
a string, it is always wrapped in `kbd' before being used.

It is possible to specify multiple KEY and DEF pairs in BINDINGS.

\(fn STATE KEYMAP KEY DEF [KEY DEF] ...)"
  (declare (indent 2))
  (let* ((bindings (append `(,key ,def) bindings))
         (index 0)
         (cur nil)
         (wrapped-bindings '())
         (features '(evil)))
    ;; Try to find the feature that defines `keymap'.
    (when-let ((feature (and (symbolp keymap)
                             (zy--get-keymap-feature keymap))))
      (push feature features))
    ;; Wrap all string keys in `bindings' with `kbd'.
    (while bindings
      (setq cur (pop bindings))
      (push (if (and (eq (% index 2) 0)
                     (stringp cur))
                `(kbd ,cur) cur)
            wrapped-bindings)
      (setq index (+ index 1)))
    (setq wrapped-bindings (reverse wrapped-bindings))
    `(after! '(,@features)
       (declare-function evil-define-key* 'evil-core)
       (evil-define-key* ,state ,keymap ,@wrapped-bindings))))

(defmacro defprefix! (command name state keymap key &rest bindings)
  "Define COMMAND as a prefix command. COMMAND should be a symbol.

A new sparse keymap is stored as COMMAND's function definition
and its value. NAME is the menu name string for the map.

Bind this prefix command to KEY in STATE and KEYMAP as per
`keybind!'. If KEY is a string, it will be wrapped in `kbd'
before being used to bind the key. If KEYMAP is a unquoted list,
bind this prefix command to KEY in each KEYMAP.

You may continue to define keybindings using KEY and DEF pairs as
per `keybind!' in BINDINGS."
  (declare (indent 5))
  (let* ((keybind-form-upper
          (if (or (symbolp keymap)
                  (eq (car-safe keymap) 'quote))
              `(keybind! ,state ,keymap ,key '(,name . ,command))
            (macroexp-progn
             (seq-map (lambda (x)
                        `(keybind! ,state ,x ,key '(,name . ,command)))
                      keymap))))
         (form `(prog1
                    (define-prefix-command ',command)
                  (defvar ,command)
                  ,keybind-form-upper))
         (keybind-form-lower (when bindings
                               `(keybind! nil ,command ,@bindings))))
    (append form `(,keybind-form-lower))))

(defun zy--other-window-prefix ()
  "Display the buffer of the next command in a new window.
This is like `other-window-prefix', but it echoes nothing, and
returns a function used to undo the prefix."
  (display-buffer-override-next-command
   (lambda (buffer alist)
     (let ((alist (append '((inhibit-same-window . t)) alist))
           window type)
       (if (setq window (display-buffer-pop-up-window buffer alist))
           (setq type 'window)
         (setq window (display-buffer-use-some-window buffer alist)
               type 'reuse))
       (cons window type)))))

(defun zy--other-frame-prefix ()
  "Display the buffer of the next command in a new frame.
This is like `other-frame-prefix', but it echoes nothing, and
returns a function used to undo the prefix."
  (display-buffer-override-next-command
   (lambda (buffer alist)
     (cons (display-buffer-pop-up-frame
            buffer (append '((inhibit-same-window . t))
                           alist))
           'frame))))

(defun zy--other-tab-prefix ()
  "Display the buffer of the next command in a new tab.
This is like `other-tab-prefix', but it echoes nothing, and
returns a function used to undo the prefix."
  (display-buffer-override-next-command
   (lambda (buffer alist)
     (cons (progn
             (display-buffer-in-tab
              buffer (append alist '((inhibit-same-window . nil))))
             (selected-window))
           'tab))))

(defmacro other-placed! (place string keymap)
  "Get the \"other place\" version of KEYMAP.
The \"other place\" version of KEYMAP is an extended menu item
definition which can be used in `define-key'. It behaves like
KEYMAP, but displays the buffer of the subsequent command in
another place, in accordance with PLACE, which have the following
possible values:

`window' a new window
`frame'  a new frame
`tab'    a new tab

STRING is used as the menu item name for the definition."
  (let ((fn (intern (format "%s-other-%s-%s" keymap place (cl-gensym)))))
    `(progn
       ;; This function acts as the `:filter' property of the extended menu
       ;; item definition. It is called every time the menu item is accessed
       ;; to dynamically compute the keymap for the menu item. What the
       ;; function does is: (a) Call `zy--other-window-prefix' so that the
       ;; next command displays its buffer in another window; (b) Compose an
       ;; `aux-map' as the backup map; (c) Return the (STRING . DEFN) pair
       ;; used by `define-key', where STRING is the menu item name extracted
       ;; from `keymap', and `defn' is the keymap composed from `keymap' and
       ;; `aux-map'.
       ;;
       ;; Using `aux-map' as a backup keymap here ensures that if a key not in
       ;; `keymap' is hit, the effect of `zy--other-window-prefix' is
       ;; cancelled, and an "X is undefined" message is displayed.
       (defun ,fn (_cmd)
         ,(format "Trigger `%s' in another window." keymap)
         (let* ((exitfun (,(pcase (unquote! place)
                             ('window 'zy--other-window-prefix)
                             ('frame 'zy--other-frame-prefix)
                             ('tab 'zy--other-tab-prefix)
                             (_ (error "%s is not a valid place" place)))))
                (aux-map (make-sparse-keymap)))
           (define-key aux-map [t]
                       (lambda ()
                         (interactive)
                         (funcall exitfun)
                         (funcall 'undefined)))
           (cons ,string (make-composed-keymap ,keymap aux-map))))
       ;; Now compose and return the extended menu item definition.
       '(menu-item "" nil :filter ,fn))))

(defmacro other-windowed! (string keymap)
  "Get the \"other window\" version of KEYMAP.
The \"other window\" version of KEYMAP is an extended menu item
definition which can be used in `define-key'. It behaves like
KEYMAP, but displays the buffer of the sebsequent command in
another window.

STRING is used as the menu item for the definition."
  `(other-placed! 'window ,string ,keymap))

(defmacro other-framed! (string keymap)
  "Get the \"other frame\" version of KEYMAP.
The \"other frame\" version of KEYMAP is an extended menu item
definition which can be used in `define-key'. It behaves like
KEYMAP, but displays the buffer of the sebsequent command in
another frame.

STRING is used as the menu item for the definition."
  `(other-placed! 'frame ,string ,keymap))

(defmacro other-tabbed! (string keymap)
  "Get the \"other tab\" version of KEYMAP.
The \"other tab\" version of KEYMAP is an extended menu item
definition which can be used in `define-key'. It behaves like
KEYMAP, but displays the buffer of the sebsequent command in
another tab.

STRING is used as the menu item for the definition."
  `(other-placed! 'tab ,string ,keymap))

(provide 'zylib-key)

;;; zylib-key.el ends here
