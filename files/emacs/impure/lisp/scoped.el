;;; scoped.el ---                                -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Alvin Hsu

;; Author: Alvin Hsu <aurtzy@gmail.com>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
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

;; 

;;; Code:

(require 'transient)

(defclass scoped-scope ()
  ((values :initarg :values
           :initform nil
           :documentation "\
An association list of scoped values for variables.  Each element should
element should be of the form (SYMBOL . VALUE), where SYMBOL is a
variable that is to be set to VALUE when executing
`scoped-prefix-apply'.")))

(defclass scoped-prefix (transient-prefix)
  ((init-scope :initarg :init-scope
               :initform nil
               :documentation "\
Function that is called with the `scoped-scope' object that this prefix
will be initialized with.  The object may have values propagated from
the previously-active menu."))
  "Transient prefix, whose scope can be shared with other scoped prefixes.")

(cl-defmethod transient-init-scope ((obj scoped-prefix))
  "Initialize OBJ scope.

Reuse the current (calling) prefix's scope if it is of type
`scoped-prefix'; otherwise, a new `scoped-scope' is initialized for the
prefix being set up.

Scope values are merged into the existing `scoped-scope' object, if any."
  (let* ((scope (cond
                 ((cl-typep transient-current-prefix 'scoped-prefix)
                  (transient-scope nil 'scoped-prefix))
                 ;; This method is also called for situations like returning
                 ;; from a sub-prefix, in which case we want to preserve the
                 ;; existing scope.
                 ((oref obj scope))
                 ((scoped-scope))))
         (init-scope (oref obj init-scope)))
    (unless (cl-typep scope 'scoped-scope)
      (error "Unexpected scope type `%s'; should be `%s'"
             (type-of scope) 'scoped-scope))
    (when init-scope (funcall init-scope scope))
    (oset obj scope scope)))

(defun scoped-prefix-apply (fun &rest args)
  "Apply FUN to ARGS within the `scoped-prefix' scope context."
  (let* ((scope (transient-scope nil 'scoped-prefix))
         (values (oref scope values))
         orig-values)
    (unwind-protect
        (progn
          (seq-each (pcase-lambda (`(,symbol . ,form))
                      (push (cons symbol (symbol-value symbol)) orig-values)
                      (set symbol form))
                    values)
          (apply fun args))
      (seq-each (pcase-lambda (`(,symbol . ,value))
                  (set symbol value))
                orig-values))))

(defclass scoped-option (transient-infix)
  ((always-read :initform t)
   (variable :initarg :variable))
  "Class for options that set scope for `scoped-option' prefixes.")

(cl-defmethod transient-infix-set ((obj scoped-option) value)
  (let* ((scope (transient-scope nil 'scoped-prefix))
         (values (oref scope values))
         (key (oref obj variable)))
    (setf (alist-get key values) (oset obj value value))
    (oset scope values values)))

(cl-defmethod transient-format-description ((obj scoped-option))
  (or (cl-call-next-method obj)
      (symbol-name (oref obj variable))))

(cl-defmethod transient-format-value ((obj scoped-option))
  (let* ((values (oref (transient-scope nil 'scoped-prefix) values))
         (key (oref obj variable))
         (value (alist-get key values)))
    (propertize (prin1-to-string value) 'face 'transient-value)))

(cl-defmethod transient-prompt ((obj scoped-option))
  (if (and (slot-boundp obj 'prompt)
           (oref obj prompt))
      (cl-call-next-method obj)
    (format "Set %s: " (oref obj variable))))

(provide 'scoped)
;;; scoped.el ends here
