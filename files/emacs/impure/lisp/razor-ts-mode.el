;;; razor-ts-mode.el --- Major mode for Razor markup  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Alvin Hsu
;; Copyright (C) 2022-2025 Free Software Foundation, Inc.

;; Author: Alvin Hsu <aurtzy@gmail.com>
;; Keywords: languages

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

(require 'cl-lib)
(require 'csharp-mode)
(require 'html-ts-mode)
(require 'range)
(require 'seq)
(require 'treesit)


;;; Adapted range functions from `range.el'.
;; Some modifications are required:
;;
;; - `range.el' expects intervals that are inclusive (e.g., (1 . 5) includes
;;   5).  `treesit.el' expects exclusive intervals, so functions are adjusted
;;   to comply with this.
;;
;; - `range.el' may put single integers in place of ranges if they span
;;   one-length ranges, which `treesit.el' does not consider valid.

(defun razor-ts-mode--range-difference (range1 range2)
  "Return the range of elements in RANGE1 that do not appear in RANGE2.
Both ranges must be in ascending order."
  (setq range1 (range-normalize range1))
  (setq range2 (range-normalize range2))
  (let* ((new-range (cons nil (copy-sequence range1)))
         (r new-range))
    (while (cdr r)
      (let* ((r1 (cadr r))
             (r2 (car range2))
             (min1 (if (numberp r1) r1 (car r1)))
             (max1 (if (numberp r1) r1 (cdr r1)))
             (min2 (if (numberp r2) r2 (car r2)))
             (max2 (if (numberp r2) r2 (cdr r2))))

        (cond ((>= min1 max1)
               ;; Invalid range: may result from overlap condition (below)
               ;; remove Invalid range
               (setcdr r (cddr r)))
              ((not min2)
               ;; All done with range2
               (setq r nil))
              ((<= max1 min2)
               ;; No overlap: range1 precedes range2
               (pop r))
              ((<= max2 min1)
               ;; No overlap: range2 precedes range1
               (pop range2))
              ((and (<= min2 min1) (<= max1 max2))
               ;; Complete overlap: range1 removed
               (setcdr r (cddr r)))
              (t
               (setcdr r (nconc (list (cons min1 min2)
                                      (cons max2 max1))
                                (cddr r)))))))
    (cdr new-range)))


;;; Razor major mode settings.


;;;; Range rules.

(defvar razor-ts-mode-razor-match-predicates
  ;; NOTE: Don't match "@KEYWORD"; these are used as anchor points for
  ;; indentation in `razor-ts-mode--indent-rules'.
  `(,(rx (or "at_"
             (seq bos (or "@" "{" "}" "(" ")" "in"
                          "razor_html_attribute"
                          "razor_explicit_expression"
                          "razor_implicit_expression"
                          "razor_condition"
                          "razor_block"
                          "razor_page_directive"
                          "razor_using_directive"
                          "razor_inherits_directive")
                  eos)))
    ,(lambda (node)
       ;; HACK: @foreach doesn't wrap its conditional in a single node, so
       ;; match by their field names instead.  The `treesit-node-field-name'
       ;; call is expensive, so try to avoid calling it for
       ;; obviously-not-razor nodes.
       (when-let* ((_ (not (treesit-node-match-p
                            node (rx (or "html_"
                                         "tag"
                                         (seq bos "element" eos))))))
                   (field-name (treesit-node-field-name node)))
         (string-match-p (rx (or "type"
                                 "left"
                                 "right"))
                         field-name))))
  "List of predicates to determine whether a node represents Razor syntax.

Predicates are as defined in `treesit-thing-settings'.  If any predicate
in the list matches, a node will be considered a Razor node.

\"Razor syntax\" refers to any Razor markup that is not native HTML.")

(defun razor-ts-mode-language-at-point (pos)
  "Return the language associated with POS."
  ;; Razor is C#, but Razor is also HTML.  The HTML parser provides more
  ;; useful node information in some cases, so use that where applicable.
  (cond
   ((treesit-parser-range-on (treesit-parser-create 'html) pos) 'html)
   ('razor)))

(defun razor-ts-mode--match-predicates-p (node predicates)
  "Return whether NODE matches one of PREDICATES.

PREDICATES is a list of matcher specifications as defined in
`treesit-thing-settings'."
  (seq-some (apply-partially #'treesit-node-match-p node) predicates))

(defun razor-ts-mode--razor-node-p (node)
  "Return non-nil if NODE is a Razor node.

A Razor node is determined by `razor-ts-mode-razor-match-predicates'."
  (razor-ts-mode--match-predicates-p node razor-ts-mode-razor-match-predicates))

(defun razor-ts-mode--razor-ranges (&optional _start _end)
  "Return ranges of Razor syntax in the current buffer, from START to END."
  (let ((ranges nil)
        (ranges-end-pos (point-min)))
    (treesit-search-subtree
     (treesit-buffer-root-node 'razor)
     (lambda (node)
       ;; Skip nodes whose ancestors have already been matched.
       (when-let* ((node-end (treesit-node-end node))
                   (_ (and (not (<= node-end ranges-end-pos))
                           (razor-ts-mode--razor-node-p node)))
                   (node-start (treesit-node-start node)))
         (push (cons node-start node-end) ranges)
         (setq ranges-end-pos node-end))
       ;; We are using `treesit-search-subtree' to intentionally traverse
       ;; the entire tree, so we don't actually want to match anything.
       nil)
     nil
     :all)
    (seq-reverse ranges)))

(defun razor-ts-mode--set-ranges (_start _end)
  "Set ranges for the current buffer's parsers, between START and END."
  (treesit-parser-set-included-ranges
   (treesit-parser-create 'html)
   ;; TODO: Optimize by modifying the existing included ranges instead of
   ;; re-computing with the entire buffer.
   (razor-ts-mode--range-difference (cons (point-min) (point-max))
                                    (razor-ts-mode--razor-ranges
                                     (point-min) (point-max)))))


;;;; Editing.

(defvar razor-ts-mode--indent-rules
  `((razor
     ((node-is "}") parent-bol 0)
     ((match "{" "razor_else") parent-bol 0)
     ((node-is "razor_else") parent-bol 0)
     ((parent-is "razor_else") parent-bol csharp-ts-mode-indent-offset)
     ((match "{" "razor_if") parent-bol 0)
     ((parent-is "razor_if") parent-bol csharp-ts-mode-indent-offset)
     ((match "{" "razor_foreach") parent-bol 0)
     ((parent-is "razor_foreach") parent-bol csharp-ts-mode-indent-offset)
     ((match "{" "razor_finally") parent-bol 0)
     ((node-is "razor_finally") parent-bol 0)
     ((parent-is "razor_finally") parent-bol csharp-ts-mode-indent-offset)
     ((match "{" "razor_catch") parent-bol 0)
     ((node-is "razor_catch") parent-bol 0)
     ((parent-is "razor_catch") parent-bol csharp-ts-mode-indent-offset)
     ((match "{" "razor_try") parent-bol 0)
     ((parent-is "razor_try") parent-bol csharp-ts-mode-indent-offset)
     ((match "{" "razor_lock") parent-bol 0)
     ((parent-is "razor_lock") parent-bol csharp-ts-mode-indent-offset)

     ((node-is "html_comment") prev-sibling 0)
     ((parent-is "html_comment") prev-adaptive-prefix 0)

     ,@(pcase (and (boundp 'csharp-ts-mode--indent-rules)
                   (symbol-value 'csharp-ts-mode--indent-rules))
         (`((,_ . ,rules)) rules))

     ;; HTML rules.
     ((node-is "/>") parent-bol 0)
     ((node-is ">") parent-bol 0)
     ((node-is "</") parent-bol 0)
     ((parent-is "element") parent-bol csharp-ts-mode-indent-offset))))


;;;; Font lock.

(defvar razor-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   ;; Copied from `html-ts-mode--font-lock-settings'.
   :language 'html
   :override t
   :feature 'comment
   `((comment) @font-lock-comment-face)
   :language 'html
   :override t
   :feature 'keyword
   `("doctype" @font-lock-keyword-face)
   :language 'html
   :override t
   :feature 'definition
   `((tag_name) @font-lock-function-name-face)
   :language 'html
   :override t
   :feature 'string
   `((quoted_attribute_value) @font-lock-string-face)
   :language 'html
   :override t
   :feature 'property
   `((attribute_name) @font-lock-variable-name-face)

   ;; Copied from `csharp-ts-mode--font-lock-settings', with `:language'
   ;; modified for matching razor nodes instead.
   :language 'razor
   :feature 'expression
   '((conditional_expression (identifier) @font-lock-variable-use-face)
     (postfix_unary_expression (identifier)* @font-lock-variable-use-face)
     (initializer_expression (assignment_expression left: (identifier) @font-lock-variable-use-face)))

   :language 'razor
   :feature 'bracket
   '((["(" ")" "[" "]" "{" "}"]) @font-lock-bracket-face)

   :language 'razor
   :feature 'delimiter
   '((["," ":" ";"]) @font-lock-delimiter-face)

   :language 'razor
   :feature 'error
   '((ERROR) @font-lock-warning-face)

   :language 'razor
   :override t
   :feature 'comment
   '((comment) @font-lock-comment-face)

   :language 'razor
   :override t
   :feature 'keyword
   `([,@csharp-ts-mode--keywords] @font-lock-keyword-face
     (modifier) @font-lock-keyword-face
     ,@(if (csharp-ts-mode--test-this-expression)
           '((this_expression) @font-lock-keyword-face)
         '("this" @font-lock-keyword-face)))

   :language 'razor
   :override t
   :feature 'property
   `((attribute (identifier) @font-lock-property-use-face (attribute_argument_list))
     (attribute (identifier) @font-lock-property-use-face))

   :language 'razor
   :override t
   :feature 'escape-sequence
   '((escape_sequence) @font-lock-escape-face)

   :language 'razor
   :override t
   :feature 'literal
   `((integer_literal) @font-lock-number-face
     (real_literal) @font-lock-number-face
     (null_literal) @font-lock-constant-face
     (boolean_literal) @font-lock-constant-face)

   :language 'razor
   :override t
   :feature 'string
   `([(string_literal)
      (verbatim_string_literal)
      ,@(if (csharp-ts-mode--test-interpolated-string-text)
            '((interpolated_string_text)
              (interpolated_verbatim_string_text)
              (character_literal)
              "\""
              "$\""
              "@$\""
              "$@\"")
          '((interpolated_string_expression)
            (interpolation_start)
            (interpolation_quote)))]
     @font-lock-string-face)

   :language 'razor
   :override t
   :feature 'type
   `((predefined_type) @font-lock-type-face
     (implicit_type) @font-lock-type-face
     (nullable_type) @font-lock-type-face
     (type_parameter
      (identifier) @font-lock-type-face)
     (type_argument_list
      (identifier) @font-lock-type-face)
     (type_argument_list
      (generic_name
       (identifier) @font-lock-type-face))
     (base_list
      (generic_name
       (identifier) @font-lock-type-face))
     (array_type
      (identifier) @font-lock-type-face)
     (cast_expression (identifier) @font-lock-type-face)
     (cast_expression (generic_name (identifier) @font-lock-type-face))
     ["operator"] @font-lock-type-face
     (type_parameter_constraints_clause
      (identifier) @font-lock-type-face)
     ,@(if (csharp-ts-mode--test-type-constraint)
           '((type_constraint type: (identifier) @font-lock-type-face)
             (type_constraint type: (generic_name (identifier) @font-lock-type-face)))
         '((type_parameter_constraint (type type: (identifier) @font-lock-type-face))
           (type_parameter_constraint (type type: (generic_name (identifier) @font-lock-type-face)))))

     ,@(when (csharp-ts-mode--test-type-of-expression)
         '((type_of_expression (identifier) @font-lock-type-face))
         '((typeof_expression (identifier) @font-lock-type-face)))

     (object_creation_expression
      type: (identifier) @font-lock-type-face)
     (object_creation_expression
      type: (generic_name (identifier) @font-lock-type-face))
     (as_expression right: (identifier) @font-lock-type-face)
     (as_expression right: (generic_name (identifier) @font-lock-type-face)))

   :language 'razor
   :feature 'definition
   :override t
   `((qualified_name (identifier) @font-lock-type-face)
     (using_directive (identifier) @font-lock-type-face)
     ,@(when (csharp-ts-mode--test-name-equals)
         '((using_directive (name_equals
                             (identifier) @font-lock-type-face))))

     (enum_declaration (identifier) @font-lock-type-face)
     (enum_member_declaration (identifier) @font-lock-variable-name-face)

     (interface_declaration (identifier) @font-lock-type-face)

     (struct_declaration (identifier) @font-lock-type-face)

     (record_declaration (identifier) @font-lock-type-face)
     (namespace_declaration (identifier) @font-lock-type-face)
     (base_list (identifier) @font-lock-type-face)
     (property_declaration
      type: (nullable_type) @font-lock-type-face
      name: (identifier) @font-lock-variable-name-face)
     (property_declaration
      type: (predefined_type) @font-lock-type-face
      name: (identifier) @font-lock-variable-name-face)
     (property_declaration
      type: (identifier) @font-lock-type-face
      name: (identifier) @font-lock-variable-name-face)
     (class_declaration (identifier) @font-lock-type-face)

     (constructor_declaration name: (_) @font-lock-type-face)
;;;;; Handle different releases of tree-sitter-c-sharp.
;;;;; Check if keyword void_keyword is available, then return the correct rule."
     ,@(condition-case nil
           (progn (treesit-query-capture 'csharp '((void_keyword) @capture))
                  `((method_declaration ,csharp-ts-mode--type-field [(identifier) (void_keyword)] @font-lock-type-face)))
         (error
          `((method_declaration ,csharp-ts-mode--type-field [(identifier) (predefined_type)] @font-lock-type-face))))
     (method_declaration ,csharp-ts-mode--type-field (generic_name (identifier) @font-lock-type-face))
     (method_declaration name: (_) @font-lock-function-name-face)

     (catch_declaration
      ((identifier) @font-lock-type-face))
     (catch_declaration
      ((identifier) @font-lock-type-face
       (identifier) @font-lock-variable-name-face))

     (variable_declaration (identifier) @font-lock-type-face)
     (variable_declaration (generic_name (identifier) @font-lock-type-face))
     (variable_declarator (identifier) @font-lock-variable-name-face)

     (parameter type: (identifier) @font-lock-type-face)
     (parameter type: (generic_name (identifier) @font-lock-type-face))
     (parameter name: (identifier) @font-lock-variable-name-face)

     (lambda_expression (identifier) @font-lock-variable-name-face)

     (declaration_expression type: (identifier) @font-lock-type-face)
     (declaration_expression name: (identifier) @font-lock-variable-name-face))

   :language 'razor
   :feature 'function
   '((invocation_expression
      function: (member_access_expression
                 name: (identifier) @font-lock-function-call-face))
     (invocation_expression
      function: (identifier) @font-lock-function-call-face)
     (invocation_expression
      function: (member_access_expression
                 name: (generic_name (identifier) @font-lock-function-call-face)))
     (invocation_expression
      function: (generic_name (identifier) @font-lock-function-call-face)))

   :language 'razor
   :feature 'escape-sequence
   :override t
   '((escape_sequence) @font-lock-escape-face)

   :language 'razor
   :feature 'directives
   :override t
   (if (csharp-ts-mode--test-if-directive)
       '((if_directive
          "if" @font-lock-preprocessor-face
          (identifier) @font-lock-variable-use-face)
         (elif_directive
          "elif" @font-lock-preprocessor-face
          (identifier) @font-lock-variable-use-face)
         (else_directive) @font-lock-preprocessor-face
         (endif_directive) @font-lock-preprocessor-face
         (define_directive
          "define" @font-lock-preprocessor-face
          (identifier) @font-lock-variable-use-face)
         (nullable_directive) @font-lock-preprocessor-face
         (pragma_directive) @font-lock-preprocessor-face
         (region_directive) @font-lock-preprocessor-face
         (endregion_directive) @font-lock-preprocessor-face
         (region_directive
          (preproc_message) @font-lock-variable-use-face)
         (endregion_directive
          (preproc_message) @font-lock-variable-use-face))
     '((preproc_if
        "#if" @font-lock-preprocessor-face
        (identifier) @font-lock-variable-use-face)
       (preproc_elif
        "#elif" @font-lock-preprocessor-face
        (identifier) @font-lock-variable-use-face)
       (preproc_else) @font-lock-preprocessor-face
       "#endif" @font-lock-preprocessor-face
       (preproc_define
        "#define" @font-lock-preprocessor-face
        (preproc_arg) @font-lock-variable-use-face)
       (preproc_nullable) @font-lock-preprocessor-face
       (preproc_pragma) @font-lock-preprocessor-face
       (preproc_region) @font-lock-preprocessor-face
       (preproc_endregion) @font-lock-preprocessor-face
       (preproc_region
        (preproc_arg) @font-lock-variable-use-face)
       (preproc_endregion
        (preproc_arg) @font-lock-variable-use-face)))

   ;; Additional HTML font lock rules.
   :default-language 'html
   :feature 'error
   '((erroneous_end_tag_name) @font-lock-warning-face)

   ;; Additional Razor font lock rules.
   :default-language 'razor
   :override t
   :feature 'escape-sequence
   '(((_ ["@" "at_at_escape"] @font-lock-escape-face)))
   :feature 'keyword
   '((((_ ["at_using" "at_page" "at_inject" "at_inherits" "at_block"
           "at_try" "at_if" "at_lock" "at_foreach"
           "else" "if" "bind"]
          @font-lock-keyword-face)))))
  "Tree-sitter font-lock settings for `razor-ts-mode'.")


;;;; Navigation.

(defun razor-ts-mode--defun-name (node)
  "Return the defun name for NODE, or nil if there is none."
  (pcase (treesit-node-type node)
    ((or "method_declaration"
         "record_declaration"
         "struct_declaration"
         "enum_declaration"
         "interface_declaration"
         "class_declaration")
     (treesit-node-text (treesit-node-child-by-field-name node "name") t))
    ("tag_name"
     (treesit-node-text node t))))


;;; Keymaps.

(defvar-keymap razor-ts-mode-map
  :doc "Keymap for Razor tree-sitter mode."
  :parent html-mode-map
  "C-M-q" #'prog-indent-sexp)


;;; Razor major mode.

(define-derived-mode razor-ts-mode html-mode "Razor"
  "A mode for editing Razor markup."
  :group 'razor-mode
  (when (and (treesit-ready-p 'razor) (treesit-ready-p 'html))
    (setq-local treesit-primary-parser (treesit-parser-create 'razor))

;;;; Set up embedded HTML parser.

    (treesit-parser-create 'html)
    (setq-local treesit-range-settings
                (treesit-range-rules #'razor-ts-mode--set-ranges))
    (setq-local treesit-language-at-point-function
                #'razor-ts-mode-language-at-point)

;;;; Editing.

    (setq-local treesit-simple-indent-rules razor-ts-mode--indent-rules)
    (setq-local electric-indent-chars
                (append "{}():;," electric-indent-chars))
    ;; TODO: set up comments.

;;;; Font lock.

    (setq-local treesit-font-lock-settings razor-ts-mode--font-lock-settings)
    (setq-local treesit-font-lock-feature-list
                ;; Combine feature lists.
                '(( comment definition)
                  ( property string keyword type directives)
                  ( constant escape-sequence expression literal property)
                  ( function bracket delimiter error)))

;;;; Navigation.

    (setq-local treesit-defun-type-regexp
                (rx (or "element" "razor" "declaration")))
    (setq-local treesit-defun-name-function #'razor-ts-mode--defun-name)
    (setq-local treesit-thing-settings
                `((html
                   (sexp (or "element"
                             "text"
                             "attribute"
                             "value"))
                   (sentence "tag")
                   (text (or "comment" "text")))
                  (razor
                   (sexp (or "razor"
                             "attribute"
                             "string"
                             "list"
                             "initializer_expression"
                             "identifier"
                             "text"))
                   (text (or "comment" "text")))))
    (setq-local treesit-simple-imenu-settings
                '(("Element" "\\`tag_name\\'" nil nil)
                  ;; FIXME: These don't get matched, for some reason.
                  ;; ("Class" "\\`class_declaration\\'" nil nil)
                  ;; ("Interface" "\\`interface_declaration\\'" nil nil)
                  ;; ("Enum" "\\`enum_declaration\\'" nil nil)
                  ;; ("Record" "\\`record_declaration\\'" nil nil)
                  ;; ("Struct" "\\`struct_declaration\\'" nil nil)
                  ;; ("Method" "\\`method_declaration\\'" nil nil)
                  ))
    (setq-local treesit-outline-predicate
                (rx (or (seq bos "element" eos) "declaration")))

;;;; Override inherited defaults.

    ;; `html-ts-mode' inherits from `html-mode' that sets
    ;; regexp-based outline variables.  So need to restore
    ;; the default values of outline variables to be able
    ;; to use `treesit-outline-predicate' above.
    (kill-local-variable 'outline-regexp)
    (kill-local-variable 'outline-heading-end-regexp)
    (kill-local-variable 'outline-level)
    ;; `sgml-mode' sets `syntax-ppss-table', which seems to mess with
    ;; `electric-pair-mode'.
    (kill-local-variable 'syntax-ppss-table)

;;;; Conclude setup.

    (treesit-major-mode-setup)

    ;; HACK: Don't use HTML parser for any indentation.  In that aspect, the
    ;; Razor parser will have more information, so override it for calls to
    ;; indent functions.
    (cl-flet ((wrap-language-override (fun)
                (lambda (&rest args)
                  (cl-letf (((symbol-function 'razor-ts-mode-language-at-point)
                             (cl-constantly 'razor)))
                    (apply fun args)))))
      (setq-local indent-line-function
                  (wrap-language-override #'treesit-indent))
      (setq-local indent-region-function
                  (wrap-language-override #'treesit-indent-region)))))

(provide 'razor-ts-mode)
;;; razor-ts-mode.el ends here
