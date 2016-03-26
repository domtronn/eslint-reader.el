;;; eslint-reader.el --- Read an eslint file for usefulness

;; Copyright (C) 2016  Dominic Charlesworth <dgc336@gmail.com>

;; Author: Dominic Charlesworth <dgc336@gmail.com>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Read the contents of an eslint file and parse some of the rules
;; into useful variables for formatting etc

;;; Code:

(require 'json)
(require 'flycheck)

;; Core functions
(require 'eslint-reader-core)

;; Rule functions
(require 'eslint-reader-indent)
(require 'eslint-reader-semi)
(require 'eslint-reader-quotes)

(defun eslint-reader-strict (&optional pfx)
  "Whether or not you need a 'use strict' statement.
Returns nil if statement is not needed, otherwise t.  When given
a PFX it will return the string to insert with quote
characters."
  (interactive "P")
  (let ((rule (plist-get (eslint-reader--read) :strict))
        (qc   (eslint-reader-quotes t)))
    (if (and (vectorp rule) (not (equal (elt rule 1) "never")))
      (if pfx (format "%suse strict%s" qc qc) t)
      (if pfx "" t))))

(defun eslint-reader-block-spacing (&optional pfx)
  "Whether or not you should have block spacing.
Given a PFX it will return the character to insert."
  (interactive "P")
  (let ((rule (plist-get (eslint-reader--read) :block-spacing)))
    (if (and (vectorp rule) (equal (elt rule 1) "always"))
      (if pfx " " t)
      (if pfx "" nil))))

;;; Padded Block rules

(defun eslint-reader-padded-blocks (&optional pfx)
  "Whether to pad blocks of code.
Given a PFX it will return the character to insert."
  (interactive "P")
  (let ((rule (plist-get (eslint-reader--read) :padded-blocks)))
    (if (and rule (> (elt rule 0) 0))
      (cond
       ((equal (elt rule 1) "always") (if pfx "\n" t))
       ((equal (elt rule 1) "never") (if pfx "" nil))
       (t (elt rule 1)))
      (if pfx "" nil))))

(defun eslint-reader-padded-blocks-blocks (&optional pfx)
  "Whether to pad actual blocks.
This is the version for the more granular settings.
If PFX is provided, provide the character"
  (interactive "P")
  (eslint-reader--padded-blocks :blocks pfx))

(defun eslint-reader-padded-blocks-switches (&optional pfx)
  "Whether to pad switch statements.
If PFX is provided, provide the character"
  (interactive "P")
  (eslint-reader--padded-blocks :switches pfx))

(defun eslint-reader-padded-blocks-classes (&optional pfx)
  "Whether to pad classes.
If PFX is provided, provide the character"
  (interactive "P")
  (eslint-reader--padded-blocks :classes pfx))

(defun eslint-reader--padded-blocks (prop &optional pfx)
  "Whether to pad PROP.
This is the logic for the more granular settings.
If PFX is provided, provide the character instead."
  (let* ((result (eslint-reader-padded-blocks pfx))
         (setting (plist-get result prop)))
    (if (listp result)
      (if (equal setting "always")
        (if pfx "\n" t)
        (if pfx "" nil))
      result)))


(defun eslint-reader-space-before-function-paren (&optional pfx)
  "Whether or not to add space before function paren.
Given a PFX it will return the character to insert instead."
  (interactive "P")
  (let ((rule (plist-get (eslint-reader--read) :space-before-function-paren)))
    (if (and (vectorp rule) (equal (elt rule 1) "always"))
      (if pfx " " t)
      (if pfx "" nil))))

(provide 'eslint-reader)

;;; eslint-reader.el ends here
;; Local Variables:
;; indent-tabs-mode: nil
;; eval: (flycheck-mode 0)
;; eval: (nameless-mode 0)
;; End:
