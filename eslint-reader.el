;;; eslint-reader.el --- Read an eslint file for usefulness

;; Copyright (C) 2016  Dominic Charlesworth <dgc336@gmail.com>

;; Author: Dominic Charlesworth <dgc336@gmail.com>
;; Package-Requires: ((dash "2.12.1") (flycheck "0.16") (emacs "24.3") (js2-mode "))
;; URL: https://github.com/domtronn/eslint-reader.el
;; Keywords: convenient, lisp

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
(require 'eslint-reader-strict)
(require 'eslint-reader-block-spacing)
(require 'eslint-reader-space-before-function-paren)

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

(provide 'eslint-reader)

;;; eslint-reader.el ends here
;; Local Variables:
;; indent-tabs-mode: nil
;; eval: (flycheck-mode 0)
;; eval: (nameless-mode 0)
;; End:
