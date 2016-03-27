;;; eslint-reader-space-before-blocks.el --- Logic for the space-before-blocks rule in ESLint

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

;;; Code:

(defvar 'eslint-reader-space-before-functions-default ""
  "Default value for the eslint rule for space-before-blocks functions.")
(defvar 'eslint-reader-space-before-keywords-default ""
  "Default value for the eslint rule for space-before-blocks keywords.")
(defvar 'eslint-reader-space-before-classes-default ""
  "Default value for the eslint rule for space-before-blocks classes.")
(defvar eslint-reader-space-before-blocks-default ""
  "Default value for the eslint rule for space-before-blocks.")

(defun eslint-reader-space-before-blocks (&optional pfx)
  "Whether to add space-before-blocks colons.
Returns t if space-before-blocks should be used, nil otherwise.
Given a PFX it will return the space-before-blocks colon character."
  (interactive "P")
  (let* ((rule (eslint-reader--parse-rule :space-before-blocks))
         (enabled (plist-get rule :enabled))
         (setting (plist-get rule :setting)))
    (cond
     ((and enabled (equal setting "always")) (if pfx " " t))
     ((and enabled (equal setting "never"))  (if pfx "" nil))
     ((and enabled setting (listp setting))  (if pfx setting 'detailed))
     (t                                      (if pfx eslint-reader-space-before-blocks-default 'default)))))

(defun eslint-reader-space-before-keywords (&optional pfx)
  "Return the detailed rule for keywords.
Given PFX it will return the character to enter"
  (interactive "P")
  (eslint-reader--space-before :keywords pfx))

(defun eslint-reader-space-before-functions (&optional pfx)
  "Return the detailed rule for functions.
Given PFX it will return the character to enter"
  (interactive "P")
  (eslint-reader--space-before :functions pfx))

(defun eslint-reader-space-before-classes (&optional pfx)
  "Return the detailed rule for classes.
Given PFX it will return the character to enter"
  (interactive "P")
  (eslint-reader--space-before :classes pfx))

(defun eslint-reader--space-before (prop &optional pfx)
  "Get the property PROP for the detailed `space-before-blocks`.
Given PFX it will return the character instead."
  (let* ((result (eslint-reader-space-before-blocks t))
         (setting (plist-get result prop)))
    (cond
     ((equal setting "always") (if pfx " " t))
     ((equal setting "never")  (if pfx "" nil))
     (t                        (if pfx eslint-reader-space-before-blocks-default 'default)))))

(provide 'eslint-reader-space-before-blocks)

;;; eslint-reader-space-before-blocks.el ends here
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
