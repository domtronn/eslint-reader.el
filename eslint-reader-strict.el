;;; eslint-reader-strict.el --- Logic for the strict rule in ESLint

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

(require eslint-reader-quotes)

(defun eslint-reader-strict (&optional pfx)
  "Whether or not you need a 'use strict' statement.
Returns nil if statement is not needed, otherwise t.

When given a PFX it will return the string to insert with quote
characters."
  (interactive "P")
  (let* ((rule (eslint-reader--parse-rule :strict))
         (enabled (plist-get rule :enabled))
         (setting (plist-get rule :setting))
		 (qc (eslint-reader-quotes)))
	(cond
	 ((and enabled (equal setting "never")) (if pfx "" nil))
	 (t (if pfx (format "%suse strict%s" qc qc) t)))))

(provide 'eslint-reader-strict)

;;; eslint-reader-strict.el ends here
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
