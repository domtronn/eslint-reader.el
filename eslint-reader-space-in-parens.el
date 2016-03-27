;;; eslint-reader-space-in-parens.el --- Logic for the space-in-parens rule in ESLint

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

(defvar eslint-reader-space-in-parens-default ""
  "Default value for the eslint rule for space-in-parens.")

(defun eslint-reader-space-in-parens (&optional pfx)
  "Whether to add space-in-parens colons.
Returns t if a space should be used in paranthese, nil otherwise.
Given a PFX it will return the space character."
  (interactive "P")
  (let* ((rule (eslint-reader--parse-rule :space-in-parens))
         (enabled (plist-get rule :enabled))
         (setting (plist-get rule :setting)))
    (cond
     ((and enabled (equal setting "always")) (if pfx " " t))
     ((and enabled (equal setting "never"))  (if pfx "" nil))
     (t                                      (if pfx eslint-reader-space-in-parens-default 'default)))))

(provide 'eslint-reader-space-in-parens)

;;; eslint-reader-space-in-parens.el ends here
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
