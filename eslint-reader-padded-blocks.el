;;; eslint-reader-padded-blocks.el --- Logic for the padded-blocks rule in ESLint

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

(defvar eslint-reader-padded-blocks-default ""
  "Default value for the eslint rule for padded-blocks.")

(defun eslint-reader-padded-blocks (&optional pfx)
  "Whether to add padded-blocks colons.
Returns t if padded-blocks should be used, nil otherwise.
Given a PFX it will return the padded-blocks colon character."
  (interactive "P")
  (let* ((rule (eslint-reader--parse-rule :padded-blocks))
         (enabled (plist-get rule :enabled))
         (setting (plist-get rule :setting)))
    (cond
     ((and enabled (equal setting "always")) (if pfx " " t))
     ((and enabled (equal setting "never"))  (if pfx "" nil))
     ((and enabled setting (listp setting))  (if pfx setting 'detailed))
     (t                                      (if pfx eslint-reader-padded-blocks-default 'default)))))

(provide 'eslint-reader-padded-blocks)

;;; eslint-reader-padded-blocks.el ends here
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
