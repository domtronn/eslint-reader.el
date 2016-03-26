;;; eslint-reader-indent.el --- Logic for the indent rule in ESLint

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

(require 'js2-mode)

(defvar eslint-reader-indent-default-tabs nil
	"These are the default settings for for when indent is enabled.")

(defun eslint-reader--make-elisp-indent-string ()
  "Make the the indentation string based on elisp values."
  (if indent-tabs-mode "	"
    (make-string (or js-indent-level 4) ? )))

(defun eslint-reader-indent (&optional pfx)
  "Whether or not eslint is using tabs.
When given a PFX, return the indentation character."
  (interactive "P")
  (let* ((rule (eslint-reader--parse-rule :indent))
         (enabled (plist-get rule :enabled))
         (setting (plist-get rule :setting)))
    (cond
     ;; Fallback to elisp defaults
     ((not enabled)
      (if pfx
        (eslint-reader--make-elisp-indent-string)
        indent-tabs-mode))
     ;; Tab character
     ((and enabled (equal setting "tab"))
      (if pfx "	" t))
     ;; Spaces of width equal to setting
     ((and enabled (numberp setting))
      (if pfx (make-string setting ? ) nil))
     ;; Eslint defaults of 4 width space
     (t (if pfx "    " nil)))))

(provide 'eslint-reader-indent)

;;; eslint-reader-indent.el ends here
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
