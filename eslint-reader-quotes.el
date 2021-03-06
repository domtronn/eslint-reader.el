;;; eslint-reader-quotes.el --- Logic for the quotes rule in ESLint

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

(require 'dash)

(defvar eslint-reader-quote-priority '(single double backtick)
  "The priority order for `single` `double` and `backtick` quotes.")

(defun eslint-reader-quotes-default () (car (eslint-reader--dominant-quotes)))
(defun eslint-reader--dominant-quotes ()
  "Calculates the dominating quote style in the file.
Used for the default behaviour if quotes is not set or is set to consistent."
  (let* ((matches
          (list `(,(count-matches "'" (point-min) (point-max)) ("'" single))
                `(,(count-matches "\"" (point-min) (point-max)) ("\"" double))
                `(,(count-matches "`" (point-min) (point-max)) ("`" backtick))))
         (sorted
          (--sort (< (-elem-index (cadr (cadr it)) eslint-reader-quote-priority)
                     (-elem-index (cadr (cadr other)) eslint-reader-quote-priority)) matches)))
    (cadr (--max-by (> (car it) (car other)) sorted))))

(defun eslint-reader-quotes (&optional pfx)
  "What kind of quotes to use.
When given a PFX, return the quote character instead."
  (interactive "P")
  (let* ((rule (eslint-reader--parse-rule :quotes))
         (enabled (plist-get rule :enabled))
         (setting (plist-get rule :setting)))
    (cond
     ((and enabled (equal setting "single"))   (if pfx "'" 'single))
     ((and enabled (equal setting "double"))   (if pfx "\"" 'double))
     ((and enabled (equal setting "backtick")) (if pfx "`" 'backtick))
     ((not enabled)
      (let ((quotes (eslint-reader--dominant-quotes)))
        (if pfx (car quotes) (cadr quotes)))))))

(provide 'eslint-reader-quotes)
;;; eslint-reader-quotes.el ends here
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
