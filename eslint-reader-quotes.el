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

(defvar eslint-reader-quote-priority '(double single backtick)
  "The priority order for `single` `double` and `backtick` quotes.")

(defun eslint-reader--dominant-quotes ()
  "Calculates the dominating quote style in the file.
Used for the default behaviour if quotes is not set or is set to consistent."
  (let* ((matches (list `(,(count-matches "'" (point-min) (point-max)) ("'" single))
						`(,(count-matches "\"" (point-min) (point-max)) ("\"" double))
						`(,(count-matches "`" (point-min) (point-max)) ("`" backtick))))
		 (sorted (--sort (< (-elem-index (cadadr it) eslint-reader-quote-priority)
							(-elem-index (cadadr other) eslint-reader-quote-priority)) matches)))
    (cadr (--max-by (> (car it) (car other)) sorted))))

(provide 'eslint-reader-quotes)

;;; eslint-reader-quotes.el ends here
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
