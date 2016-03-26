;;; eslint-reader-core.el --- Logic for the core rule in ESLint

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

(require 'flycheck)
(require 'json)

(defun eslint-reader--read (&optional eslintrc)
  "Read the ruleset of the closest eslint file.

When given an ESLINTRC file, it should locate this file over `flycheck-eslintrc`."
  (let* ((eslintrc (or eslintrc flycheck-eslintrc))

         (eslint-loc (locate-dominating-file (buffer-file-name) eslintrc))
         (eslint-path (format "%s/%s" eslint-loc eslintrc))

         (json-object-type 'plist)
         (json-plist (json-read-file eslint-path)))
    (plist-get json-plist :rules)))

(provide 'eslint-reader-core)

;;; eslint-reader-core.el ends here
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
