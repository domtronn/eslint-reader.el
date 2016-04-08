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

(defvar eslint-reader-prioritize-eslint t
  "Whether to prioritize eslint when found alongside jshint.")

(defun eslint-reader--read (&optional eslintrc)
  "Read the ruleset of the closest eslint file.

When given an ESLINTRC file, it should locate this file over `flycheck-eslintrc`."
  (let* ((eslintrc (or eslintrc flycheck-eslintrc))

         (eslint-loc (locate-dominating-file (eslint-reader--base-path) eslintrc))
         (eslint-path (format "%s/%s" eslint-loc eslintrc))

         (json-object-type 'plist)
         (json-plist (json-read-file eslint-path)))
    (plist-get json-plist :rules)))

(defun eslint-reader--parse-rule (prop)
  "Parse the rule PROP."
  (let ((rule (plist-get (eslint-reader--read) prop)))
    (cond
     ((vectorp rule) `(:enabled ,(> (elt rule 0) 0) :setting ,(elt rule 1)))
     ((numberp rule) `(:enabled ,(> rule 0)))
     ((eq nil rule)  '(:enabled nil)))))

(defun eslint-reader--dir-depth (dir)
  "Calculate the depth of DIR."
  (length (split-string (expand-file-name dir) "/")))

(defun eslint-reader--base-path ()
  "Get the base path for finding the eslintrc."
  (or (buffer-file-name) (getenv "HOME")))

;; Callable Functions
(defun eslint-reader? ()
  "Guard function to check whether you should be using eslint from current file."
  (let ((eslint-loc (locate-dominating-file (eslint-reader--base-path) flycheck-eslintrc))
        (jshint-loc (locate-dominating-file (eslint-reader--base-path) flycheck-jshintrc)))

    (cond
     ((and eslint-loc jshint-loc)
      (funcall (if eslint-reader-prioritize-eslint '>= '>)
               (eslint-reader--dir-depth eslint-loc)
               (eslint-reader--dir-depth jshint-loc)))
     ((and eslint-loc (not jshint-loc)) t)
     ((not eslint-loc) nil))))

(defun er? (rule &optional pfx)
  "Read RULE for the eslintrc file.
When given a PFX, return the character that should be used by
that rule."
  (interactive "P")
  (let ((rule-f  (intern (format "eslint-reader-%s" rule)))
        (rule-v  (intern (format "eslint-reader-%s-default" rule)))
        (eslint? ))
    (unless (fboundp rule-f) (error "Function for rule `%s` is not defined" rule))
    (cond
     ((eslint-reader?) (funcall rule-f pfx))
     ((and pfx (fboundp rule-v)) (funcall rule-v))
     ((and pfx (boundp rule-v))  (symbol-value rule-v))
     (t nil))))

(defun er!? (rule)
  "Call through to `er?` for RULE with a static prefix argument.
This function can be used for simple snippet evaluation to get the value you want."
  (er? rule t))

(provide 'eslint-reader-core)

;;; eslint-reader-core.el ends here
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
