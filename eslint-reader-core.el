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

(defvar eslint-reader-config-cache '()
  "A cache of all of the eslint configs that have been created.")

(defun eslint-reader--read-from-cache (path)
  "Read the config for PATH from `eslint-reader-config-cache'.

If it does not exist, read the config and add it to the cache."
  (let* ((expanded-path path)
         (eslint-cache-config (assoc expanded-path eslint-reader-config-cache))
         (eslint-config-cmd (format "eslint --print-config %s" default-directory))

         (json-object-type 'plist))
    (unless eslint-cache-config
      (add-to-list 'eslint-reader-config-cache
                   `(,expanded-path . ,(json-read-from-string (shell-command-to-string eslint-config-cmd)))))
    (cdr (assoc expanded-path eslint-reader-config-cache))))

(defun eslint-reader--read (&optional eslintrc)
  "Read the ruleset of the closest eslint file.

When given an ESLINTRC file, it should locate this file over `flycheck-eslintrc`."
  (let* ((eslintrc (or eslintrc flycheck-eslintrc))
         (eslint-loc (locate-dominating-file (eslint-reader--base-path) eslintrc))

         (eslint-config (eslint-reader--read-from-cache eslint-loc)))
    (plist-get eslint-config :rules)))

(defun eslint-reader--parse-rule (prop)
  "Parse the rule PROP."
  (let ((rule (plist-get (eslint-reader--read) prop)))
    (cond
     ((vectorp rule) `(:enabled ,(eslint-reader--enabled (elt rule 0)) :setting ,(elt rule 1)))
     ((numberp rule) `(:enabled ,(eslint-reader--enabled rule)))
     ((eq nil rule)  '(:enabled nil)))))

(defun eslint-reader--enabled (rule)
  "Wrap up logic to determine whether RULE is `on` or `off`."
  (if (stringp rule) (not (equal rule "off")) (> rule 0)))

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
