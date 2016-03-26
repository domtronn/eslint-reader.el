;;; eslint-reader.el --- Read an eslint file for usefulness

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

;; Read the contents of an eslint file and parse some of the rules
;; into useful variables for formatting etc

;;; Code:

(require 'json)
(require 'flycheck)

(defun eslint-reader--read ()
  "Read in the .eslintrc file as json."
  (let* ((eslint-loc (locate-dominating-file (buffer-file-name) flycheck-eslintrc))
         (eslint-path (format "%s/%s" eslint-loc flycheck-eslintrc))
         (json-object-type 'plist)
         (json-plist (json-read-file eslint-path)))
    (plist-get json-plist :rules)))

;; Rule Functions

;; Called with prefix arguments, rule functions should return the
;; stylised thing, e.g.  semi => ;

(defun eslint-reader-indent-tabs ()
  "Whether or not to use tabs."
  (interactive)
  (let ((rule (plist-get (eslint-reader--read) :indent)))
    (cond
     ((vectorp rule) (equal (elt rule 1) "tab"))  ;; Indenting set to tab
     ((> rule 0) nil)                             ;; Rule is set to default
     (t indent-tabs-mode))))                      ;; Rule is not there

(defun eslint-reader-indent-width ()
  "The indent width of spaces."
  (interactive)
  (let ((rule (plist-get (eslint-reader--read) :indent)))
    (unless (eslint-reader-indent-tabs)
      (if (vectorp rule) (elt rule 1) 4))))

(defun eslint-reader-semi (&optional pfx)
  "Whether to add semi colons.
Returns t if semi colons should be used, nil otherwise.
Given a PFX it will return the semi colon character."
  (interactive)
  (let ((rule (plist-get (eslint-reader--read) :semi)))
    (if (and (vectorp rule) (equal (elt rule 1) "always"))
      (if pfx ";" t)
      (if pfx "" nil))))

(defun eslint-reader-quotes (&optional pfx)
  "The style of quotation used.
Returns 'single, 'double or 'backtick.  When given a PFX, it will
return the quote character to be used."
  (interactive)
  (let* ((rule    (plist-get (eslint-reader--read) :quotes))
         (setting (if (vectorp rule) (elt rule 0) rule)))
    (if (and rule (> setting 0))
      (cond
       ((equal (elt rule 1) "single") (if pfx "'" 'single))
       ((equal (elt rule 1) "double") (if pfx "\"" 'double))
       ((equal (elt rule 1) "backtick") (if pfx "`" 'backtick)))
      (if pfx "'" 'single))))

(defun eslint-reader-strict (&optional pfx)
  "Whether or not you need a 'use strict' statement.
Returns nil if statement is not needed, otherwise t.  When given
a PFX it will return the string to insert with quote
characters."
  (interactive)
  (let ((rule  (plist-get (eslint-reader--read) :strict))
        (qc    (eslint-reader-quotes t)))
    (if (and (vectorp rule) (not (equal (elt rule 1) "never")))
      (if pfx (format "%suse strict%s" qc qc) t)
      (if pfx "" t))))

(defun eslint-reader-block-spacing ()
  "Whether or not you should have block spacing"
  (interactive)
  (let ((rule (plist-get (eslint-reader--read) :block-spacing)))
    (if (vectorp rule) (equal (elt rule 1) "always") t)))

(defun eslint-reader-space-before-function-paren ()
  "Whether or not to add space before function paren."
  (interactive)
  (let ((rule (plist-get (eslint-reader--read) :space-before-function-paren)))
    (if (vectorp rule) (equal (elt rule 1) "always") nil)))

;; Calling Functions

(defun eslint-reader--depth (path)
  "Get the depth of PATH"
  (length (split-string (expand-file-name path) "/")))

(defalias 'er? 'eslint-reader?)
(defun eslint-reader? (&optional f)
  "Guard function to check whether you should be using eslint.
Call the namespaced function rule F if criteria is met."
  (when (buffer-file-name)
    (let ((jshintrc-loc (locate-dominating-file (buffer-file-name) flycheck-jshintrc))
          (eslintrc-loc (locate-dominating-file (buffer-file-name) flycheck-eslintrc)))
      (when (or (and eslintrc-loc jshintrc-loc
                     (> (eslint-reader--depth eslintrc-loc) (eslint-reader--depth jshintrc-loc)))
                (and eslintrc-loc (not jshintrc-loc)))
        (if f (funcall (intern (format "eslint-reader-%s" f))) t)))))

;;; eslint-reader.el ends here
;; Local Variables:
;; indent-tabs-mode: nil
;; eval: (flycheck-mode 0)
;; eval: (nameless-mode 0)
;; End:
