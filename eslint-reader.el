;;; eslint-reader.el --- Read an eslint file for usefulness

;; Copyright (C) 2016  Dominic Charlesworth <dgc336@gmail.com>

;; Author: Dominic Charlesworth <dgc336@gmail.com>
;; Version: 1.0.0
;; Package-Requires: ((dash "20160306.1222") (flycheck "20160307.739") (emacs "24.3") (js2-mode "20160305.452"))
;; URL: https://github.com/domtronn/eslint-reader.el
;; Keywords: convenient, lisp

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

;; Core functions
(require 'eslint-reader-core)

;; Rule functions
(require 'eslint-reader-indent)
(require 'eslint-reader-semi)
(require 'eslint-reader-quotes)
(require 'eslint-reader-strict)
(require 'eslint-reader-padded-blocks)
(require 'eslint-reader-block-spacing)
(require 'eslint-reader-space-before-function-paren)

(provide 'eslint-reader)

;;; eslint-reader.el ends here
;; Local Variables:
;; indent-tabs-mode: nil
;; eval: (flycheck-mode 0)
;; eval: (nameless-mode 0)
;; End:
