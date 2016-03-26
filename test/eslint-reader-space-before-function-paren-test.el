;; Tests for the eslint Rule Space-Before-Function-Paren
;; http://eslint.org/docs/rules/space-before-function-paren

(require 'f)
(require 'ert)
(require 'noflet)

(defvar code-base-path (f-parent (f-parent (f-this-file))))
(require 'eslint-reader-core (f-expand "eslint-reader-core.el" code-base-path))
(require 'eslint-reader-space-before-function-paren (f-expand "eslint-reader-space-before-function-paren.el" code-base-path))

;;; eslint-reader-space-before-function-paren-test.el ends here
;; Local Variables:
;; indent-tabs-mode: nil
;; eval: (flycheck-mode 0)
;; eval: (nameless-mode 0)
;; End:
