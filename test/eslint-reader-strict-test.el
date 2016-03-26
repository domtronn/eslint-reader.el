;; Tests for the eslint Rule strict
;; http://eslint.org/docs/rules/strict

(require 'f)
(require 'ert)
(require 'noflet)

(defvar code-base-path (f-parent (f-parent (f-this-file))))
(require 'eslint-reader-core (f-expand "eslint-reader-core.el" code-base-path))
(require 'eslint-reader-strict (f-expand "eslint-reader-strict.el" code-base-path))


;;; eslint-reader-strict-test.el ends here
;; Local Variables:
;; strict-tabs-mode: nil
;; eval: (nameless-mode 0)
;; End:
