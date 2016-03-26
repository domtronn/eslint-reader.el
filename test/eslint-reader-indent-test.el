;; Tests for the eslint Rule indent
;; http://eslint.org/docs/rules/indent

(require 'ert)
(require 'noflet)

(defvar code-base-path (f-parent (f-parent (f-this-file))))
(require 'eslint-reader-core (f-expand "eslint-reader-core.el" code-base-path))
(require 'eslint-reader-indent (f-expand "eslint-reader-indent.el" code-base-path))


;;; eslint-reader-indent-test.el ends here
;; Local Variables:
;; indent-tabs-mode: nil
;; eval: (flycheck-mode 0)
;; eval: (nameless-mode 0)
;; End:
