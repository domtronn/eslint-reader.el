;; Tests for the eslint Rule quotes
;; http://eslint.org/docs/rules/quotes

(require 'f)
(require 'ert)
(require 'noflet)

(defvar code-base-path (f-parent (f-parent (f-this-file))))
(require 'eslint-reader-core (f-expand "eslint-reader-core.el" code-base-path))
(require 'eslint-reader-quotes (f-expand "eslint-reader-quotes.el" code-base-path))

;;; eslint-reader-quotes-test.el ends here
;; Local Variables:
;; quotes-tabs-mode: nil
;; eval: (nameless-mode 0)
;; End:
