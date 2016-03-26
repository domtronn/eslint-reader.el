;; Tests for the eslint Rule indent
;; http://eslint.org/docs/rules/indent

(require 'ert)
(require 'noflet)

(defvar code-base-path (f-parent (f-parent (f-this-file))))
(require 'eslint-reader-core (f-expand "eslint-reader-core.el" code-base-path))
(require 'eslint-reader-indent (f-expand "eslint-reader-indent.el" code-base-path))


(ert-deftest should-return-as-expected-when-setting-is-on ()
  "When the `indent` rule is set on it should revert to the eslint defaults"
  (noflet ((eslint-reader--read (&rest any) '(:indent 2)))
    (should (equal nil (eslint-reader-indent)))
    (should (equal "    " (eslint-reader-indent t)))))

(ert-deftest should-return-base-elisp-indentation-values-when-setting-is-off ()
  "When the `indent` rule is off it should revert to the elisp defaults `indent-tabs-mode` and `js-indent-level`"
  (noflet ((eslint-reader--read (&rest any) '(:indent 0)))
    (let ((indent-tabs-mode nil)
          (js-indent-level 9)
          (expected-indent-char (make-string 9 ? )))
      (should (equal indent-tabs-mode (eslint-reader-indent)))
      (should (equal expected-indent-char (eslint-reader-indent t))))
    (let ((indent-tabs-mode t))
      (should (equal indent-tabs-mode (eslint-reader-indent)))
      (should (equal "	" (eslint-reader-indent t)))))
  (noflet ((eslint-reader--read (&rest any) '()))
	(let ((indent-tabs-mode t))
	  (should (equal indent-tabs-mode (eslint-reader-indent)))
      (should (equal "	" (eslint-reader-indent t))))))

(ert-deftest should-return-as-expected-when-given-more-granularity ()
  "When the `indent` rule is on and has been set to tabs, it should return t and the tab character"
  (noflet ((eslint-reader--read (&rest any) '(:indent [2 "tab"])))
    (should (equal t (eslint-reader-indent)))
    (should (equal "	" (eslint-reader-indent t)))))

(ert-deftest should-retun-as-expected-when-given-more-spaces-granulartiy ()
  "When the `indent` rule is on and has been set to a number, it should return nil and the correct spaced character"
  (noflet ((eslint-reader--read (&rest any) '(:indent [2 8])))
    (should (equal nil (eslint-reader-indent)))
    (should (equal "        " (eslint-reader-indent t)))))

;;; eslint-reader-indent-test.el ends here
;; Local Variables:
;; indent-tabs-mode: nil
;; eval: (flycheck-mode 0)
;; eval: (nameless-mode 0)
;; End:
