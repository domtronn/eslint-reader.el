;; Tests for the eslint Rule strict
;; http://eslint.org/docs/rules/strict

(require 'f)
(require 'ert)
(require 'noflet)

(defvar code-base-path (f-parent (f-parent (f-this-file))))
(require 'eslint-reader-core (f-expand "eslint-reader-core.el" code-base-path))
(require 'eslint-reader-strict (f-expand "eslint-reader-strict.el" code-base-path))

(ert-deftest strict-should-return-t-when-the-setting-is-on ()
  "When the `strict` rule is on, it should return t and the use strict string"
  (noflet ((eslint-reader--read (&rest any) '(:strict 2))
           (eslint-reader-quotes (&rest any) "<"))
    (should (equal t (eslint-reader-strict)))
    (should (equal "<use strict<" (eslint-reader-strict t)))))

(ert-deftest strict-should-return-default-when-the-rule-is-off ()
  "When the `strict` rule is disabled then you should return the default"
  (noflet ((eslint-reader--read (&rest any) '(:strict 0))
           (eslint-reader-quotes (&rest any) "<"))
    (should (equal 'default (eslint-reader-strict)))
    (should (equal "<use strict<" (eslint-reader-strict t)))))

(ert-deftest strict-should-return-default-when-the-rule-isnt-defined ()
  "When the `strict` rule is on, it should return t and the use strict string"
  (noflet ((eslint-reader--read (&rest any) '())
           (eslint-reader-quotes (&rest any) "<"))
    (should (equal 'default (eslint-reader-strict)))
    (should (equal "<use strict<" (eslint-reader-strict t)))))

(ert-deftest strict-should-return-nil-when-the-setting-is-on-and-set-to-never ()
  "When the `strict` rule is on and set to `never`, it should return nil and provide a blank string"
  (noflet ((eslint-reader--read (&rest any) '(:strict [2 "never"]))
           (eslint-reader-quotes (&rest any) "<"))
    (should (equal nil (eslint-reader-strict)))
    (should (equal "" (eslint-reader-strict t)))))

;;; eslint-reader-strict-test.el ends here
;; Local Variables:
;; strict-tabs-mode: nil
;; eval: (nameless-mode 0)
;; End:
