;; Tests for the eslint Rule Space-Before-Function-Paren
;; http://eslint.org/docs/rules/space-before-function-paren

(require 'f)
(require 'ert)
(require 'noflet)

(defvar code-base-path (f-parent (f-parent (f-this-file))))
(require 'eslint-reader-core (f-expand "eslint-reader-core.el" code-base-path))
(require 'eslint-reader-space-before-function-paren (f-expand "eslint-reader-space-before-function-paren.el" code-base-path))

(ert-deftest space-before-function-paren-should-return-as-expected-when-setting-is-on-always ()
  "When the `space-before-function-paren` rule is set to `always` we should return t and the space-before-function-paren colon char"
  (noflet ((eslint-reader--read (&rest any) '(:space-before-function-paren [2 "always"])))
    (should (equal t (eslint-reader-space-before-function-paren)))
    (should (equal " " (eslint-reader-space-before-function-paren t)))))

(ert-deftest space-before-function-paren-should-return-the-default-when-setting-is-always-but-disabled ()
  "When the `space-before-function-paren` rule is set to `always` but the rule is set to 0, we should return the default options"
  (noflet ((eslint-reader--read (&rest any) '(:space-before-function-paren [0 "always"])))
    (let ((eslint-reader-space-before-function-paren-default "foobar"))
      (should (equal 'default (eslint-reader-space-before-function-paren)))
      (should (equal "foobar" (eslint-reader-space-before-function-paren t))))))

(ert-deftest space-before-function-paren-should-return-as-expected-when-setting-is-never ()
  "When the `space-before-function-paren` rule is set to `never` we should return nil and no space-before-function-paren colon"
  (noflet ((eslint-reader--read (&rest any) '(:space-before-function-paren [2 "never"])))
    (should (equal nil (eslint-reader-space-before-function-paren)))
    (should (equal "" (eslint-reader-space-before-function-paren t)))))

(ert-deftest space-before-function-paren-should-return-as-expected-when-setting-is-never-but-disabled ()
  "When the `space-before-function-paren` rule is set to `never` but the rule is off, we should return the defaults"
  (noflet ((eslint-reader--read (&rest any) '(:space-before-function-paren [0 "never"])))
    (let ((eslint-reader-space-before-function-paren-default "foobar"))
      (should (equal 'default (eslint-reader-space-before-function-paren)))
      (should (equal "foobar" (eslint-reader-space-before-function-paren t))))))

(ert-deftest space-before-function-paren-should-return-as-expected-when-the-setting-is-on-but-has-no-value ()
  "When the `space-before-function-paren` rule is on, but has not been given as setting, we should return the defaults"
  (noflet ((eslint-reader--read (&rest any) '(:space-before-function-paren 2)))
    (let ((eslint-reader-space-before-function-paren-default "foobar"))
      (should (equal 'default (eslint-reader-space-before-function-paren)))
      (should (equal "foobar" (eslint-reader-space-before-function-paren t))))))

(ert-deftest space-before-function-paren-should-return-as-expected-when-the-setting-is-off-and-has-no-value ()
  "When the `space-before-function-paren` rule is off, but has not been given a setting, we should return the defaults"
  (noflet ((eslint-reader--read (&rest any) '(:space-before-function-paren 0)))
    (let ((eslint-reader-space-before-function-paren-default "foobar"))
      (should (equal 'default (eslint-reader-space-before-function-paren)))
      (should (equal "foobar" (eslint-reader-space-before-function-paren t))))))

(ert-deftest space-before-function-paren-should-return-as-expected-when-the-rule-is-not-defined ()
  "When the `space-before-function-paren` rule is not defined, we should return the defaults"
  (noflet ((eslint-reader--read (&rest any) '()))
    (let ((eslint-reader-space-before-function-paren-default "foobar"))
      (should (equal 'default (eslint-reader-space-before-function-paren)))
      (should (equal "foobar" (eslint-reader-space-before-function-paren t))))))

;;; eslint-reader-space-before-function-paren-test.el ends here
;; Local Variables:
;; indent-tabs-mode: nil
;; eval: (flycheck-mode 0)
;; eval: (nameless-mode 0)
;; End:
