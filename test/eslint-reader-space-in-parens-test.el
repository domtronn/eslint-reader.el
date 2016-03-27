;; Tests for the eslint Rule Space-In-Parens
;; http://eslint.org/docs/rules/space-in-parens

(require 'f)
(require 'ert)
(require 'noflet)

(defvar code-base-path (f-parent (f-parent (f-this-file))))
(require 'eslint-reader-core (f-expand "eslint-reader-core.el" code-base-path))
(require 'eslint-reader-space-in-parens (f-expand "eslint-reader-space-in-parens.el" code-base-path))

(ert-deftest space-in-parens-should-return-as-expected-when-setting-is-on-always ()
  "When the `space-in-parens` rule is set to `always` we should return t and the space-in-parens colon char"
  (noflet ((eslint-reader--read (&rest any) '(:space-in-parens [2 "always"])))
    (should (equal t (eslint-reader-space-in-parens)))
    (should (equal " " (eslint-reader-space-in-parens t)))))

(ert-deftest space-in-parens-should-return-the-default-when-setting-is-always-but-disabled ()
  "When the `space-in-parens` rule is set to `always` but the rule is set to 0, we should return the default options"
  (noflet ((eslint-reader--read (&rest any) '(:space-in-parens [0 "always"])))
    (let ((eslint-reader-space-in-parens-default "foobar"))
      (should (equal 'default (eslint-reader-space-in-parens)))
      (should (equal "foobar" (eslint-reader-space-in-parens t))))))

(ert-deftest space-in-parens-should-return-as-expected-when-setting-is-never ()
  "When the `space-in-parens` rule is set to `never` we should return nil and no space-in-parens colon"
  (noflet ((eslint-reader--read (&rest any) '(:space-in-parens [2 "never"])))
    (should (equal nil (eslint-reader-space-in-parens)))
    (should (equal "" (eslint-reader-space-in-parens t)))))

(ert-deftest space-in-parens-should-return-as-expected-when-setting-is-never-but-disabled ()
  "When the `space-in-parens` rule is set to `never` but the rule is off, we should return the defaults"
  (noflet ((eslint-reader--read (&rest any) '(:space-in-parens [0 "never"])))
    (let ((eslint-reader-space-in-parens-default "foobar"))
      (should (equal 'default (eslint-reader-space-in-parens)))
      (should (equal "foobar" (eslint-reader-space-in-parens t))))))

(ert-deftest space-in-parens-should-return-as-expected-when-the-setting-is-on-but-has-no-value ()
  "When the `space-in-parens` rule is on, but has not been given as setting, we should return the defaults"
  (noflet ((eslint-reader--read (&rest any) '(:space-in-parens 2)))
    (let ((eslint-reader-space-in-parens-default "foobar"))
      (should (equal 'default (eslint-reader-space-in-parens)))
      (should (equal "foobar" (eslint-reader-space-in-parens t))))))

(ert-deftest space-in-parens-should-return-as-expected-when-the-setting-is-off-and-has-no-value ()
  "When the `space-in-parens` rule is off, but has not been given a setting, we should return the defaults"
  (noflet ((eslint-reader--read (&rest any) '(:space-in-parens 0)))
    (let ((eslint-reader-space-in-parens-default "foobar"))
      (should (equal 'default (eslint-reader-space-in-parens)))
      (should (equal "foobar" (eslint-reader-space-in-parens t))))))

(ert-deftest space-in-parens-should-return-as-expected-when-the-rule-is-not-defined ()
  "When the `space-in-parens` rule is not defined, we should return the defaults"
  (noflet ((eslint-reader--read (&rest any) '()))
    (let ((eslint-reader-space-in-parens-default "foobar"))
      (should (equal 'default (eslint-reader-space-in-parens)))
      (should (equal "foobar" (eslint-reader-space-in-parens t))))))

;;; eslint-reader-space-in-parens-test.el ends here
;; Local Variables:
;; indent-tabs-mode: nil
;; eval: (flycheck-mode 0)
;; eval: (nameless-mode 0)
;; End:
