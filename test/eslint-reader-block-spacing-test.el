;; Tests for the eslint Rule Block-Spacing
;; http://eslint.org/docs/rules/block-spacing

(require 'f)
(require 'ert)
(require 'noflet)

(defvar code-base-path (f-parent (f-parent (f-this-file))))
(require 'eslint-reader-core (f-expand "eslint-reader-core.el" code-base-path))
(require 'eslint-reader-block-spacing (f-expand "eslint-reader-block-spacing.el" code-base-path))

(ert-deftest should-return-as-expected-when-setting-is-on-always ()
  "When the `block-spacing` rule is set to `always` we should return t and the block-spacing space char"
  (noflet ((eslint-reader--read (&rest any) '(:block-spacing [2 "always"])))
    (should (equal t (eslint-reader-block-spacing)))
    (should (equal " " (eslint-reader-block-spacing t)))))

(ert-deftest should-return-the-default-when-setting-is-always-but-disabled ()
  "When the `block-spacing` rule is set to `always` but the rule is set to 0, we should return the default options"
  (noflet ((eslint-reader--read (&rest any) '(:block-spacing [0 "always"])))
    (let ((eslint-reader-block-spacing-default "foobar"))
      (should (equal 'default (eslint-reader-block-spacing)))
      (should (equal "foobar" (eslint-reader-block-spacing t))))))

(ert-deftest should-return-as-expected-when-setting-is-never ()
  "When the `block-spacing` rule is set to `never` we should return nil and no block-spacing space"
  (noflet ((eslint-reader--read (&rest any) '(:block-spacing [2 "never"])))
    (should (equal nil (eslint-reader-block-spacing)))
    (should (equal "" (eslint-reader-block-spacing t)))))

(ert-deftest should-return-as-expected-when-setting-is-never-but-disabled ()
  "When the `block-spacing` rule is set to `never` but the rule is off, we should return the defaults"
  (noflet ((eslint-reader--read (&rest any) '(:block-spacing [0 "never"])))
    (let ((eslint-reader-block-spacing-default "foobar"))
      (should (equal 'default (eslint-reader-block-spacing)))
      (should (equal "foobar" (eslint-reader-block-spacing t))))))

(ert-deftest should-return-as-expected-when-the-setting-is-on-but-has-no-value ()
  "When the `block-spacing` rule is on, but has not been given as setting, we should return the defaults"
  (noflet ((eslint-reader--read (&rest any) '(:block-spacing 2)))
    (let ((eslint-reader-block-spacing-default "foobar"))
      (should (equal 'default (eslint-reader-block-spacing)))
      (should (equal "foobar" (eslint-reader-block-spacing t))))))

(ert-deftest should-return-as-expected-when-the-setting-is-off-and-has-no-value ()
  "When the `block-spacing` rule is off, but has not been given a setting, we should return the defaults"
  (noflet ((eslint-reader--read (&rest any) '(:block-spacing 0)))
    (let ((eslint-reader-block-spacing-default "foobar"))
      (should (equal 'default (eslint-reader-block-spacing)))
      (should (equal "foobar" (eslint-reader-block-spacing t))))))

(ert-deftest should-return-as-expected-when-the-rule-is-not-defined ()
  "When the `block-spacing` rule is not defined, we should return the defaults"
  (noflet ((eslint-reader--read (&rest any) '()))
    (let ((eslint-reader-block-spacing-default "foobar"))
      (should (equal 'default (eslint-reader-block-spacing)))
      (should (equal "foobar" (eslint-reader-block-spacing t))))))

;;; eslint-reader-block-spacing-test.el ends here
;; Local Variables:
;; indent-tabs-mode: nil
;; eval: (flycheck-mode 0)
;; eval: (nameless-mode 0)
;; End:
