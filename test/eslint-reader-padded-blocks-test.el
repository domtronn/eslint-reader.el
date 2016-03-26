;; Tests for the eslint Rule Padded-Blocks
;; http://eslint.org/docs/rules/padded-blocks

(require 'f)
(require 'ert)
(require 'noflet)

(defvar code-base-path (f-parent (f-parent (f-this-file))))
(require 'eslint-reader-core (f-expand "eslint-reader-core.el" code-base-path))
(require 'eslint-reader-padded-blocks (f-expand "eslint-reader-padded-blocks.el" code-base-path))

(ert-deftest padded-blocks-should-return-as-expected-when-setting-is-on-always ()
  "When the `padded-blocks` rule is set to `always` we should return t and the padded-blocks space char"
  (noflet ((eslint-reader--read (&rest any) '(:padded-blocks [2 "always"])))
    (should (equal t (eslint-reader-padded-blocks)))
    (should (equal " " (eslint-reader-padded-blocks t)))))

(ert-deftest padded-blocks-should-return-the-default-when-setting-is-always-but-disabled ()
  "When the `padded-blocks` rule is set to `always` but the rule is set to 0, we should return the default options"
  (noflet ((eslint-reader--read (&rest any) '(:padded-blocks [0 "always"])))
    (let ((eslint-reader-padded-blocks-default "foobar"))
      (should (equal 'default (eslint-reader-padded-blocks)))
      (should (equal "foobar" (eslint-reader-padded-blocks t))))))

(ert-deftest padded-blocks-should-return-as-expected-when-setting-is-never ()
  "When the `padded-blocks` rule is set to `never` we should return nil and no padded-blocks space"
  (noflet ((eslint-reader--read (&rest any) '(:padded-blocks [2 "never"])))
    (should (equal nil (eslint-reader-padded-blocks)))
    (should (equal "" (eslint-reader-padded-blocks t)))))

(ert-deftest padded-blocks-should-return-as-expected-when-setting-is-never-but-disabled ()
  "When the `padded-blocks` rule is set to `never` but the rule is off, we should return the defaults"
  (noflet ((eslint-reader--read (&rest any) '(:padded-blocks [0 "never"])))
    (let ((eslint-reader-padded-blocks-default "foobar"))
      (should (equal 'default (eslint-reader-padded-blocks)))
      (should (equal "foobar" (eslint-reader-padded-blocks t))))))

(ert-deftest padded-blocks-should-return-as-expected-when-the-setting-is-on-but-has-no-value ()
  "When the `padded-blocks` rule is on, but has not been given as setting, we should return the defaults"
  (noflet ((eslint-reader--read (&rest any) '(:padded-blocks 2)))
    (let ((eslint-reader-padded-blocks-default "foobar"))
      (should (equal 'default (eslint-reader-padded-blocks)))
      (should (equal "foobar" (eslint-reader-padded-blocks t))))))

(ert-deftest padded-blocks-should-return-as-expected-when-the-setting-is-off-and-has-no-value ()
  "When the `padded-blocks` rule is off, but has not been given a setting, we should return the defaults"
  (noflet ((eslint-reader--read (&rest any) '(:padded-blocks 0)))
    (let ((eslint-reader-padded-blocks-default "foobar"))
      (should (equal 'default (eslint-reader-padded-blocks)))
      (should (equal "foobar" (eslint-reader-padded-blocks t))))))

(ert-deftest padded-blocks-should-return-as-expected-when-the-rule-is-not-defined ()
  "When the `padded-blocks` rule is not defined, we should return the defaults"
  (noflet ((eslint-reader--read (&rest any) '()))
    (let ((eslint-reader-padded-blocks-default "foobar"))
      (should (equal 'default (eslint-reader-padded-blocks)))
      (should (equal "foobar" (eslint-reader-padded-blocks t))))))

(ert-deftest padded-blocks-should-return-setting-when-more-detailed ()
  "When the `padded-blocks` rule is not defined, we should return the defaults"
  (noflet ((eslint-reader--read (&rest any) '(:padded-blocks [2 (1 2 3)])))
    (let ((eslint-reader-padded-blocks-default "foobar"))
      (should (equal 'detailed (eslint-reader-padded-blocks)))
      (should (equal '(1 2 3) (eslint-reader-padded-blocks t))))))

;;; eslint-reader-padded-blocks-test.el ends here
;; Local Variables:
;; indent-tabs-mode: nil
;; eval: (flycheck-mode 0)
;; eval: (nameless-mode 0)
;; End:
