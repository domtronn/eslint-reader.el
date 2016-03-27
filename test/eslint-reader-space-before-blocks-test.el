;; Tests for the eslint Rule Space-Before-Blocks
;; http://eslint.org/docs/rules/space-before-blocks

(require 'f)
(require 'ert)
(require 'noflet)

(defvar code-base-path (f-parent (f-parent (f-this-file))))
(require 'eslint-reader-core (f-expand "eslint-reader-core.el" code-base-path))
(require 'eslint-reader-space-before-blocks (f-expand "eslint-reader-space-before-blocks.el" code-base-path))

(ert-deftest space-before-blocks-should-return-as-expected-when-setting-is-on-always ()
  "When the `space-before-blocks` rule is set to `always` we should return t and the space-before-blocks space char"
  (noflet ((eslint-reader--read (&rest any) '(:space-before-blocks [2 "always"])))
    (should (equal t (eslint-reader-space-before-blocks)))
    (should (equal " " (eslint-reader-space-before-blocks t)))))

(ert-deftest space-before-blocks-should-return-the-default-when-setting-is-always-but-disabled ()
  "When the `space-before-blocks` rule is set to `always` but the rule is set to 0, we should return the default options"
  (noflet ((eslint-reader--read (&rest any) '(:space-before-blocks [0 "always"])))
    (let ((eslint-reader-space-before-blocks-default "foobar"))
      (should (equal 'default (eslint-reader-space-before-blocks)))
      (should (equal "foobar" (eslint-reader-space-before-blocks t))))))

(ert-deftest space-before-blocks-should-return-as-expected-when-setting-is-never ()
  "When the `space-before-blocks` rule is set to `never` we should return nil and no space-before-blocks space"
  (noflet ((eslint-reader--read (&rest any) '(:space-before-blocks [2 "never"])))
    (should (equal nil (eslint-reader-space-before-blocks)))
    (should (equal "" (eslint-reader-space-before-blocks t)))))

(ert-deftest space-before-blocks-should-return-as-expected-when-setting-is-never-but-disabled ()
  "When the `space-before-blocks` rule is set to `never` but the rule is off, we should return the defaults"
  (noflet ((eslint-reader--read (&rest any) '(:space-before-blocks [0 "never"])))
    (let ((eslint-reader-space-before-blocks-default "foobar"))
      (should (equal 'default (eslint-reader-space-before-blocks)))
      (should (equal "foobar" (eslint-reader-space-before-blocks t))))))

(ert-deftest space-before-blocks-should-return-as-expected-when-the-setting-is-on-but-has-no-value ()
  "When the `space-before-blocks` rule is on, but has not been given as setting, we should return the defaults"
  (noflet ((eslint-reader--read (&rest any) '(:space-before-blocks 2)))
    (let ((eslint-reader-space-before-blocks-default "foobar"))
      (should (equal 'default (eslint-reader-space-before-blocks)))
      (should (equal "foobar" (eslint-reader-space-before-blocks t))))))

(ert-deftest space-before-blocks-should-return-as-expected-when-the-setting-is-off-and-has-no-value ()
  "When the `space-before-blocks` rule is off, but has not been given a setting, we should return the defaults"
  (noflet ((eslint-reader--read (&rest any) '(:space-before-blocks 0)))
    (let ((eslint-reader-space-before-blocks-default "foobar"))
      (should (equal 'default (eslint-reader-space-before-blocks)))
      (should (equal "foobar" (eslint-reader-space-before-blocks t))))))

(ert-deftest space-before-blocks-should-return-as-expected-when-the-rule-is-not-defined ()
  "When the `space-before-blocks` rule is not defined, we should return the defaults"
  (noflet ((eslint-reader--read (&rest any) '()))
    (let ((eslint-reader-space-before-blocks-default "foobar"))
      (should (equal 'default (eslint-reader-space-before-blocks)))
      (should (equal "foobar" (eslint-reader-space-before-blocks t))))))

(ert-deftest space-before-blocks-should-return-setting-when-more-detailed ()
  "When the `space-before-blocks` rule is detailed we should return the expanded default"
  (noflet ((eslint-reader--read (&rest any) '(:space-before-blocks [2 (1 2 3)])))
    (let ((eslint-reader-space-before-blocks-default "foobar"))
      (should (equal 'detailed (eslint-reader-space-before-blocks)))
      (should (equal '(1 2 3) (eslint-reader-space-before-blocks t))))))

(ert-deftest space-before-blocks-should-return-the-correct-detailed-setting ()
  "When the `space-before-blocks` rule is detailed, the detailed function should return as expected"
  (noflet ((eslint-reader--read
            (&rest any)
            '(:space-before-blocks [2 (:functions "always" :keywords "never" :classes "always" )])))
    (should (equal t (eslint-reader-space-before-functions)))
    (should (equal " " (eslint-reader-space-before-functions t)))
    (should (equal nil (eslint-reader-space-before-keywords)))
    (should (equal "" (eslint-reader-space-before-keywords t)))
    (should (equal t (eslint-reader-space-before-classes)))
    (should (equal " " (eslint-reader-space-before-classes t)))))

(ert-deftest space-before-blocks-should-return-the-default-items-when-detailed-but-disabled ()
  "When the `space-before-blocks` rule is detailed, it should still adhere to the defaults"
  (noflet ((eslint-reader--read (&rest any) '(:space-before-blocks [0 (:blocks "always")])))
    (let ((eslint-reader-space-before-blocks-default "foobar"))
      (should (equal 'default (eslint-reader-space-before-functions)))
      (should (equal "foobar" (eslint-reader-space-before-functions t))))))

;;; eslint-reader-space-before-blocks-test.el ends here
;; Local Variables:
;; indent-tabs-mode: nil
;; eval: (flycheck-mode 0)
;; eval: (nameless-mode 0)
;; End:
