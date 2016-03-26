;; Tests for the eslint Rule Semi
;; http://eslint.org/docs/rules/semi

(require 'f)
(require 'ert)
(require 'noflet)

(defvar code-base-path (f-parent (f-parent (f-this-file))))
(require 'eslint-reader-core (f-expand "eslint-reader-core.el" code-base-path))
(require 'eslint-reader-semi (f-expand "eslint-reader-semi.el" code-base-path))

(ert-deftest should-return-as-expected-when-setting-is-on-always ()
  "When the `semi` rule is set to `always` we should return t and the semi colon char"
  (noflet ((eslint-reader--read (&rest any) '(:semi [2 "always"])))
    (should (equal t (eslint-reader-semi)))
    (should (equal ";" (eslint-reader-semi t)))))

(ert-deftest should-return-the-default-when-setting-is-always-but-disabled ()
  "When the `semi` rule is set to `always` but the rule is set to 0, we should return the default options"
  (noflet ((eslint-reader--read (&rest any) '(:semi [0 "always"])))
    (let ((eslint-reader-semi-default "foobar"))
      (should (equal 'default (eslint-reader-semi)))
      (should (equal "foobar" (eslint-reader-semi t))))))

(ert-deftest should-return-as-expected-when-setting-is-never ()
  "When the `semi` rule is set to `never` we should return nil and no semi colon"
  (noflet ((eslint-reader--read (&rest any) '(:semi [2 "never"])))
    (should (equal nil (eslint-reader-semi)))
    (should (equal "" (eslint-reader-semi t)))))

(ert-deftest should-return-as-expected-when-setting-is-never-but-disabled ()
  "When the `semi` rule is set to `never` but the rule is off, we should return the defaults"
  (noflet ((eslint-reader--read (&rest any) '(:semi [0 "never"])))
    (let ((eslint-reader-semi-default "foobar"))
      (should (equal 'default (eslint-reader-semi)))
      (should (equal "foobar" (eslint-reader-semi t))))))

(ert-deftest should-return-as-expected-when-the-setting-is-on-but-has-no-value ()
  "When the `semi` rule is on, but has not been given as setting, we should return the defaults"
  (noflet ((eslint-reader--read (&rest any) '(:semi 2)))
    (let ((eslint-reader-semi-default "foobar"))
      (should (equal 'default (eslint-reader-semi)))
      (should (equal "foobar" (eslint-reader-semi t))))))

(ert-deftest should-return-as-expected-when-the-setting-is-off-and-has-no-value ()
  "When the `semi` rule is off, but has not been given a setting, we should return the defaults"
  (noflet ((eslint-reader--read (&rest any) '(:semi 0)))
    (let ((eslint-reader-semi-default "foobar"))
      (should (equal 'default (eslint-reader-semi)))
      (should (equal "foobar" (eslint-reader-semi t))))))

(ert-deftest should-return-as-expected-when-the-rule-is-not-defined ()
  "When the `semi` rule is not defined, we should return the defaults"
  (noflet ((eslint-reader--read (&rest any) '()))
    (let ((eslint-reader-semi-default "foobar"))
      (should (equal 'default (eslint-reader-semi)))
      (should (equal "foobar" (eslint-reader-semi t))))))

;;; eslint-reader-semi-test.el ends here
;; Local Variables:
;; indent-tabs-mode: nil
;; eval: (flycheck-mode 0)
;; eval: (nameless-mode 0)
;; End:
