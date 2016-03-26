;; Tests for the eslint Rule Semi
;; http://eslint.org/docs/rules/semi

(require 'f)
(require 'ert)
(require 'noflet)
(require 'flycheck)

(defvar sandbox-base-path "/tmp/eslint-reader")
(defvar sandbox-higher-path (f-expand "higher" sandbox-base-path))
(defvar sandbox-lower-path (f-expand "lower" sandbox-higher-path))

(defmacro with-sandbox (&rest body)
  "Evaluate BODY in an empty temporary directory."
  `(let ((default-directory sandbox-base-path))
     (when (f-dir? sandbox-base-path) (f-delete sandbox-base-path :force))
     (f-mkdir sandbox-base-path)
     (f-mkdir sandbox-higher-path)
     (f-mkdir sandbox-lower-path)
     ,@body
     (f-delete sandbox-base-path :force)))

(defmacro with-only-eslint (&rest body)
  "Create a temporary directory with just an eslintrc file in it"
  `(let ((eslint-1 (f-expand flycheck-eslintrc sandbox-lower-path))
         (eslint-2 (f-expand flycheck-eslintrc sandbox-higher-path))
         (eslint-3 (f-expand ".my-eslintrc" sandbox-higher-path)))
     (with-sandbox
      (f-write "{\"rules\":\"eslint-1\"}" 'utf-8 eslint-1)
      (f-write "{\"rules\":\"eslint-2\"}" 'utf-8 eslint-2)
      (f-write "{\"rules\":\"eslint-3\"}" 'utf-8 eslint-3)
      ,@body)))

;;; READ Suite

(ert-deftest should-read-the-rules-from-closest-eslint-file ()
  "When calling the read function from the nested directory with only eslint files, it should return the eslint closest eslint file."
  (with-only-eslint
   (noflet ((buffer-file-name (&rest any) (f-expand "test.el" sandbox-lower-path)))
     (should (equal "eslint-1" (eslint-reader--read))))))

(ert-deftest should-read-the-rules-from-closest-provided-file-as-override ()
  "When calling the read function with the `eslintrc` option, it should find that file over default `flycheck-eslintrc` file"
  (with-only-eslint
   (noflet ((buffer-file-name (&rest any) (f-expand "test.el" sandbox-lower-path)))
     (should (equal "eslint-3" (eslint-reader--read ".my-eslintrc"))))))

;;; PARSING RULES suite

(ert-deftest should-parse-a-given-rule-correctly-when-a-vector ()
  "When calling `elsint-reader-parse-rule` with a certain rule, it should return consistently."
  (noflet ((eslint-reader--read (&rest any) '(:prop [2 "never"])))
    (let ((result (eslint-reader--parse-rule :prop)))
      (should (listp result))
      (should (equal t (plist-get result :enabled)))
      (should (equal "never" (plist-get result :setting))))))

(ert-deftest should-parse-a-given-rule-disabled-status-correctly ()
  "When calling `elsint-reader-parse-rule` with a certain rule, it should return the correct disabled status."
  (noflet ((eslint-reader--read (&rest any) '(:prop [0 "never"])))
    (let ((result (eslint-reader--parse-rule :prop)))
      (should (listp result))
      (should (equal nil (plist-get result :enabled)))
      (should (equal "never" (plist-get result :setting))))))

(ert-deftest should-parse-a-given-rule-correctly-when-property-is-nested ()
  "When calling `elsint-reader-parse-rule` with a certain rule, it should return consistently."
  (noflet ((eslint-reader--read (&rest any) '(:prop [2 (:foo "bar" :bish "bash")])))
    (let ((result (eslint-reader--parse-rule :prop)))
      (should (listp result))
      (should (equal t (plist-get result :enabled)))
      (should (equal '(:foo "bar" :bish "bash") (plist-get result :setting))))))

(ert-deftest should-say-rule-is-disabled-when-it-is-not-defined ()
  "When calling `eslint-reader-parse-rule` with a rule that is not defined, it should return disabled"
  (noflet ((eslint-reader--read (&rest any) '()))
    (let ((result (eslint-reader--parse-rule :prop)))
      (should (listp result))
      (should (equal nil (plist-get result :enabled)))
      (should (equal nil (plist-get result :setting))))))

;; READER Suite


(defmacro with-eslint-closer-than-jshint (&rest body)
  "Create a temporary directory with an eslintrc file closer than a jshintrc in it"
  `(let ((eslint (f-expand flycheck-eslintrc sandbox-lower-path))
         (jshint (f-expand flycheck-jshintrc sandbox-higher-path)))
     (with-sandbox
      (f-write "{\"rules\":{\"name\":\"eslint\"}}" 'utf-8 eslint)
      (f-write "{\"rules\":{\"name\":\"jshint\"}}" 'utf-8 jshint)
      ,@body)))

(defmacro with-jshint-closer-than-eslint (&rest body)
  "Create a temporary directory with an jshintrc file closer than the eslintrc in it"
  `(let ((eslint (f-expand flycheck-eslintrc sandbox-higher-path))
         (jshint (f-expand flycheck-jshintrc sandbox-lower-path)))
     (with-sandbox
      (f-write "{\"rules\":{\"name\":\"eslint\"}}" 'utf-8 eslint)
      (f-write "{\"rules\":{\"name\":\"jshint\"}}" 'utf-8 jshint)
      ,@body)))

(defmacro with-jshint-at-same-level-as-eslint (&rest body)
  "Create a temporary directory with an eslintrc and jshintrc at the same level"
  `(let ((eslint (f-expand flycheck-eslintrc sandbox-higher-path))
         (jshint (f-expand flycheck-jshintrc sandbox-higher-path)))
     (with-sandbox
      (f-write "{\"rules\":{\"name\":\"eslint\"}}" 'utf-8 eslint)
      (f-write "{\"rules\":{\"name\":\"jshint\"}}" 'utf-8 jshint)
      ,@body)))

(ert-deftest should-return-nil-when-not-in-a-buffer ()
  "When the buffer reading eslint is not associated with a file, we should return nil"
  (with-jshint-closer-than-eslint
   (noflet ((buffer-file-name (&rest any) nil))
     (should (equal nil (eslint-reader?))))))

(ert-deftest should-return-t-when-eslintrc-is-closer ()
  "When the file reading eslint is closer to an eslintrc, we should return t"
  (with-eslint-closer-than-jshint
   (noflet ((buffer-file-name (&rest any) (f-expand "test.el" sandbox-lower-path)))
     (should (equal t (eslint-reader?))))))

(ert-deftest should-adhere-to-prioritization-when-eslintrc-is-at-same-level-as-jshint ()
  "When the file reading eslint is at the same level as the jshint file, we should obey `eslint-reader-prioritize-eslint` flag"
  (with-jshint-at-same-level-as-eslint
   (noflet ((buffer-file-name (&rest any) (f-expand "test.el" sandbox-lower-path)))
     (let ((eslint-reader-prioritize-eslint t))
       (should (equal t (eslint-reader?))))
     (let ((eslint-reader-prioritize-eslint nil))
       (should (equal nil (eslint-reader?)))))))

(ert-deftest should-return-nil-when-jshintrc-is-closer ()
  "When the file reading eslint is closer to a jshintrc, we should return nil"
  (with-jshint-closer-than-eslint
   (noflet ((buffer-file-name (&rest any) (f-expand "test.el" sandbox-lower-path)))
     (should (equal nil (eslint-reader?))))))

(ert-deftest should-return-nil-when-no-eslint-file-is-found ()
  "When you cannot locate an eslintrc file it should return nil"
  (with-sandbox
   (noflet ((buffer-file-name (&rest any) (f-expand "test.el" sandbox-higher-path)))
     (should (equal nil (eslint-reader?))))))

;; Actual reading functions

(ert-deftest should-return-the-rule-when-an-eslint-file-is-found ()
  "When appropriately finding an eslint file, it should call the appropiate rule function"
  (let ((rule-1-called nil)
        (rule-2-called nil))
    (noflet ((eslint-reader? (&rest any) t)
             (eslint-reader-rule-1 (&rest any) (setq rule-1-called t))
             (eslint-reader-rule-2 (&rest any) (setq rule-2-called t)))
      (er? 'rule-1)
      (should rule-1-called)
      (should-not rule-2-called)
      (er? 'rule-2)
      (should rule-2-called))))

(ert-deftest should-return-the-rule-when-an-eslint-file-is-found ()
  "When appropriately finding an eslint file, it should call the appropiate rule function with the prefix argument passed inthe `er?`"
  (let ((rule-call nil))
    (noflet ((eslint-reader? (&rest any) t)
             (eslint-reader-rule (pfx) (setq rule-call pfx)))
      (er? 'rule "bing")
      (should (equal "bing" rule-call)))))

(ert-deftest should-return-default-values-when-an-eslint-file-is-not-found ()
  "When we do not correctly find an eslintrc file then, we should not try to read eslintrc file, and instead return the default character"
  (let ((rule-call "unchanged")
        (eslint-reader-rule-default "default rule value"))
    (noflet ((eslint-reader? (&rest any) nil)
             (eslint-reader-rule (pfx) (setq rule-call pfx)))
      (should (equal "default rule value" (er? 'rule "bing")))
      (should (equal "unchanged" rule-call)))))

(ert-deftest should-error-if-no-rule-function-is-defined ()
  "When there is not a rule function defined, we should throw an error"
  (should-error (er? 'rule)))

(ert-deftest should-return-nil-if-no-default-value-is-defined-when-an-eslint-file-is-not-found ()
  "When we do not correctly find an eslintrc file and there is no default value for the rule call, we should just return nil"
  (noflet ((eslint-reader? (&rest any) nil)
           (eslint-reader-rule (&rest any) t))
    (should (equal nil (er? 'rule "bing")))))

;;; eslint-reader-semi-test.el ends here
;; Local Variables:
;; indent-tabs-mode: nil
;; eval: (flycheck-mode 0)
;; eval: (nameless-mode 0)
;; End:
