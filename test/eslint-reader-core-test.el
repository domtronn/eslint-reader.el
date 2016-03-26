;; Tests for the eslint Rule Semi
;; http://eslint.org/docs/rules/semi

(require 'f)
(require 'ert)
(require 'noflet)
(require 'flycheck)

(defvar root-test-path (f-dirname (f-this-file)))
(defvar root-sandbox-path (f-expand "sandbox" root-test-path))
(defvar root-nested-path (f-expand "nested1" root-sandbox-path))

(defmacro with-sandbox (&rest body)
  "Evaluate BODY in an empty temporary directory."
  `(let ((default-directory root-sandbox-path))
     (when (f-dir? root-sandbox-path) (f-delete root-sandbox-path :force))
     (f-mkdir root-sandbox-path)
     (f-mkdir root-nested-path)
     ,@body
     (f-delete root-sandbox-path :force)))

(defmacro with-only-eslint (&rest body)
  "Create a temporary directory with just an eslintrc file in it"
  `(let ((eslint-1 (f-expand flycheck-eslintrc root-nested-path))
         (eslint-2 (f-expand flycheck-eslintrc root-sandbox-path))
         (eslint-3 (f-expand ".my-eslintrc" root-sandbox-path)))
     (with-sandbox
      (f-write "{\"rules\":\"eslint-1\"}" 'utf-8 eslint-1)
      (f-write "{\"rules\":\"eslint-1\"}" 'utf-8 eslint-1)
      (f-write "{\"rules\":\"eslint-3\"}" 'utf-8 eslint-3)
      ,@body)))

(ert-deftest should-return-the-rules-from-closest-eslint-file ()
  "When calling the read function from the nested directory with only eslint files, it should return the eslint closest eslint file."
  (with-only-eslint
   (noflet ((buffer-file-name (&rest any) (f-expand "test.el" root-nested-path)))
     (should (equal "eslint-1" (eslint-reader--read))))))

(ert-deftest should-return-the-rules-from-closest-provided-file-as-override ()
  "When calling the read function with the `eslintrc` option, it should find that file over default `flycheck-eslintrc` file"
  (with-only-eslint
   (noflet ((buffer-file-name (&rest any) (f-expand "test.el" root-nested-path)))
     (should (equal "eslint-3" (eslint-reader--read ".my-eslintrc"))))))


;;; eslint-reader-semi-test.el ends here
;; Local Variables:
;; indent-tabs-mode: nil
;; eval: (flycheck-mode 0)
;; eval: (nameless-mode 0)
;; End:
