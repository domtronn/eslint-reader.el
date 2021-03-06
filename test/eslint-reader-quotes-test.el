;; Tests for the eslint Rule quotes
;; http://eslint.org/docs/rules/quotes

(require 'f)
(require 'ert)
(require 'noflet)

(defvar code-base-path (f-parent (f-parent (f-this-file))))
(require 'eslint-reader-core (f-expand "eslint-reader-core.el" code-base-path))
(require 'eslint-reader-quotes (f-expand "eslint-reader-quotes.el" code-base-path))

(ert-deftest getting-dominant-quote-should-return-as-expected ()
  "The helper function should return which quote character, single, double or backtick, is more prevalent in a buffer."
  (with-temp-buffer
    (insert "''''''''\"\"\"\"\"\"\"```````") ;; 8 single 7 double 7 backtick
    (should (equal '("'" single) (eslint-reader--dominant-quotes))))
  (with-temp-buffer
    (insert "'''''''\"\"\"\"\"\"\"\"```````") ;; 7 single 8 double 7 backtick
    (should (equal '("\"" double) (eslint-reader--dominant-quotes))))
  (with-temp-buffer
    (insert "'''''''\"\"\"\"\"\"\"````````") ;; 7 single 7 double 8 backtick
    (should (equal '("`" backtick) (eslint-reader--dominant-quotes)))))

(ert-deftest getting-dominant-quote-should-adhere-to-the-priority-ordering ()
  "When the buffer contains as many single as double quotes, it should follow the defined priority order."
  (with-temp-buffer
    (insert "'''''''\"\"\"\"\"\"\"```````") ;; 7 single 7 double 7 backtick
    (let ((eslint-reader-quote-priority '(single double backtick)))
      (should (equal '("'" single) (eslint-reader--dominant-quotes))))
    (let ((eslint-reader-quote-priority '(double single backtick)))
      (should (equal '("\"" double) (eslint-reader--dominant-quotes))))
    (let ((eslint-reader-quote-priority '(backtick double single)))
      (should (equal '("`" backtick) (eslint-reader--dominant-quotes)))))
  (with-temp-buffer
    (insert "''''''\"\"\"\"\"\"\"```````") ;; 6 single 7 double 7 backtick
    (let ((eslint-reader-quote-priority '(single backtick double)))
      (should (equal '("`" backtick) (eslint-reader--dominant-quotes))))))

(ert-deftest should-return-the-dominant-quote-character-when-the-setting-is-off ()
  "When the setting is off, we should use the most dominant character in the file"
  (noflet ((eslint-reader--read (&rest any) '(:quotes 0)))
    (with-temp-buffer
      (insert "''''''''''''''''''''```````````")
      (should (equal 'single (eslint-reader-quotes)))
      (should (equal "'" (eslint-reader-quotes t))))
    (with-temp-buffer
      (insert "''''''''''''''''''''``````````````````````````````````````````")
      (should (equal 'backtick (eslint-reader-quotes)))
      (should (equal "`" (eslint-reader-quotes t)))))
  (noflet ((eslint-reader--read (&rest any) '(:quotes [0 "single"])))
    (with-temp-buffer
      (insert "``````")
      (should (equal 'backtick (eslint-reader-quotes)))
      (should (equal "`" (eslint-reader-quotes t))))))

(ert-deftest should-return-the-right-setting-when-the-setting-is-on ()
  "When the setting is on, we should return the correct character and quote style"
  (let ((dominant-called nil))
    (noflet ((eslint-reader--read (&rest any) '(:quotes [2 "single"]))
             (eslint-reader--dominant-quotes (&rest any) (setq dominant-called t)))
      (should-not dominant-called)
      (should (equal 'single (eslint-reader-quotes)))
      (should (equal "'" (eslint-reader-quotes t))))

    (noflet ((eslint-reader--read (&rest any) '(:quotes [2 "double"]))
             (eslint-reader--dominant-quotes (&rest any) (setq dominant-called t)))
      (should-not dominant-called)
      (should (equal 'double (eslint-reader-quotes)))
      (should (equal "\"" (eslint-reader-quotes t))))

    (noflet ((eslint-reader--read (&rest any) '(:quotes [2 "backtick"]))
             (eslint-reader--dominant-quotes (&rest any) (setq dominant-called t)))
      (should-not dominant-called)
      (should (equal 'backtick (eslint-reader-quotes)))
      (should (equal "`" (eslint-reader-quotes t))))))

;;; eslint-reader-quotes-test.el ends here
;; Local Variables:
;; quotes-tabs-mode: nil
;; eval: (nameless-mode 0)
;; End:
