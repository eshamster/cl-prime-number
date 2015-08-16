(in-package :cl-user)
(defpackage cl-prime-number-test
  (:use :cl
	:cl-lazy
        :cl-prime-number
        :prove))
(in-package :cl-prime-number-test)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-prime-number)' in your Lisp.

(plan 1)

(subtest
    "Test the series of prime"
  (is (llist-to-list *prime-series* :max-length 5)
      '(2 3 5 7 11)
      :test #'equalp))

(finalize)
