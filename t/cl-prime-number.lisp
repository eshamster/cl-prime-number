(in-package :cl-user)
(defpackage cl-prime-number-test
  (:use :cl
	:cl-lazy
        :cl-prime-number
        :prove))
(in-package :cl-prime-number-test)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-prime-number)' in your Lisp.

(plan 3)

(subtest
    "Test the series of prime"
  (is (llist-to-list *prime-series* :max-length 5)
      '(2 3 5 7 11)
      :test #'equalp))

(subtest
    "Test factorize-in-prime"
  (is (factorize-in-prime 1) nil)
  (is (factorize-in-prime 4) '(2) :test #'equalp)
  (is (factorize-in-prime 72) '(3 2) :test #'equalp)
  (is (factorize-in-prime 100) '(2 0 2) :test #'equalp)
  (is (factorize-in-prime 37) '(0 0 0 0 0 0 0 0 0 0 0 1) :test #'equal)
  (is (factorize-in-prime 171) '(0 2 0 0 0 0 0 1) :test #'equalp))

(subtest
    "Test inverse-factorize-from-prime"
  (dolist (x '(1 4 72 100 37 171))
    (is (inverse-factorize-from-prime
	 (factorize-in-prime x))
	x)))

(finalize)
