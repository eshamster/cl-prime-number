(in-package :cl-user)
(defpackage cl-prime-number-test
  (:use :cl
	:cl-lazy
        :cl-prime-number
        :prove))
(in-package :cl-prime-number-test)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-prime-number)' in your Lisp.

(plan 6)

(subtest
    "Test the series of prime"
  (is (llist-to-list *prime-series* :max-length 5)
      '(2 3 5 7 11)
      :test #'equalp))

(subtest
    "Test factorize-in-prime"
  (labels ((test-factorize (val expected)
	     (is (llist-to-list (factorize-in-prime val)) expected :test #'equalp)))
    (test-factorize 1 nil)
    (test-factorize 4 '(2))
    (test-factorize 72 '(3 2))
    (test-factorize 100 '(2 0 2))
    (test-factorize 37 '(0 0 0 0 0 0 0 0 0 0 0 1))
    (test-factorize 171 '(0 2 0 0 0 0 0 1))))

(progn (block blk (return-from blk)
(subtest
    "Test inverse-factorize-from-prime"
  (dolist (x '(1 4 72 100 37 171))
    (is (inverse-factorize-from-prime
	 (factorize-in-prime x))
	x)))

(subtest
    "Test calc-GCD (greatest common divisor)"
  (is (calc-gcd 1) 1)
  (is (calc-gcd 12) 12)
  (is (calc-gcd 2 9) 1)
  (is (calc-gcd 2 8) 2)
  (is (calc-gcd 8 12) 4)
  (is (calc-gcd 9 18 24) 3))

(subtest
    "Test clac-LCM (latest common multiple)"
  (is (calc-lcm 1) 1)
  (is (calc-lcm 12) 12)
  (is (calc-lcm 2 9) 18)
  (is (calc-lcm 2 8) 8)
  (is (calc-lcm 8 12) 24)
  (is (calc-lcm 9 18 24) 72))

(subtest
    "Test coprimep"
  (ok (coprimep 2 9))
  (ok (not (coprimep 2 8)))
  (ok (not (coprimep 8 12)))
  (ok (coprimep 2 8 9))
  (ok (not (coprimep 2 12 100)))
  (ok (not (coprimep 6 12 96))))
))
(finalize)
