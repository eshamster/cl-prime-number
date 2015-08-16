(in-package :cl-user)
(defpackage cl-prime-number
  (:use :cl
	:anaphora
	:cl-lazy))
(in-package :cl-prime-number)

(cl-annot:enable-annot-syntax)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (lexport-readtable))

(defun check-series-mod (target a n)
  (cond ((< target 2) nil)
	((= target 2) t)
	(t (do-series (val a to (1- n))
	     (if (= (mod target val) 0)
		 (return-from check-series-mod nil))
	     (if (> (expt val 2) target)
		 (return-from check-series-mod t)))
	   t)))

@export
(defparameter *prime-series* (filter-series-using-little
			      #'check-series-mod
			      #<a[n] = 2, (+ (* n 2) 1)>))

@export
(defun factorize-in-prime (target &key (prime-series *prime-series*))
  (if (eq target 1)
      (return-from factorize-in-prime nil))
  (let ((count 0)
	(factor (lcar prime-series)))
    (loop while (= (mod target factor) 0) do
	 (setf target (/ target factor))
	 (incf count))
    (cons count (factorize-in-prime target :prime-series (lcdr prime-series)))))

@export
(defun inverse-factorize-from-prime (target &key (prime-series *prime-series*))
  (labels ((f (result rest-target rest-prime-series)
	     (if (null rest-target)
		 (return-from f result))
	     (f (* result (expt (lcar rest-prime-series)
				(car rest-target)))
		(cdr rest-target)
		(lcdr rest-prime-series))))
    (f 1 target prime-series)))

; GCD = greatest common divisor
@export
(defun calc-GCD (&rest nums)
  (inverse-factorize-from-prime
   (apply #'bundle-lists-by-min
	  (mapcar #'factorize-in-prime nums))))

(defun bundle-lists-by-min (&rest lists)
  (apply #'map `(list min ,@lists)))

; LCM = least common multiple
@export
(defun calc-LCM (&rest nums)
  (inverse-factorize-from-prime
   (apply #'bundle-lists-by-max
	  (mapcar #'factorize-in-prime nums))))

(defun bundle-lists-by-max (&rest lists)
  (let ((cars (mapcar #'car lists))
	(cdrs (mapcar #'cdr lists)))
    (if (every #'null cars)
	nil
	(cons (apply #'max-plus cars) (apply #'bundle-lists-by-max cdrs)))))

(defun max-plus (&rest rest-lst)
  (aif (remove-if #'null rest-lst)
       (apply #'max it)
       nil))

@export
(defun coprimep (&rest nums)
  (every #'zerop
	 (apply #'bundle-lists-by-min
		(mapcar #'factorize-in-prime nums))))
