(in-package :cl-user)
(defpackage cl-prime-number
  (:use :cl
	:anaphora
	:alexandria
	:cl-lazy))
(in-package :cl-prime-number)

(cl-annot:enable-annot-syntax)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-series-processing-syntax))

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
  (macrolet ((body (target prime-series)
	       (with-gensyms (factor
			      rest-target
			      rest
			      count)
		 `(let ((,rest-target ,target))
		    (concat-series 
		     #'(lambda (,factor)
			 (if (= ,rest-target 1)
			     nil
			     (multiple-value-bind (,rest ,count)
				 (div-while-can ,rest-target ,factor)
			       (setf ,rest-target ,rest)
			       ,count)))
		     ,prime-series)))))
    (body target prime-series)))

(defun div-while-can (target div)
  (let ((count 0))
    (loop while (= (mod target div) 0) do
	 (setf target (/ target div))
	 (incf count))
    (values target count)))

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
   (bundle-lists #'min
    (mapcar #'factorize-in-prime nums)
    :stops-by-min t)))

; LCM = least common multiple
@export
(defun calc-LCM (&rest nums)
  (inverse-factorize-from-prime
   (bundle-lists #'max
    (mapcar #'factorize-in-prime nums))))

@export
(defun bundle-lists (fn-reduce lists
		     &key (default-value 0) (stops-by-min nil))
  (labels ((lastp (cars)
	     (if stops-by-min
		 (some #'null cars)
		 (every #'null cars)))
	   (f (lst rest)
	     (let ((cars (mapcar #'car rest))
		   (cdrs (mapcar #'cdr rest)))
	       (if (lastp cars)
		   lst
		   (progn (nsubstitute default-value nil cars)
			  (f (cons (apply fn-reduce cars) lst) cdrs))))))
    (reverse (f nil lists))))

@export
(defun coprimep (&rest nums)
  (every #'zerop
	 (bundle-lists #'min
	  (mapcar #'factorize-in-prime nums)
	  :stops-by-min t)))
