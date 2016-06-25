#|
  This file is a part of cl-prime-number project.
  Copyright (c) 2015 eshamster (hamgoostar@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-prime-number-test-asd
  (:use :cl :asdf))
(in-package :cl-prime-number-test-asd)

(defsystem cl-prime-number-test
  :author "eshamster"
  :license "MIT"
  :depends-on (:cl-prime-number
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "cl-prime-number"))))

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
