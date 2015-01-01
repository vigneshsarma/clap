#|
  This file is a part of clap.cl project.
|#

(in-package :cl-user)
(defpackage clap.cl-test-asd
  (:use :cl :asdf))
(in-package :clap-test-asd)

(defsystem clap-test
  :author ""
  :license ""
  :depends-on (:clap
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "clap"))))

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
