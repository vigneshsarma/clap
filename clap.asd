#|
  This file is a part of clap.cl project.
|#

(in-package :cl-user)
(defpackage clap-asd
  (:use :cl :asdf))
(in-package :clap-asd)

(defsystem clap
  :version "0.1"
  :author ""
  :license ""
  :defsystem-depends-on (:prove-asdf)
  :depends-on (:cl-ppcre)
  :components ((:module "src"
                :components
                ((:file "clap"))))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op clap-test))))
