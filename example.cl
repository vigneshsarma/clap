(in-package :cl-user)
(defpackage clap-ql
  (:use :cl :clap)
  ;; (:export :tokenize-args)
  )
(in-package :clap-ql)

(defparameter cli-options
  `(((:id :port)
     (:short-opt "-p")
     (:long-opt "--port" "Port Number")
     (:required t)
     (:default 80)
     (:parse-fn ,(lambda (arg) (princ arg)))
     (:validate-fn ,(lambda (arg) (< 0 arg 60000))))
    ((:short-opt "-v")
     (:required t)
     (:id :verbosity)
     (:default 0))
    ((:id :help)
     (:short-opt "-h")
     (:long-opt "--help"))))

(parse-opts cli-options)
