(in-package :cl-user)
(defpackage clap-ql
  (:use :cl :clap)
  ;; (:export :tokenize-args)
  )
(in-package :clap-ql)

(defparameter cli-options
  `(((:short-opt "-p")
     (:long-opt "--port")
     (:required t)
     (:default 80))
    ((:short-opt "-v")
     (:required t)
     (:id :verbosity)
     (:default 0))
    ((:short-opt "-h")
     (:long-opt "--help"))))

(parse-opts cli-options)
