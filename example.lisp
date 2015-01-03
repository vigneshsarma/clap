#!/usr/bin/env sbcl --script

(load "~/.sbclrc")
(ql:quickload "clap" :verbose nil)

(in-package :cl-user)
(defpackage clap-ql
  (:use :cl :clap :ql))
(in-package :clap-ql)

(defparameter cli-options
  '(("-s" "--search" "Search for a given system."
     :required "SYSTEM")
    ("-i" "--install" "Install the given system."
     :required "SYSTEM")
    ("-u" "--update-all-dists" "Update the default dist.")
    ("-U" "--update-client" "Update the client.")
    ("-v" nil "Verbosity"
     :required t
     :id :verbosity
     :default 0)
    ("-h" "--help" "Print help message and exit")))

(defun main (options arguments summary errors)
  (labels ((get-value (key)
             (cdr (assoc key options))))
   (cond
    (errors
     (format t "~{~a~%~}" errors))
    ((get-value :install)
     (ql:quickload (get-value :install)))
    ((get-value :search)
     (ql:system-apropos (get-value :search)))
    ((get-value :update-all-dists)
     (ql:update-all-dists))
    ((get-value :update-client)
     (ql:update-client))
    ((get-value :help)
     (progn
       (format t "Usage: ./example.lisp [options] arguments~%~%")
       (format t "This is a command line interface to quicklisp.~%~%")
       (princ summary))))))

(clap:parse-opts (clap:get-argv) cli-options :handler-fn #'main)
