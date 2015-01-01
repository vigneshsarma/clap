(in-package :cl-user)
(defpackage clap-test
  (:use :cl
        :clap
        :prove))
(in-package :clap-test)

;; NOTE: To run this test file, execute `(asdf:test-system :clap)' in your Lisp.

(plan 7)
(diag "expands clumped short options")
(is (clap:tokenize-args '("-p") '("-abcp80"))
    '(((:short-opt "-a") (:short-opt "-b") (:short-opt "-c") (:short-opt "-p" "80")) ()))
(diag "detects arguments to long options")
(is (clap:tokenize-args '("--port" "--host") '("--port=80" "--host" "example.com"))
    '(((:long-opt "--port" "80") (:long-opt "--host" "example.com")) nil))
(is (clap:tokenize-args '() '("--foo=bar" "--noarg=" "--bad =opt"))
    '(((:long-opt "--foo" "bar") (:long-opt "--noarg" "") (:long-opt "--bad =opt")) nil))
(diag "stops option processing on double dash")
(is (clap:tokenize-args '() '("-a" "--" "-b"))
    '(((:short-opt "-a")) ("-b")))
(diag "finds trailing options unless :in-order is true")
(is (clap:tokenize-args '() '("-a" "foo" "-b"))
    '(((:short-opt "-a")(:short-opt "-b")) ("foo")))
(is (clap:tokenize-args '() '("-a" "foo" "-b") :in-order t)
    '(((:short-opt "-a")) ("foo" "-b")))
(diag "does not interpret single dash as an option")
(is (clap:tokenize-args '() '("-"))
    '(() ("-")))
(finalize)

(plan 1)
(diag "does not set values for :default unless specified")
(is (mapcar (lambda (item) (if (assoc :default item) t nil))
            (compile-option-spec '(("-f" "--foo") ("-b" "--bar=ARG" :default 0))))
    '(nil t))
(finalize)
