# Clap.Cl
CLAP is a port cum re-imagining of Clojure library [tools.cli](https://github.com/clojure/tools.cli/). I found most of the existing tools for the purpose to be old and unmaintained.

## Usage

You can set up your script like so.
```lisp
(in-package :cl-user)

(defpackage my-program
  (:use :cl :clap ))

(in-package :my-program)

(defparameter cli-options2
  '(("-p" "--port" "Port number for the server."
          :required "PORT"
          :default 80)
    ("-v" nil "Verbosity"
          :id :verbosity
          :default 0)
    ("-h" "--help" "Print help message and exit")))

(clap:parse-opts (clap:get-argv) cli-options)
```

Execute the command line:

    my-program -vvvp8080 foo --help --invalid-opt

to produce the map:
```lisp
((:options    ((:port 8080)
               (:verbosity 3)
               (:help true)))
  (:arguments ("foo"))
  (:summary   "  -p, --port PORT  80  Port number
                 -v                   Verbosity level
                 -h, --help")
  (:errors    ("Unknown option: \"--invalid-opt\"")))
```
You can optionaly give a keyword argument `:handler-fn` to parse-opts which takes as arguemnts `options, arguments, summary, errors`. Check out `example.lisp` to for a working example.


## Installation
Right now this is not part of quicklisp so the easiest way to install it is in `shell`:
```bash
$ cd QUICKLISP_FOLDER/local-projects
$ git clone git@github.com:vigneshsarma/clap.git
```
go to quicklisp loaded lisp `repl`:

    (ql:quickload :clap)

should now work
