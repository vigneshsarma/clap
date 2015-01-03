(in-package :cl-user)
(defpackage clap
  (:use :cl :cl-ppcre)
  (:export :parse-opts))
(in-package :clap)

(defun re-matches? (regex string)
  (if (cl-ppcre:all-matches-as-strings regex string)
      t nil))

(defun conj (first second)
  (if first
      (append first (list second))
      (list second)))

(defun contains? (sequence item)
  (member item sequence :test #'equal))

(defun tokenize-args (required-set args &key in-order)
  "Reduce arguments sequence into (opt-type opt ?optarg?) lists and a list
  of remaining arguments. Returns as (option-tokens remaining-args)."
  (labels ((handle-short-opt (os x xs)
             (let* ((c (subseq x 0 1))
                    (cs (subseq x 1))
                    (o (format nil "-~a" c)))
               ;; (format t "os: ~a x: ~a o: ~a c: ~a cs: ~a~%" os x o c cs)
               (if (contains? required-set o)
                   (if (not (equal "" cs))
                       (list (conj os (list :short-opt o cs)) xs)
                       (list (conj os (list :short-opt (first xs))) (rest xs)))
                   (if (not (equal "" cs))
                       (handle-short-opt (conj os (list :short-opt o)) cs xs)
                       (list (conj os (list :short-opt o)) xs)))))
           (iterate (opts argv args)
             ;; (format t "opts: ~a argv: ~a args: ~a~%" opts argv args)
             (let ((x (first args))
                   (xs (rest args)))
               (if x
                 (cond
                   ;; Double dash always ends options processing
                   ((equal "--" x) (iterate opts (append argv xs) '()))
                   ;; Long options with assignment always passes optarg, required or not
                   ((re-matches? "^--\\S+=" x)
                    (iterate (conj opts (append
                                         '(:long-opt)
                                         (cl-ppcre:split "=" x :limit 2)))
                             argv xs))
                   ;; Long options, consumes cdr head if needed
                   ((re-matches? "^--" x)
                    (let* ((optarg-xs (if (contains? required-set x)
                                          (list (first xs) (rest xs))
                                          (list nil xs)))
                           (optarg (first optarg-xs))
                           (xs (second optarg-xs)))
                      (iterate (conj opts (append (list :long-opt x)
                                                  (if optarg (list optarg) '())))
                               argv xs)))
                   ;; Short options, expands clumped opts until an optarg is required
                   ((re-matches? "^-." x)
                    (let* ((os-cdr (handle-short-opt
                                    '() (subseq x 1) xs))
                           (os (first os-cdr))
                           (xs (second os-cdr)))
                      (iterate (append opts os) argv xs)))
                   (in-order (iterate opts (append argv (cons x xs)) '()))
                   (t (iterate opts (conj argv x) xs)))
                 (list opts argv)))))
    (iterate '() '() args)))

(defparameter spec-keys
  '(:id :short-opt :long-opt :required :desc :default :default-desc :parse-fn
    :assoc-fn :validate-fn :validate-msg))

(defun summarize (specs)
  (format nil "~a" specs))

(defun default-option-map (specs)
  "Go through each spec and find the arguments that have a
default value. Use it to create a dictionary with spec id
as key and  default value."
  (reduce (lambda (m spec)
            (if (assoc :default spec)
                (acons  (second (assoc :id spec))
                        (second (assoc :default spec)) m)
                  m))
          specs :initial-value nil))

(define-condition clap-error (error)
  ((opt :initarg :opt :reader opt))
  (:report "Some error in argument."))

(define-condition clap-no-id-for-spec (clap-error)
  ()
  (:report (lambda (condition stream)
             (format stream "There should be either :id or :long-opt in: ~a"
                     (opt condition)))))

(define-condition clap-parse-error (clap-error)
  ((optarg :initarg :optarg :reader optarg)
   (err-condition :initarg err-condition :reader err-condition))
  (:report (lambda (condition stream)
             (format stream "Error while parsing option ~a ~a: ~a"
                     (opt condition) (optarg condition) (err-condition condition)))))

(define-condition missing-required-error (clap-error)
  ((example-required :initarg :example-required :reader example-required))
  (:report (lambda (condition stream)
             (format stream "Missing required argument for ~a ~a"
                     (opt condition) (example-required condition)))))

;; TODO: add call to custom validation function.
(defun parse-value (value spec opt optarg)
  (let ((parse-fn (second (assoc :parse-fn spec))))
    (if parse-fn
        (handler-case (apply #'parse-fn (list value))
          (error (e)
            (error 'clap-parse-error :opt opt :optarg optarg :err-condition e)))
        value)))

(defun parse-optarg (spec opt optarg)
  "Given a spec, option and option-argument. decide what its value should be."
  (let ((required (second (assoc :required spec))))
    (if (and required (null optarg))
        (error 'missing-required-error opt required)
        (parse-value (if required optarg t) spec opt optarg))))

(defun parse-option-tokens (specs tokens &key no-defaults)
  (let ((defaults (default-option-map specs)))
    (labels ((missing-required-error (opt example-required)
               (format nil "Missing required argument for ~a ~a"
                       opt example-required))
             (find-spec (specs opt-type opt)
               (first (remove-if-not (lambda (spec)
                                       (equal opt (second (assoc opt-type spec))))
                              specs)))
             (reducer (acc item)
               (let* ((m (first acc))
                      (errors (second acc))
                      (opt-type (first item))
                      (opt (second item))
                      (optarg (third item))
                      (spec (find-spec specs opt-type opt))
                      (id (second (assoc :id spec))))
                 (if spec
                     (handler-case (let ((value (parse-optarg spec opt optarg)))
                                     (list (acons id value m) errors))
                       (clap-error (e)
                         (list m (conj errors (format nil "~a" e)))))
                     (list m (append errors (format nil "Unsupported option ~a"
                                                    opt)))))))
      (reduce #'reducer tokens :initial-value (list defaults '())))))

(defun required-arguments (specs)
  (reduce (lambda (acc item)
            (let ((required (assoc :required item))
                  (short-opt (assoc :short-opt item))
                  (long-opt (assoc :long-opt item)))
              (if (second required)
                  (append acc (remove-if
                               #'null `(,(second short-opt) ,(second long-opt))))
                  acc)))
          specs :initial-value '()))

(defun compile-spec (spec)
  (let* ((long-opt (second (assoc :long-opt spec)))
         (id (second (assoc :id spec)))
         (long-opt (when long-opt
                     (let ((groups (nth-value 1 (cl-ppcre:scan-to-strings
                                                 "^--(.*)$" long-opt))))
                       (when groups (elt groups 0))))))
    (cond ((and (not id) (not long-opt)) (error 'clap-no-id-for-spec spec))
          ((not id) (conj spec (list :id (intern (string-upcase long-opt)
                                                 :keyword))))
          (t spec))))

;; TODO: Implementation incomplete.
(defun compile-option-specs (options-specs)
  (mapcar #'compile-spec options-specs))

(defun parse-opts (args option-specs
                   &key in-order no-defaults summary-fn)
  (let* ((specs (compile-option-specs option-specs))
        (req (required-arguments specs))
        (tokens-&-rest-args (tokenize-args req args :in-order in-order))
        (opts-&-errors (parse-option-tokens specs (first tokens-&-rest-args)
                                            :no-defaults no-defaults)))
    `((:options ,(first opts-&-errors))
      (:arguments ,(second tokens-&-rest-args))
      (:summary ,(summarize specs))
      (:errors ,(second opts-&-errors)))))
