(defpackage #:let-over-lambda
  (:nicknames #:lol)
  (:use #:cl #:cl-user #:cl-pprce)
  (:export
    #:group
    #:flatten
    #:g!-symbol-p
    #:defmacro/g!
    #:o!-symbol-p
    #:o!-symbol-to-g!-symbol
    #:defmacro!))
