(defpackage #:let-over-lambda
  (:nicknames #:lol)
  (:use #:cl #:cl-user #:cl-pprce)
  (:import-from #:alexandria
                #:parse-body)
  (:import-from #:named-readtables
                #:defreadtable
                #:in-readtable)
  (:export
    #:group
    #:flatten
    #:dlambda
    #:g!-symbol-p
    #:defmacro/g!
    #:o!-symbol-p
    #:o!-symbol-to-g!-symbol
    #:alambda
    #:defmacro!
    #:|#`-reader|))
