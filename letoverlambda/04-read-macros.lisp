(use-package :let-over-lambda)

;; #= and ## read macros

(defvar not-shared '((1) (1)))

(eq (car not-shared) (cadr not-shared))
;; => NIL

(defvar shared '(#1=(1) #1#))

(eq (car shared) (cadr shared))
;; => T

(list
  #1=(list 0)
  #1#
  #1#)
;; => ((0) (0) (0))
