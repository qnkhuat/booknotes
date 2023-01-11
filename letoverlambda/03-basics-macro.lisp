(ql:quickload "let-over-lambda")

(defmacro sleep-units (value unit)
  (princ unit)
  `(sleep
     (* ,value
        ,(case unit
           ((s) 1)
           ((m) 60)
           ((h) 3600)))))

(sleep-units 1 s)

(defvar fast-mode t)

(sleep-units 1 (if fast-mode 's 'm))
;; this will not works because the macro expects `unit` to be a symbol


;; a macro that try to simulates the named-let feature in scheme,
;; where you can name a let and use it as a function call
;; it's quite convinient when use with loop
;; an example:
;; (define (number->list n)
;;   (let loop ((n n)
;;              (acc '()))
;;     (if (< n 10)
;;         (cons n acc)
;;         (loop (quotient n 10)
;;               (cons (remainder n 10) acc)))))

(defmacro nlet (n letargs &rest body)
 `(labels ((,n ,(mapcar #'car letargs)
               ,@body))
    (,n ,@(mapcar #'cadr letargs))))

(defun nlet-fact (n)
  (nlet fact ((n n))
    (if (zerop n)
      1
      (* n (fact (- n 1))))))

(nlet-fact 3)
;; => 6

(macroexpand '(nlet fact ((n n))
               (if (zerop n)
                 1
                 (* n (fact (- n 1))))))
;; => (LABELS ((FACT (N)
;; =>            (IF (ZEROP N)
;; =>                1
;; =>                (* N (FACT (- N 1))))))
;; =>   (FACT N))

(lol:flatten (list 1 (list 2 3)))
