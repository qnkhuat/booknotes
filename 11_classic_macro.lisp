;; let

(defmacro our-let (binds &body body)
  `((lambda ,(mapcar #'(lambda (x)
                         (if (consp x) (car x) x))
                     binds)
      ,!body)
    ,@(mapcar #'(lambda (x)
                  (if (consp x) (cadr x) nil))
              binds)))


(mapcar (lambda (x) (+ 1 x)) '( 1 2 3 4))

(macroexpand '(let
                ((x 1))
                (+ x 1)))
