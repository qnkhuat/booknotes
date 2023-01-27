(use-package :let-over-lambda)

(defmacro! defunits% (quantity base-unit &rest units)
  `(defmacro ,(symb 'unit-of- quantity) (,g!val ,g!un)
     `(* ,,g!val
         ,(case ,g!un
            ((,base-unit ) 1)
            ,@(mapcar (lambda (x)
                        `((,(car x)) ,(cadr x)))
                      (group units 2))))))

(defunits% time s
           m 60
           h 3600)

(unit-of-time 3 h)
;; => 10800


(defun defunits-chaining (u units prev)
  (if (member u prev)
    (error "~{~a~^ depends on ~}"
           (cons u prev)))
  (let ((spec (find u units :key #'car)))
    (if (null spec)
      (error "Unknown unit ~a" u)
      (let ((chain (cadr spec)))
        (if (listp chain)
          (* (car chain)
             (defunits-chaining (cadr chain) units (cons u prev)))
          chain)))))

(defmacro! defunits (quantity base-unit &rest units)
  `(defmacro ,(symb 'unit-of- quantity)
     (,g!val ,g!un)
     `(* ,,g!val
         ,(case ,g!un
            ((,base-unit) 1)
            ,@(mapcar (lambda (x)
                        `((, (car x))
                          ,(defunits-chaining
                             (car x)
                             (cons `(,base-unit 1)
                                   (group units 2))
                             nil)))
                      (group units 2))))))

(defunits time s
          m 60
          h (60 m))

(unit-of-time 1 h)
;; => 3600


;; usage of macrolet

(defmacro! nlet-tail (n letargs &rest body)
  (let ((gs (loop for i in letargs
                  collect (gensym))))
    `(macrolet
       ((,n ,gs
            `(progn
               (psetq
                 ,@(apply #'nconc
                          (mapcar
                            #'list
                            ',(mapcar #'car letargs)
                            (list ,@gs))))
               (go ,',g!n))))
       (block ,g!b
              (let ,letargs
                (tagbody
                  ,g!n (return-from
                         ,g!b (progn ,@body))))))))

(defun nlet-tail-fact (n)
  (nlet-tail fact ((n n) (acc 1))
             (if (zerop n)
               acc
               (fact (- n 1) (* acc n)))))

(macroexpand '(nlet-tail fact ((n n) (acc 1))
               (if (zerop n)
                 acc
                 (fact (- n 1) (* acc n)))))
