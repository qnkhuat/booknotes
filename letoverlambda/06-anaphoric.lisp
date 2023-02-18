(use-package :let-over-lambda)

(defmacro aif (test then &optional else)
  `(let ((it ,test))
     (if it ,then ,else)))

(aif t
     (princ it))

'#`((,a1))

;; the #` reader macro is just lke the anonymous fn macro reader in clojure

(mapcar #`(,a1 'empty)
    '(var-a var-b var-c))
;; => ((VAR-A 'EMPTY) (VAR-B 'EMPTY) (VAR-C 'EMPTY))

(let ((vars '(var-a var-b var-c)))
    (mapcar #2`(,a1 ',a2)
      vars
      (loop for v in vars
            collect (gensym
                      (symbol-name v)))))

(let (x (caddr (#3`(((,@a2)) ,a3 (,a1 ,a1))
                (gensym)
                '(a b c)
                'hello)))
  (eq (car x) (cadr x)))

(defmacro alet% (letargs &rest body)
  `(let ((this) ,@letargs)
     (setq this ,@(last body))
     ,@(butlast body)
     this))

(defmacro alet (letargs &rest body)
  `(let ((this)
         ,@letargs)
     (setq this ,@(last body))
     ,@(butlast body)
     (lambda (&rest params)
       (apply this params))))

;; the following code will show the implementation of alet-test with various techniques
;; with each improve a bit
(defparameter alet-test (alet ((acc 0))
                            (alambda (n)
                              (if (eq n 'invert)
                                (setq this
                                      (lambda (n)
                                        (if (eq n 'invert)
                                          (setq this #'self)
                                          (decf acc n))))
                                (incf acc n)))))


(funcall alet-test 10)
;; => 10
(funcall alet-test 10)
;; => 20

(funcall alet-test 'invert)

(funcall alet-test 10)
;; => 10


;; second version with the use of labels
(alet ((acc 0))
  (labels ((going-up (n)
             (if (eq n 'invert)
               (setq this #'going-down)
               (incf acc n)))
           (going-down (n)
             (if (eq n 'invert)
               (setq this #'going-up)
               (incf acc (- n)))))
    #'going-up))

;; third version with alet-fsm

(defmacro alet-fsm (&rest states)
  `(macrolet ((state (s)
                `(setq this #',s)))
     (labels (,@states) #',(caar states))))

(alet ((acc 0))
  (alet-fsm
    (going-up (n)
      (if (eq n 'invert)
        (state going-down)
        (incf acc n)))
    (going-down (n)
      (if (eq n 'invert)
        (state going-up)
        (decf acc n)))))

(macroexpand '(alet-fsm
                  (going-up (n)
                    (if (eq n 'invert)
                      (state going-down)
                      (incf acc n)))
                  (going-down (n)
                    (if (eq n 'invert)
                      (state going-up)
                      (decf acc n)))))

;; -- Indrection chains
(defmacro! ichain-before (&rest body)
  `(let ((,g!indir-env this))
     (setq this
       (lambda (&rest ,g!temp-args)
         ,@body
         (apply ,g!indir-env
                ,g!temp-args)))))

(defparameter c (alet ((acc 0))
                    (ichain-before
                      (format t "Changing from ~a~%" acc))
                    (lambda (n)
                      (incf acc n))))

(funcall c 2)

(defmacro! ichain-after (&rest body)
  `(let ((,g!indir-env this))
     (setq this
       (lambda (&rest ,g!temp-args)
         (prog1
           (apply ,g!indir-env
                  ,g!temp-args)
           ,@body)))))

(defparameter c2 (alet ((acc 0))
                     (ichain-before
                       (format t "Changing from ~a~%" acc))
                     (ichain-after
                       (format t "Changed to ~a~%" acc))
                     (lambda (n)
                       (incf acc n))))

(funcall c2 3)


;; hotpatching closures

(defmacro alet-hotpatch% (letargs &rest body)
  `(let ((this) ,@letargs)
     (setq this ,@(last body))
     ,@(butlast body)
     (lambda (&rest args)
       (if (eq (car args) ':hotpatch)
         (setq this (cadr args))
         (apply this args)))))

(setf (symbol-function 'hotpatch-test)
      (alet-hotpatch% ((acc 0))
         (lambda (n)
           (incf acc n))))

(hotpatch-test 3)
;; => 3

(hotpatch-test :hotpatch
  (let ((acc 0))
    (lambda (n)
      (incf acc (* 2 n)))))

(hotpatch-test 5)
;; 10


;; hot patch is fun and useful but it introduces a this anaphor to the macro
;; if you want to avoid using this, we have a technique called "anaphor closing"
;; by using gensym

(defmacro! alet-hotpatch (letargs &rest body)
  `(let ((,g!this) ,@letargs)
     (setq ,g!this ,@(last body))
     ,@(butlast body)
     (lambda (&rest args)
       (if (eq (car args) ':hotpatch)
         (setq ,g!this (cadr args))
         (apply ,g!this args)))))


;; Pandoric macros

(defun let-binding-transform (bs)
  (if bs
    (cons
      (cond ((symbolp (car bs))
             (list (car bs)))
            ((consp (car bs))
             (car bs))
            (t
              (error "Bad let bindings")))
      (let-binding-transform (cdr bs)))))

;; sym is bounded by dlambda
(defun pandoriclet-get (letargs)
  `(case sym
     ,@(mapcar #`((,(car a1)) ,(car a1))
               letargs)
     (t (error "Unknown pandoric get : ~a" sym))))

;; sym,val is bounded by dlambda
(defun pandoriclet-set (letargs)
  `(case sym
     ,@(mapcar #`((,(car a1)) (setq ,(car a1) val))
               letargs)
     (t (error "Unknown pandoric get : ~a" sym val))))

(defmacro pandoriclet (letargs &rest body)
  (let ((letargs (cons
                   '(this)
                   (let-binding-transform letargs))))
    `(let (,@letargs)
       (setq this ,@(last body))
       ,@(butlast body)
       (dlambda
         (:pandoric-get (sym)
           ,(pandoriclet-get letargs))
         (:pandoric-set (sym val)
           ,(pandoriclet-set letargs))
         (t (&rest args)
           (apply this args))))))

(setf (symbol-function 'pantest)
     (pandoriclet ((acc 0))
       (lambda (n) (incf acc n))))

(pantest 3)
;; => 3
(pantest 3)
;; => 6

(pantest :pandoric-get 'acc)
;; => 6

(pantest :pandoric-set 'acc 100)
;; => 100


(declaim (inlne get-pandoric))

(defun get-pandoric (box sym)
  (funcall box :pandoric-get sym))

(defsetf get-pandoric (box sym) (val)
  `(progn
     (funcall ,box :pandoric-set ,sym ,val)
     ,val))

(get-pandoric #'pantest 'acc)

(defmacro! with-pandoric (syms o!box &rest body)
  `(symbol-macrolet
     (,@(mapcar #`(,a1 (get-pandoric ,g!box ',a1))
                syms))
     ,@body))

(with-pandoric (acc) #'pantest
  (format t "Value of acc: ~a~%" acc)
  acc)


(with-pandoric (acc) #'pantest
  (setq acc 5))
;; => 5

(pantest 1)
;; => 6
