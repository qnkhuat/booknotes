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
