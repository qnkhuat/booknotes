;; with-macro
;; operations with binds has to be defined in macros

(defmacro our-let (binds &body body)
  `((lambda ,(mapcar #'(lambda (x)
                         (if (consp x) (car x) x))
                     binds)
      ,!body)
    ,@(mapcar #'(lambda (x)
                  (if (consp x) (cadr x) nil))
              binds)))



(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s) `(,s (gensym))) syms)
         ,@body))


;; conditional evaluation of arguments
;;

(defmacro mvdo* (parm-cl test-cl &body body)
  (mvdo-gen parm-cl parm-cl test-cl body))

(defun mvdo-gen (binds rebinds test body)
  (if (null binds)
    (let ((label (gensym)))
      `(prog nil
             ,label
             (if ,(car test)
               (return (progn ,@(cdr test))))
             ,@body
             ,@(mvdo-rebind-gen rebinds)
             (go ,label)))
    (let ((rec (mvdo-gen (cdr binds) rebinds test body)))
      (let ((var/s (caar binds)) (expr (cadar binds)))
        (if (atom var/s)
          `(let ((,var/s ,expr )) ,rec)
          `(multiple-value-bind ,var/s ,expr ,rec))))))

(defun mvdo-rebind-gen (rebinds)
  (cond ((null rebinds) nil)
        ((< (length (car rebinds)) 3)
         (mvdo-rebind-gen (cdr rebinds)))
        (t
          (cons (list (if (atom (car rebinds))
                        `setq
                        `multiple-value-bind)
                      (caar rebinds)
                      (third (car rebinds)))
                (mvdo-rebind-gen (cdr rebinds))))))

(mvdo* ((x 1 (1+ x))
        ((y z) (values 0 0) (values z x)))
       ((> x 5) (list x y z))
       (princ (list x y z)))


(multiple-value-bind (x y) (values 110 12)
  (+ x y))
