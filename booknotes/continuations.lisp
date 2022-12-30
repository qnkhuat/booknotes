;; contiunation can be understood as a generalization of closures
;; a closure is a function plus pointers to the lexical variables visible at the time it was created
;; a contiunation is a function plus a pointer to the whole satck pending at the time it was created


;; contiunation is built-in in SCHEME but we could make it works in lisp using closure
;; continuation gives us 2 things:
;; - the binding of all variables at the time of continuation was made
;; - the state of computation - what was going to happen from then on

;; example in scheme

;; (define frozen)
;; (append '(the call/cc returned)
;;         (list (call-with-current-continuation
;;                 (lambda (cc)
;;                   (set! frozen cc
;;                     'a)))))
;; => (the call/cc returned a)
;; (frozen 'again)
;; => (the call/cc returned again)
(setq *cont* #'identity)

(defmacro =lambda (parms &body body)
  `#'(lambda (*cont* ,@parms) ,@body))

(defmacro =defun (name parms &body body)
  (let ((f (intern (concatenate 'string "=" (symbol-name name)))))
    `(progn
       (defmacro ,name ,parms
         `(,',f *cont* ,,@parms))
       (defun ,f (*cont* ,@parms) ,@body))))

(defmacro =bind (parms expr &body body)
  `(let ((*cont* #'(lambda ,parms ,@body))) ,expr))

(defmacro =values (&rest retvals)
  `(funcall *cont* ,@retvals))

(defmacro =funcall (fn &rest args)
  `(funcall ,fn *cont* ,@args))

(defmacro =apply (fn &rest args)
  `(apply ,fn *cont* ,@args))


;; tree traversal using continuation

(defun dft (tree)
  (cond ((null tree) nil)
        ((atom tree) (princ tree))
        (t (dft (car tree))
           (dft (cdr tree)))))

(setq *saved* nil)

(=defun dft-node (tree)
  (cond ((null tree) (restart*))
        ((atom tree) (=values tree))
        (t (push #'(lambda () (dft-node (cdr tree)))
                 *saved*)
           (dft-node (car tree)))))

(=defun restart* ()
  (if *saved*
    (funcall (pop *saved*))
    (=values 'done)))

(=defun dft2 (tree)
  (setq *saved* nil)
  (=bind (node) (dft-node tree)
    (cond ((eq node 'done) (=values nil))
          (t (princ node)
             (restart*)))))

(setq t1 '(a (b (d h)) (c e (f i) g))
      t2 '(1 (2 (3 6 7 ) 4 5)))

(dft2 t1)

(dft-node t1)
;; => A
(restart*)
;; => B
(restart*)
;; => D
;; TODO: how to do the append shit like we did in scheme?
