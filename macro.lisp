(defmacro nil! (var)
  (list 'setq var nil))

;; comma in LISP has a special meaning when use with quote
;; `(a b c) =>  (list 'a 'b 'c)
;; `(a, b c) => (list 'a b 'c)
;; basically a comma in side a quoted expressino means: turn off the quoting
;; it's like the ~ symbol in C:ojure

;; redefine the nil! macro with backquote
(defmacro nil! (var)
  `(setq ,var nil))

(setq a 1)

(nil! a)
a

(defvar b)
'(a ,b c)

;; `@ in lisp is ~@ in clojure

(defmacro our-when
  (test &body body)
  `(if ,test
     (progn
       ,@body)))

(our-when t
          (print 1)
          (print 2))


;; EXERCISE : writes a macro that expand to a member call
;; (memq 3 '(1 2 3)) =>
(member 3 '(1 2 3) :test #'eq)
;; => (3)

(defmacro memq
  (x l)
  `(member ,x ,l :test #'eq))

(memq 3 '(1 2 3))
;; => (3)

(macroexpand-1 '(memq 3 '(1 2 3)))
;; => (MEMBER 3 '(1 2 3) :TEST #'EQ)
(pprint (macroexpand '(memq 3 '(1 2 3))))
;; => (MEMBER 3 '(1 2 3) :TEST #'EQ)


;; DESTRUCTURING

;; there is a intereting macro called dolist that make it easier to loop through a list with a var binding
(dolist (x '(1 2 3))
  (print x))


(defmacro our-dolist
  ((var list &optional result) &body body)
  `(progn
     (mapc #'(lambda (,var) ,@body)
           ,list)
     (let ((,var nil))
       ,result)))


(our-dolist
  (x '(1 2 3))
  (print x))


;; A cruel definition of `defmacro`

(defmacro our-expander (name) `(get ,name 'expander))

(defmacro our-defmacro
  (name params &body body)
  (let ((g (gensym)))
    `(progn
       (setf (our-expander ',name)
             #'(lambda (,g)
                 (block ,name
                        (destructuring-bind ,params (cdr ,g)
                          ,@body))))
       ',name)))

(defun our-defmacroexpand-1 (expr)
  (if (and (consp expr) (our-expander (car expr)))
    (funcall (our-expander (car expr)) expr)
    expr))

(defun inc
  (x)
  (+ 1 x))

(our-defmacro
  our-add-1
  (x y)
  `(+ (inc ,x) (inc ,y)))


(our-add-1 3 4)


(let ((a 1))
  (setq a 2 b a)
  (list a b))
;; => (2 2)


(let ((a 1))
  (psetq a 2 b a)
  (list a b))
;; => (2 1)
;; psetq is different form setq in that psetq set things in parallel
;; this psetq is primarily used to write macro `do`

(defmacro our-do (bindforms (test &rest result) &body body)
  (let ((label (gensym)))
    `(prog ,(make-init-forms bindforms)
           ,label
           (if ,test
             (return (progn ,@result)))
           ,@body
           (psetq ,@(make-stepforms bindforms))
           (go ,label))))

(defun make-init-forms (bindforms)
  (mapcar #'(lambda (b)
              (if (consp b)
                (list (car b) (cadr b))
                (list b nil)))
          bindforms))


(defun make-stepforms (bindforms)
  (mapcan #'(lambda (b)
              (if (and (consp b) (third b))
                (list (car b) (third b))
                nil))
          bindforms))

(do ((x 0 (+ x 1)))
  ((= x 3) "FOUND IT")
  (print x))


;; some note about prog and its variation
(progn
  (print 1)
  (print 2)
  2)
;; => 2
;; `progn` works like `do` in clojure, it returns the last expression, Thus the `n`
(prog1
  1
  (print 1)
  (print 2)
  2)
;; => 1
;; `prog1` and prog2 returns the 1st and second expressions respectively

(prog ((x 0))
      redo (if (= 3 x)
             (return "DONE"))
      (print x)
      (setq x (+ x 1))
      (print x)
      (go redo))

;; prog is a bit special, it supports the return and labeling

(mapcar #'(lambda (x) (+ 1 x)) '(2 3 4))

;; one note with dependence on Macros is that when we re-define a macro, any functions that were expanded with the old macro will stay the same. If we want those to be re-defined ,we need to re-evaluate it.



(defmacro
  do-if (x yes no)
  `(if ,x
     ,yes
     ,no))

(do-if nil
       (print "YES")
       (print "NO"))

(defmacro simple-add
  (x)
  (+ 1 x))

(macroexpand '(do-if nil
                     (simple-add 3)
                     (simple-add 4)))




;; TECHNIQUE TO AVOID VARIALBE CAPTUREs
;; variable captures is when a macro unintentionally use variables that outside of its scoped environment
;; this for has `limit` as a caputrable variable
(defmacro for ((var start stop) &body body)
  `(do ((,var ,start (1+ ,var))
        (limit ,stop))
     ((> ,var limit))
     ,@body))

;;(for (limit 1 10)
;;     (princ limit))


;; TECHNIQUE 0: avoid captureable by prior evaluation
;; Don't fully understand this so you should look it up
;; TECHNIQUE 1: avoid captureable by using closure
(defmacro for ((var start stop) &body body)
  `(do ((b #'(lambda (,var) ,@body))
        (count ,start (1+ count))
        (limit ,stop))
     ((> count limit))
     (funcall b count)))
;; this has one drawback of introducing overhead with funcall though


;; TECHNIQUE 2: using Gensyms
(defmacro for ((var start stop) &body body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
          (,gstop ,stop))
       ((> ,var ,gstop)) ,@body)))
;; TECHNIQUE 3: Avoiding Capture with Packages
;; so if you define a macro in its own namespace
;; then the `for` under this is porbably still be fine because
;; the `limit` variable is under the `your-ns/limit`. So unless
;; you (let ((your-ns/limit 1))) then it's a problem.
;; But looks like you won't do that, will you?
(defmacro for ((var start stop) &body body)
  `(do ((,var ,start (1+ ,var))
        (limit ,stop))
     ((> ,var limit))
     ,@body))



;;----------------- Macro pitfalls ---------------------
