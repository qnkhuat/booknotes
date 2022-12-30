(defun my-double (x) (* x 2))
;; => MY-DOUBLE

(my-double 4)
;; => 8

#'my-double
;; => #<FUNCTION MY-DOUBLE>

(eq #'my-double (car (list #'my-double)))
;; => T

(defparameter my-double 2)
(symbol-value 'my-double)
;; => 2

(symbol-function 'my-double)
;; => #<FUNCTION MY-DOUBLE>

;; ^ is to illustrate that lisp has different ns for functions and variable.
;; so you can use one name but for variable and functions seperately


(setq x #'append)
(eq (symbol-value 'x) (symbol-function 'append))
;; => T

(apply #'+ 1 2 '(3))
;; => 6

(funcall #'+ 1 2 3)
;; => 6

(mapcar (lambda (x) (+ x 10)) '(1 2 3))
;; => (11 12 13)

;; SCOPE
;; CL uses lexical scoped.

(let ((x 3))
  (defun do-add (y)
    (+ x y))
  (do-add 1))
;; => 4

(let ((x 4))
 (do-add 1))
;; => 4 returns 4 because CL is lexical scoped.
;; in a dynamically scoped language, this will returns 5 instead

;; => this property will be come handy for Closures

;; CLOSURES
(defun make-dbms (db)
  (list
    (lambda (key)
            (cdr (assoc key db)))
    (lambda (key val)
            (push (cons key val) db) key)
    (lambda (key)
            (setf db (delete key db :key car)) key)))

(setq cities (make-dbms '((boston . us) (paris . france))))

(funcall (car cities) 'boston)
;; => US

(funcall (second cities) 'london 'england)
;; => LONDON

(funcall (car cities) 'london)
;; => ENGLAND

;; unrelated to closures but seems like setq create a mutable variable


;; lisp has `labels` syntax that works similart to let-fn

(labels ((inc-one (x) (+ 1 x)))
  (inc-one 3))
;; => 4

(progn
  (+ 1 2)
  (+ 3 4))


(defun 50th (lst) (nth 49 lst))

;; INLINE DEF
;; define this function to be compiled inline
(proclaim '(inline 50th))

;; an inline function will not requires a function call to call itself
;; If another function call it, that function will becompiled with the comiled function of the inline functions

(defun foo (lst)
 (+ (50th lst) 1))

;; The code is sent to compile will be:
(defun foo (lst)
  (+ (nth 49 lst) 1))

(defun bad-reverse (lst)
  (let* ((len (length lst))
         (ilimit (truncate (/ len 2))))
    (do ((i 0 (1+ i))
         (j (1- len) (1- j))) ((>= i ilimit))
      (princ (format "i: %d, j: %d", i, j))
     (rotatef (nth i lst) (nth j lst)))))



;; MUTIPLE RETURN VALUES
(truncate 25.5)
;; => 25 0.5

(= (truncate 25.5) 25)
;; => T
(= (truncate 25.5) (values 25 .5))
;; => T

(multiple-value-bind (int frac) (truncate 25.5)
  (princ (= int 25))
  (print (= frac .5)))
;; => T
;; => T


'(a b . c)

(nconc nil  '(1 2 3) '(3 4 5))

(car '(1 2 . 2))


(defun qualify (expr)
  (nconc expr (list 'sup)))

(let ((x '(1 2 3)))
  (append x '(123))
  x)


(defvar inc (lambda (x) (+ 1 x)))

(funcall inc 3)

(last '(1 2 3))

(defun last1 (lst)
  (car (last lst)))

(last1 '( 1 2 3))

(member 4 '(1 2 4 3))


(do ((x '(1 2 3)))
  (print x))

(do ((x 0 (+ 2 x))
     (y 20 ( - y 2)))
   ((= x y) (- x y))
   (format t "~% x = ~d  y = ~d" x y))

