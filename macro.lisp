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
