;; setf and how to implements it

(setq lst '(a b c))
;; => (A B C)

lst
;; => (A B C)

(setf (car lst) 420)
;; => 420

(car lst)

lst
;; => (420 B C)

(setf (cdr lst) 420)
;; => 420

lst
;; => (420 . 420)

(cdr lst)
;; => 420


;; (setf x y) => see to it that `x` evaluates to `y`

(setq a 1)
;; => 1

(car a)
;; fail to execute

(setf (car a) 123)
;; will also fail. remember that `setf` is a macro. how does it knows this will fail at construction time?
