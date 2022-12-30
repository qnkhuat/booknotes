;; https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node90.html

(defun inc
  (x)
  (+ 1 x))

(defparameter x '(1 2 3))

(defparameter y '(a b))

(mapcar #'cons x y) ;; map with car of the argument lists
;; => ((1 . A) (2 . B))

(maplist #'cons x y) ;; map with car of the argument lists
;; => (((1 2 3) A B) ((2 3) B))

(mapcan #'cons x y) ;; like mapcar but concatnate results with nconc
;; => (1 2 . B)

;; don't run this because it'll stackoverflow
(mapcon #'cons x y) ;; like maplist but concatnate results with nconc
