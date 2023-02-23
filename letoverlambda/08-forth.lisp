;; https://letoverlambda.com/lol-orig.lisp
(use-package :let-over-lambda)

(defvar forth-registers
  '(pstack     ;; parameter stack
    rstack     ;; return stack
    pc         ;; program counter
    dict       ;; forth words
    compiling
    dtable))

(defstruct forth-word
  name       ;; used for lookup
  prev       ;; linked list to the previous forth-word
  immediate  ;; boolean on whether this word is immediate or not,
             ;; an immediate word is executed at compile-time instead of run-time
  thread)

(defun forth-lookup (w last)
  (if last
    (if (eql (forth-word-name last) w)
      last
      (forth-lookup
        (forth-word-prev last)))))


(defmacro forth-inner-interpreter ()
  `(loop
     do (cond
          ((functionp (car pc))
           (funcall (car pc)))
          ((consp (car pc))
           (push (cdr pc) rstack)
           (setf pc (car pc)))
          ((null pc)
           (setf pc (pop rstack)))
          (t
             (push (car pc) pstack)
             (setf pc (cdr pc))))
     until (and (null pc) (null rstack))))
