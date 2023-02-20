(use-package :let-over-lambda)

(set-dispatch-macro-character #\# #\f
                              (lambda (stream sub-char numarg)
                                (declare (ignore stream sub-char))
                                (setq numarg (or numarg 3))

                                (unless (<= numarg 3)
                                  (error "Bad value for #f: ~a" numarg))
                                `(declare (optimize (speed ,numarg)
                                                    (safety ,(- 3 numarg))))))

'#f
;; => (DECLARE (OPTIMIZE (SPEED 3) (SAFETY 0)))

'#2f
;; => (DECLARE (OPTIMIZE (SPEED 2) (SAFETY 1)))



(defun fast-keywords-strip (args)
  (print args)
  (if args
    (cond
      ((eq (car args) '&key)
       (fast-keywords-strip (cdr args)))
      ((consp (car args))
       (cons (caar args)
             #1=(fast-keywords-strip (cdr args))))
      (t
        (cons (car args) #1#)))))

(defmacro! defun-with-fast-keywords
           (name args &rest body)
           `(progn
              (defun ,name ,args ,@body)
              (defun ,g!fast-fun
                ,(fast-keywords-strip args)
                ,@body)
              (compile ',g!fast-fun)
              (define-compiler-macro ,name (&rest ,g!rest)
                                     (destructuring-bind ,args ,g!rest
                                       (list ',g!fast-fun
                                             ,@(fast-keywords-strip args))))))

(defun slow-keywords-test
  (a b &key (c 0) (d 0))
  (+ a b c d))

(compile 'slow-keywords-test) ;; compile to make it faster

(defun-with-fast-keywords fast-keywords-test
                          (a b &key (c 0) (d 0))
                          (+ a b c d))

(defun keywords-benchmark (n)
  (format t "Slow keys: ~%")
  (time
    (loop for i from 1 to n do
          (slow-keywords-test 1 2 :d 3 :c n)))

  (format t "Fast keys: ~%")
  (time
    (loop for i from 1 to n do
          (fast-keywords-test 1 2 :d 3 :c n))))

(compile 'keywords-benchmark)

(keywords-benchmark 100000000)

(disassemble
  (lambda (x) (+ x 3)))
; ; disassembly for (LAMBDA (X))
; ; Size: 28 bytes. Origin: #x7005B3DEC4                        ; (LAMBDA (X))
; ; C4:       EA030CAA         MOV R0, R2
; ; C8:       CB0080D2         MOVZ R1, #6
; ; CC:       960080D2         MOVZ NARGS, #4
; ; D0:       3B9080D2         MOVZ TMP, #1153
; ; D4:       9E6B7BF8         LDR LR, [NULL, TMP]              ; TWO-ARG-+
; ; D8:       C0031FD6         BR LR
; ; DC:       E08120D4         BRK #1039                        ; Invalid argument count trap


(defmacro dis (args &rest body)
  `(disassemble
     (compile nil
              (lambda ,(mapcar (lambda (a)
                                 (if (consp a)
                                   (cadr a)
                                   a))
                               args)
                (declare
                  ,@(mapcar
                      #`(type ,(car a1) ,(cadr a1))
                      (remove-if-not #'consp args)))
                ,@body))))

(macroexpand
  '(dis ((fixnum a) b) (+ a b)))

(dis ((fixnum a) (fixnum b))
     #f
     (+ a b))


;; Pointer scope
(defmacro! pointer-& (obj)
  `(lambda (&optional (,g!set ',g!temp))
     (if (eq ,g!set ',g!temp)
       ,obj
       (setf ,obj ,g!set))))

(defun pointer-* (addr)
  (funcall addr))

(defsetf pointer-* (addr) (val)
  `(funcall ,addr ,val))

(defsetf pointer-& (addr) (val)
  `(setf (pointer-* ,addr) ,val))


(defparameter temp-pointer (let ((x 0))
                             (pointer-& (pointer-& x))))

(setf (pointer-* (pointer-* temp-pointer)) 5)


(dis (arr ind)
     (aref arr ind))

;; tlists and cons pools
(declaim (inlne make-tlist tlist-left tlist-right tlist-empty-p tlist-add-left tlist-add-right))

(defun make-tlist () (cons nil nil))
(defun tlist-left (tl) (caadr tl))
(defun tlist-right (tl) (cadr tl))
(defun tlist-empty-p (tl) (null (car tl)))

(defun tlist-add-left (tl it)
  (let ((x (cons it (car tl))))
    (if (tlist-empty-p tl)
      (setf (cdr tl) x))
    (setf (car tl) x)))

(defun tlist-add-right (tl it)
  (let ((x (cons it nil)))
    (if (tlist-empty-p tl)
      (setf (car tl) x)
      (setf (cddr tl) x))
    (setf (cdr tl) x)))
