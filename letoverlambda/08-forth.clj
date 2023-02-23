;; registers
(def pstack    (atom []))  ;; parameter stack
(def rstack    (atom []))  ;; return stack
(def pc        (atom []))  ;; program counter
(def dict      (atom {}))  ;; forth words
(def compiling (atom nil))
(def dtable    (atom nil)) ;; we'll see what this is

(defrecord ForthWord [name prev immediate thread])


(defn forth-lookup
  [w last]
  (when last
    (if (= (.name last) w)
      last
      (forth-lookup w (.prev last)))))

(defmacro froth-inner-interpreter
  []
  `(loop [pc*     (first @pc)
          rstack* (first @rstack)]
     (cond
       (fn? (first pc*))
       ;; execute it
       ((first pc*))

       (seq? (first pc*))
       (do))))


