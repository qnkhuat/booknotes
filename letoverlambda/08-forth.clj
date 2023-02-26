;; registers
;; NOTE: It's important for pstack, rstack, pc to be an vector, not a list because conj to a vector will add to last,
;; but add to first for list. these are used to simulat Lisp Conscell
;; wonder if we should use this? https://gist.github.com/Solaxun/2e010f8e14c87456f243c0b972e69d1f

(defprotocol ConsCellP
  "Adds a prefix for this method because cons is a preserved keyword in clojure."
  (l-cons [this x] "Add to the car of ConsCell")
  (l-car [this])
  (l-cdr [this]))

(deftype ConsCell [head tail]
  ConsCellP
  (l-car [this x]))


(def pstack    (atom []))  ;; parameter stack
(def rstack    (atom []))  ;; return stack
(def pc        (atom []))  ;; program counter, it could contains list of list
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

(defn l-pop!
  "Remove the first element from an atom list then returns it.

  Used to simulate the pop in lisp"
  [x]
  (let [f (first @x)]
    (swap! x rest)
    f))

(defn l-push-last!
  "Push an element to the end of the vector.

  Used to simulate the push in lisp"
  [x e]
  (swap! x conj e))

(defn l-push-first!
  "Push an element to the end of the vector.

  Used to simulate the push in lisp"
  [x e]
  (swap! x #(cons e %)))

(defmacro froth-inner-interpreter
  []
  `(loop [pc*     (first @pc)
          rstack* (first @rstack)]
     (cond
       (fn? (first pc*))
       ;; if is a function, execute it, notice it's up to the function
       ;; to update the pc register
       ((first pc*))

       ;; if this is a cons threaded code, it's assumed that this is a
       ;; subroutine call -- a word invocation
       (seq? (first pc*))
       (do
         (l-push-last! pc @rstack)
         ()))))

