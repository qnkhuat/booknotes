(defmacro aif
  [test then & else]
  `(let [~'it ~test]
     (if ~'it ~then ~@else)))

(aif true
     1)
;; => 1

(aif false
     1
     2)
;; => 2


(defmacro alet
  [letargs & body]
  `(let [~'this   (atom nil)
         ~@letargs]
     (reset! ~'this ~@body)
     ~@(butlast body)
     ~'this))

(def x (alet [acc (atom 0)]
             (fn [n]
               (if (= n :invert)
                 (let [original @this]
                   (reset! this (fn [n]
                                  (if (= n :invert)
                                    (reset! this original)
                                    (swap! acc (fn [x] (- x n)))))))
                 (swap! acc (fn [x] (+ n x)))))))

(@x 3)
;; => 3
(@x 3)
;; => 6
(@x :invert)
;; => #object[sci.impl.fns$fun$arity_1__3549 0x6f4355c5 "sci.impl.fns$fun$arity_1__3549@6f4355c5"]
(@x 6)
;; => 0
