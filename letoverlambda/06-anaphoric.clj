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

(def x (alet [accc (atom 0)]
             (fn [n]
               (if (= n :invert)
                 (let [original @this]
                   (reset! this (fn [n]
                                  (if (= n :invert)
                                    (reset! this original)
                                    (swap! accc (fn [x] (- x n)))))))
                 (swap! accc (fn [x] (+ n x)))))))

(@x 3)
