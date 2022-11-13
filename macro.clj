(defmacro with-gensym
  [bind & body]
  `(let [~bind (gensym ~bind)]
     ((fn [~bind] ~@body) ~bind)))

(with-gensym a
  (println a))

(macroexpand-1 '(with-gensym a
                  (println a)))

(let [a (gensym 'a)]
  (println a))

(println (gensym 'a))


(defmacro let-gensym
  [[bind value] & body]
  `(let [~bind# ~value]
     ((fn [bind#]
        ~@body) ~bind#)))

(let [a# 1]
  (println a#))


(defmacro my-let
  [binds & body]
  `(let [pair#  (partition 2 ~binds)
         binds  (map pair# first)
         values (map pair# second)]
     ((fn [@binds]
       ~@body) @values)))

(defn get-bind
  [x]
  (first x))


(defmacro my-let
  [binds & body]
  `((fn ~(mapv first binds) ~@body)
    ~@(mapv second binds)))


(my-let [[a 1]
         [b 2]]
        (println a b))
