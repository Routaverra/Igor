(ns igor.utils.test)

(defmacro throws? [body]
  `(try
     ~body
     false
     (catch Throwable ~'e true)))

(def only-val (comp first vals))
