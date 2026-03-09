(ns routaverra.igor.utils.string)

(def rx #"\{\{[a-z-]*\}\}")

(defn >> [env string]
  (->> string
       (re-seq rx)
       (map (partial drop-last 2))
       (map (partial drop 2))
       (map (partial apply str))
       (map (fn [s] (get env (keyword s) "")))
       (apply format (clojure.string/replace string rx "%s"))))
