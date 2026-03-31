(ns routaverra.igor.adapter
  (:require [clojure.core.async :as async]
            [clojure.string :as string])
  (:import [java.io File StringWriter BufferedReader InputStreamReader]))

(defn env [] :clj)

(defn system-text? [x]
  (boolean
   (or (string/includes? x "===")
       (string/includes? x "---")
       (string/includes? x "UNSATISFIABLE"))))

(defn filtering-chan
  ([]
   (filtering-chan identity))
  ([xfn]
   (async/chan
    1
    (comp
     (filter (complement system-text?))
     (map (fn [x]
            (if (string? x)
              ((or xfn identity) x)
              x)))))))

(defmulti call-minizinc
  (fn [env _chan _mzn _all? _opts] env))

(defmethod call-minizinc :clj
  [_env chan mzn all? {:keys [timeout-ms]}]
  (let [temp-file (doto (java.io.File/createTempFile "igor" ".mzn") .deleteOnExit)
        _ (spit temp-file mzn)
        args (cond-> ["minizinc"]
               all?       (conj "-a")
               timeout-ms (conj "--time-limit" (str timeout-ms))
               :always    (conj (.getAbsolutePath temp-file)))
        proc (.exec
              (Runtime/getRuntime)
              (into-array String args))]
    (with-open [stdout (.getInputStream proc)
                out-reader (BufferedReader. (InputStreamReader. stdout))
                stderr (.getErrorStream proc)
                err-writer (StringWriter.)]
      (let [complete? (reduce (fn [complete? line]
                                (async/>!! chan line)
                                (or complete? (string/includes? line "==========")))
                              false
                              (line-seq out-reader))]
        (clojure.java.io/copy stderr err-writer)
        (let [exit (.waitFor proc)
              error (.toString err-writer)]
          (when (not= exit 0)
            (async/>!! chan {::error error})))
        (async/close! chan)
        complete?))))

(defn call-sync
  [all? mzn xfn & {:keys [timeout-ms]}]
  (let [chan (filtering-chan xfn)
        fut (future (call-minizinc (env) chan mzn all? {:timeout-ms timeout-ms}))
        solutions (async/<!!
                   (async/go-loop [solutions []]
                     (if-let [solution (async/<! chan)]
                       (do
                        (when (::error solution)
                          (throw (ex-info (::error solution) {})))
                        (recur (conj solutions solution)))
                       solutions)))
        complete? @fut]
    {:result (if all? solutions (first solutions))
     :complete? complete?}))

(defn call-async
  [all? mzn xfn & {:keys [timeout-ms]}]
  (let [chan (filtering-chan xfn)
        complete? (promise)]
    (future (deliver complete? (call-minizinc (env) chan mzn all? {:timeout-ms timeout-ms})))
    {:chan chan :complete? complete?}))
