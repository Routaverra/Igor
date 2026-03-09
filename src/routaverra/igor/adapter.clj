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
  (fn [env _chan _mzn _all?] env))

(defmethod call-minizinc :clj
  [_env chan mzn all?]
  (let [temp-file (doto (java.io.File/createTempFile "igor" ".mzn") .deleteOnExit)
        _ (spit temp-file mzn)
        args (cond-> ["minizinc"]
               all? (conj "-a")
               :always (conj (.getAbsolutePath temp-file)))
        proc (.exec
              (Runtime/getRuntime)
              (into-array String args))]
    (with-open [stdout (.getInputStream proc)
                out-reader (BufferedReader. (InputStreamReader. stdout))
                stderr (.getErrorStream proc)
                err-writer (StringWriter.)]
      (doseq [line (line-seq out-reader)]
        (async/>!! chan line))
      (clojure.java.io/copy stderr err-writer)
      (let [exit (.waitFor proc)
            error (.toString err-writer)]
        (when (not= exit 0)
          (async/>!! chan {::error error})))
      (async/close! chan))))

(defn call-sync
  [all? mzn xfn]
  (let [chan (filtering-chan xfn)
        _ (future (call-minizinc (env) chan mzn all?))
        solutions (async/<!!
                   (async/go-loop [solutions []]
                     (if-let [solution (async/<! chan)]
                       (do
                        (when (::error solution)
                          (throw (ex-info (::error solution) {})))
                        (recur (conj solutions solution)))
                       solutions)))]
    (if all? solutions (first solutions))))

(defn call-async
  [all? mzn xfn]
  (let [chan (filtering-chan xfn)]
    (future (call-minizinc (env) chan mzn all?))
    chan))
