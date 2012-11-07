(ns indications.core
  (:require [clojure.java.io :as io]
            [clojure.set :as s]
            [clojure.string :as strs]
            [clj-tokenizer.core :as tok]
            [taoensso.carmine :as car])
  (:import  java.io.File
            java.lang.String
            java.util.Arrays
            java.util.concurrent.ConcurrentSkipListSet))

; Set-up redis connection 
(def pool         (car/make-conn-pool)) 
(def spec-server1 (car/make-conn-spec))
(defmacro wcar [& body] `(car/with-conn pool spec-server1 ~@body))

(def tokens (atom (ConcurrentSkipListSet.)))

(defn- tokenize [string] 
  (tok/token-seq (tok/token-stream-without-stopwords string)))

(defn- abstract [pmid]
  (wcar (car/hget "abstracts" pmid)))

(defn- add-tokens [string]
  (. @tokens addAll (tokenize string)))

(defn- fill-tokens []
  (do 
    (doall (pmap (fn [abstr] (add-tokens abstr)) (wcar (car/hvals "abstracts"))))
    nil))

(defn- as-float [bool] 
  (if bool 
    1.0
    0.0))

(defn emit-feature-vector [string] 
  (let [feature-vec (set (tokenize string))
        features    (vec @tokens)]
    (for [f features] (str (.indexOf features f) ":" (as-float (contains? feature-vec f))))))

(defn- emit-row [entry] 
  (let [abstracts (map abstract (val entry))
        klass     (key entry)
        row       (dorun (for [row abstracts] (str klass " " (emit-feature-vector row))))]
    (strs/join " " row)))

(defn write-svmlight [entries] 
  (with-open [wtr (io/writer (io/file "datasets/" "data.svm"))]
    (doseq [entry entries] (.write wtr (emit-row entry)))))  

(defn -main [& args]
  (let [diseases        (wcar (car/smembers "diseases"))
        disease->pmids  (zipmap diseases (map #(wcar (car/smembers %)) diseases))]
    (do
      (println "Filling tokens")
      (fill-tokens)
      (println (str "Imported " (.size @tokens) " tokens"))
      (println "Writing files")
      (write-svmlight disease->pmids)
      nil)))