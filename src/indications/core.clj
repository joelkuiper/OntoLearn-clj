(ns indications.core
  (:gen-class)
  (:use [indications.database]
        [indications.ontology] 
        [indications.util]
        [indications.preprocess.tfidf]
        [clojure.tools.cli :only [cli]])
  (:require [clojure.java.io :as io]
            [clojure.set :as s]
            [clojure.string :as strs]
            [indications.preprocess.tokenize :as tok]
            [indications.import.retrieve :as pubmed]
            [indications.import.process :as process])) 

(defn-memo abstract-tokens [pmid] 
  (tok/tokenize (abstract pmid)))

(defn tokens [pmid]
  (tok/token-count (abstract-tokens pmid)))

(def index-of
  (let [m (atom {})]
    (fn [token]
      (get (swap! m #(assoc % token (or (% token) (inc (count %))))) token))))

(defn tokens->feature [tokens transform]
  (sort (map (fn [token] [(index-of (key token)) (transform tokens token)]) tokens)))

(defn emit-row [class tokens transformer]
  (if ((comp not empty?) tokens)
    (str class " " (strs/join " " (map #(strs/join ":" %) (tokens->feature tokens transformer))) "\n")
    ""))

(defn pmids [doids]
  (mapcat members doids))

(defn main [file depth doids]
  (create-ontology! (io/resource "../resources/HumanDO.obo") "DOID")
  (let [class# (fn [classifier] (inc (.indexOf doids classifier)))
        documents (map #(set (abstract-tokens %)) (pmids (flatten (vals (ontological-children doids depth)))))
        documents# (count documents)
        term-in-documents# (memoize (fn [term] (count (filter #(contains? % term) documents))))
        transformer (fn [tokens token] (tf-idf (get tokens (key token)) (count tokens) (term-in-documents# (key token)) documents#))]
    (doseq [doid doids]
      (let [levels (ontological-children [doid] depth)
            levels->pmids (into {} (map (fn [[k v]] {k (pmids v)}) levels))]
        (doseq [[level pmids] levels->pmids] 
            (println (str doid " @ " level " with " (count pmids)))
            (with-open [wtr (io/writer (str file "/" level ".libsvm") :append true)]
              (doall (map (fn [pmid] (.write wtr (emit-row (class# (name doid)) (tokens pmid) transformer))) pmids))))))))

(defn -main [& args]
  (let [[options args banner] (cli args ["-o" "--file" "Directory to output the libsvn data without trailing slash" :default "dataset/libsvm"]
                                   ["-h" "--help" "Show help" :default false :flag true]
                                   ["-d" "--depth" "Include PMIDs of DOID children till depth n" :default 0 :parse-fn #(Integer. %)]) 
        doids (vec args)]
    (when (or (:help options) (empty? args))
      (println banner)
      (System/exit 0))
    (main (options :file) (options :depth) doids)
    (shutdown-agents)))