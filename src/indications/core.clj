(ns indications.core
  (:use     [indications.database]
            [indications.ontology] 
            [indications.util]
            [clojure.tools.cli :only [cli]])
  (:require [clojure.java.io :as io]
            [clojure.set :as s]
            [clojure.string :as strs]
            [indications.preprocess.tokenize :as tok]
            [indications.import.retrieve :as pubmed]
            [indications.import.process :as process])) 

(def tokens)

(defn emit-feature-vector [abstract] 
  (loop [words abstract features (transient [])]
    (if (empty? words)
      (sort (persistent! features))
      (recur (rest words) (conj! features [(inc (get @tokens (first words))) 1.0])))))

(defn emit-row [prefix entry] 
  (if (not (empty? (val entry)))
    (str prefix " " (strs/join " " (map #(strs/join ":" %) (emit-feature-vector (val entry)))) "\n")
    ""))

(defn write-libsvm [data]
  (let [abstracts (data :text)
        index (data :index)
        feat# (fn [entry] (.indexOf (data :feats) (get index (key entry))))]
    (with-open [wtr (io/writer (data :out))]
      (doseq [abstract abstracts] (.write wtr (emit-row (feat# abstract) abstract))))))

(defn abstract-tokens [pmid]
  (tok/tokenize (abstract pmid)))

(defn pmids [doid depth] 
  (let [get-pmids (fn [doid] (members (str "doid:" doid)))
        elements (get-pmids doid)]
    (if (>= depth 0)
      (flatten (conj (map #(get-pmids %) (children [doid] depth (transient []))) elements))
      elements)))

(defn -main [& args]
  (let [[options args banner] (cli args ["-o" "--file" "File to output the libsvn data" :default "dataset/data.libsvm"]
                                        ["-h" "--help" "Show help" :default false :flag true]
                                        ["-d" "--depth" "Include PMIDs of DOID children till depth n" :default 0 :parse-fn #(Integer. %)]) 
        doids (vec args)
        doid->pmids (into {} (map #(assoc {} % (pmids % (options :depth))) doids))
        pmids->doid (deep-reverse-map doid->pmids) 
        pmids (flatten (vals doid->pmids))
        abstracts (into {} (map (fn [x] (assoc {} x (abstract-tokens x))) pmids))]
    (when (or (:help options) (empty? args))
      (println banner)
      (System/exit 0)) 
    (def tokens (atom (tok/indexed-token-map (vals abstracts))))
    (println (str "Processing " (count pmids) " publications with a feature dimensionality of " (count @tokens))) 
    (write-libsvm {:out (options :file) :text abstracts :feats doids :index pmids->doid}))
  (shutdown-agents))