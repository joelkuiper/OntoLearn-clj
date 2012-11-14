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

(defn tokens [pmid]
  (tok/token-count (tok/tokenize (abstract pmid))))

(defn pmids [doid depth] 
  (let [get-pmids (fn [doid] (members (str "doid:" doid)))
        elements (get-pmids doid)]
    (flatten (cond (>= depth 0) (conj (mapcat #(get-pmids %) (ontological-children [doid] depth)) elements)
          (<= depth 0) (conj (mapcat #(get-pmids %) (ontological-parents [doid] depth)) elements)
          :else elements))))

(defn doids->pmids [doids & [depth]]
  (reduce (fn [map key] (assoc map key (pmids key depth))) {} doids))

(defn pmids->tokens [pmids] 
  (reduce (fn [map key] (assoc map key (tokens key))) {} pmids))

(defn doids->tokens [doids & [depth]]
  (map (doids->pmids [doids depth])

(defn-memo token-in-pmids# [token-sets token]
  (reduce (fn [acc tokens] (if (contains? tokens token) (inc acc)) acc) 0 token-sets))

(defn -main [& args]
  (let [[options args banner] (cli args ["-o" "--file" "File to output the libsvn data" :default "dataset/data.libsvm"]
                                   ["-h" "--help" "Show help" :default false :flag true]
                                   ["-d" "--depth" "Include PMIDs of DOID children till depth n" :default 0 :parse-fn #(Integer. %)]) 
        doids (vec args)]
    (when (or (:help options) (empty? args))
      (println banner)
      (System/exit 0))))