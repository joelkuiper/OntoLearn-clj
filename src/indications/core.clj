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

(def tokens)
(def -tf-idf)

(defn feature-vector [abstract] 
  (loop [words abstract features (transient [])]
    (if (empty? words)
      (sort (persistent! features))
      (let [entry (first words)
            word (key entry)
            docs-with-term# (:count (@tokens word))
            occ# (val entry)
            token# (count abstract)]
        (recur (rest words) (conj! features [(inc (:index (@tokens word))) (-tf-idf occ# token# docs-with-term#)]))))))

(defn emit-row [prefix entry] 
  (let [tokens (val entry)]
    (if (not (empty? (keys tokens)))
      (str prefix " " (strs/join " " (map #(strs/join ":" %) (feature-vector tokens))) "\n")
      "")))

(defn write-libsvm [data]
  (let [abstracts (data :text)
        index (data :index)
        feat#s (fn [entry] (map #(inc (.indexOf (data :feats) %)) (get index entry)))]
    (with-open [wtr (io/writer (data :out))]
      (doseq [abstract abstracts] 
        (doseq [feat# (feat#s (key abstract))] 
          (.write wtr (emit-row feat# abstract)))))))

(defn abstract-tokens [pmid]
  (tok/token-count (tok/tokenize (abstract pmid))))

(defn pmids [doid depth] 
  (let [get-pmids (fn [doid] (members (str "doid:" doid)))
        elements (get-pmids doid)]
    (cond (>= depth 0) (flatten (conj (map #(get-pmids %) (ontological-children [doid] depth)) elements))
          (<= depth 0) (flatten (conj (map #(get-pmids %) (ontological-parents [doid] depth)) elements))
          (== depth 0) elements)))

(defn abstracts-as-tokens
  [pmids]
  (loop [ids pmids acc (transient {})]
    (if (empty? ids)
      (persistent! acc)
      (recur (rest ids) (assoc! acc (first ids) (abstract-tokens (first ids)))))))

(defn -main [& args]
  (let [[options args banner] (cli args ["-o" "--file" "File to output the libsvn data" :default "dataset/data.libsvm"]
                                   ["-h" "--help" "Show help" :default false :flag true]
                                   ["-d" "--depth" "Include PMIDs of DOID children till depth n" :default 0 :parse-fn #(Integer. %)]) 
        doids (vec args)
        doid->pmids (into {} (map (fn [doid] [doid (pmids doid (options :depth))]) doids))
        pmids->doid (deep-invert-map doid->pmids) 
        pmids (keys pmids->doid)
        abstracts (abstracts-as-tokens pmids)]
    (when (or (:help options) (empty? args))
      (println banner)
      (System/exit 0)) 
    (do
      (println "Intializing token map and preprocessing data") 
      (def tokens (atom (tok/indexed-token-map (vals abstracts))))
      (def -tf-idf (tf-idf (count pmids)))
      (println (str "Processing " (count pmids) " publications with a feature length of " (count @tokens)))
      (doall (map (fn [doid] (println (str "Feature " doid " with " (count (doid->pmids doid)) " abstracts"))) doids))
      (write-libsvm {:out (options :file) :text abstracts :feats doids :index pmids->doid})))
  (shutdown-agents))