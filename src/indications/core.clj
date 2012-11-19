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

(def index-of
  (let [m (atom {})]
    (fn [token]
      (get (swap! m #(assoc % token (or (% token) (inc (count %)))))
           token))))

(defn doids->pmids
  [doids]
  (mapcat #(members (str "doid:" %)) doids))

(defn doids->tokens 
  [doids]
  (map tokens (doids->pmids doids)))

(defn tokens->feature [tokens transform]
  (sort (map (fn [token] [(index-of (key token)) (transform tokens token)]) tokens)))

(defn doids->elements 
  [doids & [depth]]
  (let [doids->levels (into {} (map (fn [doid] {(keyword doid) (ontological-children [doid] depth)}) doids))]
    (into {} (map (fn [[k v]] {k (map (fn [[level doids]] {level (doids->tokens doids)}) v)}) doids->levels))))

;(defn documents
  ;[doids->levels] 
  ;(mapcat (fn [[k v]] (map (fn [x] (flatten (vals x))) v)) doids->levels))

;(defn docs-with-term
  ;[documents]
  ;(fn [term] (filter #(contains? % term) documents)))

;(defn tfidf [documents]
  ;(let [doc# (reduce + (map count documents))
        ;with-term (docs-with-term (flatten documents))
        ;tfidf-fn (tf-idf doc#)]
   ;(fn [abstract term]
    ;(tfidf-fn (get abstract (key term)) (count abstract) (count (with-term (key term)))))))

(defn emit-row [classifier tokens transformer]
  (str classifier " " (strs/join " " (map #(strs/join ":" %) (tokens->feature tokens transformer))) "\n"))

(defn main [file depth doids]
  (create-ontology! (io/resource "../resources/HumanDO.obo") "DOID")
  (let [doids->levels (doids->elements doids depth)
        class# (fn [classifier] (inc (.indexOf doids classifier)))
        transfomer (fn [tokens token] 1.0)]
    (doseq [klass doids->levels]
      (doseq [level (val klass)]
        (doseq [feats level] 
          (with-open [wtr (io/writer (str file "/" (key feats) ".libsvm") :append true)]
          (doseq [feat (val feats)]
              (.write wtr (emit-row (class# (name (key klass))) feat transfomer)) 
              )))))))

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