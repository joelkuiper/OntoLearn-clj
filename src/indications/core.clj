(ns indications.core
  (:require [clojure.java.io :as io]
            [clojure.set :as s]
            [clojure.string :as strs]
            [taoensso.carmine :as car]
            [indications.preprocess.tokenize :as tok])
  (:import java.io.File
           java.lang.String
           java.util.Arrays
           java.util.concurrent.ConcurrentSkipListSet
           (uk.ac.ebi.ontocat OntologyTerm
                              OntologyServiceException
                              OntologyService$SearchOptions
                              ols.OlsOntologyService
                              bioportal.BioportalOntologyService
                              file.FileOntologyService)
           java.net.URLEncoder))

; Set-up redis connection 
(def pool         (car/make-conn-pool)) 
(def spec-server1 (car/make-conn-spec))
(defmacro wcar [& body] `(car/with-conn pool spec-server1 ~@body))

; defn-memo by Chouser:
(defmacro defn-memo
  "Just like defn, but memoizes the function using clojure.core/memoize"
  [fn-name & defn-stuff]
  `(do
     (defn ~fn-name ~@defn-stuff)
     (alter-var-root (var ~fn-name) memoize)
     (var ~fn-name)))

(defn synonyms [ontology accession]
  (seq (. (ontology :service) getSynonyms (ontology :accession) accession)))

(defn-memo accessions [terms] 
  (map #(. % getAccession) terms))

(defn-memo annotations [ontology annotation terms] 
  (seq (first (map #(get % annotation)
                   (seq (map #(. (ontology :service) getAnnotations (ontology :accession) %) terms))))))
(defn mesh-id [term] 
  (wcar (car/hget "mesh" term)))

(defn- abstract [pmid]
  (wcar (car/hget "abstracts" pmid)))

;(defn- mesh-index [ontology]
  ;"Create MESH->DOID map"
  ;(let [all-terms (seq (. (ontology :service) getAllTerms (ontology :accession)))
        ;size (count all-terms)]
    ;(loop [idx 0 mesh->doid (transient {})] 
      ;(if (>= idx size) 
        ;(persistent! mesh->doid)
        ;(let [doid (.getAccession (nth all-terms idx))
              ;xrefs (annotations ontology "xref" [doid])
              ;mapping (into {} (map #(assoc {} % doid) xrefs))]
          ;(recur (inc idx) (conj! mesh->doid mapping)))))))

(defn create-ontology [file id]
  (let [ontology {:service (FileOntologyService.
                             (java.net.URI. 
                               (.toString (io/as-url file))) id)
                  :accession id}]))
    ;(assoc ontology :mesh->doid (mesh-index ontology))))

(def disease-ontology (atom (create-ontology (io/resource "../resources/HumanDO.obo") "DOID")))
(def tokens (atom (ConcurrentSkipListSet.)))

;(defn doids [ontology mesh-terms] 
  ;(let [mesh-ids (filter (comp not nil?) (map (fn [x] (mesh-id x)) mesh-terms))
        ;mesh-matches (into {} (map #(get (ontology :mesh->doid) %) mesh-ids))]
    ;(vec mesh-matches)))

(defn- as-num [bool] 
  (if bool 
    1.0
    0.0))

(defn- add-tokens [string]
  (. @tokens addAll string))

(defn emit-feature-vector [abstract] 
  (loop [words abstract features (transient [])]
    (if (empty? words)
      (sort (persistent! features))
      (recur (rest words) (conj! features [(inc (java.util.Arrays/binarySearch @tokens (first words))) 1.0])))))

(defn emit-row [prefix entry] 
  (if (not (empty? (val entry)))
    (str prefix " " (strs/join " " (map #(strs/join ":" %) (emit-feature-vector (val entry)))) "\n")
    ""))

(defn write-libsvm [data]
  (let [abstracts (data :text)
        index (data :index)
        feat# (fn [entry] (.indexOf (data :feats) (get index (key entry))))]
    (with-open [wtr (io/writer "dataset/data.svm")]
      (doseq [abstract abstracts] (.write wtr (emit-row (feat# abstract) abstract))))))

(defn abstract-tokens [pmid]
  (set (tok/tokenize (wcar (car/hget "abstracts" pmid)))))

(defn reverse-map [m]
  (into {} (map (fn [a] (into {} (map (fn [b] (assoc {} b (key a))) (val a)))) m)))

(defn -main [& args]
  (let [doids (vec args)
        doid->pmids (into {} (map #(assoc {} % (wcar (car/smembers %))) doids))
        pmids->doid (reverse-map doid->pmids) 
        pmids (flatten (vals doid->pmids))
        abstracts (into {} (map (fn [x] (assoc {} x (abstract-tokens x))) pmids))]
    (do 
      (doall (map add-tokens (vals abstracts)))
      (def tokens (atom (.toArray @tokens))) ; nastry redefinition ... will figure out how to do it proper
      (println (str "Processing " (count pmids) " publications with a feature dimensionality of " (count @tokens))) 
      (write-libsvm {:text abstracts :feats doids :index pmids->doid}))
      (shutdown-agents)
    ))