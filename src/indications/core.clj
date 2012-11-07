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
;(def tokens (atom (ConcurrentSkipListSet.)))

;(defn doids [ontology mesh-terms] 
  ;(let [mesh-ids (filter (comp not nil?) (map (fn [x] (mesh-id x)) mesh-terms))
        ;mesh-matches (into {} (map #(get (ontology :mesh->doid) %) mesh-ids))]
    ;(vec mesh-matches)))

(defn- as-float [bool] 
  (if bool 
    1.0
    0.0))

(defn- add-tokens [string]
  (do 
    (wcar (car/multi)
      (doall (map #(car/sadd "tokens" %) (tok/tokenize string)))
      (car/exec))))
  ;(. @tokens addAll (tok/tokenize string)))

(defn fill-tokens 
  "Intensive task of filling the tokens set for the creation of the feature vector" 
  []
  (map add-tokens (wcar (car/hvals "abstracts"))))